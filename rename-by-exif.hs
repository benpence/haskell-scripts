{-# LANGUAGE OverloadedStrings #-}

{-| Renames files from stdin using a timestamp in the exif data.

    Using the third-party command 'exiftool', we examine an exif field:

> $ exiftool imgs/* -"CreateDate"
> ======== imgs/IMG_3323.jpg
> Create Date                     : 2011:10:29 15:08:36
> ======== imgs/IMG_3324.jpg
> Create Date                     : 2011:10:29 15:08:36
> ======== imgs/IMG_3325
> Create Date                     : 2011:10:29 15:09:01

    Now, let's rename them with `rename-by-exif`

> $ ls -d imgs/* | rename-by-exif CreateDate
> mv './imgs/IMG_3323.jpg' ./imgs/2011-10-29_15:08:36_1.jpg'
> mv './imgs/IMG_3324.jpg' ./imgs/2011-10-29_15:08:36_2.jpg'
> mv './imgs/IMG_3325' './imgs/2011-10-29_15:09:01_1'

    Files that do not have the specified field or have an unparsable timestamp
    value will be ignored.
-}

import Control.Applicative (empty)
import Control.Applicative (liftA3)
import Control.Monad (foldM)
import Control.Monad.Trans (liftIO)
import Data.Maybe (listToMaybe)
import Data.Optional (Optional(Default))
import qualified Data.Text as Text
import Filesystem.Path (FilePath)
import Filesystem.Path (directory)
import Filesystem.Path (extension)
import qualified Filesystem.Path.CurrentOS as FilePath
import Prelude hiding (FilePath)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Turtle

data Timestamp = Timestamp
    { tsYear   :: Int
    , tsMonth  :: Int
    , tsDay    :: Int
    , tsHour   :: Int
    , tsMinute :: Int
    , tsSecond :: Int
    } deriving (Eq, Show, Ord)

newtype ExifField = ExifField { getExifField :: Text } deriving (Show, Eq)

description :: Description
description =
    "For each filename in stdin, rename it by the timestamp in the exifField.\n\
    \Format: 'YYYY-MM-DD_hh:mm:ss_count.extension'\n\
    \Requires exiftool be available on path"

main = sh $ do
    exifField  <- options description exifArg
    filePath   <- FilePath.fromText <$> stdin
    exifOutput <- exiftool exifField filePath

    timestamp  <- case parseExiftoolOutput exifOutput of
        Just ts -> pure ts
        _       -> empty

    renameFile timestamp filePath

exifArg :: Parser ExifField
exifArg = ExifField <$> argText "exifField" Default

filePathToText :: FilePath -> Text
filePathToText = either id id . FilePath.toText

-- | Rename the file based on timestamp
renameFile :: Timestamp -> FilePath -> Shell ()
renameFile ts src = do
    let ext = maybe "" ("." <>) (extension src)
    let srcDir = directory src
    dest <- liftIO $ countUntilNewFile (formatTimestamp ts srcDir ext)
    mv src dest
    echo $ "mv '" <> filePathToText src <> "' '" <> filePathToText dest <> "'"

-- | Count until we find a file
countUntilNewFile :: (Int -> FilePath) -> IO FilePath
countUntilNewFile = countUntilNewFile' 1

countUntilNewFile' :: Int -> (Int -> FilePath) -> IO FilePath
countUntilNewFile' i formatter = do
    let filePath = formatter i
    doesExist <- doesFileExist (Text.unpack $ filePathToText filePath)
    if doesExist
       then countUntilNewFile' (succ i) formatter
       else pure filePath

exiftool :: ExifField -> FilePath -> Shell Text
exiftool (ExifField exifField) filePath =
  let
    -- Example command: exiftool -"FileModifyDate" my_image.jpg
    command = format
        ("exiftool -'" % s % "' '" % s % "'")
        exifField
        (filePathToText filePath)
  in inshell command empty

-- | TODO: This may erroneously succeed if `exiftool` starts to output >1 records
parseExiftoolOutput :: Text -> Maybe Timestamp
parseExiftoolOutput = listToMaybe . match exiftoolTimestampPattern

-- | Example input: File Modification Date/Time     : 2014:08:02 10:49:54-07:00
exiftoolTimestampPattern :: Pattern Timestamp
exiftoolTimestampPattern = do
    plus (notChar ':') *> ":" *> spaces1

    let trio = liftA3 (,,) (decimal <* ":") (decimal <* ":") decimal
    (year, month, day)  <- trio

    spaces1

    (hour, minute, second)  <- trio
    option $ skip $ "-" *> decimal *> ":" *> decimal

    pure $ Timestamp year month day hour minute second

formatTimestamp :: Timestamp
                -> FilePath  -- ^ The directory that contains the file
                -> Text      -- ^ The extension
                -> Int       -- ^ This is the 'nth' file with the same timestamp
                -> FilePath
formatTimestamp (Timestamp yr mo da ho mi se) dir ext count =
  let
    tShow = Text.pack . show
    dd = makeFormat (\i -> if i > 9 then tShow i else "0" <> tShow i)
  in dir <> (FilePath.fromText $ format
      (d % "-" % dd % "-" % dd % "_" % dd % ":" % dd % ":" % dd % "_" % d %   s)
       yr        mo         da         ho         mi         se         count ext)
