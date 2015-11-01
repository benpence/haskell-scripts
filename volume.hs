{-# LANGUAGE OverloadedStrings #-}

{-| Modify or print the volume, using the `amixer` command.

    Refreshes tmux if it is displaying the volume.
    Creates a notification announcing the new volume using `notify`.
-}

import Data.Optional (Optional)
import Data.Optional (Optional(Specific))
import Data.String (fromString)
import qualified Data.Text as Text
import System.Environment (getArgs)
import Turtle

data Command = Up Int | Down Int | Toggle | Print deriving (Eq, Show)
type Volume = Text

main = sh $ do
    command <- options "Modify or print the volume" argsParser
    changeVolume command

    -- TODO: Handle no output
    output <- amixerOutput
    volume <- case match signedVolumePattern output of
        [volume] -> pure volume
        _        -> die "Unable to parse amixer output"
    
    announceVolumeChanges command volume
    printVolume command volume

argsParser :: Parser Command
argsParser = Up           <$> (argConst "up"          (Specific "Increase the volume")
                           *>  argInt   "up_amount"   (Specific "Increase by percentage [1-100]") )
         <|> Down         <$> (argConst "down"        (Specific "Decrease the volume")
                           *>  argInt   "down_amount" (Specific "Decrease by percentage [1-100]") )
         <|> const Toggle <$>  argConst "toggle"      (Specific "Toggle sound mute")
         <|> const Print  <$>  argConst "print"       (Specific "Print the volume")

changeVolume :: Command -> Shell Text
changeVolume (Up   amt) = run ("amixer -q sset Master,0 " <> showS amt <> "%+")
changeVolume (Down amt) = run ("amixer -q sset Master,0 " <> showS amt <> "%-")
changeVolume Toggle     = run  "amixer -q sset Master,0 toggle"
changeVolume Print      = empty

announceVolumeChanges :: Command -> Volume -> Shell ()
announceVolumeChanges Print _      = empty
announceVolumeChanges _     volume = do
    -- Create desktop notification
    run ("notify volume " <> volume)
    -- Update tmux
    run "tmux refresh -S"
    empty

printVolume :: Command -> Volume -> Shell ()
printVolume Print volume = echo volume
printVolume _     _      = empty

amixerOutput :: Shell Text
amixerOutput = run "amixer get Master,0"

signedVolumePattern :: Pattern Volume
signedVolumePattern = do
    count 4 (chars1 <> spaces1)

    volume <- "[" *> decimal <* "%]"

    spaces1

    muted <- "[" *> ("on" <|> "off") <* "]"

    pure $ showS volume <> if muted == "off" then "+" else "-"

argConst :: Text -> Optional HelpMessage -> Parser Text
argConst text = arg (\t -> if t == text then Just t else Nothing) (fromString (Text.unpack text))

showS :: (Show a, IsString b) => a -> b
showS = fromString . show

run :: Text -> Shell Text
run cmd = inshell cmd empty
