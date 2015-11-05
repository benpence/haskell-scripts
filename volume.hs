{-# LANGUAGE OverloadedStrings #-}

{-| Modify or print the volume, using the `amixer` command.

    Refreshes tmux if it is displaying the volume.
    Creates a notification announcing the new volume using `notify`.
-}

import Data.String (fromString)
import Turtle

data Command = Up Int | Down Int | Toggle | Print deriving (Eq, Show)
type Volume = Text

main = do
    command <- options "Modify or print the volume" argsParser
    changeVolume command

    -- TODO: Handle no output
    output  <- amixerOutput
    volume  <- case match signedVolumePattern output of
        [left, _] -> pure left
        _         -> die "Unable to parse amixer output"
    
    announceVolumeChanges command volume
    printVolume command volume

argsParser :: Parser Command
argsParser = Up     <$> optInt "up"     'u' "Increase the volume by percentage [1 to 100]"
         <|> Down   <$> optInt "down"   'd' "Decrease the volume by percentage [1 to 100]"
         <|> Print  <$  switch "print"  'p' "Print the volume"
         <|> Toggle <$  switch "toggle" 't' "Toggle sound mute"

changeVolume :: Command -> IO ()
changeVolume (Up   amt) = run ("amixer -q sset Master,0 " <> showS amt <> "%+")
changeVolume (Down amt) = run ("amixer -q sset Master,0 " <> showS amt <> "%-")
changeVolume Toggle     = run  "amixer -q sset Master,0 toggle"
changeVolume Print      = pure ()

announceVolumeChanges :: Command -> Volume -> IO ()
announceVolumeChanges Print _      = pure ()
announceVolumeChanges _     volume = do
    -- Update tmux
    run "tmux refresh -S"

    -- Create desktop notification
    run ("notify volume " <> volume)

printVolume :: Command -> Volume -> IO ()
printVolume Print volume = echo volume
printVolume _     _      = pure ()

amixerOutput :: IO Text
amixerOutput = snd <$> procStrict "amixer" ["get", "Master,0"] empty

signedVolumePattern :: Pattern Volume
signedVolumePattern = do
    chars

    volume <- "[" *> decimal <* "%]"
    spaces1
    on <- "[" *> ("on" <|> "off") <* "]"

    chars

    pure (showS volume <> if on == "on" then "+" else "-")

showS :: (Show a, IsString b) => a -> b
showS = fromString . show

run :: Text -> IO ()
run cmd = sh (inshell cmd empty)
