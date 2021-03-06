{-# LANGUAGE OverloadedStrings #-}

{-| For each m3u music playlist file read from stdin, copy each music file in the
    m3u file to a directory on an Android device using the Android Debug Bridge
    tool "adb". Also copy the playlist file to the Android device.

    The first argument specifies from which directory all the playlist file
    locations are relative (i.e. the music directory on the computer). The second
    argument specifies the absolute music directory on the Android device.

    For example, if my music on my computer is all in a directory "/Music/" and
    my music on the Android device is all in a directory "/sdcard/Music/", I
    might run the command like this:

> $ ls /Music/playlists/*.m3u | android-copy /Music/ /sdcard/Music/
> /Music/playlists/Favorites.m3u
> adb push /Music/playlists/Favorites.m3u /sdcard/Music/
> /Music/Justin Bieber/Singles/Baby.mp3
> /Music/Justin Bieber/Believe/Beauty and a Beat.mp3
> adb push /Music/Justin Bieber/Believe/Beauty and a Beat.mp3 /sdcard/Music/Justin Bieber/Believe/Beauty and a Beat.mp3

    Note: This script only copies a music file to the Android device if it does
    not exist there already. This can be seen above. The first song was not
    copied to the phone. If this is a problem, consider deleting the contents of
    your Android music directory before beginning:

> $ adb shell
> $ rm -rf /sdcard/Music/*
-}

import qualified Control.Foldl as Fold
import Prelude hiding (FilePath)
import Turtle

main = sh (do
    (srcDir, androidDir) <- options description args
    existsOrDie srcDir (testdir srcDir)

    m3uFile <- fmap fromText stdin
    existsOrDie m3uFile (testfile m3uFile)
    echo (format fp m3uFile)
    adbPush m3uFile androidDir

    relMusicFilePath <- grep (invert spaces) (input m3uFile)
    let relMusicFile  = fromText relMusicFilePath
    let musicFile     = srcDir <> relMusicFile
    existsOrDie musicFile (testfile musicFile)

    echo relMusicFilePath
    let destPath = androidDir </> relMusicFile
    True <- isNew destPath
    adbPush musicFile destPath)

description :: Description
description = "Copy files referenced from m3u files in STDIN to android device, preserving hierarchy"

args :: Parser (FilePath, FilePath)
args = (,) <$> argPath "musicSourceDir" "The directory from which the m3u playlist files are relative"
           <*> argPath "androidDestDir" "The destination directory for the music on the android device"

existsOrDie :: MonadIO io => FilePath -> io Bool -> io ()
existsOrDie path test = do
    exists <- test
    unless exists (die (format ("Error: '" % fp % "' must exist") path))

adbPush :: MonadIO io => FilePath -> FilePath -> io ()
adbPush srcPath dstPath = do
    (exitCode, output) <- procStrict "adb" ["push", format fp srcPath, format fp dstPath] empty
    let command = format ("adb push " % fp % " " % fp) srcPath dstPath
    case exitCode of
        ExitFailure code -> die (command <> " failed with output: " <> output)
        ExitSuccess      -> echo command

-- Uses Android "ls" failures to determine if a file is new
isNew :: FilePath -> Shell Bool
isNew androidPath = fmap not (fold (grep pattern pipe) Fold.null)
  where
    pattern = has "No such file or directory"
    pipe    = inproc "adb" ["shell", format ("ls \"" % fp % "\"") androidPath] empty
