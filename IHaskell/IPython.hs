{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
-- | Description : Shell scripting wrapper using @Shelly@ for the @notebook@, @setup@, and
--                 @console@ commands.
module IHaskell.IPython (
  runIHaskell,
  setupIPythonProfile,
  ipythonVersion,
  parseVersion
) where

import ClassyPrelude
import Prelude (read, reads)
import Shelly hiding (find, trace)
import System.Argv0
import System.Directory
import qualified Filesystem.Path.CurrentOS as FS
import Data.List.Utils (split)
import Data.String.Utils (rstrip)
import Text.Printf

import qualified System.IO.Strict as StrictIO

import qualified Paths_ihaskell as Paths
import qualified Codec.Archive.Tar as Tar

-- | Run IPython with any arguments.
ipython :: Bool         -- ^ Whether to suppress output.
        -> [Text]       -- ^ IPython command line arguments.
        -> Sh String    -- ^ IPython output.
ipython suppress args = do
  path <- which "ipython"
  case path of
    Nothing -> do
      putStrLn "Could not find `ipython` executable."
      fail "`ipython` not on $PATH."
    Just ipythonPath -> runHandles ipythonPath args handles doNothing
      where handles = [InHandle Inherit, outHandle suppress, errorHandle suppress]
            outHandle True = OutHandle CreatePipe
            outHandle False = OutHandle Inherit
            errorHandle True =  ErrorHandle CreatePipe
            errorHandle False = ErrorHandle Inherit
            doNothing _ stdout _ = if suppress 
                                   then liftIO $ StrictIO.hGetContents stdout
                                   else return ""

-- | Use the `ipython --version` command to figure out the version.
-- Return a tuple with (major, minor, patch).
ipythonVersion :: IO (Int, Int, Int)
ipythonVersion = shelly $ do
  [major, minor, patch] <- parseVersion <$>  ipython True ["--version"]
  return (major, minor, patch)

-- | Parse an IPython version string into a list of integers.
parseVersion :: String -> [Int]
parseVersion versionStr = map read' $ split "." versionStr
    where read' x = case reads x of
                        [(n, _)] -> n
                        _ -> error $ "cannot parse version: "++ versionStr

-- | Run an IHaskell application using the given profile.
runIHaskell :: String   -- ^ IHaskell profile name. 
           -> String    -- ^ IPython app name.
           -> [String]  -- ^ Arguments to IPython.
           -> IO ()
runIHaskell profile app args = void . shelly $ do
  -- Try to locate the profile. Do not die if it doesn't exist.
  errExit False $ ipython True ["locate", "profile", pack profile]

  -- If the profile doesn't exist, create it.
  exitCode <- lastExitCode
  when (exitCode /= 0) $ liftIO $ do
    putStrLn "Creating IPython profile."
    setupIPythonProfile profile

  -- Run the IHaskell command.
  ipython False $ map pack $ [app, "--profile", profile] ++ args

-- | Create the IPython profile.
setupIPythonProfile :: String -- ^ IHaskell profile name.
                    -> IO ()
setupIPythonProfile profile = shelly $ do
  -- Create the IPython profile.
  void $ ipython True ["profile", "create", pack profile]

  -- Find the IPython profile directory. Make sure to get rid of trailing
  -- newlines from the output of the `ipython locate` call.
  ipythonDir <- pack <$> rstrip <$> ipython True ["locate"]
  let profileDir = ipythonDir ++ "/profile_" ++ pack profile ++ "/"

  liftIO $ copyProfile profileDir
  insertIHaskellPath profileDir
{-
  writeConfigFilesTo profileDir path

-- | Write IPython configuration files to the profile directory.
writeConfigFilesTo :: Text      -- ^ Profile directory to write to. Must have a trailing slash.
                   -> String    -- ^ Path to IHaskell executable.
                   -> Sh ()
writeConfigFilesTo profileDir ihaskellPath = do
    writeFile (conf "ipython_config.py")          $ Config.ipython ihaskellPath
    writeFile (conf "ipython_notebook_config.py")   Config.notebook
    writeFile (conf "ipython_console_config.py")    Config.console
    writeFile (conf "ipython_qtconsole_config.py")  Config.qtconsole

    -- The custom directory many not exist, in which case we'll create it.
    mkdir_p (conf "static/custom/")
    writeFile (conf "static/custom/custom.js")      Config.customjs

    -- Make directory for images.
    mkdir_p (conf "static/base/images")

    -- The notebook/js directory many not exist, in which case we'll create it.
    mkdir_p (conf "static/notebook/")
    mkdir_p (conf "static/notebook/js")
    forM_ Config.notebookJavascript $ \(file, content) ->
      writeFile (conf "static/notebook/js/" ++ file) content

  where
    conf filename = fromText $ profileDir ++ filename
-}

-- | Copy the profile files into the IPython profile. 
copyProfile :: Text -> IO ()
copyProfile profileDir = do
  profileTar <- Paths.getDataFileName "profile/profile.tar"
  Tar.extract (unpack profileDir) profileTar 

-- | Insert the IHaskell path into the IPython configuration.
insertIHaskellPath :: Text -> Sh ()
insertIHaskellPath profileDir = do
  path <- getIHaskellPath
  let filename = profileDir ++ "ipython_config.py"
      template = "exe = '%s'.replace(' ', '\\\\ ')"
      exeLine = printf template $ unpack path :: String

  liftIO $ do
    contents <- StrictIO.readFile $ unpack filename
    writeFile (fromText filename) $ exeLine ++ "\n" ++ contents

-- | Get the absolute path to this IHaskell executable.
getIHaskellPath :: Sh String
getIHaskellPath = do
  --  Get the absolute filepath to the argument.
  f <- liftIO getArgv0

  -- If we have an absolute path, that's the IHaskell we're interested in.
  if FS.absolute f
  then return $ FS.encodeString f
  else
    -- Check whether this is a relative path, or just 'IHaskell' with $PATH
    -- resolution done by the shell. If it's just 'IHaskell', use the $PATH
    -- variable to find where IHaskell lives.
    if FS.filename f == f
    then do
      ihaskellPath <- which "IHaskell"
      case ihaskellPath of
        Nothing -> error "IHaskell not on $PATH and not referenced relative to directory."
        Just path -> return $ FS.encodeString path
    else do
      -- If it's actually a relative path, make it absolute.
      cd <- liftIO getCurrentDirectory
      return $ FS.encodeString $ FS.decodeString cd FS.</> f
