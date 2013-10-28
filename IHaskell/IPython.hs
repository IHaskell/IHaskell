module IHaskell.IPython (
  runIHaskell,
  setupIPythonProfile,
  ipythonVersion
) where

import ClassyPrelude
import Shelly hiding (find, trace)
import Text.Printf
import System.Argv0
import System.Directory
import qualified Filesystem.Path.CurrentOS as FS
import Data.List.Utils (split)

import Prelude (read)
import qualified System.IO.Strict as StrictIO

import qualified IHaskell.Config as Config

-- | Run IPython with any arguments.
ipython :: [Text] -> Sh ()
ipython args = do
  path <- which "ipython"
  case path of
    Nothing -> putStrLn "Could not find `ipython` executable."
    Just ipythonPath -> runHandles ipythonPath args inheritHandles doNothing
      where inheritHandles = [InHandle Inherit, OutHandle Inherit, ErrorHandle Inherit]
            doNothing _ _ _ = return ()


-- | Use the `ipython --version` command to figure out the version.
-- Return a tuple with (major, minor, patch).
ipythonVersion :: IO (Int, Int, Int)
ipythonVersion = shelly $ do
  path <- which "ipython"
  case path of
    Nothing -> error "Could not find `ipython` executable."
    Just path -> do
      [major, minor, patch] <- map read <$> split "." <$> runHandle path ["--version"] (liftIO . StrictIO.hGetContents) :: Sh [Int]
      return (major, minor, patch)

-- | Run an IHaskell application using the given profile.
runIHaskell :: String   -- ^ IHaskell profile name. 
           -> String    -- ^ IPython app name.
           -> [String]  -- ^ Arguments to IPython.
           -> IO ()
runIHaskell profile app args = shelly . ipython $ [pack app, "--profile", pack profile] ++ map pack args

-- | Create the IPython profile.
setupIPythonProfile :: String -- ^ IHaskell profile name.
                    -> IO ()
setupIPythonProfile profile = shelly $ do
  -- Create the IPython profile.
  ipython ["profile", "create", pack profile]

  -- Find the IPython profile directory.
  ipythonDirs <- catMaybes <$> sequence [get_env "IPYTHON_DIR", get_env "IPYTHONDIR"]
  ipythonDir <-  case ipythonDirs of
    dir:_ -> return dir
    [] -> do
      home <- get_env "HOME"
      case home of
        Nothing -> error "Could not locate $HOME."
        Just home -> do
          dotIpython <- test_d . fromText $ home ++ "/.ipython"
          dotConfigIpython <- test_d . fromText $ home ++ "/.config/ipython"

          when (not dotIpython && not dotConfigIpython) $ do
            putStrLn "Could not find ~/.ipython or ~/.config/ipython."
            error "Could not find IPython directory."

          return $ home ++ (if dotIpython
                          then "/.ipython"
                          else "/.config/ipython")

  let profileDir = ipythonDir ++ "/profile_" ++ pack profile ++ "/"

  path <- getIHaskellPath
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
  where
    conf filename = fromText $ profileDir ++ filename

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
