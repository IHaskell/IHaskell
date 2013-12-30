{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
-- | Description : Shell scripting wrapper using @Shelly@ for the @notebook@, @setup@, and
--                 @console@ commands.
module IHaskell.IPython (
  ipythonInstalled,
  installIPython,
  removeIPython,
  runConsole,
  runNotebook,
  readInitInfo,
  defaultConfFile,
  getIHaskellDir,
) where

import ClassyPrelude
import Prelude (read, reads, init)
import Shelly hiding (find, trace, path)
import System.Argv0
import System.Directory
import qualified Filesystem.Path.CurrentOS as FS
import Data.List.Utils (split)
import Data.String.Utils (rstrip)
import Text.Printf

import qualified System.IO.Strict as StrictIO
import qualified Paths_ihaskell as Paths
import qualified Codec.Archive.Tar as Tar

import IHaskell.Types

-- | Which commit of IPython we are on.
ipythonCommit :: Text
ipythonCommit = "1faf2f6e77fa31f4533e3edbe101c38ddf8943d8"

-- | The IPython profile name.
ipythonProfile :: String
ipythonProfile = "haskell"

-- | Run IPython with any arguments.
ipython :: Bool         -- ^ Whether to suppress output.
        -> [Text]       -- ^ IPython command line arguments.
        -> Sh String    -- ^ IPython output.
ipython suppress args = do
  (_, ipythonDir, _) <- ihaskellDirs
  let ipythonPath = fromText $ ipythonDir ++ "/bin/ipython"
  sub $ do
    setenv "PYTHONPATH" $ ipythonDir ++ "/lib/python2.7/site-packages"
    runHandles ipythonPath args handles doNothing
  where handles = [InHandle Inherit, outHandle suppress, errorHandle suppress]
        outHandle True = OutHandle CreatePipe
        outHandle False = OutHandle Inherit
        errorHandle True =  ErrorHandle CreatePipe
        errorHandle False = ErrorHandle Inherit
        doNothing _ stdout _ = if suppress 
                                then liftIO $ StrictIO.hGetContents stdout
                                else return ""

-- | Run while suppressing all output.
quietRun path args = runHandles path args handles nothing
  where
    handles =  [InHandle Inherit, OutHandle CreatePipe, ErrorHandle CreatePipe]
    nothing _ _ _ = return ()

-- | Return the data directory for IHaskell and the IPython subdirectory. 
ihaskellDirs :: Sh (Text, Text, Text)
ihaskellDirs = do
  home <- maybe (error "$HOME not defined.") id <$> get_env "HOME" :: Sh Text
  let ihaskellDir = home ++ "/.ihaskell"
      ipythonDir = ihaskellDir ++ "/ipython"
      notebookDir = ihaskellDir ++ "/notebooks"

  -- Make sure the directories exist.
  mkdir_p $ fromText ipythonDir
  mkdir_p $ fromText notebookDir

  return (ihaskellDir, ipythonDir, notebookDir)

getIHaskellDir :: IO String
getIHaskellDir = shellyNoDir $ do
  (ihaskellDir, _, _) <- ihaskellDirs
  return $ unpack ihaskellDir

defaultConfFile :: IO (Maybe String)
defaultConfFile = shellyNoDir $ do
  (ihaskellDir, _, _) <- ihaskellDirs
  let filename = ihaskellDir ++ "/rc.hs"
  exists <- test_f $ fromText filename
  return $ if exists
           then Just $ unpack filename
           else Nothing

-- | Remove IPython so it can be reinstalled.
removeIPython :: IO ()
removeIPython = void . shellyNoDir $ do
  (ihaskellDir, _, _) <- ihaskellDirs
  cd $ fromText ihaskellDir
  rm_rf "ipython-src"

-- | Install IPython from source.
installIPython :: IO ()
installIPython = void . shellyNoDir $ do
  (ihaskellDir, ipythonDir, _) <- ihaskellDirs

  -- Install all Python dependencies.
  pipPath <- path "pip"
  let pipDeps = ["pyzmq", "tornado", "jinja2"]
      installDep dep = do
        putStrLn $ "Installing dependency: " ++ dep 
        let opt = "--install-option=--prefix=" ++ ipythonDir
        run_ pipPath ["install", opt, dep]
  mapM_ installDep pipDeps

  -- Get the IPython source.
  gitPath <- path "git"
  putStrLn "Downloading IPython... (this may take a while)"
  cd $ fromText ihaskellDir
  run_ gitPath ["clone", "--recursive", "https://github.com/ipython/ipython.git", "ipython-src"]
  cd "ipython-src"
  run_ gitPath ["checkout", ipythonCommit]

  -- Install IPython locally.
  pythonPath <- path "python"
  putStrLn "Installing IPython."
  run_ pythonPath ["setup.py", "install", "--prefix=" ++ ipythonDir]
  cd ".."

  -- Patch the IPython executable so that it doesn't use system IPython.
  -- Using PYTHONPATH is not enough due to bugs in how `easy_install` sets
  -- things up, at least on Mac OS X.
  let patchLines =
        [ "#!/usr/bin/python"
        , "import sys"
        , "sys.path = [\"" ++ ipythonDir ++
         "/lib/python2.7/site-packages\"] + sys.path"]
      ipythonPath = ipythonDir ++ "/bin/ipython"
  contents <- readFile $ fromText ipythonPath
  writeFile (fromText ipythonPath) $ unlines patchLines ++ "\n" ++ contents
      

-- | Check whether IPython is properly installed.
ipythonInstalled :: IO Bool
ipythonInstalled = shellyNoDir $ do
  (_, ipythonDir, _) <- ihaskellDirs
  let ipythonPath = ipythonDir ++ "/bin/ipython"
  test_f $ fromText ipythonPath

-- | Get the path to an executable. If it doensn't exist, fail with an
-- error message complaining about it.
path :: Text -> Sh FilePath
path exe = do
  path <- which $ fromText exe
  case path of
    Nothing -> do
      putStrLn $ "Could not find `" ++ exe ++ "` executable."
      fail $ "`" ++ unpack exe ++ "` not on $PATH."
    Just exePath -> return exePath

-- | Use the `ipython --version` command to figure out the version.
-- Return a tuple with (major, minor, patch).
ipythonVersion :: IO (Int, Int, Int)
ipythonVersion = shellyNoDir $ do
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
           -> Sh ()
runIHaskell profile app args = void $ do
  -- Try to locate the profile. Do not die if it doesn't exist.
  errExit False $ ipython True ["locate", "profile", pack profile]

  -- If the profile doesn't exist, create it.
  exitCode <- lastExitCode
  when (exitCode /= 0) $ liftIO $ do
    putStrLn "Creating IPython profile."
    setupIPythonProfile profile

  -- Run the IHaskell command.
  ipython False $ map pack $ [app, "--profile", profile] ++ args

runConsole :: InitInfo -> IO ()
runConsole initInfo = void . shellyNoDir $ do
  writeInitInfo initInfo
  runIHaskell ipythonProfile "console" []

runNotebook :: InitInfo -> Maybe String -> IO ()
runNotebook initInfo maybeServeDir = void . shellyNoDir $ do
  (_, _, notebookDir) <- ihaskellDirs
  let args = case maybeServeDir of 
               Nothing -> ["--notebook-dir", unpack notebookDir]
               Just dir -> ["--notebook-dir", dir]

  writeInitInfo initInfo
  runIHaskell ipythonProfile "notebook" args

writeInitInfo :: InitInfo -> Sh ()
writeInitInfo info = do
  (ihaskellDir, _, _) <- ihaskellDirs
  let filename = fromText $ ihaskellDir ++ "/last-arguments"
  liftIO $ writeFile filename $ show info

readInitInfo :: IO InitInfo
readInitInfo = shellyNoDir $ do
  (ihaskellDir, _, _) <- ihaskellDirs
  let filename = fromText $ ihaskellDir ++ "/last-arguments"
  read <$> liftIO (readFile filename)

-- | Create the IPython profile.
setupIPythonProfile :: String -- ^ IHaskell profile name.
                    -> IO ()
setupIPythonProfile profile = shellyNoDir $ do
  -- Create the IPython profile.
  void $ ipython True ["profile", "create", pack profile]

  -- Find the IPython profile directory. Make sure to get rid of trailing
  -- newlines from the output of the `ipython locate` call.
  ipythonDir <- pack <$> rstrip <$> ipython True ["locate"]
  let profileDir = ipythonDir ++ "/profile_" ++ pack profile ++ "/"

  liftIO $ copyProfile profileDir
  insertIHaskellPath profileDir

-- | Copy the profile files into the IPython profile. 
copyProfile :: Text -> IO ()
copyProfile profileDir = do
  profileTar <- Paths.getDataFileName "profile/profile.tar"
  {-
  -- Load profile from Resources directory of Mac *.app.
  ihaskellPath <- shellyNoDir getIHaskellPath
  profileTar <- if "IHaskell.app/Contents/MacOS" `isInfixOf` ihaskellPath
               then
                let pieces = split "/" ihaskellPath
                    pathPieces = init pieces ++ ["..", "Resources", "profile.tar"] in
                  return $ intercalate "/" pathPieces
               else Paths.getDataFileName "profile/profile.tar"
  -}
  putStrLn $ pack $ "Loading profile from " ++ profileTar 
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
