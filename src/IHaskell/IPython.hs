{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
-- | Description : Shell scripting wrapper using @Shelly@ for the @notebook@, @setup@, and
--                 @console@ commands.
module IHaskell.IPython (
  ipythonInstalled,
  runConsole,
  runNotebook,
  readInitInfo,
  defaultConfFile,
  getIHaskellDir,
  getSandboxPackageConf,
  nbconvert,
  ViewFormat(..),
) where

import ClassyPrelude
import Prelude (read, reads, init)
import Shelly hiding (find, trace, path, (</>))
import System.Argv0
import System.Directory
import qualified Filesystem.Path.CurrentOS as FS
import Data.List.Utils (split)
import Data.String.Utils (rstrip, endswith, strip, replace)
import Text.Printf

import qualified System.IO.Strict as StrictIO
import qualified Paths_ihaskell as Paths
import qualified Codec.Archive.Tar as Tar

import IHaskell.Types

-- | Which commit of IPython we are on.
ipythonCommit :: Text
ipythonCommit = "9c922f54af799704f4000aeee94ec7c74cada194"

-- | The IPython profile name.
ipythonProfile :: String
ipythonProfile = "haskell"

-- | Run IPython with any arguments.
ipython :: Bool         -- ^ Whether to suppress output.
        -> [Text]       -- ^ IPython command line arguments.
        -> Sh String    -- ^ IPython output.
ipython suppress args = do
  ipythonPath <- ipythonExePath
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

-- | Create the directory and return it.
ensure :: Sh FilePath -> Sh FilePath
ensure getDir = do
  dir <- getDir
  mkdir_p dir
  return dir

-- | Return the data directory for IHaskell.
ihaskellDir :: Sh FilePath
ihaskellDir = do
  home <- maybe (error "$HOME not defined.") fromText <$> get_env "HOME"
  ensure $ return (home </> ".ihaskell")

ipythonDir :: Sh FilePath
ipythonDir = ensure $ (</> "ipython") <$> ihaskellDir

ipythonUpdatePath :: Sh FilePath
ipythonUpdatePath = (</> "ipython_updater.sh") <$> ihaskellDir

pythonUpdatePath :: Sh FilePath
pythonUpdatePath = (</> "python_updater.sh") <$> ihaskellDir

ipythonExePath :: Sh FilePath
ipythonExePath = (</> ("bin" </> "ipython")) <$> ipythonDir

notebookDir :: Sh FilePath
notebookDir = ensure $ (</> "notebooks") <$> ihaskellDir

ipythonSourceDir :: Sh FilePath
ipythonSourceDir = ensure $ (</> "ipython-src") <$> ihaskellDir

getIHaskellDir :: IO String
getIHaskellDir = shellyNoDir $ fpToString <$> ihaskellDir

defaultConfFile :: IO (Maybe String)
defaultConfFile = shellyNoDir $ do
  filename <- (</> "rc.hs") <$> ihaskellDir
  exists <- test_f filename
  return $ if exists
           then Just $ fpToString filename
           else Nothing

-- | Find a notebook and then convert it into the provided format.
-- Notebooks are searched in the current directory as well as the IHaskell
-- notebook directory (in that order).
nbconvert :: ViewFormat -> String -> IO ()
nbconvert fmt name = void . shellyNoDir $ do
  curdir <- pwd
  nbdir <- notebookDir
  -- Find which of the options is available. 
  let notebookOptions = [
        curdir </> fpFromString name,
        curdir </> fpFromString (name ++ ".ipynb"),
        nbdir  </> fpFromString name,
        nbdir  </> fpFromString (name ++ ".ipynb")
        ]
  maybeNb <- headMay <$> filterM test_f notebookOptions
  case maybeNb of
    Nothing -> do
      putStrLn $ "Cannot find notebook: " ++ pack name
      putStrLn "Tried:"
      mapM_ (putStrLn . ("  " ++) . fpToText) notebookOptions 

    Just notebook ->
      let viewArgs = case fmt of
            Pdf ->  ["--to=latex", "--post=pdf"]
            Html -> ["--to=html", "--template=ihaskell"]
            fmt ->  ["--to=" ++ show fmt] in
      void $ runIHaskell ipythonProfile "nbconvert" $ viewArgs ++ [fpToString notebook]

-- Not sure what to do with this - is there going to be a haskell profile to start with,
-- or every time we start the build process we want to start with a new profile?
-- removeIPythonProfile ipythonProfile

-- It'd be a good idea to still include an updater that runs an external script,
-- but haskell's hard and I can't figure out how to make this thing run.
-- updateIPython :: IO ()
-- updateIPython = void . shellyNoDir $ do
--   ipythonUpdater <- fromText ipythonUpdatePath
--   run_ ipythonUpdatePath []

-- | Attempt to guess the Python version.
pythonVersion :: Sh Text
pythonVersion = do
  python <- path "python"
  versions <- run python ["-c", "import sys; print(sys.version_info.major, sys.version_info.minor)"]
  let replacer = replace "(" "" .
                 replace ")" "" .
                 replace "," ""
  let [major, minor] = map read . split " " . strip . replacer $ unpack versions :: [Int]
  return $ "python" ++ pack (show major) ++ "." ++ pack (show minor)

-- | Check whether IPython is properly installed.
ipythonInstalled :: IO Bool
ipythonInstalled = shellyNoDir $ do
  ipythonPath <- ipythonExePath
  test_f ipythonPath

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
  -- We have an ugly hack that removes the profile whenever the IPython
  -- version is updated. This means profiles get updated with IPython.
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
  notebookDirStr <- fpToString <$> notebookDir
  let args = case maybeServeDir of 
               Nothing -> ["--notebook-dir", unpack notebookDirStr]
               Just dir -> ["--notebook-dir", dir]

  writeInitInfo initInfo
  runIHaskell ipythonProfile "notebook" args

writeInitInfo :: InitInfo -> Sh ()
writeInitInfo info = do
  filename <- (</> ".last-arguments") <$> ihaskellDir
  liftIO $ writeFile filename $ show info

readInitInfo :: IO InitInfo
readInitInfo = shellyNoDir $ do
  filename <- (</>  ".last-arguments") <$> ihaskellDir
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

removeIPythonProfile :: String -> Sh ()
removeIPythonProfile profile = do
  -- Try to locate the profile. Do not die if it doesn't exist.
  errExit False $ ipython True ["locate", "profile", pack profile]

  -- If the profile exists, delete it.
  exitCode <- lastExitCode
  dir <- pack <$> rstrip <$> ipython True ["locate"]
  when (exitCode == 0 && dir /= "") $ do
    putStrLn "Updating IPython profile."
    let profileDir = dir ++ "/profile_" ++ pack profile ++ "/"
    rm_rf $ fromText profileDir

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

getSandboxPackageConf :: IO (Maybe String)
getSandboxPackageConf = shellyNoDir $ do
  myPath <- getIHaskellPath
  let sandboxName = ".cabal-sandbox"
  if not $ sandboxName`isInfixOf` myPath
  then return Nothing
  else do
    let pieces = split "/" myPath
        sandboxDir = intercalate "/" $ takeWhile (/= sandboxName) pieces  ++ [sandboxName]
    subdirs <- ls $ fpFromString sandboxDir
    let confdirs = filter (endswith "packages.conf.d") $ map fpToString subdirs
    case confdirs of
      [] -> return Nothing
      dir:_ -> 
        return $ Just dir
