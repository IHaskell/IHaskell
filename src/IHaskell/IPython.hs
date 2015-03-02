{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- | Description : Shell scripting wrapper using @Shelly@ for the @notebook@, and
--                 @console@ commands.
module IHaskell.IPython (
    withIPython,
    runConsole,
    runNotebook,
    readInitInfo,
    defaultConfFile,
    getIHaskellDir,
    getSandboxPackageConf,
    nbconvert,
    subHome,
    kernelName,
    ViewFormat(..),
    ) where

import           ClassyPrelude
import           Control.Concurrent (threadDelay)
import           Prelude (read, reads, init)
import           Shelly hiding (find, trace, path, (</>))
import           System.Argv0
import           System.Directory
import qualified Filesystem.Path.CurrentOS as FS
import           Data.List.Utils (split)
import           Data.String.Utils (rstrip, endswith, strip, replace)
import           Text.Printf
import           Data.Maybe (fromJust)
import           System.Exit (exitFailure)
import           Data.Aeson (toJSON)
import           Data.Aeson.Encode (encodeToTextBuilder)
import           Data.Text.Lazy.Builder (toLazyText)

import qualified System.IO.Strict as StrictIO
import qualified Paths_ihaskell as Paths
import qualified Codec.Archive.Tar as Tar

import           IHaskell.Types
import           System.Posix.Signals

-- | The IPython kernel name.
kernelName :: IsString a => a
kernelName = "haskell"

kernelArgs :: IsString a => [a]
kernelArgs = ["--kernel", kernelName]

-- | Run the IPython command with any arguments. The kernel is set to IHaskell.
ipython :: Bool         -- ^ Whether to suppress output.
        -> [Text]       -- ^ IPython command line arguments.
        -> Sh String    -- ^ IPython output.
ipython suppress args = do
  liftIO $ installHandler keyboardSignal (CatchOnce $ return ()) Nothing

  -- We have this because using `run` does not let us use stdin.
  runHandles "ipython" (args ++ kernelArgs) handles doNothing

  where
    handles = [InHandle Inherit, outHandle suppress, errorHandle suppress]
    outHandle True = OutHandle CreatePipe
    outHandle False = OutHandle Inherit
    errorHandle True = ErrorHandle CreatePipe
    errorHandle False = ErrorHandle Inherit
    doNothing _ stdout _ = if suppress
                             then liftIO $ StrictIO.hGetContents stdout
                             else return ""

-- | Run while suppressing all output.
quietRun path args = runHandles path args handles nothing
  where
    handles = [InHandle Inherit, OutHandle CreatePipe, ErrorHandle CreatePipe]
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

notebookDir :: Sh FilePath
notebookDir = ensure $ (</> "notebooks") <$> ihaskellDir

getIHaskellDir :: IO String
getIHaskellDir = shelly $ fpToString <$> ihaskellDir

defaultConfFile :: IO (Maybe String)
defaultConfFile = shelly $ do
  filename <- (</> "rc.hs") <$> ihaskellDir
  exists <- test_f filename
  return $ if exists
             then Just $ fpToString filename
             else Nothing

-- | Find a notebook and then convert it into the provided format.
-- Notebooks are searched in the current directory as well as the IHaskell
-- notebook directory (in that order).
nbconvert :: ViewFormat -> String -> IO ()
nbconvert fmt name = void . shelly $ do
  curdir <- pwd
  nbdir <- notebookDir

  -- Find which of the options is available. 
  let notebookOptions = [ curdir </> fpFromString name
                        , curdir </> fpFromString (name ++ ".ipynb")
                        , nbdir </> fpFromString name
                        , nbdir </> fpFromString (name ++ ".ipynb")
                        ]
  maybeNb <- headMay <$> filterM test_f notebookOptions
  case maybeNb of
    Nothing -> do
      putStrLn $ "Cannot find notebook: " ++ pack name
      putStrLn "Tried:"
      mapM_ (putStrLn . ("  " ++) . fpToText) notebookOptions

    Just notebook ->
      let viewArgs =
            case fmt of
              Pdf  -> ["--to=latex", "--post=pdf"]
              Html -> ["--to=html", "--template=ihaskell"]
              fmt  -> ["--to=" ++ pack (show fmt)]
          args = "nbconvert" : fpToText notebook : viewArgs
      in void $ ipython False args

-- | Run an action after having verified that a proper IPython installation exists.
-- This ensures that an IHaskell kernelspec exists; if it doesn't, it creates it.
-- Note that this exits with an error if IPython isn't installed properly.
withIPython :: IO a -> IO a
withIPython act = do
  verifyIPythonVersion
  installKernelspec
  act

-- | Verify that a proper version of IPython is installed and accessible.
verifyIPythonVersion :: IO ()
verifyIPythonVersion = shelly $ do
  pathMay <- which "ipython"
  case pathMay of
    Nothing -> badIPython "No IPython detected -- install IPython 3.0+ before using IHaskell."
    Just path -> do
      output <- unpack <$> silently (run path ["--version"])
      case parseVersion output of
        Just (3:_) -> return ()
        Just (2:_) -> oldIPython
        Just (1:_) -> oldIPython
        Just (0:_) -> oldIPython
        _          -> badIPython "Detected IPython, but could not parse version number."
  where
    badIPython :: Text -> Sh ()
    badIPython message = liftIO $ do
      hPutStrLn stderr message
      exitFailure
    oldIPython = badIPython "Detected old version of IPython. IHaskell requires 3.0.0 or up."

-- | Install an IHaskell kernelspec into the right location.
-- The right location is determined by using `ipython kernelspec install --user`.
installKernelspec :: IO ()
installKernelspec = void $ shelly $ do
  ihaskellPath <- getIHaskellPath
  let kernelSpec = KernelSpec {
        kernelDisplayName = "Haskell",
        kernelLanguage = kernelName,
        kernelCommand = [ihaskellPath, "kernel", "{connection_file}"]
      }

  -- Create a temporary directory. Use this temporary directory to make a kernelspec
  -- directory; then, shell out to IPython to install this kernelspec directory.
  withTmpDir $ \tmp -> do
    let kernelDir = tmp </> kernelName
    let filename = kernelDir </> "kernel.json"

    mkdir_p kernelDir
    writefile filename $ toStrict $ toLazyText $ encodeToTextBuilder $ toJSON kernelSpec

    Just ipython <- which "ipython"
    run ipython ["kernelspec", "install", "--user", fpToText kernelDir]

-- | Replace "~" with $HOME if $HOME is defined.
-- Otherwise, do nothing.
subHome :: String -> IO String
subHome path = shelly $ do
  home <- unpack <$> fromMaybe "~" <$> get_env "HOME"
  return $ replace "~" home path


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

-- | Parse an IPython version string into a list of integers.
parseVersion :: String -> Maybe [Int]
parseVersion versionStr =
  let versions = map read' $ split "." versionStr
      parsed = all isJust versions
  in if parsed
       then Just $ map fromJust versions
       else Nothing
  where
    read' :: String -> Maybe Int
    read' x =
      case reads x of
        [(n, _)] -> Just n
        _        -> Nothing

runConsole :: InitInfo -> IO ()
runConsole initInfo = void . shelly $ do
  writeInitInfo initInfo
  ipython False ["console"]

runNotebook :: InitInfo -> Maybe Text -> IO ()
runNotebook initInfo maybeServeDir = void . shelly $ do
  notebookDirStr <- fpToText <$> notebookDir
  let args =
        case maybeServeDir of
          Nothing  -> ["--notebook-dir", notebookDirStr]
          Just dir -> ["--notebook-dir", dir]

  writeInitInfo initInfo
  ipython False $ "notebook" : args

writeInitInfo :: InitInfo -> Sh ()
writeInitInfo info = do
  filename <- (</> ".last-arguments") <$> ihaskellDir
  liftIO $ writeFile filename $ show info

readInitInfo :: IO InitInfo
readInitInfo = shelly $ do
  filename <- (</> ".last-arguments") <$> ihaskellDir
  exists <- test_f filename
  if exists
    then read <$> liftIO (readFile filename)
    else do
      dir <- fromMaybe "." <$> fmap unpack <$> get_env "HOME"
      return InitInfo { extensions = [], initCells = [], initDir = dir, frontend = IPythonNotebook }

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
          Nothing   -> error "IHaskell not on $PATH and not referenced relative to directory."
          Just path -> return $ FS.encodeString path
      else do
        -- If it's actually a relative path, make it absolute.
        cd <- liftIO getCurrentDirectory
        return $ FS.encodeString $ FS.decodeString cd FS.</> f

getSandboxPackageConf :: IO (Maybe String)
getSandboxPackageConf = shelly $ do
  myPath <- getIHaskellPath
  let sandboxName = ".cabal-sandbox"
  if not $ sandboxName `isInfixOf` myPath
    then return Nothing
    else do
      let pieces = split "/" myPath
          sandboxDir = intercalate "/" $ takeWhile (/= sandboxName) pieces ++ [sandboxName]
      subdirs <- ls $ fpFromString sandboxDir
      let confdirs = filter (endswith "packages.conf.d") $ map fpToString subdirs
      case confdirs of
        [] -> return Nothing
        dir:_ ->
          return $ Just dir
