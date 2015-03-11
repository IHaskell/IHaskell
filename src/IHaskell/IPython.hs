{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- | Description : Shell scripting wrapper using @Shelly@ for the @notebook@, and
--                 @console@ commands.
module IHaskell.IPython (
    replaceIPythonKernelspec,
    defaultConfFile,
    getIHaskellDir,
    getSandboxPackageConf,
    subHome,
    kernelName,
    KernelSpecOptions(..),
    defaultKernelSpecOptions,
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
import qualified Data.Text as T
import           Data.Maybe (fromJust)
import           System.Exit (exitFailure)
import           Data.Aeson (toJSON)
import           Data.Aeson.Encode (encodeToTextBuilder)
import           Data.Text.Lazy.Builder (toLazyText)

import qualified System.IO.Strict as StrictIO
import qualified Paths_ihaskell as Paths
import qualified Codec.Archive.Tar as Tar

import qualified GHC.Paths
import           IHaskell.Types
import           System.Posix.Signals


data KernelSpecOptions = KernelSpecOptions { kernelSpecGhcLibdir :: String           -- ^ GHC libdir.
                                           , kernelSpecDebug :: Bool                 -- ^ Spew debugging output?
                                           , kernelSpecConfFile :: IO (Maybe String) -- ^ Filename of profile JSON file.
                                           }

defaultKernelSpecOptions :: KernelSpecOptions
defaultKernelSpecOptions = KernelSpecOptions { kernelSpecGhcLibdir = GHC.Paths.libdir
                                             , kernelSpecDebug = False
                                             , kernelSpecConfFile = defaultConfFile
                                             }
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
  runHandles "ipython" args handles doNothing

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

replaceIPythonKernelspec :: KernelSpecOptions -> IO ()
replaceIPythonKernelspec kernelSpecOpts = shelly $ do
  verifyIPythonVersion
  installKernelspec True kernelSpecOpts

-- | Verify that a proper version of IPython is installed and accessible.
verifyIPythonVersion :: Sh ()
verifyIPythonVersion = do
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
installKernelspec :: Bool -> KernelSpecOptions -> Sh ()
installKernelspec replace opts = void $ do
  ihaskellPath <- getIHaskellPath
  confFile <- liftIO $ kernelSpecConfFile opts

  let kernelFlags :: [String]
      kernelFlags =
        ["--debug" | kernelSpecDebug opts] ++
        ["--conf"] ++ maybe [] singleton confFile ++
        ["--ghclib", kernelSpecGhcLibdir opts]

  let kernelSpec = KernelSpec { kernelDisplayName = "Haskell"
                              , kernelLanguage = kernelName
                              , kernelCommand = [ihaskellPath, "kernel", "{connection_file}"] ++ kernelFlags
                              }

  -- Create a temporary directory. Use this temporary directory to make a kernelspec
  -- directory; then, shell out to IPython to install this kernelspec directory.
  withTmpDir $ \tmp -> do
    let kernelDir = tmp </> kernelName
    let filename = kernelDir </> "kernel.json"

    mkdir_p kernelDir
    writefile filename $ toStrict $ toLazyText $ encodeToTextBuilder $ toJSON kernelSpec
    let files = ["kernel.js", "logo-64x64.png"]
    forM_ files $ \file -> do
      src <- liftIO $ Paths.getDataFileName $ "html/" ++ file
      cp (fpFromString src) (tmp </> kernelName </> fpFromString file)

    Just ipython <- which "ipython"
    let replaceFlag = ["--replace" | replace]
        cmd = ["kernelspec", "install", "--user", fpToText kernelDir] ++ replaceFlag
    silently $ run ipython cmd

kernelSpecCreated :: Sh Bool
kernelSpecCreated = do
    Just ipython <- which "ipython"
    out <- silently $ run ipython ["kernelspec", "list"]
    let kernelspecs = map T.strip $ lines out
    return $ kernelName `elem` kernelspecs

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
        ihaskellPath <- which "ihaskell"
        case ihaskellPath of
          Nothing   -> error "ihaskell not on $PATH and not referenced relative to directory."
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
