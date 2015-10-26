{-# LANGUAGE CPP #-}

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

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

import           Control.Concurrent (threadDelay)
import           System.Argv0
import qualified Shelly as SH
import qualified System.IO as IO
import qualified System.FilePath as FP
import           System.Directory
import           System.Exit (exitFailure)
import           Data.Aeson (toJSON)
import           Data.Aeson.Encode (encodeToTextBuilder)
import           Data.Text.Lazy.Builder (toLazyText)
import           Control.Monad (mplus)

import qualified System.IO.Strict as StrictIO
import qualified Paths_ihaskell as Paths

import qualified GHC.Paths
import           IHaskell.Types
import           System.Posix.Signals

import           StringUtils (replace, split)

data KernelSpecOptions =
       KernelSpecOptions
         { kernelSpecGhcLibdir :: String           -- ^ GHC libdir.
         , kernelSpecDebug :: Bool                 -- ^ Spew debugging output?
         , kernelSpecConfFile :: IO (Maybe String) -- ^ Filename of profile JSON file.
         , kernelSpecInstallPrefix :: Maybe String
         , kernelSpecUseStack :: Bool              -- ^ Whether to use @stack@ environments.
         }

defaultKernelSpecOptions :: KernelSpecOptions
defaultKernelSpecOptions = KernelSpecOptions
  { kernelSpecGhcLibdir = GHC.Paths.libdir
  , kernelSpecDebug = False
  , kernelSpecConfFile = defaultConfFile
  , kernelSpecInstallPrefix = Nothing
  , kernelSpecUseStack = False
  }

-- | The IPython kernel name.
kernelName :: String
kernelName = "haskell"

kernelArgs :: [String]
kernelArgs = ["--kernel", kernelName]

ipythonCommand :: SH.Sh SH.FilePath
ipythonCommand = do
  jupyterMay <- SH.which "jupyter"
  return $
    case jupyterMay of
      Nothing -> "ipython"
      Just _  -> "jupyter"

locateIPython :: SH.Sh SH.FilePath
locateIPython = do
  mbinary <- SH.which "ipython"
  case mbinary of
    Nothing      -> SH.errorExit "The IPython binary could not be located"
    Just ipython -> return ipython

-- | Run the IPython command with any arguments. The kernel is set to IHaskell.
ipython :: Bool         -- ^ Whether to suppress output.
        -> [Text]       -- ^ IPython command line arguments.
        -> SH.Sh String    -- ^ IPython output.
ipython suppress args = do
  liftIO $ installHandler keyboardSignal (CatchOnce $ return ()) Nothing

  -- We have this because using `run` does not let us use stdin.
  cmd <- ipythonCommand
  SH.runHandles cmd args handles doNothing

  where
    handles = [SH.InHandle SH.Inherit, outHandle suppress, errorHandle suppress]
    outHandle True = SH.OutHandle SH.CreatePipe
    outHandle False = SH.OutHandle SH.Inherit
    errorHandle True = SH.ErrorHandle SH.CreatePipe
    errorHandle False = SH.ErrorHandle SH.Inherit
    doNothing _ stdout _ = if suppress
                             then liftIO $ StrictIO.hGetContents stdout
                             else return ""

-- | Run while suppressing all output.
quietRun path args = SH.runHandles path args handles nothing
  where
    handles = [SH.InHandle SH.Inherit, SH.OutHandle SH.CreatePipe, SH.ErrorHandle SH.CreatePipe]
    nothing _ _ _ = return ()

fp :: SH.FilePath -> FilePath
fp = T.unpack . SH.toTextIgnore

-- | Create the directory and return it.
ensure :: SH.Sh SH.FilePath -> SH.Sh SH.FilePath
ensure getDir = do
  dir <- getDir
  SH.mkdir_p dir
  return dir

-- | Return the data directory for IHaskell.
ihaskellDir :: SH.Sh FilePath
ihaskellDir = do
  home <- maybe (error "$HOME not defined.") SH.fromText <$> SH.get_env "HOME"
  fp <$> ensure (return (home SH.</> ".ihaskell"))

notebookDir :: SH.Sh SH.FilePath
notebookDir = ensure $ (SH.</> "notebooks") <$> ihaskellDir

getIHaskellDir :: IO String
getIHaskellDir = SH.shelly ihaskellDir

defaultConfFile :: IO (Maybe String)
defaultConfFile = fmap (fmap fp) . SH.shelly $ do
  filename <- (SH.</> "rc.hs") <$> ihaskellDir
  exists <- SH.test_f filename
  return $ if exists
             then Just filename
             else Nothing

replaceIPythonKernelspec :: KernelSpecOptions -> IO ()
replaceIPythonKernelspec kernelSpecOpts = SH.shelly $ do
  verifyIPythonVersion
  installKernelspec True kernelSpecOpts

-- | Verify that a proper version of IPython is installed and accessible.
verifyIPythonVersion :: SH.Sh ()
verifyIPythonVersion = do
  cmd <- ipythonCommand
  pathMay <- SH.which cmd
  case pathMay of
    Nothing -> badIPython
                 "No Jupyter / IPython detected -- install Jupyter 3.0+ before using IHaskell."
    Just path -> do
      stdout <- SH.silently (SH.run path ["--version"])
      stderr <- SH.lastStderr
      let majorVersion = join . fmap listToMaybe . parseVersion . T.unpack
      case mplus (majorVersion stderr) (majorVersion stdout) of
        Nothing -> badIPython $ T.concat
                                  [ "Detected Jupyter, but could not parse version number."
                                  , "\n"
                                  , "(stdout = "
                                  , stdout
                                  , ", stderr = "
                                  , stderr
                                  , ")"
                                  ]

        Just version -> when (version < 3) oldIPython

  where
    badIPython :: Text -> SH.Sh ()
    badIPython message = liftIO $ do
      IO.hPutStrLn IO.stderr (T.unpack message)
      exitFailure
    oldIPython = badIPython
                   "Detected old version of Jupyter / IPython. IHaskell requires 3.0.0 or up."

-- | Install an IHaskell kernelspec into the right location. The right location is determined by
-- using `ipython kernelspec install --user`.
installKernelspec :: Bool -> KernelSpecOptions -> SH.Sh ()
installKernelspec replace opts = void $ do
  ihaskellPath <- getIHaskellPath
  confFile <- liftIO $ kernelSpecConfFile opts

  let kernelFlags :: [String]
      kernelFlags =
        ["--debug" | kernelSpecDebug opts] ++
        (case confFile of
           Nothing   -> []
           Just file -> ["--conf", file])
        ++ ["--ghclib", kernelSpecGhcLibdir opts]
           ++ ["--stack" | kernelSpecUseStack opts]

  let kernelSpec = KernelSpec
        { kernelDisplayName = "Haskell"
        , kernelLanguage = kernelName
        , kernelCommand = [ihaskellPath, "kernel", "{connection_file}"] ++ kernelFlags
        }

  -- Create a temporary directory. Use this temporary directory to make a kernelspec directory; then,
  -- shell out to IPython to install this kernelspec directory.
  SH.withTmpDir $ \tmp -> do
    let kernelDir = tmp SH.</> kernelName
    let filename = kernelDir SH.</> "kernel.json"

    SH.mkdir_p kernelDir
    SH.writefile filename $ LT.toStrict $ toLazyText $ encodeToTextBuilder $ toJSON kernelSpec
    let files = ["kernel.js", "logo-64x64.png"]
    forM_ files $ \file -> do
      src <- liftIO $ Paths.getDataFileName $ "html/" ++ file
      SH.cp (SH.fromText $ T.pack src) (tmp SH.</> kernelName SH.</> file)

    ipython <- locateIPython

    let replaceFlag = ["--replace" | replace]
        installPrefixFlag = maybe ["--user"] (\prefix -> ["--prefix", T.pack prefix]) (kernelSpecInstallPrefix opts)
        cmd = concat [["kernelspec", "install"], installPrefixFlag, [SH.toTextIgnore kernelDir], replaceFlag]

    SH.silently $ SH.run ipython cmd

kernelSpecCreated :: SH.Sh Bool
kernelSpecCreated = do
  ipython <- locateIPython
  out <- SH.silently $ SH.run ipython ["kernelspec", "list"]
  let kernelspecs = map T.strip $ T.lines out
  return $ T.pack kernelName `elem` kernelspecs

-- | Replace "~" with $HOME if $HOME is defined. Otherwise, do nothing.
subHome :: String -> IO String
subHome path = SH.shelly $ do
  home <- T.unpack <$> fromMaybe "~" <$> SH.get_env "HOME"
  return $ replace "~" home path

-- | Get the path to an executable. If it doensn't exist, fail with an error message complaining
-- about it.
path :: Text -> SH.Sh SH.FilePath
path exe = do
  path <- SH.which $ SH.fromText exe
  case path of
    Nothing -> do
      liftIO $ putStrLn $ "Could not find `" ++ T.unpack exe ++ "` executable."
      fail $ "`" ++ T.unpack exe ++ "` not on $PATH."
    Just exePath -> return exePath

-- | Parse an IPython version string into a list of integers.
parseVersion :: String -> Maybe [Int]
parseVersion versionStr =
  let versions = map readMay $ split "." versionStr
      parsed = all isJust versions
  in if parsed
       then Just $ map fromJust versions
       else Nothing

-- | Get the absolute path to this IHaskell executable.
getIHaskellPath :: SH.Sh FilePath
getIHaskellPath = do
  --  Get the absolute filepath to the argument.
  f <- T.unpack <$> SH.toTextIgnore <$> liftIO getArgv0

  -- If we have an absolute path, that's the IHaskell we're interested in.
  if FP.isAbsolute f
    then return f
    else 
    -- Check whether this is a relative path, or just 'IHaskell' with $PATH resolution done by
    -- the shell. If it's just 'IHaskell', use the $PATH variable to find where IHaskell lives.
    if FP.takeFileName f == f
      then do
        ihaskellPath <- SH.which "ihaskell"
        case ihaskellPath of
          Nothing   -> error "ihaskell not on $PATH and not referenced relative to directory."
          Just path -> return $ T.unpack $ SH.toTextIgnore path
      else liftIO $ makeAbsolute f
#if !MIN_VERSION_directory(1, 2, 2)
-- This is included in later versions of `directory`, but we cannot use later versions because GHC
-- library depends on a particular version of it.
makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = fmap FP.normalise . absolutize
  where
    absolutize path -- avoid the call to `getCurrentDirectory` if we can
      | FP.isRelative path = fmap (FP.</> path) getCurrentDirectory
      | otherwise = return path
#endif
getSandboxPackageConf :: IO (Maybe String)
getSandboxPackageConf = SH.shelly $ do
  myPath <- getIHaskellPath
  let sandboxName = ".cabal-sandbox"
  if not $ sandboxName `isInfixOf` myPath
    then return Nothing
    else do
      let pieces = split "/" myPath
          sandboxDir = intercalate "/" $ takeWhile (/= sandboxName) pieces ++ [sandboxName]
      subdirs <- map fp <$> SH.ls (SH.fromText $ T.pack sandboxDir)
      let confdirs = filter (isSuffixOf ("packages.conf.d" :: String)) subdirs
      case confdirs of
        [] -> return Nothing
        dir:_ ->
          return $ Just dir
