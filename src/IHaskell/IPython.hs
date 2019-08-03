{-# language NoImplicitPrelude, DoAndIfThenElse, OverloadedStrings, ExtendedDefaultRules #-}

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

import qualified Shelly as SH
import qualified System.IO as IO
import qualified System.FilePath as FP
import           System.Directory
import           System.Environment (getExecutablePath)
import           System.Exit (exitFailure)
import           Data.Aeson (toJSON)
import           Data.Aeson.Text (encodeToTextBuilder)
import           Data.Text.Lazy.Builder (toLazyText)

import qualified Paths_ihaskell as Paths

import qualified GHC.Paths
import           IHaskell.Types

import           StringUtils (replace, split)

data KernelSpecOptions =
       KernelSpecOptions
         { kernelSpecGhcLibdir :: String           -- ^ GHC libdir.
         , kernelSpecRTSOptions :: [String]        -- ^ Runtime options to use.
         , kernelSpecDebug :: Bool                 -- ^ Spew debugging output?
         , kernelSpecConfFile :: IO (Maybe String) -- ^ Filename of profile JSON file.
         , kernelSpecInstallPrefix :: Maybe String
         , kernelSpecUseStack :: Bool              -- ^ Whether to use @stack@ environments.
         }

defaultKernelSpecOptions :: KernelSpecOptions
defaultKernelSpecOptions = KernelSpecOptions
  { kernelSpecGhcLibdir = GHC.Paths.libdir
  , kernelSpecRTSOptions = ["-M3g", "-N2"]  -- Memory cap 3 GiB,
                                            -- multithreading on two processors.
  , kernelSpecDebug = False
  , kernelSpecConfFile = defaultConfFile
  , kernelSpecInstallPrefix = Nothing
  , kernelSpecUseStack = False
  }

-- | The IPython kernel name.
kernelName :: String
kernelName = "haskell"

ipythonCommand :: SH.Sh SH.FilePath
ipythonCommand = do
  jupyterMay <- SH.which "jupyter"
  return $
    case jupyterMay of
      Nothing -> "ipython"
      Just _  -> "jupyter"

locateIPython :: SH.Sh SH.FilePath
locateIPython = do
  mbinary <- SH.which "jupyter"
  case mbinary of
    Nothing      -> SH.errorExit "The Jupyter binary could not be located"
    Just ipython -> return ipython

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
  fp <$> ensure (return (home SH.</> (".ihaskell" :: SH.FilePath)))

getIHaskellDir :: IO String
getIHaskellDir = SH.shelly ihaskellDir

defaultConfFile :: IO (Maybe String)
defaultConfFile = fmap (fmap fp) . SH.shelly $ do
  filename <- (SH.</> ("rc.hs" :: SH.FilePath)) <$> ihaskellDir
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
    Just _ -> pure ()

  where
    badIPython :: Text -> SH.Sh ()
    badIPython message = liftIO $ do
      IO.hPutStrLn IO.stderr (T.unpack message)
      exitFailure

-- | Install an IHaskell kernelspec into the right location. The right location is determined by
-- using `ipython kernelspec install --user`.
installKernelspec :: Bool -> KernelSpecOptions -> SH.Sh ()
installKernelspec repl opts = void $ do
  ihaskellPath <- getIHaskellPath
  confFile <- liftIO $ kernelSpecConfFile opts

  let kernelFlags :: [String]
      kernelFlags =
        ["--debug" | kernelSpecDebug opts] ++
        (case confFile of
           Nothing   -> []
           Just file -> ["--conf", file])
        ++ ["--ghclib", kernelSpecGhcLibdir opts]
        ++ (case kernelSpecRTSOptions opts of
             [] -> []
             _ -> "+RTS" : kernelSpecRTSOptions opts ++ ["-RTS"])
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
    let filename = kernelDir SH.</> ("kernel.json" :: SH.FilePath)

    SH.mkdir_p kernelDir
    SH.writefile filename $ LT.toStrict $ toLazyText $ encodeToTextBuilder $ toJSON kernelSpec
    let files = ["kernel.js", "logo-64x64.svg"]
    forM_ files $ \file -> do
      src <- liftIO $ Paths.getDataFileName $ "html/" ++ file
      SH.cp (SH.fromText $ T.pack src) (tmp SH.</> kernelName SH.</> file)

    ipython <- locateIPython

    let replaceFlag = ["--replace" | repl]
        installPrefixFlag = maybe ["--user"] (\prefix -> ["--prefix", T.pack prefix]) (kernelSpecInstallPrefix opts)
        cmd = concat [["kernelspec", "install"], installPrefixFlag, [SH.toTextIgnore kernelDir], replaceFlag]

    SH.silently $ SH.run ipython cmd

-- | Replace "~" with $HOME if $HOME is defined. Otherwise, do nothing.
subHome :: String -> IO String
subHome path = SH.shelly $ do
  home <- T.unpack <$> fromMaybe "~" <$> SH.get_env "HOME"
  return $ replace "~" home path

-- | Get the absolute path to this IHaskell executable.
getIHaskellPath :: SH.Sh FilePath
getIHaskellPath = do
  --  Get the absolute filepath to the argument.
  f <- liftIO getExecutablePath

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
