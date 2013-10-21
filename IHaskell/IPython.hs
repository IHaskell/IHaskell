module IHaskell.IPython (
  runIHaskell,
  setupIPythonProfile
) where

import ClassyPrelude
import Shelly hiding (find, trace)
import Text.Printf

-- | Run IPython with any arguments.
ipython :: [Text] -> Sh ()
ipython args = do
  path <- which "ipython"
  case path of
    Nothing -> putStrLn "Could not find `ipython` executable."
    Just ipythonPath -> run_ ipythonPath args

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

  -- Find out where IHaskell lives.
  ihaskellPath <- which "IHaskell"
  case ihaskellPath of
    Nothing -> putStrLn "IHaskell not on $PATH."
    Just path -> 
      -- Finally, write configs!
      writeConfigFilesTo profileDir (trace (unpack $ toTextIgnore path) $ unpack $ toTextIgnore path)

-- | Write IPython configuration files to the profile directory.
writeConfigFilesTo :: Text      -- ^ Profile directory to write to. Must have a trailing slash.
                   -> String    -- ^ Path to IHaskell executable.
                   -> Sh ()
writeConfigFilesTo profileDir ihaskellPath = writeFile (fromText configFile) config
  where
    configFile = profileDir ++ "ipython_config.py"

    config :: String
    config = unlines
        [ "c = get_config()"
        , printf "exe = '%s'.replace(' ', '\\\\ ')" ihaskellPath
        , "c.KernelManager.kernel_cmd = [exe, 'kernel', '{connection_file}']"
        , "c.Session.key = b''"
        , "c.Session.keyfile = b''"
        ]
