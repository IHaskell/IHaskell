module IHaskell.Eval.Util (
  extensionFlag, setExtension,
  ExtFlag(..),
  ) where

-- GHC imports.
import GHC
import GhcMonad
import DynFlags

import Data.List (find)

data ExtFlag
     = SetFlag   ExtensionFlag
     | UnsetFlag ExtensionFlag

extensionFlag :: String -> Maybe ExtFlag
extensionFlag ext = 
  case find (flagMatches ext) xFlags of
    Just (_, flag, _) -> Just $ SetFlag flag
    -- If it doesn't match an extension name, try matching against
    -- disabling an extension.
    Nothing ->
      case find (flagMatchesNo ext) xFlags of
        Just (_, flag, _) -> Just $ UnsetFlag flag
        Nothing -> Nothing

  where
    -- Check if a FlagSpec matches an extension name.
    flagMatches ext (name, _, _) = ext == name

    -- Check if a FlagSpec matches "No<ExtensionName>".
    -- In that case, we disable the extension.
    flagMatchesNo ext (name, _, _) = ext == "No"  ++ name

-- Set an extension and update flags.
-- Return Nothing on success. On failure, return an error message.
setExtension :: GhcMonad m => String -> m (Maybe String)
setExtension ext = do
  flags <- getSessionDynFlags
  case extensionFlag ext of
    Nothing -> return $ Just $ "Could not parse extension name: " ++ ext
    Just flag -> do
      setSessionDynFlags $ 
        case flag of
          SetFlag ghcFlag -> xopt_set flags ghcFlag
          UnsetFlag ghcFlag -> xopt_unset flags ghcFlag
      return Nothing
