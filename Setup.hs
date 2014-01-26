import Distribution.Simple

import Control.Applicative ((<$>))
import Data.List (isInfixOf)
import Codec.Archive.Tar (create)
import System.Directory (getDirectoryContents)

main = defaultMainWithHooks simpleUserHooks {
    preBuild = makeProfileTar
  }

makeProfileTar args flags = do
  putStrLn "Building profile.tar."

  let profileDir = "profile"
      tarFile = profileDir ++ "/profile.tar"
  files <- filter realFile <$> filter notProfileTar <$> getDirectoryContents profileDir
  print files
  create tarFile profileDir files
  preBuild simpleUserHooks args flags
  where
    notProfileTar str = not $ "profile.tar" `isInfixOf` str
    realFile str = str /= "." && str /= ".."
