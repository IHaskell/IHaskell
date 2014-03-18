-- | Description : UUID generator and data structure
--
-- Generate, parse, and pretty print UUIDs for use with IPython.
module IPython.Message.UUID (
  UUID,
  random, randoms,
  ) where

import Control.Monad (mzero, replicateM)
import Control.Applicative ((<$>))
import Data.Text (pack)
import Data.Aeson
import Data.UUID.V4 (nextRandom)

import Text.Read as Read hiding (pfail, String)


-- We use an internal string representation because for the purposes of
-- IPython, it matters whether the letters are uppercase or lowercase and
-- whether the dashes are present in the correct locations. For the
-- purposes of new UUIDs, it does not matter, but IPython expects UUIDs
-- passed to kernels to be returned unchanged, so we cannot actually parse
-- them.

-- | A UUID (universally unique identifier).
data UUID = UUID String deriving (Show, Read, Eq, Ord)

-- | Generate a list of random UUIDs.
randoms :: Int      -- ^ Number of UUIDs to generate.
        -> IO [UUID]
randoms n = replicateM n random

-- | Generate a single random UUID.
random :: IO UUID
random = UUID <$> show <$> nextRandom

-- Allows reading and writing UUIDs as Strings in JSON. 
instance FromJSON UUID where
  parseJSON val@(String _) = UUID <$> parseJSON val

  -- UUIDs must be Strings.
  parseJSON _ = mzero

instance ToJSON UUID where
  -- Extract the string from the UUID.
  toJSON (UUID str) = String $ pack str
