{-# language NoImplicitPrelude, DoAndIfThenElse, OverloadedStrings, ExtendedDefaultRules #-}
{-# LANGUAGE CPP #-}
module IHaskellPrelude (
  module IHaskellPrelude,
  module X,

  -- Select reexports
  Data.Typeable.Typeable,
  Data.Typeable.cast,

  Data.Typeable.Proxy,

  GHC.Exts.IsString,
  GHC.Exts.IsList,

  System.IO.hPutStrLn,
  System.IO.hPutStr,
  System.IO.hPutChar,
  System.IO.hPrint,
  System.IO.stdout,
  System.IO.stderr,
  System.IO.stdin,
  System.IO.getChar,
  System.IO.getLine,
  System.IO.writeFile,
  System.IO.Handle,

  System.IO.Strict.readFile,
  System.IO.Strict.getContents,
  System.IO.Strict.hGetContents,

  Control.Exception.catch,
  Control.Exception.SomeException,

  Control.Applicative.Applicative(..),
  Control.Applicative.ZipList(..),
  (Control.Applicative.<$>),

  Control.Concurrent.MVar.MVar,
  Control.Concurrent.MVar.newMVar,
  Control.Concurrent.MVar.newEmptyMVar,
  Control.Concurrent.MVar.isEmptyMVar,
  Control.Concurrent.MVar.readMVar,
  Control.Concurrent.MVar.takeMVar,
  Control.Concurrent.MVar.putMVar,
  Control.Concurrent.MVar.modifyMVar,
  Control.Concurrent.MVar.modifyMVar_,

  Data.IORef.IORef,
  Data.IORef.readIORef,
  Data.IORef.writeIORef,
  Data.IORef.modifyIORef',
  Data.IORef.newIORef,



  -- Miscellaneous names
  Data.Map.Map,
  GHC.IO.FilePath,
  Data.Text.Text,
  Data.ByteString.ByteString,
  Text.Printf.printf,
  Data.Function.on,
  ) where

import           Prelude

import           Data.Semigroup         as X
import           Data.Monoid            as X hiding ((<>), First(..), Last(..))
import           Data.Tuple             as X
import           Control.Monad          as X
import           Data.Maybe             as X
import           Data.Either            as X
import           Control.Monad.IO.Class as X
import           Data.Ord               as X
import           GHC.Show               as X
import           GHC.Enum               as X
import           GHC.Num                as X
import           GHC.Real               as X
import           GHC.Err                as X hiding (absentErr)
import           GHC.Base               as X hiding (Any, mapM, foldr, sequence, many, (<|>), Module(..))
import           Data.List              as X hiding (head, last, tail, init, transpose, subsequences, permutations,
                                        foldl, foldl1, maximum, minimum, scanl, scanl1, scanr, scanr1,
                                        span, break, mapAccumL, mapAccumR, dropWhileEnd, (!!),
                                        elemIndices, elemIndex, findIndex, findIndices, zip5, zip6,
                                        zip7, zipWith5, zipWith6, zipWith7, unzip5, unzip6, unzip6,
                                        delete, union, lookup, intersect, insert, deleteBy,
                                        unionBy, intersectBy, group, groupBy, insertBy,
                                        maximumBy, minimumBy, genericLength, genericDrop, genericTake,
                                        genericSplitAt, genericIndex, genericReplicate, inits, tails)

import qualified Control.Applicative
import qualified Data.Typeable
import qualified Data.IORef
import qualified Data.Map
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Function
import qualified Data.List.NonEmpty
import qualified GHC.Exts
import qualified System.IO
import qualified System.IO.Strict
import qualified GHC.IO
import qualified Text.Printf
import qualified Control.Exception
import qualified Control.Concurrent.MVar

import qualified Data.List
import qualified Prelude as P

type LByteString = Data.ByteString.Lazy.ByteString

type LText = Data.Text.Lazy.Text

headMay :: [a] -> Maybe a
headMay = wrapEmpty head

tailMay :: [a] -> Maybe [a]
tailMay = wrapEmpty tail

lastMay :: [a] -> Maybe a
lastMay = wrapEmpty last

initMay :: [a] -> Maybe [a]
initMay = wrapEmpty init

maximumMay :: Ord a => [a] -> Maybe a
maximumMay = wrapEmpty maximum

minimumMay :: Ord a => [a] -> Maybe a
minimumMay = wrapEmpty minimum

wrapEmpty :: ([a] -> b) -> [a] -> Maybe b
wrapEmpty _ [] = Nothing
wrapEmpty f xs = Just (f xs)

maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumByMay _ [] = Nothing
maximumByMay f xs = Just (Data.List.maximumBy f xs)

minimumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumByMay _ [] = Nothing
minimumByMay f xs = Just (Data.List.minimumBy f xs)

readMay :: Read a => String -> Maybe a
readMay = fmap fst . headMay . reads

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . P.putStrLn

putStr :: (MonadIO m) => String -> m ()
putStr = liftIO . P.putStr

putChar :: MonadIO m => Char -> m ()
putChar = liftIO . P.putChar

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . P.print

nonEmptyToList :: Data.List.NonEmpty.NonEmpty a -> [a]
nonEmptyToList = Data.List.NonEmpty.toList
