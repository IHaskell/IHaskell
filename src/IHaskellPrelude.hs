{-# LANGUAGE CPP #-}
module IHaskellPrelude (
  module IHaskellPrelude,
  module X,

  -- Select reexports
  Data.Typeable.Typeable,
  Data.Typeable.cast,

#if MIN_VERSION_ghc(7,8,0)
  Data.Typeable.Proxy,

  GHC.Exts.IsString,
  GHC.Exts.IsList,
#endif

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

import           Data.Monoid            as X
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
#if MIN_VERSION_ghc(7,10,0)
import           GHC.Base               as X hiding (Any, mapM, foldr, sequence, many, (<|>))
#else
import           GHC.Base               as X hiding (Any)
#endif
import           Data.List              as X hiding (head, last, tail, init, transpose, subsequences, permutations,
                                        foldl, foldl1, maximum, minimum, scanl, scanl1, scanr, scanr1,
                                        span, break, mapAccumL, mapAccumR, dropWhileEnd, (!!),
                                        elemIndices, elemIndex, findIndex, findIndices, zip5, zip6,
                                        zip7, zipWith5, zipWith6, zipWith7, unzip5, unzip6, unzip6,
                                        delete, union, lookup, intersect, insert, deleteBy,
                                        deleteFirstBy, unionBy, intersectBy, group, groupBy, insertBy,
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

(headMay, tailMay, lastMay, initMay, maximumMay, minimumMay) =
  (wrapEmpty head, wrapEmpty tail, wrapEmpty last,
   wrapEmpty init, wrapEmpty maximum, wrapEmpty minimum)
  where
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
