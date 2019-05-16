{-# LANGUAGE BangPatterns #-}

module CHT
  ( ConcurrentHashTable (..)
  , getCHT
  , newCHT
  , putCHT
  , sizeCHT
  ) where

import Control.Concurrent.MVar (modifyMVar_, MVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Exception (bracket)
import Control.Loop (numLoop, numLoopState)
import Control.Monad (forM_)
import Control.Monad.Loops (whileM_)
import Data.Array.IO (IOArray)
import qualified Data.Array.Base as A (getNumElements, unsafeNewArray_, unsafeRead, unsafeWrite)
import Data.Hashable (hash, Hashable)
import Data.IORef (atomicModifyIORef', atomicWriteIORef, IORef,
                   modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import System.Random (randomIO)

data Node k v
  = Empty
  | Shifted
  | Node !k !v
  deriving (Show)

data NextCHT k v = None | Creating | Next (CHT k v)

fromNext :: NextCHT k v -> CHT k v
fromNext (Next cht) = cht
fromNext Creating   = error "fromNext: Creating"
fromNext None       = error "fromNext: None"

data CHT k v
  = CHT {
    chtNext    :: !(IORef (NextCHT k v))
  , chtDataArr :: !(IOArray Int (MVar (Node k v)))
  }

newtype Counter = Counter (IOArray Int (MVar Int))

getCounterVal :: Counter -> IO Int
getCounterVal (Counter arr) = do
  arrSize <- A.getNumElements arr
  numLoopState 0 (arrSize - 1) 0 $ \acc i -> do
    cur  <- A.unsafeRead arr i
    cur' <- readMVar cur
    return $ cur' + acc

incCounter :: Counter -> IO ()
incCounter (Counter arr) = do
  i    <- randomIO
  ith  <- A.unsafeRead arr (i `mod` 4)
  modifyMVar_ ith $ \s -> return $ s + 1

data ConcurrentHashTable k v = ConcurrentHashTable {
    chtSize :: !Counter
  , chtCHT  :: !(IORef (CHT k v))
  }

lookup_ :: (Eq k, Hashable k) => k -> CHT k v -> IO (MVar (Node k v), Node k v)
lookup_ !k !cht' = do
  let !h    = hash k
      !arr' = chtDataArr cht'
  !cap' <- A.getNumElements arr'
  cht   <- newIORef cht'
  arr   <- newIORef arr'
  cap   <- newIORef cap'
  ind   <- newIORef $ h `mod` cap'
  c     <- newIORef True
  res   <- newIORef Nothing
  whileM_ (readIORef c) $ do
    curArr' <- readIORef arr
    ind' <- readIORef ind
    el   <- A.unsafeRead curArr' ind'
    el'  <- takeMVar el
    !c'  <- case el' of Node key _ -> if hash key == h
                                      then if key == k
                                           then return False
                                           else do
                                                  putMVar el el'
                                                  curCap <- readIORef cap
                                                  modifyIORef' ind $ (`mod` curCap) . (+ 1)
                                                  return True
                                      else do
                                             putMVar el el'
                                             curCap <- readIORef cap
                                             modifyIORef' ind $ (`mod` curCap) . (+ 1)
                                             return True
                        Shifted    -> do
                                        putMVar el el'
                                        curCht  <- readIORef cht
                                        nextCht <- fromNext <$> (readIORef $ chtNext curCht)
                                        writeIORef cht nextCht
                                        let curArr = chtDataArr nextCht
                                        curCap  <- A.getNumElements curArr
                                        writeIORef arr curArr
                                        writeIORef cap curCap
                                        writeIORef ind $ h `mod` curCap
                                        return True
                        Empty      -> return False
    writeIORef c c'
    if not c'
    then writeIORef res $ Just (el, el')
    else return ()
  fromJust <$> readIORef res

getLastCHT_ :: (Eq k, Hashable k) => ConcurrentHashTable k v -> IO (CHT k v)
getLastCHT_ !table = do
  !cht'  <- readIORef $ chtCHT table
  !cht   <- newIORef cht'
  !c     <- newIORef True
  whileM_ (readIORef c) $ do
    curCht  <- readIORef cht
    curNext <- readIORef $ chtNext curCht
    case curNext of Next nextCht -> writeIORef cht nextCht
                    _            -> writeIORef c False
  readIORef cht

shiftEls_ :: (Eq k, Hashable k) => CHT k v -> CHT k v -> IO ()
shiftEls_ !src !dst = do
  let srcArr = chtDataArr src
  !srcCap <- A.getNumElements srcArr
  forM_ [0..(srcCap - 1)] $ \i -> do
    el  <- A.unsafeRead srcArr i
    el' <- takeMVar el
    case el' of Node k' v' -> do
                                (newEl, _) <- lookup_ k' dst
                                putMVar newEl $ Node k' v'
                                putMVar el Shifted
                Empty      -> putMVar el Shifted
                Shifted    -> error "shiftEls_: Shifted element" -- must be impossible to get

rmCHT_ :: (Eq k, Hashable k) => ConcurrentHashTable k v -> CHT k v -> IO ()
rmCHT_ !table !cht = do
  chtCap   <- A.getNumElements $ chtDataArr cht
  nextCHT  <- fromNext <$> (readIORef $ chtNext cht)
  firstCHT <- readIORef $ chtCHT table
  firstCap <- A.getNumElements $ chtDataArr firstCHT
  if firstCap == chtCap
  then atomicWriteIORef (chtCHT table) nextCHT
  else do
    !curCHT <- newIORef firstCHT
    !c     <- newIORef True
    whileM_ (readIORef c) $ do
      curCHT'  <- readIORef curCHT
      nextCHT' <- fromNext <$> (readIORef $ chtNext curCHT')
      nextCap  <- A.getNumElements $ chtDataArr nextCHT'
      if chtCap == nextCap
      then do
             atomicWriteIORef (chtNext curCHT') (Next nextCHT)
             writeIORef c False
      else writeIORef curCHT nextCHT'

startCap_ :: Int
startCap_ = 32

newCHT_ :: (Eq k, Hashable k) => Int -> IO (CHT k v)
newCHT_ !cap = do
  n    <- newIORef None
  !arr <- A.unsafeNewArray_ (0, cap - 1)
  numLoop 0 (cap - 1) $ \i -> do
    el <- newMVar Empty
    A.unsafeWrite arr i el
  return $ CHT n arr

newCHT :: (Eq k, Hashable k) => IO (ConcurrentHashTable k v)
newCHT = do
  !cnt  <- A.unsafeNewArray_ (0, 3)
  numLoop 0 3 $ \i -> do
    el <- newMVar 0
    A.unsafeWrite cnt i el
  !arr' <- newCHT_ startCap_
  !arr  <- newIORef arr'
  return $ ConcurrentHashTable (Counter cnt) arr

getCHT :: (Eq k, Hashable k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT !key !table = do
  cht' <- readIORef $ chtCHT table
  bracket
    (lookup_ key cht')
    (\(!el, !el') -> putMVar el el')
    (\(_, !el') ->
      case el' of Node _ v' -> return $ Just v'
                  Empty     -> return $ Nothing
                  Shifted   -> error "getCHT: Shifted from lookup_") -- must be impossible to get

putCHT :: (Eq k, Hashable k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT !key !val !table = do
  cht' <- readIORef $ chtCHT table
  bracket
    (lookup_ key cht')
    (\(!el, !el') ->
      case el' of Node _ _ -> putMVar el $ Node key val
                  Empty    -> do
                                incCounter $ chtSize table
                                size' <- getCounterVal $ chtSize table
                                putMVar el $ Node key val
                                lastCHT <- getLastCHT_ table
                                let curArr = chtDataArr lastCHT
                                curCap  <- A.getNumElements curArr
                                if size' * 2 > curCap
                                then do
                                  modified <- atomicModifyIORef' (chtNext lastCHT) $ \mbCHT ->
                                    case mbCHT of None     -> (Creating, True)
                                                  Creating -> (Creating, False)
                                                  Next cht -> (Next cht, False)
                                  if modified
                                  then do
                                    nextCHT  <- newCHT_ $ curCap * 2
                                    atomicModifyIORef' (chtNext lastCHT) $ \_ ->
                                      (Next nextCHT, ())
                                    newLastCHT <- fromNext <$> (readIORef $ chtNext lastCHT)
                                    shiftEls_ lastCHT newLastCHT
                                    rmCHT_ table lastCHT
                                  else return ()
                                else return ()
                  Shifted  -> error "putCHT: Shifted from lookup_") -- must be impossible to get
    (\_ -> return ())

sizeCHT :: (Eq k, Hashable k) => ConcurrentHashTable k v -> IO Int
sizeCHT !table = do
  let si = chtSize table
  getCounterVal si
