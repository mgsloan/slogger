{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE BangPatterns #-}

module Slogger where

import           Control.Monad.Logger
import qualified Data.IntMap as IM
import           GHC.Conc.Sync (ThreadId(..), myThreadId)
import           System.IO.Unsafe (unsafePerformIO)

type ThreadInt = Int

data ThreadLogInfo = ThreadLogInfo
    { parentIds :: [Int]
    , nextId :: Int
    }

data LogId = LogId ThreadInt Int

-- Yuck, I know!
sloggerMap :: IORef (IM.IntMap (IORef ThreadInfo))
sloggerMap = unsafePerformIO (newIORef IM.empty)
{-# NOINLINE sloggerMap #-}

getThreadLogInfoRef :: IO (IORef ThreadInfo)
getThreadLogInfoRef = do
    tid <- threadIdToInt <$> myThreadId
    mp <- readIORef sloggerMap
    minfo <- IM.lookup tid mp
    case minfo of
        Just info -> return info
        Nothing -> do
            info <- newIORef (ThreadLogInfo [] 0)
            atomicModifyIORef sloggerMap $ \mp' ->
                let (mexisting, mp'') = IM.insertLookupWithKey (\_ _ exising -> existing) tid info mp'
                 in (mp'', fromMaybe info mexisting)

pushParentLogId :: IO

popParentLogId :: LogId -> IO

getFreshLogIds :: IO (LogId, Maybe LogId)
getFreshLogIds = do
    ref <- getThreadLogInfoRef
    readIORef ref

threadIdToInt :: ThreadId -> ThreadInt
threadIdToInt !(ThreadId tid) = getThreadId tid

-- Copied from GHC.Conc.Sync
foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CInt

log :: LogLevel ->

-- openForAppending :: FilePath -> IO Handle
-- openForAppending fp = do
--     h <- openBinaryFile fp AppendMode
--     hSetBuffering h (BlockBuffering Nothing)
--     return h

-- openForReading :: FilePath -> IO Handle
-- openForReading fp = do
--     h <- openBinaryFile fp ReadMode
--     hSetBuffering h (BlockBuffering Nothing)
--     return h

-- withFileForAppending :: FilePath -> (Handle -> IO a) -> IO a
-- withFileForAppending fp f = do
--     h <- openForAppending fp
--     f h `finally` hClose h

-- withFileForReading :: FilePath -> (Handle -> IO a) -> IO a
-- withFileForReading fp f = do
--     h <- openForReading fp
--     f h `finally` hClose h
