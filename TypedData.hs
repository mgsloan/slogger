{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module TypedData where

import           Control.Concurrent.Async
import           Control.Monad.State
import           Data.Binary (Binary, encode, decode)
import qualified Data.Binary.Typed.Internal as BT
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import           Data.Conduit.Binary (sourceHandle, sinkHandle)
import qualified Data.Conduit.List as CL
import           Data.Conduit.Serialization.Binary (conduitDecode, conduitEncode)
import           Data.Maybe
import           Data.String
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Typeable
import           GHC.Generics
import           GHC.IO.Handle
import           Prelude hiding (log)
import           System.IO
import           System.Process

-- TODO: memoize type serialization?  Serialize to a separate file?

appendTypedData :: (Typeable a, Binary a) => FilePath -> a -> IO Integer
appendTypedData fp x = withBinaryFile fp AppendMode (\h -> appendTypedData' h x)

readTypedData :: (Typeable a, Binary a) => FilePath -> Integer -> IO a
readTypedData fp pos = withBinaryFile fp ReadMode (\h -> readTypedData' h pos)

appendTypedData' :: forall a. (Typeable a, Binary a) => Handle -> a -> IO Integer
appendTypedData' h x = do
    let ty = BT.stripTypeRep (typeOf x)
    appendData h (RawData ty (encode x))

readTypedData' :: forall a. (Typeable a, Binary a) => Handle -> Integer -> IO a
readTypedData' h pos = do
    RawData ty bs <- readData h pos
    let expected = BT.stripTypeRep (typeOf (undefined :: a))
    if ty /= expected
        then fail $ "Type mismatch in decodeData (got " ++ show ty ++ ", but expected " ++ show expected ++ ")"
        else return $ decode bs

data RawData = RawData BT.TypeRep LB.ByteString
    deriving (Generic)

instance Binary RawData where

appendData :: Binary a => Handle -> a -> IO Integer
appendData h raw = do
    pos <- hFileSize h
    yield raw $= conduitEncode $$ sinkHandle h
    return pos

--TODO: handle EOF case

readData :: Binary a => Handle -> Integer -> IO a
readData h pos = do
    hSeek h AbsoluteSeek pos
    mdecoded <- sourceHandle h $= conduitDecode $$ CL.head
    maybe (fail "Failed to decode data.") return mdecoded

--TODO: avoid overhead of decoding twice, by computing the offset of
--the data which comes after the type.

ghciStr :: FilePath -> Integer -> IO String
ghciStr fp pos = do
    RawData ty _ <- withBinaryFile fp ReadMode (\h -> readData h pos)
    return $ "readTypedData \"" ++ fp ++ "\" " ++ show pos ++ " :: IO (" ++ show (BT.unStripTypeRep ty) ++ ")"

runGhciWithData :: FilePath -> Integer -> IO ()
runGhciWithData fp pos = do
    cmd <- ghciStr fp pos
    (hin, hout, herr, _ph) <- runInteractiveProcess "ghci" ["Slogger.hs"] Nothing Nothing
    void $ (sourceHandle hout $$ sinkHandle stdout) `concurrently`
        (sourceHandle herr $$ sinkHandle stderr) `concurrently` do
            yield (encodeUtf8 (T.pack ("x <- " ++ cmd ++ "\n"))) $$ sinkHandle hin
            (sourceHandle stdin $$ sinkHandle hin)
    return ()
