{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module TypedData where

import           Control.Concurrent.Async
import           Control.Monad.State
import           Data.Binary (Binary, encode, decode)
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import           Data.Conduit.Binary (sourceHandle, sinkHandle)
import           Data.Maybe
import           Data.String
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Typeable hiding (TypeRep, TyCon)
import qualified Data.Typeable as Ty
import           GHC.Generics
import           GHC.IO.Handle
import           Prelude hiding (log)
import           System.IO
import           System.Process
import           Language.Haskell.TH.Lift
import           Instances.TH.Lift ()

appendTypedData :: (Typeable a, Binary a) => FilePath -> a -> IO Integer
appendTypedData fp x = withBinaryFile fp AppendMode (\h -> appendTypedData' h x)

readTypedData :: (Typeable a, Binary a) => FilePath -> Integer -> IO a
readTypedData fp pos = withBinaryFile fp ReadMode (\h -> readTypedData' h pos)

appendTypedData' :: forall a. (Typeable a, Binary a) => Handle -> a -> IO Integer
appendTypedData' h x = do
    let ty = stripTypeRep (typeOf x)
    appendData' h (RawData ty (encode x))

readTypedData' :: forall a. (Typeable a, Binary a) => Handle -> Integer -> IO a
readTypedData' h pos = do
    RawData ty bs <- readData h pos
    let expected = stripTypeRep (typeOf (undefined :: a))
    if ty /= expected
        then fail $ "Type mismatch in decodeData (got " ++ show ty ++ ", but expected " ++ show expected ++ ")"
        else return $ decode bs

toRawData :: forall a. (Typeable a, Binary a) => a -> RawData
toRawData x = RawData ty (encode x)
  where
    ty = stripTypeRep (typeOf x)

data RawData = RawData TypeRep LB.ByteString
    deriving (Generic)

instance Binary RawData where

appendData :: Binary a => FilePath -> a -> IO Integer
appendData fp x = withBinaryFile fp AppendMode $ \h -> appendData' h x

appendData' :: Binary a => Handle -> a -> IO Integer
appendData' h raw = do
    pos <- hFileSize h
    LB.hPut h (encode raw)
    return pos

--TODO: handle EOF case

readData :: Binary a => Handle -> Integer -> IO a
readData h pos = do
    hSeek h AbsoluteSeek pos
    fmap decode $ LB.hGetContents h

--TODO: avoid overhead of decoding twice, by computing the offset of
--the data which comes after the type.

ghciStr :: FilePath -> Integer -> IO String
ghciStr fp pos = do
    RawData ty _ <- withBinaryFile fp ReadMode (\h -> readData h pos)
    return $ "readTypedData \"" ++ fp ++ "\" " ++ show pos ++ " :: IO (" ++ show (unStripTypeRep ty) ++ ")"

runGhciWithData :: FilePath -> Integer -> IO ()
runGhciWithData fp pos = do
    cmd <- ghciStr fp pos
    (hin, hout, herr, _ph) <- runInteractiveProcess "ghci" ["Slogger.hs"] Nothing Nothing
    void $ (sourceHandle hout $$ sinkHandle stdout) `concurrently`
        (sourceHandle herr $$ sinkHandle stderr) `concurrently` do
            yield (encodeUtf8 (T.pack ("x <- " ++ cmd ++ "\n"))) $$ sinkHandle hin
            (sourceHandle stdin $$ sinkHandle hin)
    return ()

-- Copied from binary-typed package, by David Luposchainsky
-- http://hackage.haskell.org/package/binary-typed

-- | 'Ty.TypeRep' without the (internal) fingerprint.
data TypeRep = TypeRep TyCon [TypeRep]
      deriving (Eq, Ord, Generic)
instance Binary TypeRep

instance Show TypeRep where
      show = show . unStripTypeRep

-- | 'Ty.TyCon' without the (internal) fingerprint.
data TyCon = TyCon String String String -- ^ Package, module, constructor name
      deriving (Eq, Ord, Generic)
instance Binary TyCon

instance Show TyCon where
      show = show . unStripTyCon

-- | Strip a 'Ty.TypeRep' off the fingerprint. Inverse of 'unStripTypeRep'.
stripTypeRep :: Ty.TypeRep -> TypeRep
stripTypeRep typerep = TypeRep (stripTyCon tycon) (map stripTypeRep args)
      where (tycon, args) = splitTyConApp typerep

-- | Add a fingerprint to a 'TypeRep'. Inverse of 'stripTypeRep'.
unStripTypeRep :: TypeRep -> Ty.TypeRep
unStripTypeRep (TypeRep tyCon args) = Ty.mkTyConApp (unStripTyCon tyCon)
                                                    (map unStripTypeRep args)

-- | Strip a 'Ty.TyCon' off the fingerprint. Inverse of 'unStripTyCon'.
stripTyCon :: Ty.TyCon -> TyCon
stripTyCon tycon = TyCon (tyConPackage tycon)
                         (tyConModule  tycon)
                         (tyConName    tycon)
                         -- The Typeable API doesn't expose the
                         -- TyCon constructor, so pattern matching
                         -- is not possible here (without depending
                         -- on Typeable.Internal).

-- | Add a fingerprint to a 'TyCon'. Inverse of 'stripTyCon'.
unStripTyCon :: TyCon -> Ty.TyCon
unStripTyCon (TyCon p m n) = Ty.mkTyCon3 p m n -- package, module, name

$(deriveLiftMany [''RawData, ''TypeRep, ''TyCon])
