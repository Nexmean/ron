{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module RON.Text.Serialize
    ( serializeAtom
    , serializeFrame
    , serializeFrames
    , serializeOp
    , serializeString
    , serializeUuid
    ) where

import           RON.Internal.Prelude

import           Control.Monad.State.Strict (State, evalState, state)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy.Search as BSL
import           Data.Text (Text)
import           Data.Traversable (for)

import           RON.Text.Common (opZero)
import           RON.Text.Serialize.UUID (serializeUuid, serializeUuidAtom,
                                          serializeUuidKey)
import           RON.Types (Atom (AFloat, AInteger, AString, AUuid),
                            Chunk (Query, Raw, Value), Frame, Op' (..),
                            RChunk (..), RawOp (..))
import           RON.UUID (UUID, zero)

serializeFrame :: Frame -> ByteStringL
serializeFrame chunks
    = (`BSLC.snoc` '.')
    . mconcat
    . (`evalState` opZero)
    $ traverse serializeChunk chunks

serializeFrames :: [Frame] -> ByteStringL
serializeFrames = foldMap serializeFrame

serializeChunk :: Chunk -> State RawOp ByteStringL
serializeChunk = \case
    Raw op      -> (<> " ;\n") <$> serializeOpZip op
    Value chunk -> serializeReducedChunk False chunk
    Query chunk -> serializeReducedChunk True  chunk

serializeReducedChunk :: Bool -> RChunk -> State RawOp ByteStringL
serializeReducedChunk isQuery RChunk{rchunkHeader, rchunkBody} =
    BSLC.unlines <$> liftA2 (:) serializeHeader serializeBody
  where
    serializeHeader = do
        h <- serializeOpZip rchunkHeader
        pure $ BSLC.unwords [h, if isQuery then "?" else "!"]
    serializeBody = for rchunkBody $ \op -> do
        o <- serializeOpZip op
        pure $ "\t" <> BSLC.unwords [o, ","]

serializeOp :: RawOp -> ByteStringL
serializeOp op = evalState (serializeOpZip op) opZero

serializeOpZip :: RawOp -> State RawOp ByteStringL
serializeOpZip this = state $ \prev -> let
    prev' = op' prev
    typ = serializeUuidKey (opType   prev)  zero             (opType   this)
    obj = serializeUuidKey (opObject prev)  (opType   this)  (opObject this)
    evt = serializeUuidKey (opEvent  prev') (opObject this)  (opEvent  this')
    loc = serializeUuidKey (opRef    prev') (opEvent  this') (opRef    this')
    payload  = serializePayload (opObject this) (opPayload this')
    in
    ( BSLC.unwords
        $   key '*' typ
        ++  key '#' obj
        ++  key '@' evt
        ++  key ':' loc
        ++  [payload | not $ BSL.null payload]
    , this
    )
  where
    this' = op' this
    key c u = [BSLC.cons c u | not $ BSL.null u]

serializeAtom :: Atom -> ByteStringL
serializeAtom a = evalState (serializeAtomZip a) zero

serializeAtomZip :: Atom -> State UUID ByteStringL
serializeAtomZip = \case
    AFloat   f -> pure $ BSLC.cons '^' $ BSLC.pack (show f)
    AInteger i -> pure $ BSLC.cons '=' $ BSLC.pack (show i)
    AString  s -> pure $ serializeString s
    AUuid    u ->
        state $ \prev -> (BSLC.cons '>' $ serializeUuidAtom prev u, u)

serializeString :: Text -> ByteStringL
serializeString =
    fixQuotes . BSL.replace "'" ("\\'" :: ByteString) . Json.encode
  where
    fixQuotes = (`BSLC.snoc` '\'') . BSLC.cons '\'' . BSL.init . BSL.tail

serializePayload :: UUID -> [Atom] -> ByteStringL
serializePayload prev =
    BSLC.unwords . (`evalState` prev) . traverse serializeAtomZip
