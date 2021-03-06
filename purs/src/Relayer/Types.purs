module Relayer.Types 
  ( SignedOrder(..)
  , UnsafeSignedOrder(..)
  , EthereumAddress(..)
  , GQLBody(..)
  , GraphQlQuery(..)
  , GraphQlQueryResponse(..)
  , GraphQlQueryResponseError(..)
  , Relayer(..)
  , runRelayer
  , Promise(..)
  , fromAff
  -- , Address
  ) where


import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Promise as Promise
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import HasJSRep (class HasJSRep)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Core.Signatures (Address)
import OhYes (class HasTSRep, toTSRep)
import Relayer.Errors (RelayerError, handleError)
import Type.Proxy (Proxy(..))

-------------------------------------------------------------------------------
-- | RelayerT
-------------------------------------------------------------------------------
newtype Relayer a = Relayer (ExceptT RelayerError Aff a)

derive newtype instance functorRelayer :: Functor Relayer
derive newtype instance applyRelayer :: Apply Relayer
derive newtype instance applicativeRelayer :: Applicative Relayer
derive newtype instance bindRelayer :: Bind Relayer 
derive newtype instance monadRelayer :: Monad Relayer 
derive newtype instance monadThrowRelayer :: MonadThrow RelayerError Relayer
derive newtype instance monadErrorRelayer :: MonadError RelayerError Relayer
derive newtype instance monadEffectRelayer :: MonadEffect Relayer
derive newtype instance monadAffRelayer :: MonadAff Relayer

runRelayer :: forall a. Relayer a -> Effect (Fiber a)
runRelayer (Relayer f) = launchAff (errH =<< (runExceptT f))
  where 
    errH = either handleError pure


-------------------------------------------------------------------------------
-- | UnsafeSignedOrder
-------------------------------------------------------------------------------
-- | Signed order with using javascript types for properties
type UnsafeSignedOrder = {
  hash :: String,
  senderAddress :: String,
  makerAddress :: String,
  takerAddress :: String,
  makerAssetData :: String,
  takerAssetData :: String,
  exchangeAddress :: String,
  feeRecipientAddress :: String,
  expirationTimeSeconds :: Number,
  makerFee :: String,
  takerFee :: String,
  makerAssetAmount :: String,
  takerAssetAmount :: String,
  salt :: String,
  signature :: String
}

-------------------------------------------------------------------------------
-- | SignedOrder
-------------------------------------------------------------------------------
type SignedOrder = {
  hash :: HexString,
  senderAddress :: Address,
  makerAddress :: Address,
  takerAddress :: Address,
  makerAssetData :: HexString,
  takerAssetData :: HexString,
  exchangeAddress :: Address,
  feeRecipientAddress :: Address,
  expirationTimeSeconds :: Int,
  makerFee :: BigNumber,
  takerFee :: BigNumber,
  makerAssetAmount :: BigNumber,
  takerAssetAmount :: BigNumber,
  salt :: BigNumber,
  signature :: HexString
}
-------------------------------------------------------------------------------
-- | EthereumAddress
-------------------------------------------------------------------------------
type EthereumAddress = {
    address :: String
}

-------------------------------------------------------------------------------
-- | GQLBody
-------------------------------------------------------------------------------
type GQLBody = {
  query :: String
}

-------------------------------------------------------------------------------
-- | GraphQlQuery
-------------------------------------------------------------------------------
data GraphQlQuery a = GraphQlQuery GQLBody (Proxy a)

derive instance genericGraphQlQuery :: Generic (GraphQlQuery a) _

instance showGraphQlQuery :: (Show a) => Show (GraphQlQuery a) where
  show = genericShow

instance eqGraphQlQuery :: (Eq a) => Eq (GraphQlQuery a) where
  eq = genericEq

-------------------------------------------------------------------------------
-- | GraphQlQueryResponseError
-------------------------------------------------------------------------------
type GraphQlQueryResponseError =  {
  message :: String,
  locations :: Array {
    line :: Int,
    column :: Int
  },
  path :: Array String
}

-------------------------------------------------------------------------------
-- | GraphQlQueryResponse
-------------------------------------------------------------------------------
type GraphQlQueryResponse a =  {
  data :: Maybe a,
  errors :: Maybe (Array GraphQlQueryResponseError)
}


-------------------------------------------------------------------------------
-- | Promise
-------------------------------------------------------------------------------
-- | Wrapper for Promise.Promise type for creating typescript types
newtype Promise a = Promise (Promise.Promise a)

instance hasJSRepPromise :: (HasJSRep a) => HasJSRep (Promise a)
instance hasTSRepPromise :: (HasTSRep a) => HasTSRep (Promise a) where
  toTSRep _ = "Promise<" <> a <> ">"  
    where
      a = toTSRep (Proxy :: Proxy a)

fromAff :: forall a. Aff a -> Effect (Promise a)
fromAff = map Promise <<< Promise.fromAff

-- -------------------------------------------------------------------------------
-- -- | Address
-- -------------------------------------------------------------------------------
-- newtype Address = Address Eth.Address

-- derive newtype instance addressShow :: Show Address
-- derive newtype instance addressEq :: Eq Address
-- derive newtype instance addressOrd :: Ord Address
-- derive newtype instance encodeAddress :: Encode Address
-- derive newtype instance decodeAddress :: Decode Address where
--   decode a = do
--     hxString <- decode a
--     either (fail <<< ForeignError) pure $ _decode hxString

-- instance decodeJsonAddress :: A.DecodeJson Address where
--   decodeJson json = do
--     hxString <- A.decodeJson json
--     _decode hxString

-- instance encodeJsonAddress :: A.EncodeJson Address where
--   encodeJson = A.encodeJson <<< unAddress

-- instance readFAddress :: ReadForeign Address where
--   readImpl = decode

-- instance writeFAddress :: WriteForeign Address where
--   writeImpl = encode
