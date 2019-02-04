module Relayer.Types 
  ( SignedOrder(..)
  , EthereumAddress(..)
  , GQLBody(..)
  , GraphQlQuery(..)
  , InsertEthereumAddressResponse(..)
  , GraphQlQueryResponse(..)
  , Relayer(..)
  , runRelayer
  , Promise(..)
  , fromAff
  ) where


import Prelude

import Affjax.RequestBody (RequestBody(..))
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Promise (fromAff)
import Control.Promise as Promise
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
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
import HasJSRep (class HasJSRep)
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
-- | SignedOrder
-------------------------------------------------------------------------------
type SignedOrder = {
    hi :: String
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
newtype GQLBody = GQLBody {
  query :: String
}
derive instance genericGQLBody :: Generic GQLBody _
instance showGQLBody :: Show GQLBody where
  show = genericShow
instance eqGQLBody :: Eq GQLBody where
  eq = genericEq
instance encodeJsonGQLBody :: EncodeJson GQLBody where
  encodeJson a = genericEncodeJson a
instance decodeJsonGQLBody :: DecodeJson GQLBody where
  decodeJson a = genericDecodeJson a

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
-- | GraphQlQueryResponse
-------------------------------------------------------------------------------
newtype GraphQlQueryResponse a = GraphQlQueryResponse {
  data :: Maybe a,
  errors :: Maybe (Array String)
}

derive instance genericGraphQlQueryResponse :: Generic (GraphQlQueryResponse a) _

instance showGraphQlQueryResponse :: (Show a) => Show (GraphQlQueryResponse a) where
  show = genericShow

instance eqGraphQlQueryResponse :: (Eq a) => Eq (GraphQlQueryResponse a) where
  eq = genericEq

instance encodeJsonGraphQlQueryResponse :: (EncodeJson a) => EncodeJson (GraphQlQueryResponse a) where
  encodeJson a = genericEncodeJson a
instance decodeJsonGraphQlQueryResponse :: (DecodeJson a) => DecodeJson (GraphQlQueryResponse a) where
  decodeJson a = genericDecodeJson a

-------------------------------------------------------------------------------
-- | InsertEthereumAddressResponse
-------------------------------------------------------------------------------
newtype InsertEthereumAddressResponse = InsertEthereumAddressResponse {
  ethereumAddress :: {
    address :: String,
    createdAt :: String
  }
}
derive instance genericInsertEthereumAddressResponse :: Generic InsertEthereumAddressResponse _

instance showInsertEthereumAddressResponse :: Show InsertEthereumAddressResponse where
  show = genericShow

instance eqInsertEthereumAddressResponse ::  Eq InsertEthereumAddressResponse  where
  eq = genericEq

instance encodeJsonInsertEthereumAddressResponse :: EncodeJson InsertEthereumAddressResponse where
  encodeJson a = genericEncodeJson a
instance decodeJsonInsertEthereumAddressResponse :: DecodeJson InsertEthereumAddressResponse where
  decodeJson a = genericDecodeJson a
instance hasJSRepEthereumAddressResponse :: HasJSRep InsertEthereumAddressResponse
derive newtype instance hasTSRepEthereumAddressResponse :: HasTSRep InsertEthereumAddressResponse


newtype Promise a = Promise (Promise.Promise a)

instance hasJSRepPromise :: (HasJSRep a) => HasJSRep (Promise a)
instance hasTSRepPromise :: (HasTSRep a) => HasTSRep (Promise a) where
  toTSRep _ = "Promise<" <> a <> ">"  
    where
      a = toTSRep (Proxy :: Proxy a)

fromAff :: forall a. Aff a -> Effect (Promise a)
fromAff = map Promise <<< Promise.fromAff
