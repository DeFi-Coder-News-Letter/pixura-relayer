module Relayer.Types 
  ( SignedOrder(..)
  , EthereumAddress(..)
  , GQLBody(..)
  , GraphQLQuery(..)
  , InsertEthereumAddressResponse(..)
  , GraphQLQueryResponse(..)
  ) where


import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Type.Proxy (Proxy)


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
-- | GraphQLQuery
-------------------------------------------------------------------------------
data GraphQLQuery a = GraphQLQuery GQLBody (Proxy a)

derive instance genericGraphQLQuery :: Generic (GraphQLQuery a) _

instance showGraphQLQuery :: (Show a) => Show (GraphQLQuery a) where
  show = genericShow

instance eqGraphQLQuery :: (Eq a) => Eq (GraphQLQuery a) where
  eq = genericEq

-------------------------------------------------------------------------------
-- | GraphQLQueryResponse
-------------------------------------------------------------------------------
newtype GraphQLQueryResponse a = GraphQLQueryResponse {
  data :: Maybe a,
  errors :: Maybe (Array String)
}

derive instance genericGraphQLQueryResponse :: Generic (GraphQLQueryResponse a) _

instance showGraphQLQueryResponse :: (Show a) => Show (GraphQLQueryResponse a) where
  show = genericShow

instance eqGraphQLQueryResponse :: (Eq a) => Eq (GraphQLQueryResponse a) where
  eq = genericEq

instance encodeJsonGraphQLQueryResponse :: (EncodeJson a) => EncodeJson (GraphQLQueryResponse a) where
  encodeJson a = genericEncodeJson a
instance decodeJsonGraphQLQueryResponse :: (DecodeJson a) => DecodeJson (GraphQLQueryResponse a) where
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
