module Relayer.Queries (buildInsertEthereumAddress) where

import Prelude
import Type.Proxy (Proxy(..))

import Relayer.Types (GQLBody(..), GraphQLQuery(..), InsertEthereumAddressResponse)

wrapDoubleQuotes :: String -> String
wrapDoubleQuotes str = "\"" <> str <> "\""

buildInsertEthereumAddress :: forall r. { address :: String | r} -> GraphQLQuery InsertEthereumAddressResponse
buildInsertEthereumAddress o = GraphQLQuery (GQLBody { query: query }) (Proxy :: Proxy InsertEthereumAddressResponse)
  where
    query = """
    mutation {
      createEthereumAddress(
        input: {
          ethereumAddress: {
            address:""" <> wrapDoubleQuotes o.address <> """
          }
        }
      ) {
        ethereumAddress {
          address
          createdAt
        }
      }
    }
    """

-- buildInsertEthereumAddress :: forall i. (address :: String | i) -> GraphQLQuery GQLBody  r
-- buildInsertEthereumAddress address = print (query address)
--   where
--     query = s """
--     mutation {
--       createSignedOrder(
--         input: {
--           signedOrder: {
--             hash: "", 
--             senderAddress: "", 
--             makerAddress: "", 
--             takerAddress: "", 
--             makerAssetData: "", 
--             takerAssetData: "", 
--             exchangeAddress: "", 
--             feeRecipientAddress: "", 
--             expirationTimeSeconds: 10, 
--             makerFee: 1.5, 
--             takerFee: 1.5, 
--             makerAssetAmount: 1.5, 
--             takerAssetAmount: 1.5, 
--             salt: "", 
--             signature: ""
--           }
--         }
--       ) {
--         signedOrder {
--           hash
--         }
--       }
--     }
--     """
