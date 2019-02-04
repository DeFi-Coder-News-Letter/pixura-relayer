module Relayer.Queries (buildInsertEthereumAddress, InsertEthereumAddressResponse) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Relayer.Types (GQLBody(..), GraphQlQuery(..))
import Relayer.Utils (wrapDoubleQuotes)
import Type.Proxy (Proxy(..))

-------------------------------------------------------------------------------
-- | InsertEthereumAddressResponse
-------------------------------------------------------------------------------
type InsertEthereumAddressResponse = {
  createEthereumAddress :: Nullable {
    ethereumAddress :: {
      address :: String,
      createdAt :: String
    }
  }
}

buildInsertEthereumAddress :: forall r. { address :: String | r} -> GraphQlQuery InsertEthereumAddressResponse
buildInsertEthereumAddress o = GraphQlQuery { query: query } (Proxy :: Proxy InsertEthereumAddressResponse)
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

-- buildInsertEthereumAddress :: forall i. (address :: String | i) -> GraphQlQuery GQLBody  r
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
