module Relayer.Queries 
  ( buildInsertEthereumAddressQuery
  , InsertEthereumAddressResponse
  , buildInsertSignedOrderQuery
  , InsertSignedOrderResponse
  ) where

import Prelude

import Data.Nullable (Nullable)
import Network.Ethereum.Core.BigNumber (decimal, toString)
import Network.Ethereum.Core.Signatures (Address)
import Relayer.Types (GraphQlQuery(..), SignedOrder)
import Relayer.Utils (addressToString, hexStringToString, wrapDoubleQuotes)
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
-------------------------------------------------------------------------------
-- | buildInsertEthereumAddressQuery
-------------------------------------------------------------------------------
buildInsertEthereumAddressQuery :: forall r. { address :: Address | r} -> GraphQlQuery InsertEthereumAddressResponse
buildInsertEthereumAddressQuery { address } = GraphQlQuery { query: query } (Proxy :: Proxy InsertEthereumAddressResponse)
  where
    address' = addressToString $ address
    query = """
    mutation {
      createEthereumAddress(
        input: {
          ethereumAddress: {
            address:""" <> wrapDoubleQuotes address' <> """
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


-------------------------------------------------------------------------------
-- | InsertSignedOrderResponse
-------------------------------------------------------------------------------
type InsertSignedOrderResponse = {
  createSignedOrder :: Nullable {
    signedOrder :: {
      hash :: String
    }
  }
}
-------------------------------------------------------------------------------
-- | buildInsertSignedOrderQuery
-------------------------------------------------------------------------------
buildInsertSignedOrderQuery :: SignedOrder -> GraphQlQuery  InsertSignedOrderResponse
buildInsertSignedOrderQuery so = GraphQlQuery { query: query } (Proxy :: Proxy InsertSignedOrderResponse)
  where
    hash = hexToWrappedString so.hash
    senderAddress = addressToWrappedString so.senderAddress
    makerAddress = addressToWrappedString so.makerAddress
    takerAddress = addressToWrappedString so.takerAddress
    makerAssetData = hexToWrappedString so.makerAssetData
    takerAssetData = hexToWrappedString so.takerAssetData
    exchangeAddress = addressToWrappedString so.exchangeAddress
    feeRecipientAddress = addressToWrappedString so.feeRecipientAddress
    expirationTimeSeconds = show so.expirationTimeSeconds
    makerFee = toString decimal so.makerFee
    takerFee = toString decimal so.takerFee
    makerAssetAmount = toString decimal so.makerAssetAmount
    takerAssetAmount = toString decimal so.takerAssetAmount
    salt = wrapDoubleQuotes $ toString decimal so.salt
    signature = hexToWrappedString so.signature
    query = """
    mutation {
      createSignedOrder(
        input: {
          signedOrder: {
            hash: """ <> hash <> """, 
            senderAddress: """ <> senderAddress <>""", 
            makerAddress: """ <> makerAddress <> """, 
            takerAddress: """ <> takerAddress <> """, 
            makerAssetData: """ <> makerAssetData <> """, 
            takerAssetData: """ <> takerAssetData <> """, 
            exchangeAddress: """ <> exchangeAddress <> """, 
            feeRecipientAddress: """ <> feeRecipientAddress <> """, 
            expirationTimeSeconds: """ <> expirationTimeSeconds <> """, 
            makerFee: """ <> makerFee <> """, 
            takerFee: """ <> takerFee <> """, 
            makerAssetAmount: """ <> makerAssetAmount <> """, 
            takerAssetAmount: """ <> takerAssetAmount <> """, 
            salt: """ <> salt <> """, 
            signature: """ <> signature <> """
          }
        }
      ) {
        signedOrder {
          hash
        }
      }
    }
    """
    hexToWrappedString = wrapDoubleQuotes <<< hexStringToString
    addressToWrappedString = wrapDoubleQuotes <<< addressToString
