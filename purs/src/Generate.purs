module Generate (main) where


import Prelude

import Data.Foldable (intercalate)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import OhYes (generateTS)
import Relayer.Queries (InsertEthereumAddressResponse, InsertSignedOrderResponse)
import Relayer.Types (EthereumAddress, Promise, UnsafeSignedOrder)
import Text.Prettier (defaultOptions, format)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  writeTextFile UTF8 "./ts/src/purs/Types.ts" values
  where
    values = format defaultOptions $ intercalate "\n"
      [ 
        generateTS "EthereumAddress" (Proxy :: Proxy EthereumAddress)
      , generateTS "InsertEthereumAddressResponse" (Proxy :: Proxy InsertEthereumAddressResponse)
      , generateTS "insertEthereumAddressFn" (Proxy :: Proxy (EthereumAddress -> (Promise InsertEthereumAddressResponse)))
      
      , generateTS "SignedOrder" (Proxy :: Proxy UnsafeSignedOrder)
      , generateTS "InsertSignedOrderResponse" (Proxy :: Proxy InsertSignedOrderResponse)
      , generateTS "insertSignedOrderFn" (Proxy :: Proxy (UnsafeSignedOrder -> (Promise InsertSignedOrderResponse)))
      ]
