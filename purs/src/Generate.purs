module Generate (main) where


import Prelude

import Data.Foldable (intercalate)
import Data.Function.Uncurried (Fn1)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import OhYes (generateTS)
import Relayer.Pixura (insertEtherAddressFn)
import Relayer.Types (EthereumAddress, InsertEthereumAddressResponse, Promise(..), SignedOrder)
import Text.Prettier (defaultOptions, format)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  writeTextFile UTF8 "./ts/src/purs/Types.ts" values
  where
    values = format defaultOptions $ intercalate "\n"
      [ 
        generateTS "SignedOrder" (Proxy :: Proxy SignedOrder)
      , generateTS "EthereumAddress" (Proxy :: Proxy EthereumAddress)
      , generateTS "InsertEthereumAddressResponse" (Proxy :: Proxy InsertEthereumAddressResponse)
      , generateTS "insertEthereumAddressFn" (Proxy :: Proxy (EthereumAddress -> (Promise InsertEthereumAddressResponse)))
      ]
