module Generate (main) where


import Prelude

import Data.Foldable (intercalate)
import Effect (Effect)
import OhYes (generateTS)
import Text.Prettier (defaultOptions, format)
import Type.Proxy (Proxy(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)

import Relayer.Types (SignedOrder, EthereumAddress)

main :: Effect Unit
main = do
  writeTextFile UTF8 "./ts/src/purs/Types.ts" values
  where
    values = format defaultOptions $ intercalate "\n"
      [ 
        generateTS "SignedOrder" (Proxy :: Proxy SignedOrder)
      , generateTS "EthereumAddress" (Proxy :: Proxy EthereumAddress)
      ]
