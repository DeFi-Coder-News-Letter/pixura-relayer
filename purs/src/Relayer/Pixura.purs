module Relayer.Pixura (insertEthereumAddress) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError)
import Relayer.Queries (buildInsertEthereumAddress)
import Relayer.Types (EthereumAddress, GraphQLQueryResponse(..), InsertEthereumAddressResponse)
import Relayer.Utils (queryGraphQlApi)

insertEthereumAddress :: EthereumAddress -> Aff InsertEthereumAddressResponse
insertEthereumAddress ea = do
  GraphQLQueryResponse gqlRes <- queryGraphQlApi "https://ropsten.pixura.io/graphql" (buildInsertEthereumAddress ea)
  case gqlRes.data of
    Nothing -> throwError <<< error $ show gqlRes.errors
    Just insertRes -> pure insertRes


