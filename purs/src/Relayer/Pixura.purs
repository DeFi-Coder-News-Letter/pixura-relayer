module Relayer.Pixura (insertEthereumAddress, insertEtherAddressFn) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
import Effect.Aff (joinFiber, throwError)
import Effect.Aff.Class (class MonadAff)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Relayer.Errors (RelayerError(..))
import Relayer.Queries (buildInsertEthereumAddress, InsertEthereumAddressResponse)
import Relayer.Types (EthereumAddress, Promise, fromAff, runRelayer)
import Relayer.Utils (queryGraphQlApi)


insertEtherAddressFn :: EffectFn1 EthereumAddress (Promise InsertEthereumAddressResponse)
insertEtherAddressFn = mkEffectFn1 fn
  where
    fn a = (fromAff <<< joinFiber) =<< runRelayer (insertEthereumAddress a)

insertEthereumAddress 
  :: forall m. MonadAff m 
  => MonadThrow RelayerError m
  =>  EthereumAddress 
  -> m InsertEthereumAddressResponse
insertEthereumAddress ea = do

  gqlRes <- queryGraphQlApi (buildInsertEthereumAddress ea)
  case gqlRes.data of
    Nothing -> throwError <<< GraphQlNoDataError $ show gqlRes.errors
    Just insertRes -> case gqlRes.errors of
      Nothing -> pure insertRes
      Just errors -> throwError <<< GraphQlShouldNotError $ show errors


