module Relayer.Pixura (insertEthereumAddress, insertEtherAddressFn, insertSignedOrder, insertSignedOrderFn) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError)
import Data.Either (either)
import Data.Int (decimal)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Effect.Aff (joinFiber, throwError)
import Effect.Aff.Class (class MonadAff)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Network.Ethereum.Core.BigNumber (parseBigNumber)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (mkAddress)
import Relayer.Errors (RelayerError(..))
import Relayer.Queries (InsertEthereumAddressResponse, InsertSignedOrderResponse, buildInsertEthereumAddressQuery, buildInsertSignedOrderQuery)
import Relayer.Types (Promise, SignedOrder, UnsafeSignedOrder, fromAff, runRelayer)
import Relayer.Utils (queryGraphQlApi, stringToAddress)


-------------------------------------------------------------------------------
-- | insertEthereumAddress
-------------------------------------------------------------------------------
-- | Given a string address, insert into database.
insertEthereumAddress 
  :: forall m r. 
     MonadAff m 
  => MonadThrow RelayerError m
  => { address :: String | r} 
  -> m InsertEthereumAddressResponse
insertEthereumAddress { address } = do
  let eAddress  = stringToAddress address
  address' <- either throwError pure eAddress
  gqlRes <- queryGraphQlApi (buildInsertEthereumAddressQuery { address: address' })
  case gqlRes.data of
    Nothing -> throwError <<< GraphQlNoDataError $ show gqlRes.errors
    Just insertRes -> case gqlRes.errors of
      Nothing -> pure insertRes
      Just errors -> throwError <<< GraphQlShouldNotError $ show errors

-- | Uncurried javascript function for insertEthereumAddress.
insertEtherAddressFn :: forall r. EffectFn1 {address :: String | r} (Promise InsertEthereumAddressResponse)
insertEtherAddressFn = mkEffectFn1 fn
  where
    fn a = (fromAff <<< joinFiber) =<< runRelayer (insertEthereumAddress a)

-------------------------------------------------------------------------------
-- | insertSignedOrder
-------------------------------------------------------------------------------
-- | Given a signed order, ensure all addresses have been inserted into database.
insertSignedOrder 
  :: forall m . 
     MonadAff m 
  => MonadError RelayerError m
  => UnsafeSignedOrder
  -> m InsertSignedOrderResponse
insertSignedOrder uso = do
  let { senderAddress
      , makerAddress
      , takerAddress
      , exchangeAddress
      , feeRecipientAddress
      } = uso
      addresses = [ senderAddress
                  , makerAddress
                  , takerAddress
                  , exchangeAddress
                  , feeRecipientAddress
                  ]
  _ <- sequence (map (insertAddressIgnoreGraphQlError) addresses)
  so <- maybe (throwError <<< InvalidSignedOrder $ show uso) pure (toSignedOrder uso)
  gqlRes <- queryGraphQlApi (buildInsertSignedOrderQuery so)
  case gqlRes.data of
    Nothing -> throwError <<< GraphQlNoDataError $ show gqlRes.errors
    Just insertRes -> case gqlRes.errors of
      Nothing -> pure insertRes
      Just errors -> throwError <<< GraphQlShouldNotError $ show errors
  where
    insertAddressIgnoreGraphQlError a = do 
      _ <- catchError (void $ insertEthereumAddress { address: a }) catchGqlError 
      pure unit
    catchGqlError (GraphQlShouldNotError _) = pure unit
    catchGqlError err = throwError err
    toSignedOrder :: UnsafeSignedOrder -> Maybe SignedOrder
    toSignedOrder o = 
      { hash : _
      , senderAddress : _
      , makerAddress : _
      , takerAddress : _
      , makerAssetData : _
      , takerAssetData : _
      , exchangeAddress : _
      , feeRecipientAddress : _
      , expirationTimeSeconds : _
      , makerFee : _
      , takerFee : _
      , makerAssetAmount : _
      , takerAssetAmount : _
      , salt : _
      , signature : _
      } <$> mkHexString o.hash
        <*> (mkAddress =<< mkHexString o.senderAddress)
        <*> (mkAddress =<< mkHexString o.makerAddress)
        <*> (mkAddress =<< mkHexString o.takerAddress)
        <*> mkHexString o.makerAssetData
        <*> mkHexString o.takerAssetData
        <*> (mkAddress =<< mkHexString o.exchangeAddress)
        <*> (mkAddress =<< mkHexString o.feeRecipientAddress)
        <*> pure o.expirationTimeSeconds
        <*> parseBigNumber decimal o.makerFee
        <*> parseBigNumber decimal o.takerFee
        <*> parseBigNumber decimal o.makerAssetAmount
        <*> parseBigNumber decimal o.takerAssetAmount
        <*> parseBigNumber decimal o.salt
        <*> mkHexString o.signature
      

-- | Uncurried javascript function for insertSignedOrder.
insertSignedOrderFn :: EffectFn1 UnsafeSignedOrder (Promise InsertSignedOrderResponse)
insertSignedOrderFn = mkEffectFn1 fn
  where
    fn a = (fromAff <<< joinFiber) =<< runRelayer (insertSignedOrder a)
