module Relayer.Errors 
  ( RelayerError(..)
  , handleError
  ) where


import Prelude

import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Error, error)


-------------------------------------------------------------------------------
-- | Errors
-------------------------------------------------------------------------------
data RelayerError = InvalidJsonBody String
                  | HttpError StatusCode String
                  | HttpResponseFormatError String
                  | GraphQlNoDataError String

derive instance eqRelayerError :: Eq RelayerError
derive instance genericRelayerError :: Generic RelayerError _
instance showInsertRelayerError :: Show RelayerError where
  show = genericShow

handleError :: forall a m. (MonadError Error m) => RelayerError -> m a
handleError (HttpResponseFormatError err) = throwError <<< error $ "Failed to format http repsonse: " <> err
handleError (InvalidJsonBody err) = throwError <<< error $ "Failed to parse Json body: " <> err
handleError (HttpError (StatusCode sc) err) = throwError <<< error $ "Http Error, status code: " <> show sc <> ", Error: " <> err
handleError (GraphQlNoDataError err) = throwError <<< error $ "No data field returned from GraphQlQuery, errors found: " <> err
