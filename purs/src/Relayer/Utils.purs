module Relayer.Utils (queryGraphQlApi) where

import Prelude

import Affjax (URL)
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadThrow, try)
import Data.Argonaut (class DecodeJson)
import Data.Argonaut as J
import Data.Either (Either(..), either)
import Effect.Aff (throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Relayer.Config (getGraphQlApiUrl)
import Relayer.Errors (RelayerError(..))
import Relayer.Types (GraphQlQuery(..), GraphQlQueryResponse)

-------------------------------------------------------------------------------
-- | queryGraphQlApi
-------------------------------------------------------------------------------
-- | General function for querying the graphql api.
queryGraphQlApi 
  :: forall a m. 
     J.DecodeJson a 
  => MonadAff m 
  => MonadThrow RelayerError m
  => GraphQlQuery a 
  -> m (GraphQlQueryResponse a)
queryGraphQlApi (GraphQlQuery gqlBody _) = flip post gqlBody =<< getGraphQlApiUrl

-------------------------------------------------------------------------------
-- | post
-------------------------------------------------------------------------------
-- | Wrapper function for affjax's post. Handles HttpConnectionError
post 
  :: forall a b m. 
     J.EncodeJson a 
  => J.DecodeJson b 
  => MonadAff m 
  => MonadThrow RelayerError m
  => URL 
  -> a 
  -> m b
post url body = do
  eres <- liftAff (try $ AX.post ResponseFormat.string url (RequestBody.json (J.encodeJson body)))
  case eres of
    Left err -> throwError <<< HttpConnectionError <<< show $ err
    Right res -> either throwError pure $ decodeWithError (res)

-------------------------------------------------------------------------------
-- | decodeWithError
-------------------------------------------------------------------------------
-- | Decodes the body for the affjax response. 
decodeWithError 
  :: forall a.
     DecodeJson a
  => AX.Response (Either AX.ResponseFormatError String)
  -> Either RelayerError a
decodeWithError res = case res.body of
    Left err -> Left <<< HttpResponseFormatError $ AX.printResponseFormatError err
    Right bodyStr | statusOk res.status ->
                      let jsonObj = J.fromString $ bodyStr
                          eobj = J.decodeJson jsonObj
                      in case eobj  of 
                           Left err -> Left (InvalidJsonBody ("Error:" <> err <> ": JsonBody" <> bodyStr))
                           Right obj -> Right obj
                  | otherwise -> Left (HttpError res.status (res.statusText <> " : " <> bodyStr))

-------------------------------------------------------------------------------
-- | statusOk
-------------------------------------------------------------------------------
-- | Returns true if the status code is in the valid ranges.
statusOk :: StatusCode -> Boolean
statusOk (StatusCode n) = n >= 200 && n < 300
