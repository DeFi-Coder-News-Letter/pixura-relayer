module Relayer.Utils (queryGraphQlApi) where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadThrow, try)
import Control.Monad.Except (except)
import Control.Monad.Gen (resize)
import Data.Argonaut (class DecodeJson)
import Data.Argonaut as J
import Data.Either (Either(..), either)
import Effect.Aff (throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (logShow)
import Node.Stream (onFinish)
import Relayer.Errors (RelayerError(..))
import Relayer.Types (GraphQlQuery(..), GraphQlQueryResponse)

queryGraphQlApi 
  :: forall a m. 
     J.DecodeJson a 
  => MonadAff m 
  => MonadThrow RelayerError m
  => String 
  -> GraphQlQuery a 
  -> m (GraphQlQueryResponse a)
queryGraphQlApi url (GraphQlQuery gqlBody _) = post url gqlBody

  -- eres <- liftAff <<< try $ AX.post ResponseFormat.string url (RequestBody.json (J.encodeJson gqlBody))
  -- case eres of
  --   Left err -> throwError $ HttpConnectionError err
    
  -- either throwError pure $ decodeWithError res

post 
  :: forall a b m. 
     J.EncodeJson a 
  => J.DecodeJson b 
  => MonadAff m 
  => MonadThrow RelayerError m
  => String 
  -> a 
  -> m b
post url body = do
  eres <- liftAff (try $ AX.post ResponseFormat.string url (RequestBody.json (J.encodeJson body)))
  case eres of
    Left err -> throwError <<< HttpConnectionError <<< show $ err
    Right res -> either throwError pure $ decodeWithError (res)

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

statusOk :: StatusCode -> Boolean
statusOk (StatusCode n) = n >= 200 && n < 300
