module Relayer.Utils (queryGraphQlApi) where

import Prelude

import Affjax as AX
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormat(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut as J
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff, error, throwError)
import Relayer.Types (GraphQLQuery(..), GraphQLQueryResponse(..))

queryGraphQlApi :: forall a. (J.DecodeJson a) => String -> GraphQLQuery a -> Aff (GraphQLQueryResponse a)
queryGraphQlApi url (GraphQLQuery gqlBody _) = do
  res <- AX.post ResponseFormat.json url (RequestBody.json (J.encodeJson gqlBody))
  case res.body of
    Left err -> throwError <<< error $ "POST /api response failed to decode: " <> AX.printResponseFormatError err
    Right json -> either (throwError <<< error) pure  (J.decodeJson json)
