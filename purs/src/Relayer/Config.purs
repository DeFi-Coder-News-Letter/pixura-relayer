module Relayer.Config 
  ( getGraphQlApiUrl
  ) where
import Prelude

import Affjax (URL)
import Data.Maybe (maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Process (lookupEnv)


-------------------------------------------------------------------------------
-- | getGraphQlApiUrl
-------------------------------------------------------------------------------
-- | Looks up the environment variable GRAPHQL_API 
getGraphQlApiUrl
  :: forall m. 
     MonadEffect m
  => m URL
getGraphQlApiUrl = getEnvarWithDefault "GRAPHQL_API" "https://livenet-api.pixura.io/graphql"

-------------------------------------------------------------------------------
-- | getEnvarWithDefault
-------------------------------------------------------------------------------
-- | Looks up the environment variable and returns the supplied default value 
-- | if it was not found. 
getEnvarWithDefault
  :: forall m. 
     MonadEffect m
  => String 
  -> String 
  -> m String
getEnvarWithDefault varName def = maybe def identity <$> (liftEffect $ lookupEnv varName)
