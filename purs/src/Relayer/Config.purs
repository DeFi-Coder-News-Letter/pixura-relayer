module Relayer.Config 
  ( getGraphQlApiUrl
  , getPixuraApiKey
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
getGraphQlApiUrl = getEnVarWithDefault "GRAPHQL_API" "https://livenet-api.pixura.io/graphql"

-------------------------------------------------------------------------------
-- | getPixuraApiKey
-------------------------------------------------------------------------------
-- | Looks up the environment variable PIXURA_API_KEY 
getPixuraApiKey
  :: forall m. 
     MonadEffect m
  => m String
getPixuraApiKey = getEnVarWithDefault "PIXURA_API_KEY" "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImhlbGxvQHBpeHVyYS5pbyIsImlhdCI6MTU0OTMwOTYzNH0.8vk_lFmJlsxSpLo2wi32EAsdFkqcHSTKcWhXMdFBN-E"

-------------------------------------------------------------------------------
-- | getEnvarWithDefault
-------------------------------------------------------------------------------
-- | Looks up the environment variable and returns the supplied default value 
-- | if it was not found. 
getEnVarWithDefault
  :: forall m. 
     MonadEffect m
  => String 
  -> String 
  -> m String
getEnVarWithDefault varName def = maybe def identity <$> (liftEffect $ lookupEnv varName)
