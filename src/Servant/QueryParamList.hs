{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.QueryParamList where

import           Data.Text                      ( Text )
import qualified Network.HTTP.Types            as HT
import           Network.Wai
import           Servant
import           Servant.Server.Internal.Delayed
                                                ( passToServer )

-- This is a helper type that simply puts all the URL query variables
-- into a list of key and maybe value tuples.
data QueryParamList

instance HasServer api ctx => HasServer (QueryParamList :> api) ctx where
  type ServerT (QueryParamList :> api) m = [(Text, Maybe Text)] -> ServerT api m
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s
  route Proxy context subserver =
    let querytext = HT.queryToQueryText . queryString
    in  route (Proxy :: Proxy api) context (passToServer subserver querytext)
