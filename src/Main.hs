{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE LambdaCase         #-}

module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.HashMap.Strict           as HM
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as T
import           GHC.Generics
import           Lucid
import           Lucid.Base                     ( makeAttribute )
import           Network.Wai.Handler.Warp       ( run )
import           Options.Applicative
import           Servant
import           Servant.Auth.Server
import           Servant.HTML.Lucid
import           Servant.QueryParamList
import qualified Steam.Login                   as SteamLogin

-- This sub-API serves:
--   /login --> go here to be redirected to steam.
--   /login-redirect --> Steam redirects you here.
--                       This pages shows your steam info and sets up your
--                       JWT cookie + XSRF Cookie.
type LoginAPI
  =    "login"
         :> Get '[JSON, HTML] LoginRedirect
  :<|> "login-redirect"
         :> QueryParamList
         :> Get '[JSON, HTML] (Headers '[ Header "Set-Cookie" SetCookie
                                  , Header "Set-Cookie" SetCookie]
                                 DisplaySteamInfo)

-- This sub-API only serves the / route that should display your steam ID if
-- your JWT-cookie was accepted, or errors out with HTTP status 401 instead
type ProtectedAPI = Get '[JSON , HTML] SteamId

data SteamId = SteamId
  { steamIdNr :: Text
  , steamNick :: Text
  }
  deriving (Generic, Show)

instance ToJSON SteamId
instance ToJWT SteamId
instance FromJSON SteamId
instance FromJWT SteamId

instance ToHtml SteamId where
  toHtmlRaw = toHtml
  toHtml s = div_ $ do
    h1_ (toHtml $ steamNick s)
    p_ (toHtml $ steamIdNr s)

data LoginRedirect

instance ToHtml LoginRedirect where
  toHtmlRaw = toHtml
  toHtml _ =
    h1_ $ toHtml ("You are being redirected to the Steam login page..." :: Text)

instance ToJSON LoginRedirect where
  toJSON _ = toJSON ()

newtype DisplaySteamInfo = DisplaySteamInfo SteamLogin.SteamInfo
  deriving (Generic)

instance ToJSON DisplaySteamInfo
instance ToHtml DisplaySteamInfo where
  toHtmlRaw = toHtml
  toHtml (DisplaySteamInfo i) = div_ $ do
    h1_ $ toHtml $ "Hello " <> SteamLogin.steamName i <> "!"
    p_ $ img_ [makeAttribute "src" (SteamLogin.steamAvatarFull i)]
    p_ $ a_ [makeAttribute "href" "/"] "Proceed to logged-in area"

protectedApi :: Servant.Auth.Server.AuthResult SteamId -> Server ProtectedAPI
protectedApi (Servant.Auth.Server.Authenticated sId) = return sId
protectedApi _ = throwAll err401

type API auths
  = (Servant.Auth.Server.Auth auths SteamId :> ProtectedAPI) :<|> LoginAPI

server :: String -> Text -> CookieSettings -> JWTSettings -> Server (API auths)
server baseUrl steamClientKey cs jwts =
  protectedApi
    :<|> (indexHandler baseUrl :<|> loginRedirectHandler cs jwts steamClientKey)

--------------------------------------------------------------------------------

indexHandler :: String -> Handler LoginRedirect
indexHandler baseUrl = throwError $ err301
  { errHeaders = [("Location", SteamLogin.steamLoginUrl $ BS.pack baseUrl)]
  }

loginRedirectHandler
  :: CookieSettings
  -> JWTSettings
  -> Text
  -> [(Text, Maybe Text)]
  -> Handler
       ( Headers
           '[ Header "Set-Cookie" SetCookie
            , Header "Set-Cookie" SetCookie
            ]
           DisplaySteamInfo
       )
loginRedirectHandler cookieSettings jwtSettings steamClientKey queryParams = do
  verifyClaims
  steamId <- maybe (thr err400 "Claims don't contain Steam ID") return
    $ SteamLogin.steamIdFromClaims claimParamMap
  steamInfo <-
    either (thr err400 . ("Can't decode user info response: " <>) . BSL.pack)
           return
      =<< liftIO (SteamLogin.steamInfoFromId steamClientKey steamId)
  let steamIdTokenInput = SteamId (SteamLogin.steamId steamInfo)
                                  (SteamLogin.steamName steamInfo)
  returnWithCookies <- maybe (throwError err500) return
    =<< liftIO (acceptLogin cookieSettings jwtSettings steamIdTokenInput)
  return $ returnWithCookies $ DisplaySteamInfo steamInfo
 where
  claimParamMap = HM.fromList queryParams
  thr statusCode body = throwError statusCode { errBody = body }
  verifyClaims = liftIO (SteamLogin.verifyClaim claimParamMap) >>= \case
    Left (SteamLogin.BadClaimParams err) ->
      thr err400 $ BSL.fromStrict $ T.encodeUtf8 err
    Left (SteamLogin.BadSteamServerAnswer status) ->
      thr err500
        $  "Got HTTP status "
        <> BSL.pack (show status)
        <> " from steam server"
    Left SteamLogin.BadClaimVerification ->
      thr err400 "Steam did not accept claim"
    Left SteamLogin.BadUserInfoResponse ->
      thr err400 "can't parse user info response"
    Right _ -> return ()

--------------------------------------------------------------------------------

data Config = Config
  { configSteamClientKey :: Text
  , configPort           :: Int
  }

argsParser :: Parser Config
argsParser =
  Config
    <$> strOption
          (  long "steam-client-key"
          <> metavar "hexstring"
          <> help
               "Steam client key as obtained from https://steamcommunity.com/dev/apikey"
          )
    <*> option
          auto
          (  long "port"
          <> help "Port to serve on"
          <> showDefault
          <> value 8000
          <> metavar "portnumber"
          )

main :: IO ()
main =
  let cookieSettings = defaultCookieSettings
        { cookieXsrfSetting = Just $ defaultXsrfCookieSettings
        -- the idea behind disabling XSRF for GET routes is that you still need
        -- to reach the web application with your browser with only the JWT
        -- cookie. Once some JS-powered application runs, it always needs to
        -- send XSRF tokens with every request.
                                { xsrfExcludeGet = True
                                }
        }
  in  do
        (Config steamClientKey port) <- execParser $ info
          (argsParser <**> helper)
          (fullDesc <> progDesc "Minimal Website with Steam Login" <> header
            "steam-login server"
          )
        myKey <- generateKey
        let jwtCfg = defaultJWTSettings myKey
            cfg    = cookieSettings :. jwtCfg :. EmptyContext
            api    = Proxy :: Proxy (API '[Cookie , JWT])
        run port $ serveWithContext
          api
          cfg
          (server ("http://localhost:" <> show port)
                  steamClientKey
                  cookieSettings
                  jwtCfg
          )
