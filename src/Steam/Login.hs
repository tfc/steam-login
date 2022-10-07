{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}

module Steam.Login where

import           Control.Monad                  ( (>=>)
                                                , forM
                                                )
import           Data.Aeson
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Vector                   as V
import           GHC.Generics
import qualified Network.HTTP.Client           as H
import qualified Network.HTTP.Client.TLS       as H
import           Network.HTTP.Types.Status      ( Status
                                                , statusIsSuccessful
                                                )
import qualified Network.URI.Encode            as URI

steamLoginUrl :: BS.ByteString -> BS.ByteString
steamLoginUrl baseUrl =
  let
    params =
      [ ("openid.ns"       , "http://specs.openid.net/auth/2.0")
      , ("openid.mode"     , "checkid_setup")
      , ("openid.return_to", baseUrl <> "/login-redirect")
      , ("openid.realm"    , baseUrl)
      , ( "openid.identity"
        , "http://specs.openid.net/auth/2.0/identifier_select"
        )
      , ( "openid.claimed_id"
        , "http://specs.openid.net/auth/2.0/identifier_select"
        )
      ]
    encodePairs (x, y) =
      URI.encodeByteString x <> "=" <> URI.encodeByteString y
  in
    "https://steamcommunity.com/openid/login?"
      <> BS.intercalate "&" (map encodePairs params)

steamIdFromClaims :: HM.HashMap Text (Maybe Text) -> Maybe Text
steamIdFromClaims m = do
  mIdUrl <- "openid.identity" `HM.lookup` m
  idUrl  <- mIdUrl
  T.stripPrefix "https://steamcommunity.com/openid/id/" idUrl

data SteamInfo = SteamInfo
  { steamId           :: Text
  , steamName         :: Text
  , steamProfileUrl   :: Text
  , steamAvatar       :: Text
  , steamAvatarMedium :: Text
  , steamAvatarFull   :: Text
  , steamRealname     :: Text
  , steamCountryCode  :: Text
  }
  deriving (Generic, Show)

instance ToJSON SteamInfo where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5 }

instance FromJSON SteamInfo where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 5
    }

newtype SteamResponseInfo = SteamResponseInfo
  { unSteamResponse :: SteamInfo
  }

instance FromJSON SteamResponseInfo where
  parseJSON =
    withObject "response" (.: "response")
      >=> withObject "players" (.: "players")
      >=> withArray
            "players array"
            ( pure
            . fromMaybe (error "Expected one player")
            . listToMaybe
            . V.toList
            )
      >=> withObject
            "player"
            (\o ->
              SteamInfo
                <$> o
                .:  "steamid"
                <*> o
                .:  "personaname"
                <*> o
                .:  "profileurl"
                <*> o
                .:  "avatar"
                <*> o
                .:  "avatarmedium"
                <*> o
                .:  "avatarfull"
                <*> o
                .:  "realname"
                <*> o
                .:  "loccountrycode"
            )
      >=> (pure . SteamResponseInfo)

steamInfoFromId :: Text -> Text -> IO (Either String SteamInfo)
steamInfoFromId steamClientKey steamIdStr =
  let in do
    httpman  <- H.newManager H.tlsManagerSettings
    emptyReq <- H.parseRequest
      "https://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/"
    let req = H.setQueryString
          [ ("key"     , Just $ T.encodeUtf8 steamClientKey)
          , ("steamids", Just $ T.encodeUtf8 steamIdStr)
          ]
          emptyReq
    response <- H.httpLbs req httpman
    return $ unSteamResponse <$> eitherDecode (H.responseBody response)

-- This function takes all the query parameters from steam's redirect to us.
-- It then checks if the content is sane and then forms the request body
-- that we are going to send back to steam to ask if this is a legitimate claim.
claimProofReq
  :: HM.HashMap Text (Maybe Text)
  -> Either Text [(BS.ByteString, BS.ByteString)]
claimProofReq claimKeySet =
  let noEmptyValueLookup key = case key `HM.lookup` claimKeySet of
        Just Nothing ->
          Left $ "invalid empty key " <> key <> " in server response"
        Nothing       -> Left $ "missing key " <> key <> " in server response"
        Just (Just s) -> Right s
      switchDotUnderscore = T.map $ \c -> if c == '.' then '_' else c
      staticPart =
        [ ("openid.ns"  , "http://specs.openid.net/auth/2.0")
        , ("openid.mode", "check_authentication")
        ]
      toBsRequest (k, v) = (T.encodeUtf8 k, T.encodeUtf8 v)
  in  do
        signedKeys <-
          map (("openid." <>) . switchDotUnderscore)
          .   T.splitOn ","
          <$> noEmptyValueLookup "openid.signed"
        accumulated <- forM ("openid.sig" : signedKeys) $ \key -> do
          val <- noEmptyValueLookup key
          return (key, val)
        return $ map toBsRequest $ staticPart <> accumulated


data ClaimVerificationError
  = BadClaimParams Text
  | BadSteamServerAnswer Status
  | BadClaimVerification
  | BadUserInfoResponse


verifyClaim
  :: HM.HashMap Text (Maybe Text) -> IO (Either ClaimVerificationError ())
verifyClaim claimKeySet = case claimProofReq claimKeySet of
  Left  err         -> return $ Left $ BadClaimParams err
  Right claimParams -> do
    response <- do
      mgr <- H.newManager H.tlsManagerSettings
      H.httpLbs
        (H.urlEncodedBody claimParams "https://steamcommunity.com/openid/login")
        mgr
    let httpStatus = H.responseStatus response
    if not (statusIsSuccessful httpStatus)
      then return $ Left $ BadSteamServerAnswer httpStatus
      else
        if not
             ( ("is_valid:true" `T.isInfixOf`)
             $ T.decodeUtf8
             $ BSL.toStrict
             $ H.responseBody response
             )
          then return $ Left BadClaimVerification
          else return $ Right ()
