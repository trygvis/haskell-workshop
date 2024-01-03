{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module CVPartner.API
  ( -- * Client and Server
    Config(..)
  , CVPartnerBackend(..)
  , createCVPartnerClient
  , runCVPartnerServer
  , runCVPartnerMiddlewareServer
  , runCVPartnerClient
  , runCVPartnerClientWithManager
  , callCVPartner
  , CVPartnerClient
  , CVPartnerClientError(..)
  -- ** Servant
  , CVPartnerAPI
  -- ** Plain WAI Application
  , serverWaiApplicationCVPartner
  ) where

import           CVPartner.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serveWithContextT)
import           Servant.API                        hiding (addHeader)
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application, Context (EmptyContext))
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Servant type-level API, generated from the OpenAPI spec for CVPartner.
type CVPartnerAPI
    =    "api" :> "v1" :> "countries" :> Verb 'GET 200 '[JSON] [Country] -- 'findCountries' route
    :<|> "api" :> "v3" :> "cvs" :> Capture "user_id" Text :> Capture "cv_id" Text :> Capture "section_type" Text :> Capture "section_id" Text :> Verb 'GET 200 '[JSON] CvSection -- 'getCvSection' route
    :<|> "api" :> "v3" :> "cvs" :> Capture "user_id" Text :> Capture "cv_id" Text :> Verb 'GET 200 '[JSON] Cv -- 'getFullCv' route
    :<|> "api" :> "v1" :> "users" :> Capture "user_id" Text :> Verb 'GET 200 '[JSON] User -- 'getUserById' route
    :<|> "api" :> "v4" :> "search" :> ReqBody '[JSON] SearchByNameReq :> Verb 'POST 200 '[JSON] CvList -- 'searchByName' route
    :<|> "api" :> "v2" :> "users" :> "search" :> QueryParam "from" Int :> QueryParam "size" Int :> QueryParam "sort_by" Text :> QueryParam "deactivated" Bool :> QueryParam "role" Text :> QueryParam "name" Text :> QueryParam "office_ids" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [User] -- 'userSearch' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype CVPartnerClientError = CVPartnerClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for CVPartner.
-- The backend can be used both for the client and the server. The client generated from the CVPartner OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createCVPartnerClient@). Alternatively, provided
-- a backend, the API can be served using @runCVPartnerMiddlewareServer@.
data CVPartnerBackend m = CVPartnerBackend
  { findCountries :: m [Country]{- ^  -}
  , getCvSection :: Text -> Text -> Text -> Text -> m CvSection{- ^  -}
  , getFullCv :: Text -> Text -> m Cv{- ^  -}
  , getUserById :: Text -> m User{- ^  -}
  , searchByName :: SearchByNameReq -> m CvList{- ^  -}
  , userSearch :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe [Text] -> m [User]{- ^  -}
  }


newtype CVPartnerClient a = CVPartnerClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative CVPartnerClient where
  pure x = CVPartnerClient (\_ -> pure x)
  (CVPartnerClient f) <*> (CVPartnerClient x) =
    CVPartnerClient (\env -> f env <*> x env)

instance Monad CVPartnerClient where
  (CVPartnerClient a) >>= f =
    CVPartnerClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO CVPartnerClient where
  liftIO io = CVPartnerClient (\_ -> liftIO io)

createCVPartnerClient :: CVPartnerBackend CVPartnerClient
createCVPartnerClient = CVPartnerBackend{..}
  where
    ((coerce -> findCountries) :<|>
     (coerce -> getCvSection) :<|>
     (coerce -> getFullCv) :<|>
     (coerce -> getUserById) :<|>
     (coerce -> searchByName) :<|>
     (coerce -> userSearch) :<|>
     _) = client (Proxy :: Proxy CVPartnerAPI)

-- | Run requests in the CVPartnerClient monad.
runCVPartnerClient :: Config -> CVPartnerClient a -> ExceptT ClientError IO a
runCVPartnerClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runCVPartnerClientWithManager manager clientConfig cl

-- | Run requests in the CVPartnerClient monad using a custom manager.
runCVPartnerClientWithManager :: Manager -> Config -> CVPartnerClient a -> ExceptT ClientError IO a
runCVPartnerClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a CVPartnerClientError
callCVPartner
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> CVPartnerClient a -> m a
callCVPartner env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (CVPartnerClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the CVPartner server at the provided host and port.
runCVPartnerServer
  :: (MonadIO m, MonadThrow m)
  => Config -> CVPartnerBackend (ExceptT ServerError IO) -> m ()
runCVPartnerServer config backend = runCVPartnerMiddlewareServer config requestMiddlewareId backend

-- | Run the CVPartner server at the provided host and port.
runCVPartnerMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> CVPartnerBackend (ExceptT ServerError IO) -> m ()
runCVPartnerMiddlewareServer Config{..} middleware backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationCVPartner backend

-- | Plain "Network.Wai" Application for the CVPartner server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationCVPartner :: CVPartnerBackend (ExceptT ServerError IO) -> Application
serverWaiApplicationCVPartner backend = serveWithContextT (Proxy :: Proxy CVPartnerAPI) context id (serverFromBackend backend)
  where
    context = serverContext
    serverFromBackend CVPartnerBackend{..} =
      (coerce findCountries :<|>
       coerce getCvSection :<|>
       coerce getFullCv :<|>
       coerce getUserById :<|>
       coerce searchByName :<|>
       coerce userSearch :<|>
       serveDirectoryFileServer "static")


serverContext :: Context ('[])
serverContext = EmptyContext
