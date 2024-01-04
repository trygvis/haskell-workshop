module Main where
import System.Environment (getEnvironment)

import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Types as NH
import Network.HTTP.Client.TLS
import System.Exit ( exitFailure)

import qualified CVPartner as C




getCvpartnerApiKey :: [([Char], [Char])] -> IO BS.ByteString
getCvpartnerApiKey env = case lookup "CVPARTNER_API_KEY" env of
        Just k -> return $ BS.pack k
        _ -> putStrLn "Missing CVPARTNER_API_KEY" >> exitFailure

data CvCtx = CvCtx {
    cvCtxMgr :: NH.Manager
  , cvCtxConfig :: C.CVPartnerConfig
}

main :: IO ()
main = do
    env <- getEnvironment
    cvpartnerApiKey <- getCvpartnerApiKey env

    mgr <- makeManager cvpartnerApiKey

    config0 <- C.withStdoutLogging =<< C.newConfig

    let config =
            config0 {
                C.configHost = BCL.pack "https://scienta.cvpartner.com"
            }

    putStrLn $ "Config: " ++ show config

    let ctx = CvCtx {
        cvCtxMgr = mgr
      , cvCtxConfig = config
    }

    let req = C.userSearch (C.Accept C.MimeJSON)
    res <- C.dispatchMime (cvCtxMgr ctx) (cvCtxConfig ctx) req
    withSuccess res listUsers
    putStrLn "yo yo"
  where
    listUsers :: [C.User] -> IO ()
    listUsers [] = return ()
    listUsers (u: us) = do
      putStrLn $ "User " ++ T.unpack name
      listUsers us
      where
        name :: T.Text
        name = case C.userName u of
          Just n -> n
          _ -> T.pack "no name!"

withSuccess :: C.MimeResult res -> (res -> IO ()) -> IO ()
withSuccess result handler = do
  case C.mimeResult result of
      Left err -> print err
      Right value -> handler value

makeManager :: BS.ByteString -> IO NH.Manager
makeManager bearer = do
  NH.newManager tlsManagerSettings {
    NH.managerModifyRequest = yeah
  }
    where
      yeah :: NH.Request -> IO NH.Request
      yeah req = do
        -- print headers
        return req {
            NH.requestHeaders = headers
          , NH.redirectCount = 0
        }
        where
          headers' = NH.requestHeaders req
          headers = setHeader headers' "Authorization" (BS.append (BS.pack "Bearer ") bearer)

setHeader :: [NH.Header] -> String -> BS.ByteString -> [NH.Header]
setHeader old key value = new
  where
    key' = BS.pack key & CI.mk
    without = filter isKey old
    new = without ++ [(key', value)]
    isKey :: NH.Header -> Bool
    isKey (k, _) = k /= key'
