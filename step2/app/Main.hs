module Main where
import System.Environment (getEnvironment)

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Types as NH
import Network.HTTP.Client.TLS
import System.Exit ( exitFailure)

import qualified CVPartner as C
import qualified CVPartner.Model as CVPartner



getCvpartnerApiKey :: [([Char], [Char])] -> IO BS.ByteString
getCvpartnerApiKey env = case lookup "CVPARTNER_API_KEY" env of
        Just k -> return $ BS.pack k
        _ -> putStrLn "Missing CVPARTNER_API_KEY" >> exitFailure

data CvCtx = CvCtx {
    cvCtxKey :: BS.ByteString
  , cvCtxMgr :: NH.Manager
  , cvCtxConfig :: C.CVPartnerConfig
}

main :: IO ()
main = do
    env <- getEnvironment
    mgr <- makeManager

    config0 <- C.withStdoutLogging =<< C.newConfig
    cvpartnerApiKey <- getCvpartnerApiKey env

    let config =
            config0 {
                C.configHost = BCL.pack "https://scienta.cvpartner.com"
            }
            `C.addAuthMethod` C.AuthBasicBasicAuth (BS.pack "Bearer") cvpartnerApiKey

    putStrLn $ "Config: " ++ show config

    let ctx = CvCtx {
        cvCtxKey = cvpartnerApiKey
      , cvCtxMgr = mgr
      , cvCtxConfig = config
    }

    res <- userSearch ctx
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

userSearch :: CvCtx -> IO (C.MimeResult [CVPartner.User])
userSearch ctx = do
    putStrLn $ "key: " ++ BS.unpack key
    C.dispatchMime (cvCtxMgr ctx) (cvCtxConfig ctx) req'
    where
      key = cvCtxKey ctx
      req = C.userSearch (C.Accept C.MimeJSON)
      req' = C.setHeader req [(NH.hAuthorization, BS.append (BS.pack "Bearer ") key)]

makeManager :: IO NH.Manager
makeManager = do
  NH.newManager tlsManagerSettings {
    NH.managerModifyRequest = yeah
  }
    where
      yeah :: NH.Request -> IO NH.Request
      yeah req = do
        putStrLn "yeah"
        return req {
          NH.redirectCount = 0
        }
