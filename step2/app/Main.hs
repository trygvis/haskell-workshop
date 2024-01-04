module Main where
import System.Environment (getEnvironment)

import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Types as NH
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Exit (exitFailure)

import qualified CVPartner.API.ApiDefault as C
import qualified CVPartner.Client as C
import qualified CVPartner.Core as C
import qualified CVPartner.MimeTypes as C
import qualified CVPartner.Model as C
import CVPartner.Core ((-&-))

main :: IO ()
main = do
    env <- getEnvironment
    cvpartnerApiKey <- case lookup "CVPARTNER_API_KEY" env of
        Just k -> return $ BS.pack k
        _ -> putStrLn "Missing CVPARTNER_API_KEY" >> exitFailure

    mgr <- makeManager cvpartnerApiKey

    config' <- C.withStdoutLogging =<< C.newConfig
    let config = config' {
        C.configHost = BCL.pack "https://scienta.cvpartner.com"
      }

    putStrLn $ "Config: " ++ show config

--    let req = C.userSearch (C.Accept C.MimeJSON)
--    res <- C.dispatchMime mgr config req

    res <- findAllUsers mgr config

    withSuccess res showUsers
  where
    showUsers :: [C.User] -> IO ()
    showUsers [] = return ()
    showUsers (u: us) = do
      putStrLn $ "User " ++ T.unpack name
      showUsers us
      where
        name :: T.Text
        name = case C.userName u of
          Just n -> n
          _ -> T.pack "no name!"

withSuccess :: Either C.MimeError res -> (res -> IO ()) -> IO ()
withSuccess result handler = do
  case result of
      Left err -> do
        print err
        exitFailure
      Right value -> handler value

withSuccess2 :: C.MimeResult res -> (res -> IO ()) -> IO ()
withSuccess2 result handler = do
  case C.mimeResult result of
      Left err -> do
        print err
        exitFailure
      Right value -> handler value

makeManager :: BS.ByteString -> IO NH.Manager
makeManager bearer = do
  NH.newManager tlsManagerSettings {
    NH.managerModifyRequest = setAuthorization
  }
    where
      setAuthorization req = do
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

findAllUsers :: NH.Manager -> C.CVPartnerConfig -> IO (Either C.MimeError [C.User])
findAllUsers mgr config = do
    -- let req = C.userSearch (C.Accept C.MimeJSON)
    -- C.dispatchMime mgr config req
    forEachPage 0 10 [] fetchUserPage
  where
    fetchUserPage :: Int -> Int -> IO (Either C.MimeError [C.User])
    fetchUserPage offset limit = do
      putStrLn $ "userSearch, offset=" ++ show offset ++ ", limit=" ++ show limit
      let
        req = C.userSearch (C.Accept C.MimeJSON)
          -&- C.From offset
          -&- C.Size limit
      res <- C.dispatchMime mgr config req
      return $ C.mimeResult res

    forEachPage :: Int -> Int -> [a] -> (Int -> Int -> IO (Either C.MimeError [a])) -> IO (Either C.MimeError [a])
    forEachPage offset limit as getPage = do
      p <- getPage offset limit
      case p of
        Left err -> return $ Left err
        Right page -> case length page of
          0 -> return $ Right (as ++ page)
          _ -> forEachPage (offset + limit) limit (as ++ page) getPage
