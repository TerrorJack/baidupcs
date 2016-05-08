module Web.Baidu.PCS where

import ClassyPrelude.Conduit
import Crypto.Hash
import Crypto.Hash.Conduit
import Crypto.Hash.Types
import Data.ByteString.Base16
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types

data GlobalConfig = GlobalConfig {
    accessToken :: !ByteString,
    chunkSize :: !Int,
    connsLimit :: !Int,
    bandwidthQuota :: !Int,
    retryLimit :: !Int
}

data GlobalSession = GlobalSession {
    config :: !GlobalConfig,
    manager :: !Manager,
    bandwidthMeter :: !(IORef Int),
    retryMeter :: !(IORef Int),
    progressLock :: !(MVar ())
}

data FileConfig = FileConfig {
    fileURL :: !Text,
    filePath :: !FilePath,
    fileSize :: !Int,
    fileMD5 :: !ByteString
}

data FileSession = FileSession {
    globalSession :: !GlobalSession,
    fileDefaultRequest :: !Request,
    fileConfig :: !FileConfig,
    fileHandle :: Handle
}

initGlobalSession :: GlobalConfig -> IO GlobalSession
initGlobalSession cfg = do
    mgr <- newManager tlsManagerSettings
    sl <- newMVar ()
    fsl <- newMVar ()
    bm <- newIORef $ bandwidthQuota cfg
    rm <- newIORef $ retryLimit cfg
    pl <- newMVar ()
    pure GlobalSession {
        config = cfg,
        manager = mgr,
        bandwidthMeter = bm,
        retryMeter = rm,
        progressLock = pl
    }

initFileSession :: GlobalSession -> FileConfig -> IO FileSession
initFileSession gs fc = do
    let tok = (accessToken . config) gs
    req <- parseUrl "https://d.pcs.baidu.com/rest/2.0/pcs/file"
    let req' = req {
        method = methodGet,
        requestHeaders = [(hAccept,"*/*"),(hUserAgent,"netdisk;5.4.1.0;PC;PC-Windows;10.0.10586;WindowsBaiduYunGuanJia")],
        queryString = renderQuery True [("method",Just "download"),("access_token",Just tok),("path",Just $ encodeUtf8 $ fileURL fc)],
        cookieJar = Nothing,
        checkStatus = \_ _ _ -> Nothing
    }
    pl <- newMVar ()
    pure FileSession {
        globalSession = gs,
        fileDefaultRequest = req',
        fileConfig = fc
    }

-- Here we use [L,R), different from [L,R] in the original API
downloadChunk :: FileSession -> Int -> Int -> IO LByteString
downloadChunk FileSession {..} l r = go
    where
        go = do
            let req = fileDefaultRequest { requestHeaders = (hRange, renderByteRanges [ByteRangeFromTo (fromIntegral l) (fromIntegral $ r-1)]):requestHeaders fileDefaultRequest }
            print req
            catch (do
                resp <- httpLbs req $ manager globalSession
                let Status {..} = responseStatus resp
                case statusCode of
                    200 -> pure $ responseBody resp
                    206 -> pure $ responseBody resp
                    _ -> retryChunk $ "Unidentified response: " ++ show resp) (\(e :: SomeException) -> retryChunk $ show e)
        retryChunk s = do
            flag <- atomicModifyIORef' (retryMeter globalSession) $ \m -> if m == 0 then (0,False) else (m-1,True)
            if flag then go else fail s

downloadFile :: FileSession -> IO ()
downloadFile = error "todo"

verifyMD5 :: FilePath -> ByteString -> IO Bool
verifyMD5 path md5 = do
    (Digest md5') :: Digest MD5 <- hashFile path
    pure $ md5' == md5
