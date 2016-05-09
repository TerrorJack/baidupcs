module Web.Baidu.PCS where

import ClassyPrelude.Conduit
import Control.Concurrent.Async
import Control.Concurrent.SSem
import Crypto.Hash
import Crypto.Hash.Conduit
import Crypto.Hash.Types
import Network.HTTP.Client hiding (fileSize)
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import System.Directory
import System.IO (hSeek, hSetFileSize, IOMode(WriteMode), SeekMode(AbsoluteSeek), withBinaryFile)

data GlobalConfig = GlobalConfig {
    accessToken :: !ByteString,
    chunkSize :: !Int,
    connsLimit :: !Int
}

data GlobalSession = GlobalSession {
    config :: !GlobalConfig,
    manager :: !Manager,
    progressLock :: !(MVar ()),
    connSem :: !SSem
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
    fileHandle :: !Handle,
    progressMeter :: !(IORef Int)
}

initGlobalSession :: GlobalConfig -> IO GlobalSession
initGlobalSession cfg = do
    mgr <- newManager tlsManagerSettings
    pl <- newMVar ()
    cs <- new $ connsLimit cfg
    pure GlobalSession {
        config = cfg,
        manager = mgr,
        progressLock = pl,
        connSem = cs
    }

-- Here we use [L,R), different from [L,R] in the original API
downloadChunk :: FileSession -> Int -> Int -> IO LByteString
downloadChunk FileSession {..} l r = catch go $ \(_ :: SomeException) -> go
    where
        go = do
            let req = fileDefaultRequest { requestHeaders = (hRange, renderByteRanges [ByteRangeFromTo (fromIntegral l) (fromIntegral $ r-1)]):requestHeaders fileDefaultRequest }
            resp <- httpLbs req $ manager globalSession
            let Status {..} = responseStatus resp
            case statusCode of
                200 -> pure $ responseBody resp
                206 -> pure $ responseBody resp
                _ -> fail $ "Unidentified response: " ++ show resp

makeProgress :: FileSession -> Int -> Int -> Concurrently ()
makeProgress fs@FileSession {..} l r = Concurrently $ do
    chunk <- withSem (connSem globalSession) $ downloadChunk fs l r
    withMVar (progressLock globalSession) $ const $ do
        hSeek fileHandle AbsoluteSeek $ fromIntegral l
        hPut fileHandle chunk
        modifyIORef' progressMeter $ \m -> m+r-l
        m <- readIORef progressMeter
        putStrLn $ fileURL fileConfig ++ ": " ++ tshow m ++ " / " ++ tshow (fileSize fileConfig)

buildBigProgress :: FileSession -> Concurrently ()
buildBigProgress fs@FileSession {fileConfig = FileConfig {..}, globalSession = GlobalSession {config = GlobalConfig {..}, ..}} = f 0 where
    f l = if l + chunkSize >= fileSize then makeProgress fs l fileSize
        else makeProgress fs l (l + chunkSize) *> f (l + chunkSize)

downloadFile :: GlobalSession -> FileConfig -> IO ()
downloadFile gs fc = do
    let tok = (accessToken . config) gs
    req <- parseUrl "https://d.pcs.baidu.com/rest/2.0/pcs/file"
    let req' = req {
        method = methodGet,
        requestHeaders = [(hAccept,"*/*"),(hUserAgent,"netdisk;5.4.1.0;PC;PC-Windows;10.0.10586;WindowsBaiduYunGuanJia")],
        queryString = renderQuery True [("method",Just "download"),("access_token",Just tok),("path",Just $ encodeUtf8 $ fileURL fc)],
        cookieJar = Nothing,
        checkStatus = \_ _ _ -> Nothing
    }
    pm <- newIORef 0
    withBinaryFile (filePath fc) WriteMode $ \hdl -> do
        let fs = FileSession {
            globalSession = gs,
            fileDefaultRequest = req',
            fileConfig = fc,
            fileHandle = hdl,
            progressMeter = pm
        }
        hSetFileSize hdl $ fromIntegral $ fileSize fc
        runConcurrently $ buildBigProgress fs
    flag <- verifyMD5 (filePath fc) (fileMD5 fc)
    if flag then pure () else do
        removeFile $ filePath fc
        putStrLn $ pack (filePath fc) ++ ": MD5 mismatch"

verifyMD5 :: FilePath -> ByteString -> IO Bool
verifyMD5 path md5 = do
    (Digest md5') :: Digest MD5 <- hashFile path
    pure $ md5' == md5
