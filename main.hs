-- #!/usr/bin/env stack
{-# LANGUAGE RecordWildCards #-}

import CMark
import Control.Monad
import Data.Text
import Network.HTTP
import Network.Socket
import Network.URI
import System.IO

textToString :: Text -> String
textToString = unpack

stringToText :: String -> Text
stringToText = pack

readLocalFile :: FilePath -> IO (String, Handle)
readLocalFile filePath = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  return (contents, handle)

newtype ServerConfigs = ServerConfigs
  { _port :: Port Int
  }

defaultServerConfigs :: ServerConfigs
defaultServerConfigs = ServerConfigs {_port = Just 8000}

type Port = Maybe

-- | A flip fromMaybe
(//) :: Maybe a -> a -> a
Just x // _ = x
Nothing // y = y
-- ^ Try something new

startServer :: ServerConfigs -> IO b
startServer ServerConfigs {..} = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints {addrFlags = [AI_NUMERICHOST], addrSocketType = Stream}
  let defaultPort = 8080
  let customPort = return $ show $ _port // defaultPort
  addr : _ <- getAddrInfo (Just hints) (Just "127.0.0.1") customPort
  let address = addrAddress addr
  putStrLn $ "Server is listening at http://" <> show address
  bind sock $ addrAddress addr
  listen sock 1
  forever $ do
    (csock, _) <- accept sock
    hs <- socketConnection "" (_port // defaultPort) csock
    req <- receiveHTTP hs
    case req of
      Left _ -> do
        (contents, handle) <- readLocalFile "./static/404.md"
        respondHTTP hs $
          Response (4, 0, 4) "Not found" [] (textToString (commonmarkToHtml [] (stringToText contents)))
        hClose handle
        Network.HTTP.close hs
      Right (Request rqURI rqMethod rqHeader reBody) -> do
        if (uriPath rqURI == "/") || (uriPath rqURI == "/index")
          then
            ( do
                (contents, handle) <- readLocalFile "./static/index.md"
                respondHTTP hs $
                  Response (2, 0, 0) "OK" [] (textToString (commonmarkToHtml [] (stringToText contents)))
                hClose handle
                Network.HTTP.close hs
            )
          else
            ( do
                (contents, handle) <- readLocalFile "./static/404.md"
                respondHTTP hs $
                  Response (4, 0, 4) "Not found" [] (textToString (commonmarkToHtml [] (stringToText contents)))
                hClose handle
                Network.HTTP.close hs
            )

main :: IO ()
main = startServer defaultServerConfigs
