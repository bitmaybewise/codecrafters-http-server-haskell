{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Data.Functor (void)
import Network.Socket
import Network.Socket.ByteString (recv, send)
import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  -- You can use print statements as follows for debugging, they'll be visible when running tests.
  BC.putStrLn "Logs from your program will appear here"

  -- Uncomment this block to pass first stage
  let host = "127.0.0.1"
      port = "4221"

  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

  -- Get address information for the given host and port
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)

  serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
  bind serverSocket $ addrAddress $ head addrInfo
  listen serverSocket 5

  -- Accept connections and handle them forever
  forever $ do
    (clientSocket, clientAddr) <- accept serverSocket
    BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
    -- Handle the clientSocket as needed...
    msg <- recv clientSocket 64 -- limiting bytes read as will only parse the path for now
    BC.putStrLn msg

    let path = hackyHTTPPath msg
    route clientSocket path

    close clientSocket

newtype Path = Path BC.ByteString

hackyHTTPPath :: BC.ByteString -> Path
hackyHTTPPath httpMsg =
  let msgLines = BC.lines httpMsg
      pathValues = (BC.split ' ' . head) msgLines
      path = pathValues !! 1
   in Path path

route :: Socket -> Path -> IO ()
route clientSocket (Path path)
  | path == "/" = void $ send clientSocket "HTTP/1.1 200 OK\r\n\r\n"
  | "/echo/" `BC.isPrefixOf` path =
      let randomStr = BC.drop 6 path
          strlen = BC.pack . show . BC.length $ randomStr
          strResponse =
            BC.concat
              [ "HTTP/1.1 200 OK\r\n",
                "Content-Type: text/plain\r\n",
                "Content-Length: " <> strlen <> "\r\n",
                "\r\n",
                randomStr <> "\r\n"
              ]
       in void $ send clientSocket strResponse
  | otherwise = void $ send clientSocket "HTTP/1.1 404 Not Found\r\n\r\n"