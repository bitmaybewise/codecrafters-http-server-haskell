{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Data.Functor (void)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
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

  -- Setting socket to ReuseAddr will tell the OS that it can be reused right away
  -- after application stops, without waiting for remaining packets. See:
  -- https://en.wikipedia.org/wiki/Transmission_Control_Protocol#Protocol_operation
  -- codecrafters.io halts the program between automated tests, so the reason why
  -- releasing the socket is necessary here, otherwise tests will fail to acquire the
  -- connection to the socket using the same port.
  setSocketOption serverSocket ReuseAddr 1

  bind serverSocket $ addrAddress $ head addrInfo
  listen serverSocket 5

  -- Accept connections and handle them forever
  forever $ do
    (clientSocket, clientAddr) <- accept serverSocket
    BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
    -- Handle the clientSocket as needed...
    forkIO $ do
      msg <- recv clientSocket 1024 -- limiting bytes read as will only parse the path for now
      BC.putStrLn msg
      let (verb, path, rest) = extractPath $ map BC.strip $ BC.lines msg
          headers = extractHeaders rest
      route clientSocket verb path headers
      close clientSocket

route :: Socket -> BC.ByteString -> BC.ByteString -> Map.Map BC.ByteString BC.ByteString -> IO ()
route clientSocket verb path headers
  | path == "/" = void $ send clientSocket (respondWith' 200)
  | "/echo/" `BC.isPrefixOf` path =
      let randomStr = BC.drop 6 path
       in void $ send clientSocket (respondWith 200 randomStr)
  | path == "/user-agent" && verb == "GET" =
      let userAgent = fromMaybe "UNKNOWN" $ Map.lookup "User-Agent:" headers
       in void $ send clientSocket (respondWith 200 userAgent)
  | otherwise = void $ send clientSocket (respondWith' 404)

extractPath :: [BC.ByteString] -> (BC.ByteString, BC.ByteString, [BC.ByteString])
extractPath lines'
  | null lines' = ("", "", [])
  | otherwise =
      let head' = BC.words . head $ lines'
       in case head' of
            verb : path : _ -> (verb, path, tail lines')
            _ -> ("", "", [])

extractHeaders :: [BC.ByteString] -> Map.Map BC.ByteString BC.ByteString
extractHeaders rest =
  let listPairs = map BC.words rest
      asTuples = map (\each -> if length each == 2 then (each !! 0, each !! 1) else ("", "")) listPairs
   in Map.fromList asTuples

status2msg :: Map.Map Int BC.ByteString
status2msg = Map.fromList [(200, "OK"), (404, "Not Found")]

respondWith :: Int -> BC.ByteString -> BC.ByteString
respondWith statusCode body =
  let strlen = BC.length body
      toBS = BC.pack . show
      statusMsg = fromMaybe "UNKNOWN" $ Map.lookup statusCode status2msg
   in BC.concat
        [ "HTTP/1.1 " <> toBS statusCode <> " " <> statusMsg <> "\r\n",
          "Content-Type: text/plain\r\n",
          "Content-Length: " <> toBS strlen <> "\r\n",
          "\r\n",
          body
        ]

respondWith' :: Int -> BC.ByteString
respondWith' statusCode = respondWith statusCode ""
