{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
    msg <- recv clientSocket 1024 -- limiting bytes read as will only parse the path for now
    BC.putStrLn msg

    let (verb, path, rest) = extractPath $ map BC.strip $ BC.lines msg
        headers = extractHeaders rest

    route clientSocket verb path headers

    close clientSocket

route :: Socket -> BC.ByteString -> BC.ByteString -> Map.Map BC.ByteString BC.ByteString -> IO ()
route clientSocket verb path headers
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
                randomStr
              ]
       in void $ send clientSocket strResponse
  | path == "/user-agent" && verb == "GET" =
      let userAgent = fromMaybe "UNKNOWN" $ Map.lookup "User-Agent:" headers
          strlen = BC.length userAgent
          strResponse =
            BC.concat
              [ "HTTP/1.1 200 OK\r\n",
                "Content-Type: text/plain\r\n",
                "Content-Length: " <> (BC.pack . show) strlen <> "\r\n",
                "\r\n",
                userAgent
              ]
       in void $ send clientSocket strResponse
  | otherwise = void $ send clientSocket "HTTP/1.1 404 Not Found\r\n\r\n"

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
