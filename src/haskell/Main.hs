{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import           Network.EngineIO.Snap (snapAPI)
import           Network.SocketIO (initialize)
import qualified Network.SocketIO as Socketio
import           Snap (Snap, route, quickHttpServe, dir)
import Snap.Util.FileServe (serveDirectory)
import Control.Applicative ((<|>))
import qualified Data.Text as Text

data Message = Message {messageText :: Text.Text} deriving Generic
instance FromJSON Message

data Response = Response {responseText :: Text.Text} deriving Generic
instance ToJSON Response

mkRoutes :: IO (Snap ())
mkRoutes = initialize snapAPI $
  Socketio.on "message" $ \m -> Socketio.emit "response" $ Response (messageText m)

main :: IO ()
main = do
  handler <- mkRoutes
  quickHttpServe $ 
    route [("/socket.io", handler)] <|>
    dir "static" (serveDirectory "src/static/")
