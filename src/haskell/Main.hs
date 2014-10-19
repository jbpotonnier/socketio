{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State.Strict (StateT)
import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import           Network.EngineIO.Snap (snapAPI)
import           Network.SocketIO (initialize, RoutingTable, Socket)
import qualified Network.SocketIO as Socketio
import           Snap (Snap, route, quickHttpServe, dir)
import Snap.Util.FileServe (serveDirectory)
import Control.Applicative ((<|>))
import qualified Data.Text as Text

data Message = Message {messageText :: Text.Text} deriving (Generic, Show)
instance FromJSON Message

data Response = Response {responseText :: Text.Text} deriving Generic
instance ToJSON Response

mkRoutes ::  StateT RoutingTable (ReaderT Socket Snap) ()
mkRoutes = Socketio.on "message" $ \m ->
  Socketio.emit "response" $ Response (messageText m)

main :: IO ()
main = do
  handler <- initialize snapAPI mkRoutes
  quickHttpServe $ 
    route [("/socket.io", handler)] <|>
    dir "static" (serveDirectory "src/static/")
