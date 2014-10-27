module Types where

import Data.Map (Map)
import Data.Set (Set)

newtype Participant = Participant String deriving (Eq, Ord)

newtype RoomId = RoomId String deriving (Eq, Ord)

type RoomTable = Map RoomId (Set Participant)
