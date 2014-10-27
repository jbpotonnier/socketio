module Pure (
    newRoomsTable
  , addParticipant
  , removeParticipant
  , participants
  ) where

import Types (Participant, RoomId, RoomTable)

import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set

newRoomsTable :: RoomTable
newRoomsTable = Map.empty

addParticipant :: RoomId -> Participant -> RoomTable -> RoomTable
addParticipant roomId participant =
  Map.insertWith Set.union roomId (Set.singleton participant)

removeParticipant :: RoomId -> Participant -> RoomTable -> RoomTable
removeParticipant roomId participant =
  Map.alter newParticipants roomId
  where
    newParticipants :: Maybe (Set Participant) -> Maybe (Set Participant)
    newParticipants maybeSet = do
      new <- fmap (\s -> s \\ Set.singleton participant) maybeSet
      if Set.null new then Nothing else Just new

participants :: RoomId -> RoomTable -> Set Participant
participants = Map.findWithDefault Set.empty
