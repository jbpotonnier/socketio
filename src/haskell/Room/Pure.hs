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

addParticipant :: RoomTable -> RoomId -> Participant -> RoomTable
addParticipant roomTable roomId participant =
  Map.insertWith Set.union roomId (Set.singleton participant) roomTable

removeParticipant :: RoomTable -> RoomId -> Participant -> RoomTable
removeParticipant roomTable roomId participant =
  Map.alter newParticipants roomId roomTable
  where
    newParticipants :: Maybe (Set Participant) -> Maybe (Set Participant)
    newParticipants maybeSet = do
      new <- fmap (\s -> s \\ Set.singleton participant) maybeSet
      if Set.null new then Nothing else Just new

participants :: RoomTable -> RoomId -> Set Participant
participants table roomId = Map.findWithDefault Set.empty roomId table
