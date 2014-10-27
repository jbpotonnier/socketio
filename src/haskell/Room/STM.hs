module STM (
  newRoomsTable
  , addParticipant
  , removeParticipant
  , participants
  ) where

import Types (Participant, RoomId, RoomTable)
import qualified Pure

import Data.Set (Set)
import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar)
import Control.Applicative ((<$>))

newRoomsTable :: STM (TVar RoomTable)
newRoomsTable = newTVar Pure.newRoomsTable

addParticipant :: TVar RoomTable -> RoomId -> Participant -> STM ()
addParticipant roomTable roomId participant =
  updateTVar roomTable $ Pure.addParticipant roomId participant

removeParticipant :: TVar RoomTable -> RoomId -> Participant -> STM ()
removeParticipant roomTable roomId participant =
  updateTVar roomTable $ Pure.removeParticipant roomId participant

participants :: TVar RoomTable -> RoomId -> STM (Set Participant)
participants roomTable roomId =
  Pure.participants roomId <$> readTVar roomTable
      
updateTVar :: TVar a -> (a -> a) -> STM ()
updateTVar tvar h =
  writeTVar tvar =<< h <$> readTVar tvar
