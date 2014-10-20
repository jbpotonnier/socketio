module Room (
  newRoomsTable
  , addParticipant
  , removeParticipant
  , participants
  ) where

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Control.Applicative ((<$>))

newtype Participant = Participant String deriving (Eq, Ord)

newtype RoomId = RoomId String deriving (Eq, Ord)

type RoomTable = TVar (Map RoomId (Set Participant))
                       
newRoomsTable :: STM RoomTable
newRoomsTable = newTVar $ Map.empty

addParticipant :: RoomTable -> RoomId -> Participant -> STM ()
addParticipant roomTable roomId participant=
  updateTVar roomTable $ Map.insertWith Set.union roomId (Set.singleton participant)

removeParticipant :: RoomTable -> RoomId -> Participant -> STM ()
removeParticipant roomTable roomId participant =
  updateTVar roomTable $ Map.alter newParticipants roomId
  where
    newParticipants :: Maybe (Set Participant) -> Maybe (Set Participant)
    newParticipants maybeSet = do
      new <- fmap (\s -> s \\ Set.singleton participant) maybeSet
      if Set.null new then Nothing else Just new

participants :: RoomTable -> RoomId -> STM (Set Participant)
participants roomTable roomId = do
  table <- readTVar roomTable
  return $ Map.findWithDefault Set.empty roomId table
      
updateTVar :: TVar a -> (a -> a) -> STM ()
updateTVar tvar h =
  writeTVar tvar =<< h <$> readTVar tvar
