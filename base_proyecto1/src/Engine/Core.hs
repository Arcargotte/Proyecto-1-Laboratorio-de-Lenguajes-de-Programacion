module Engine.Core (processCommand) where

import Engine.Types

import qualified Data.Map as Map
import Data.Char (toLower, toUpper)

-- La función PURA que actualiza el estado del juego
processCommand :: Command -> GameState -> (String, GameState)
processCommand command state =
  -- Pista: usar pattern matching para manejar cada comando (Ir, Tomar, Mirar, etc.)
  -- Devuelve siempre una tupla de (Mensaje para el usuario, NuevoEstado)
  case command of
    Ir direction -> case Map.lookup direction (exits (currentRoom state)) of
      Just roomKey -> do
        let newRoom = Map.lookup roomKey (worldMap state)
        case newRoom of
          Just room -> ("Te has movido a " ++ roomKey, GameState{ 
            roomID = roomKey,
            currentRoom = room,
            inventory = inventory state,
            worldMap = worldMap state
          })
      Nothing -> ("Esa puerta no existe.", state)
    Mirar -> do
      case Map.toList (items (currentRoom state)) of
        [] -> (description (currentRoom state) ++ "\nObjetos:\n(No hay nada aquí. Quieres ver cosas donde no las hay).", state) 
        _ -> do
          let roomItems = foldl (\x (y, Item z) -> x ++ "\n- " ++ toUpper (head y) : tail y ++ ": " ++ z) "Objetos:" (Map.toList (items (currentRoom state)))
          (description (currentRoom state) ++ "\n" ++ roomItems, state)
    Inventario -> do
      case Map.toList (inventory state) of
        [] -> ("Estás alucinando, no tienes ni un mango.", state)
        _ -> do
          let inventoryItems = foldl (\x (y, Item z) -> x ++ "\n- " ++ toUpper (head y) : tail y ++ ": " ++ z) "Mi inventario:" (Map.toList (inventory state))
          (inventoryItems, state)
    Tomar itemKey -> case Map.lookup itemKey (Map.mapKeys (map toLower) (items (currentRoom state))) of 
      Just item -> do
        let newRoom = Room {
          description = description (currentRoom state),
          exits = exits (currentRoom state),
          items = Map.delete itemKey (items (currentRoom state))
        }
        let newWorldMap = Map.delete (roomID state) (worldMap state)
        ("Has tomado el objeto " ++ toUpper (head itemKey) : tail itemKey ++ " de la sala.", GameState{
          roomID = roomID state,
          currentRoom = newRoom,
          inventory = Map.insert itemKey item (inventory state),
          worldMap = Map.insert (roomID state) newRoom newWorldMap
        })
      Nothing -> ("Empiezas a alucinar, no existe este ítem en la sala.", state)
      