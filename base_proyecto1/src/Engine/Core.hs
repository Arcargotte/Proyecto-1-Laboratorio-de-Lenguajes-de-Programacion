module Engine.Core (processCommand) where

import Engine.Types

import qualified Data.Map as Map
import Data.Char (toLower, toUpper)

-- -------------------------------------------------------------------------------------
-- Función processCommand
-- -------------------------------------------------------------------------------------
-- Función pura responsable de procesar los comandos introducidos por el jugador.
-- A partir del comando recibido y el estado actual del juego, retorna una tupla 
-- compuesta por un mensaje para el usuario y el nuevo estado del juego.
processCommand :: Command -> GameState -> (String, GameState)
processCommand command state =
  -- Pista: usar pattern matching para manejar cada comando (Ir, Tomar, Mirar, etc.)
  -- Devuelve siempre una tupla de (Mensaje para el usuario, NuevoEstado)
  case command of
    Ir direction -> case Map.lookup direction (exits (currentRoom state)) of
      Just roomKey -> let newRoom = Map.lookup roomKey (worldMap state) in
        case newRoom of
          Just room -> ("Te has movido a " ++ roomKey, GameState{ 
            roomID = roomKey,
            currentRoom = room,
            inventory = inventory state,
            worldMap = worldMap state
          })
      Nothing -> ("Esa puerta no existe.", state)
    Mirar -> 
      case Map.toList (items (currentRoom state)) of
        [] -> (description (currentRoom state) ++ "\nObjetos:\n(No hay nada aquí. Quieres ver cosas donde no las hay).", state) 
        _ -> let roomItems = foldl (\x (y, Item z) -> x ++ "\n- " ++ toUpper (head y) : tail y ++ ": " ++ z) "Objetos:" (Map.toList (items (currentRoom state))) in
          (description (currentRoom state) ++ "\n" ++ roomItems, state)
    Inventario -> 
      case Map.toList (inventory state) of
        [] -> ("Estás alucinando, no tienes ni un mango.", state)
        _ -> let inventoryItems = foldl (\x (y, Item z) -> x ++ "\n- " ++ toUpper (head y) : tail y ++ ": " ++ z) "Mi inventario:" (Map.toList (inventory state)) in
          (inventoryItems, state)
    Tomar itemKey -> 
      case Map.lookup itemKey (Map.mapKeys (map toLower) (items (currentRoom state))) of 
      Just item -> let 
        newRoom = Room {
          description = description (currentRoom state),
          exits = exits (currentRoom state),
          items = Map.delete itemKey (items (currentRoom state))
        }
        newWorldMap = Map.delete (roomID state) (worldMap state) in
        ("Has tomado el objeto " ++ toUpper (head itemKey) : tail itemKey ++ " de la sala.", GameState{
          roomID = roomID state,
          currentRoom = newRoom,
          inventory = Map.insert itemKey item (inventory state),
          worldMap = Map.insert (roomID state) newRoom newWorldMap
        })
      Nothing -> ("Empiezas a alucinar, no existe este ítem en la sala.", state)