module Engine.Persistence (loadWorldData) where

import Engine.Types
import System.IO
import Control.Monad
import qualified Data.Map as Map
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)

-- -------------------------------------------------------------------------------------
-- Función auxiliar trim
-- -------------------------------------------------------------------------------------
-- Elimina espacios en blanco al inicio y final de una cadena de texto.
-- Se utiliza constantemente durante el parseo para limpiar valores acumulados.

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- -------------------------------------------------------------------------------------
-- Función parseRoom
-- -------------------------------------------------------------------------------------
-- Función recursiva que se encarga de interpretar los tokens asociados a una sala.
-- Cada sala puede contener:
--   - Un nombre (después de "SALA:")
--   - Una descripción (después de "DESC:")
--   - Salidas (después de "SALIDA:" y "->")
--   - Objetos (después de "OBJETO:")
-- La función actualiza progresivamente el mapa de salas y retorna un Either String RoomContainer,
-- donde Left representa un error de parseo y Right el mapa de salas parseadas correctamente.

parseRoom :: [String] -> String -> String -> String -> Direction -> String -> Map.Map Direction String -> String -> Map.Map String Item -> RoomContainer -> ItemContainer -> Either String RoomContainer
parseRoom wordsList parseType accRoomName accRoomDesc dirMemory accExitRoom accExits accItemName accItems roomsMap itemsMap =
  case wordsList of
    [] -> 
      let 
        newRoom = Room {
          description = accRoomDesc,
          items = accItems,
          exits = accExits
        }
        newRoomsMap = Map.insert accRoomName newRoom roomsMap 
      in
        Right newRoomsMap
    _ -> 
      case head wordsList of
        "SALA:" -> parseRoom (tail(wordsList)) "room" "" "" Dirn't "" accExits "" accItems roomsMap itemsMap
        "DESC:" -> parseRoom (tail(wordsList)) "desc" accRoomName "" Dirn't "" accExits "" accItems roomsMap itemsMap
        "SALIDA:" -> parseRoom (tail(wordsList)) "dir" accRoomName accRoomDesc Dirn't "" accExits "" accItems roomsMap itemsMap
        "->" -> parseRoom (tail(wordsList)) "exitRoom" accRoomName accRoomDesc dirMemory "" accExits "" accItems roomsMap itemsMap
        "OBJETO:" -> parseRoom (tail(wordsList)) "item" accRoomName accRoomDesc Dirn't "" accExits "" accItems roomsMap itemsMap
        "---" -> 
          let 
            newRoom = Room {
              description = accRoomDesc,
              items = accItems,
              exits = accExits
            }
            newRoomsMap = Map.insert accRoomName newRoom roomsMap 
          in
            parseRoom (tail(wordsList)) "" "" "" Dirn't "" (Map.fromList []) "" (Map.fromList []) newRoomsMap itemsMap
        _ ->
          case parseType of
            "room" -> 
              let
                newWord = head wordsList
                newAcc = trim (accRoomName ++ " " ++ newWord) 
              in
                parseRoom (tail(wordsList)) "room" newAcc "" Dirn't "" accExits "" accItems roomsMap itemsMap
            "desc" -> 
              let
                newWord = head wordsList
                newAcc = trim (accRoomDesc ++ " " ++ newWord)
              in
                parseRoom (tail(wordsList)) "desc" accRoomName newAcc Dirn't "" accExits "" accItems roomsMap itemsMap
            "dir" ->
              case map toLower (head wordsList) of
                "norte" -> parseRoom (tail(wordsList)) "dir" accRoomName accRoomDesc Norte "" accExits "" accItems roomsMap itemsMap
                "sur" -> parseRoom (tail(wordsList)) "dir" accRoomName accRoomDesc Sur "" accExits "" accItems roomsMap itemsMap
                "este" -> parseRoom (tail(wordsList)) "dir" accRoomName accRoomDesc Este "" accExits "" accItems roomsMap itemsMap
                "oeste" -> parseRoom (tail(wordsList)) "dir" accRoomName accRoomDesc Oeste "" accExits "" accItems roomsMap itemsMap
                _ -> Left "Error: Dirección inválida"
            "exitRoom" -> 
              let
                newWord = head wordsList
                newAcc = trim (accExitRoom ++ " " ++ newWord)
              in
                case tail(wordsList) of
                  [] -> 
                    let 
                      newAccExits = Map.insert dirMemory newAcc accExits 
                    in
                      parseRoom (tail(wordsList)) "exitRoom" accRoomName accRoomDesc Dirn't "" newAccExits "" accItems roomsMap itemsMap
                  _ ->
                    case head (tail wordsList) of
                      val | val == "---" || val == "OBJETO:" || val == "SALIDA:" -> 
                        let 
                          newAccExits = Map.insert dirMemory newAcc accExits 
                        in
                          parseRoom (tail(wordsList)) "exitRoom" accRoomName accRoomDesc Dirn't "" newAccExits "" accItems roomsMap itemsMap
                      _ -> parseRoom (tail(wordsList)) "exitRoom" accRoomName accRoomDesc dirMemory newAcc accExits "" accItems roomsMap itemsMap
            "item" -> 
              let
                newWord = head wordsList
                newAcc = trim (accItemName ++ " " ++ newWord) 
              in
                case tail(wordsList) of
                  [] ->
                    case Map.lookup newAcc itemsMap of
                      Just item ->
                        let 
                          newAccItems = Map.insert newAcc item accItems 
                        in
                          parseRoom (tail(wordsList)) "item" accRoomName accRoomDesc Dirn't "" accExits "" newAccItems roomsMap itemsMap
                      Nothing -> Left ("Error: El objeto " ++ newAcc ++ " no existe.")
                  _ ->
                    case head (tail wordsList) of
                      val | val == "---" || val == "OBJETO:" || val == "SALIDA" ->
                        case Map.lookup newAcc itemsMap of
                          Just item ->
                            let 
                              newAccItems = Map.insert newAcc item accItems 
                            in
                              parseRoom (tail(wordsList)) "item" accRoomName accRoomDesc Dirn't "" accExits "" newAccItems roomsMap itemsMap
                          Nothing -> Left ("Error: El objeto " ++ newAcc ++ " no existe.")
                      _ -> parseRoom (tail(wordsList)) "item" accRoomName accRoomDesc Dirn't "" accExits newAcc accItems roomsMap itemsMap

-- -------------------------------------------------------------------------------------
-- Función parseWorld
-- -------------------------------------------------------------------------------------
-- Encargada de iniciar el proceso de parseo del mundo completo.
-- Interpreta la lista de tokens leída del archivo, separando primero los ítems
-- y posteriormente las salas, las cuales son procesadas mediante parseRoom.
-- Retorna un Either String (ItemContainer, RoomContainer), donde Left indica
-- un error y Right contiene los mapas de objetos y salas creados correctamente.

parseWorld :: [String] -> String -> String -> String -> ItemContainer -> Either String (ItemContainer, RoomContainer)
parseWorld wordsList parseType accItemName accItemDesc itemsMap =
  case wordsList of
    [] -> Right (itemsMap, (Map.fromList []))
    _ ->
      case head wordsList of
        "ITEM:" -> parseWorld (tail(wordsList)) "item" "" "" itemsMap
        "DESC:" -> parseWorld (tail(wordsList)) "desc" accItemName "" itemsMap
        "SALA:" -> 
          let 
            parsedRoomSection = parseRoom (tail(wordsList)) "room" "" "" Dirn't "" (Map.fromList []) "" (Map.fromList []) (Map.fromList []) itemsMap
          in
            case parsedRoomSection of
              Left error -> Left error
              Right roomsMap -> Right (itemsMap, roomsMap)
        _ ->
          case parseType of
            "item" ->
              let 
                newWord = head wordsList
                newAcc = trim (accItemName ++ " " ++ newWord)
              in
                parseWorld (tail(wordsList)) "item" newAcc accItemDesc itemsMap
            "desc" ->
              let 
                newWord = head wordsList
                newAcc = trim (accItemDesc ++ " " ++ newWord)
              in
                case tail wordsList of
                  [] -> 
                    let 
                      newItem = Item newAcc
                      newItemsMap = Map.insert accItemName newItem itemsMap
                    in
                      parseWorld (tail(wordsList)) "" "" "" newItemsMap
                  _ ->
                    case head(tail wordsList) of
                      "---" ->
                        let 
                          newItem = Item newAcc
                          newItemsMap = Map.insert accItemName newItem itemsMap
                        in
                          parseWorld (tail(tail(wordsList))) "" "" "" newItemsMap
                      _ -> parseWorld (tail(wordsList)) "desc" accItemName newAcc itemsMap
            _ -> Left "¡Error inesperado!"

-- -------------------------------------------------------------------------------------
-- Función loadWorldData
-- -------------------------------------------------------------------------------------
-- Carga el archivo de texto que describe el mundo del juego.
-- Lee el contenido, lo divide en palabras y llama a parseWorld.
-- Devuelve una acción IO que produce Either:
--   Left  -> Mensaje de error de parseo.
--   Right -> Tupla con (RoomContainer, ItemContainer).
loadWorldData :: FilePath -> IO (Either String (RoomContainer, ItemContainer))
loadWorldData filePath = do
  -- Pista: usa 'readFile' para leer el archivo.
  -- Luego, parsea el contenido.
  -- Si el parseo falla, devuelve (Left "Mensaje de Error")
  -- Si tiene éxito, devuelve (Right (mapaSalas, mapaItems))
    contents <- readFile filePath
    let wordsList = words contents
    let parsedItems = parseWorld wordsList "" "" "" (Map.fromList [])
    case parsedItems of
      Left error -> return (Left error)
      Right (itemsMap, roomsMap) -> return (Right (roomsMap, itemsMap))