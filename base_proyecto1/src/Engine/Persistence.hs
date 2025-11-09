module Engine.Persistence (loadWorldData) where

import Engine.Types
import System.IO
import Control.Monad
import qualified Data.Map as Map
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseRoom :: [String] -> String -> String -> String -> Direction -> String -> [(Direction, String)] -> String -> [(String, Item)] -> RoomContainer -> ItemContainer -> Either String RoomContainer
parseRoom wordsList parseType accRoomName accRoomDesc dirMemory accExitRoom accExits accItemName accItems roomsMap itemsMap = do
  case wordsList of
    [] -> do
      let newRoom = Room {
        description = accRoomDesc,
        items = Map.fromList accItems,
        exits = Map.fromList accExits
      }
      let newRoomsMap = Map.insert accRoomName newRoom roomsMap
      Right newRoomsMap
    _ -> do
      case head wordsList of
        "SALA:" -> parseRoom (tail(wordsList)) "room" "" "" Dirn't "" accExits "" accItems roomsMap itemsMap
        "DESC:" -> parseRoom (tail(wordsList)) "desc" accRoomName "" Dirn't "" accExits "" accItems roomsMap itemsMap
        "SALIDA:" -> parseRoom (tail(wordsList)) "dir" accRoomName accRoomDesc Dirn't "" accExits "" accItems roomsMap itemsMap
        "->" -> parseRoom (tail(wordsList)) "exitRoom" accRoomName accRoomDesc dirMemory "" accExits "" accItems roomsMap itemsMap
        "OBJETO:" -> parseRoom (tail(wordsList)) "item" accRoomName accRoomDesc Dirn't "" accExits "" accItems roomsMap itemsMap
        "---" -> do
          let newRoom = Room {
            description = accRoomDesc,
            items = Map.fromList accItems,
            exits = Map.fromList accExits
          }
          let newRoomsMap = Map.insert accRoomName newRoom roomsMap
          parseRoom (tail(wordsList)) "" "" "" Dirn't "" [] "" [] newRoomsMap itemsMap
        _ -> do
          case parseType of
            "room" -> do
              let newWord = head wordsList
              let newAcc = trim (accRoomName ++ " " ++ newWord)
              parseRoom (tail(wordsList)) "room" newAcc "" Dirn't "" accExits "" accItems roomsMap itemsMap
            "desc" -> do
              let newWord = head wordsList
              let newAcc = trim (accRoomDesc ++ " " ++ newWord)
              parseRoom (tail(wordsList)) "desc" accRoomName newAcc Dirn't "" accExits "" accItems roomsMap itemsMap
            "dir" -> do
              case map toLower (head wordsList) of
                "norte" -> parseRoom (tail(wordsList)) "dir" accRoomName accRoomDesc Norte "" accExits "" accItems roomsMap itemsMap
                "sur" -> parseRoom (tail(wordsList)) "dir" accRoomName accRoomDesc Sur "" accExits "" accItems roomsMap itemsMap
                "este" -> parseRoom (tail(wordsList)) "dir" accRoomName accRoomDesc Este "" accExits "" accItems roomsMap itemsMap
                "oeste" -> parseRoom (tail(wordsList)) "dir" accRoomName accRoomDesc Oeste "" accExits "" accItems roomsMap itemsMap
                _ -> Left "Error: Dirección inválida"
            "exitRoom" -> do
              let newWord = head wordsList
              let newAcc = trim (accExitRoom ++ " " ++ newWord)
              case tail(wordsList) of
                [] -> do
                  let newAccExits = (dirMemory, newAcc) : accExits
                  parseRoom (tail(wordsList)) "exitRoom" accRoomName accRoomDesc Dirn't "" newAccExits "" accItems roomsMap itemsMap
                _ -> do
                  case head (tail wordsList) of
                    val | val == "---" || val == "OBJETO:" || val == "SALIDA:" -> do
                      let newAccExits = (dirMemory, newAcc) : accExits
                      parseRoom (tail(wordsList)) "exitRoom" accRoomName accRoomDesc Dirn't "" newAccExits "" accItems roomsMap itemsMap
                    _ -> parseRoom (tail(wordsList)) "exitRoom" accRoomName accRoomDesc dirMemory newAcc accExits "" accItems roomsMap itemsMap
            "item" -> do
              let newWord = head wordsList
              let newAcc = trim (accItemName ++ " " ++ newWord)
              case tail(wordsList) of
                [] -> do
                  case Map.lookup newAcc itemsMap of
                    Just item -> do
                      let newAccItems = (newAcc, item) : accItems
                      parseRoom (tail(wordsList)) "item" accRoomName accRoomDesc Dirn't "" accExits "" newAccItems roomsMap itemsMap
                    Nothing -> Left ("Error: El objeto " ++ newAcc ++ " no existe.")
                _ -> do
                  case head (tail wordsList) of
                    val | val == "---" || val == "OBJETO:" || val == "SALIDA" -> do
                      case Map.lookup newAcc itemsMap of
                        Just item -> do
                          let newAccItems = (newAcc, item) : accItems
                          parseRoom (tail(wordsList)) "item" accRoomName accRoomDesc Dirn't "" accExits "" newAccItems roomsMap itemsMap
                        Nothing -> Left ("Error: El objeto " ++ newAcc ++ " no existe.")
                    _ -> parseRoom (tail(wordsList)) "item" accRoomName accRoomDesc Dirn't "" accExits newAcc accItems roomsMap itemsMap



parseWorld :: [String] -> String -> String -> String -> ItemContainer -> Either String (ItemContainer, RoomContainer)
parseWorld wordsList parseType accItemName accItemDesc itemsMap = do
  case wordsList of
    [] -> Right (itemsMap, (Map.fromList []))
    _ -> do
      case head wordsList of
        "ITEM:" -> parseWorld (tail(wordsList)) "item" "" "" itemsMap
        "DESC:" -> parseWorld (tail(wordsList)) "desc" accItemName "" itemsMap
        "SALA:" -> do
          let parsedRoomSection = parseRoom (tail(wordsList)) "room" "" "" Dirn't "" [] "" [] (Map.fromList []) itemsMap
          case parsedRoomSection of
            Left error -> Left error
            Right roomsMap -> Right (itemsMap, roomsMap)
        _ -> do
          case parseType of
            "item" -> do
              let newWord = head wordsList
              let newAcc = trim (accItemName ++ " " ++ newWord)
              parseWorld (tail(wordsList)) "item" newAcc accItemDesc itemsMap
            "desc" -> do
              let newWord = head wordsList
              let newAcc = trim (accItemDesc ++ " " ++ newWord)
              case tail wordsList of
                [] -> do 
                  let newItem = Item newAcc
                  let newItemsMap = Map.insert accItemName newItem itemsMap
                  parseWorld (tail(wordsList)) "" "" "" newItemsMap
                _ -> do
                  case head(tail wordsList) of
                    "---" -> do
                      let newItem = Item newAcc
                      let newItemsMap = Map.insert accItemName newItem itemsMap
                      parseWorld (tail(tail(wordsList))) "" "" "" newItemsMap
                    _ -> parseWorld (tail(wordsList)) "desc" accItemName newAcc itemsMap
            _ -> Left "¡Error inesperado!"

-- Carga el archivo del mundo.
-- Devuelve (IO (Either Error (MapaDeSalas, MapaDeItems)))
loadWorldData :: FilePath -> IO (Either String (RoomContainer, ItemContainer))
loadWorldData filePath = do
  -- Pista: usa 'readFile' para leer el archivo.
  -- Luego, parsea el contenido.
  -- Si el parseo falla, devuelve (Left "Mensaje de Error")
  -- Si tiene éxito, devuelve (Right (mapaSalas, mapaItems))
    handleFile <- openFile filePath ReadMode
    contents <- hGetContents handleFile
    let wordsList = words contents
    let parsedItems = parseWorld wordsList "" "" "" (Map.fromList [])
    case parsedItems of
      Left error -> do
        return (Left error)
      Right (itemsMap, roomsMap) -> do 
        return (Right (roomsMap, itemsMap))