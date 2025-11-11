module Engine.Types where

import qualified Data.Map as Map

-- Direcciones posibles
data Direction = Norte | Sur | Este | Oeste | Dirn't
    deriving (Show, Eq, Ord)

-- Un objeto en el juego
newtype Item = Item String deriving (Show)

-- Una sala en el juego
data Room = Room {
    description :: String,
    exits :: Map.Map Direction String,
    items :: Map.Map String Item
} deriving (Show)

-- El estado completo del juego
data GameState = GameState {
    roomID :: String,
    currentRoom :: Room,
    inventory :: Map.Map String Item,
    worldMap :: Map.Map String Room
}

-- Comandos que el jugador puede ejecutar
data Command = Ir Direction | Tomar String | Inventario | Mirar | Salir
    deriving (Show)

-- Placeholders para loadWorldData
type RoomContainer = Map.Map String Room
type ItemContainer = Map.Map String Item