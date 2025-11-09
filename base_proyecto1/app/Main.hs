-- app/Main.hs
module Main where

import Engine.Types
import Engine.Parser
import Engine.Core
import Engine.Persistence
import System.IO

import qualified Data.Map as Map

main :: IO ()
main = do
    -- Cargar el mundo
    -- Manejar el resultado de la carga
    -- Crear el estado inicial
    -- Iniciar el bucle del juego
    rawData <- loadWorldData "mundo copy 2.txt"
    case rawData of
        Left message -> putStrLn message
        Right (roomsMap, itemsMap) -> do
            case Map.lookupMin roomsMap of
                Nothing -> putStrLn "Error: No fue creada ninguna sala en el juego."
                Just (key, room) -> do
                    let state = GameState{ 
                        currentRoom = room,
                        inventory = Map.fromList [],
                        worldMap = roomsMap
                    }
                    putStrLn "Ingrese un comando"
                    gameLoop state

-- El bucle principal del juego
gameLoop :: GameState -> IO ()
gameLoop state = do
    -- Parsear la entrada del usuario a un Command
    -- Si hay error, continuar con el mismo estado
    -- Procesar el comando
    -- Mostrar resultado
    -- Continuar el bucle con el nuevo estado
    putStr "> "
    hFlush stdout
    command <- getLine
    let parsedCommand = parseCommand command
    case parsedCommand of
        Nothing -> do
            putStrLn "Comando inválido."
            gameLoop state
        Just cmd -> case cmd of
            Salir -> putStrLn "Saliendo..."
            _ -> do 
                let (message, newState) = processCommand cmd state
                putStrLn message
                gameLoop newState