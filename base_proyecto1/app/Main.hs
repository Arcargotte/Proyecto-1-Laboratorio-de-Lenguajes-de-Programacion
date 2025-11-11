-- app/Main.hs
module Main where

import Engine.Types
import Engine.Parser
import Engine.Core
import Engine.Persistence
import System.IO

import qualified Data.Map as Map

-- -------------------------------------------------------------------------------------
-- Función main
-- -------------------------------------------------------------------------------------
-- Función principal del juego.
-- Se encarga de cargar los datos del mundo desde un archivo,
-- Manejar errores de carga, inicializar el estado del juego
-- Y finalmente iniciar el bucle principal del juego.
main :: IO ()
main = do
    -- Cargar el mundo
    -- Manejar el resultado de la carga
    -- Crear el estado inicial
    -- Iniciar el bucle del juego
    rawData <- loadWorldData "mundo.txt"
    case rawData of
        Left message -> putStrLn message
        Right (roomsMap, itemsMap) ->
            case Map.lookupMin roomsMap of
                Nothing -> putStrLn "Error: No fue creada ninguna sala en el juego."
                Just (key, room) -> do
                    let 
                        state = GameState{
                            roomID = key,
                            currentRoom = room,
                            inventory = Map.fromList [],
                            worldMap = roomsMap
                        }
                    putStrLn "Ingrese un comando"
                    gameLoop state

-- -------------------------------------------------------------------------------------
-- Función gameLoop
-- -------------------------------------------------------------------------------------
-- función que implementa el bucle principal del juego. 
-- Se encarga de leer los comandos introducidos por el jugador, 
-- procesarlos y actualizar el estado del juego de forma recursiva 
-- hasta que el jugador decida salir.
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