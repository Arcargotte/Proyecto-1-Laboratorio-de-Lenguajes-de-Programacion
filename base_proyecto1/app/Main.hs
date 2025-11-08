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
    let state = GameState{ 
        currentRoom = Room{ 
            description = "Sala llena de lindos recuerdos con holor a desayuno recién hecho y moscas en la basura", 
            items = Map.fromList [("Olla quemada", Item "Olla quemada porque dejé el agua hirviendo y no la apagué"), ("Cuchara sucia", Item "Cuchara sucia de café seco")], 
            exits =  Map.fromList [(Norte, "Sala")]
        },
        inventory = Map.fromList [("Papel", Item "Factura de la panadería que se te olvidó botar (nadie guarda facturas)")],
        worldMap = Map.fromList [
            ("Cocina",  
            Room{ 
                description = "Sala llena de lindos recuerdos con holor a desayuno recién hecho y moscas en la basura", 
                items = Map.fromList [("Olla quemada", Item "Olla quemada porque dejé el agua hirviendo y no la apagué"), ("Cuchara sucia", Item "Cuchara sucia de café seco")], 
                exits =  Map.fromList [(Norte, "Sala")]
            }),
            ("Sala",  
            Room{ 
                description = "Lugar espacioso con un gran comedor y un televisor quemado", 
                items = Map.fromList [("Televisor", Item "Un televisor (está quemado)")], 
                exits =  Map.fromList [(Sur, "Cocina")]
            })
            ]
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