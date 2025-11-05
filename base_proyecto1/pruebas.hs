import System.IO (hFlush, stdout)

data Command = Command {
    instruction :: String,
    param :: String
}

parseCommand :: String -> Maybe Command
parseCommand s = case splitCommand (==' ') s of
    [] -> Nothing
    (x:xs) -> 
        let params = unwords xs
        in case (x, params) of
            ("ir", "norte ") -> Just Command { instruction = "ir", param = params}
            ("ir", "este ") -> Just Command { instruction = "ir", param = params}
            ("ir", "sur ") -> Just Command { instruction = "ir", param = params}
            ("ir", "oeste ") -> Just Command { instruction = "ir", param = params}
            ("ir", _) -> Nothing
            ("mirar", "") -> Just Command { instruction = "mirar", param = params}
            ("mirar", _) -> Nothing
            ("tomar", _) -> Just Command { instruction = "tomar", param = params}
            ("inventario", "") -> Just Command { instruction = "inventario", param = params}
            ("inventario", _) -> Nothing
            ("salir", "") -> Just Command { instruction = "salir", param = params}
            ("salir", _) -> Nothing
            _ -> Nothing

main :: IO ()
main = gameloop

splitCommand :: (Char -> Bool) -> String -> [String]
splitCommand p "" = [""]
splitCommand p s = case dropWhile p s of 
    "" -> []
    s' -> w : splitCommand p s'' 
        where (w, s'') = break p s'

invalidCommand :: IO()
invalidCommand = do
    putStrLn("Instrucción no reconocida")
    gameloop

gameloop :: IO()
gameloop = do
    putStrLn "Ingrese un comando"
    putStr "> "
    hFlush stdout
    command <- getLine
    let entryList = splitCommand (==' ') command
    let params = tail entryList
    let cmd = parseCommand command
    -- case cmd of
    --     Just (Command "ir" params) -> putStrLn ("Envió como primer comando a IR " ++ params)
    --     Just (Command "mirar" params) -> putStrLn ("Envió como primer comando a MIRAR " ++ params)
    --     Just (Command "tomar" params) -> putStrLn ("Envió como primer comando a TOMAR " ++ params)
    --     Just (Command "inventario" params) -> putStrLn ("Envió como primer comando a INVENTARIO " ++ params)
    --     Just (Command "salir" params) -> putStrLn ("Envió como primer comando a SALIR " ++ params)
    --     Nothing -> putStrLn ("Instrucción no reconocida")
    case cmd of
        Just (Command "ir" params) -> gameloop --Crear función que procese la lógica de IR que llame luego a gameloop
        Just (Command "mirar" params) -> gameloop --Crear función que procese la lógica de MIRAR que llame luego a gameloop
        Just (Command "tomar" params) -> gameloop --Crear función que procese la lógica de TOMAR que llame luego a gameloop
        Just (Command "inventario" params) -> gameloop --Crear función que procese la lógica de INVENTARIO que llame luego a gameloop
        Just (Command "salir" params) -> putStrLn ("Saliendo del juego... ")
        Nothing -> invalidCommand

