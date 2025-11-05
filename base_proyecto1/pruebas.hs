import System.IO (hFlush, stdout)

data Command = Command {
    instruction :: String,
    param :: Maybe String
}

parseCommand :: String -> Maybe Command
parseCommand s = case splitCommand (==' ') s of
    [] -> Nothing
    (x:xs) -> case x of
        "ir" -> Just Command { instruction = "ir", param = xs}
        "mirar" -> Just Command { instruction = "mirar", param = Nothing}
        "tomar" -> Just Command { instruction = "tomar", param = xs}
        "inventario" -> Just Command { instruction = "inventario"}
        "salir" -> Just Command { instruction = "salir", param = Nothing}
        _ -> Nothing

main :: IO ()
main = gameloop

splitCommand :: (Char -> Bool) -> String -> [String]
splitCommand p "" = [""]
splitCommand p s = case dropWhile p s of 
    "" -> []
    s' -> w : splitCommand p s'' 
        where (w, s'') = break p s'

gameloop :: IO()
gameloop = do
    putStrLn "Ingrese un comando"
    putStr "> "
    hFlush stdout
    command <- getLine
    -- let entryList = splitCommand (==' ') command
    -- let firstCommand = head entryList
    let cmd = parseCommand command
    case cmd of
        Just (Command "tomar") -> putStrLn ("Envió como primer comando a TOMAR")
        Just (Command "ir") -> putStrLn ("Envió como primer comando a IR")
        Just (Command "salir") -> putStrLn ("Envió como primer comando a SALIR")
        Nothing -> putStrLn ("Instrucción no reconocida") 