module Engine.Parser (parseCommand) where

import Engine.Types
import Data.Char (toLower)

-- Parsea la entrada del usuario (String) a un (Maybe Command)
parseCommand :: String -> Maybe Command
parseCommand s = case words (map toLower s) of
    ["ir", dir] -> case dir of
      "norte" -> Just (Ir Norte)
      "sur"   -> Just (Ir Sur)
      "este"  -> Just (Ir Este)
      "oeste" -> Just (Ir Oeste)
      _       -> Nothing
    
    ("tomar" : itemName) -> Just (Tomar (unwords itemName))
    ["mirar"] -> Just Mirar
    ["salir"] -> Just Salir
    ["inventario"] -> Just Inventario
    _ -> Nothing