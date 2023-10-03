module Tokenizer where

import qualified Data.Map as Map

data Token = InstructionToken String
           | RegisterToken String
           | LiteralToken String
           deriving (Show)

tokenize :: String -> [Token]
tokenize code = map classifyToken (words code)

classifyToken :: String -> Token
classifyToken word
    | isInstruction word = InstructionToken word
    | isRegister word = RegisterToken word
    | isLiteral word = LiteralToken word
    | otherwise = error "Invalid token"

isInstruction :: String -> Bool
isInstruction word = word `elem` instructions
  where
    instructions = ["mov", "add", "sub", "jmp", "cmp"]

isRegister :: String -> Bool
isRegister word = case word of
    ('R':rest) -> all (`elem` ['0'..'9']) rest
    _ -> False

isLiteral :: String -> Bool
isLiteral word = case word of
    ('#':rest) -> all (`elem` ['0'..'9']) rest
    _ -> False