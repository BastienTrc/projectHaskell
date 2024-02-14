module Parser where

import Data.Char (isSpace, isDigit)
import Data.Bool (Bool(True, False))
import Data.Aeson 
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Map (valid)

data SExpr = Atom SAtom | List [SExpr] deriving (Show)

data SAtom = LiteralInt Int | LiteralDouble Double | Symbol String | LiteralString String deriving (Show)

-- For testing purposes:

-- main :: IO ()
-- main = do
--   let input = "(1 ((my-symbol a_p 4.85) 3 "hello \" World\" ") true () false "hi \again   \\" null)"
--     -- let input = "And she said: \n\"Hello world\" :\\e \\ "
--     -- let input = "(1 2.2a 3ae 4.4 5.5ty4.4 6plo3a2 a)"
--     -- let input = "( 1   (2 (3   4) 5)    6 )"
--     let parsedExprs = parseInput input
--     case parsedExprs of
--       Left err -> putStrLn $ "Error: " ++ err
--       Right jsonValue -> print (jsonValue)

-- Parse the input representing an SExpression and return a JSON value
parseInput :: String -> Either String Value
parseInput str = case parseSExpressions str of
  Left err -> Left err
  Right (expr, _) -> Right (sExprToJSON expr)

-- Parse the input representing an SExpression
parseSExpressions :: String -> Either String (SExpr, String)
parseSExpressions input =
  case parseSExpr input of
    Left err -> Left err
    Right (exp, "") -> Right (exp, "")
    Right (_, rest) -> Left $ "Unparsed input remaining: " ++ rest


-- Parse a single S-expression
parseSExpr :: String -> Either String (SExpr, String)
parseSExpr [] = Left "Empty input"
parseSExpr (' ':xs) = parseSExpr xs
parseSExpr ('(':xs) = parseList (dropWhile isSpace xs) []
parseSExpr (')':_) = Left "Unexpected ')'"
parseSExpr ('"':xs) = parseString xs
parseSExpr xs = parseAtom xs

-- Parse a list of SExpr. The first argument is the expression to parse, the second contains the elements of the list already parsed
parseList :: String -> [SExpr] -> Either String (SExpr, String)
parseList [] _ = Left "Unfinished list "
parseList (')':xs) exps = Right (List exps, (dropWhile isSpace xs))
parseList xs exps =
  case parseSExpr xs of
    Left err -> Left err
    Right (exp, rest) -> parseList (dropWhile isSpace rest) (exps ++ [exp])

-- Parse an atom
parseAtom :: String -> Either String (SExpr, String)
parseAtom str = case parseAtomHelper "" str of
  ([], rest) -> Left ("Invalid atom, couldn't parse from : " ++ rest)
  (atom, rest) | all isDigit atom -> Right (Atom $ LiteralInt (read atom), rest)
  (atom, rest) | isValidDouble atom -> Right (Atom $ LiteralDouble (read atom), rest)
  -- (atom, _) | (last atom /= '.') || (head atom /= '.') || length (filter (== '.') atom) > 1 && all (\c -> isDigit c || c == '.') atom -> Left ("Invalid double, couldn't parse from: " ++ atom)
  (atom, rest) | validSymbol atom -> Right (Atom $ Symbol atom, rest)
  (atom, _) -> Left ("Invalid atom, couldn't parse from : " ++ atom)

-- Indicate if a string is a valid double (only one dot and only digits)
isValidDouble :: String -> Bool
isValidDouble atom = (last atom /= '.') && (head atom /= '.') && length (filter (== '.') atom) == 1 && all (\c -> isDigit c || c == '.') atom

-- Indicate if a string is a valid symbol (no parenthesis, no quotes, no backslashes)
validSymbol :: String -> Bool
validSymbol (x:_) | x == '(' || x == ')' || x == '"' || x == '\\' = False
validSymbol [] = True
validSymbol (_:xs) = validSymbol xs

-- The goal is to split the string into two parts: the atom and the rest of the string
parseAtomHelper :: String -> String -> (String, String)
parseAtomHelper acc (')':rest) = ( acc, ')':rest)
parseAtomHelper acc ('\\':'\\':rest) = (acc, '\\':'\\':rest)
parseAtomHelper acc ('\\':'"':rest) =  (acc, '\\':'"':rest)
parseAtomHelper acc (' ':rest) = ( acc, rest)
parseAtomHelper acc (c:rest) = parseAtomHelper (acc ++ [c]) rest
parseAtomHelper acc [] = ( acc, "")

-- Parse a string
parseString :: String -> Either String (SExpr, String)
parseString content = case parseStringHelper content "" of
  Left err -> Left err
  Right (str, rest) -> Right (Atom $ LiteralString str, (dropWhile isSpace rest))


-- Parse a string of characters. The function will add each character to an accumulator.
-- Once the string is finished, we return the accumulator and the unanalyzed characters
parseStringHelper :: String -> String -> Either String (String, String)
parseStringHelper [] acc = Left ("The string is not terminated: " ++ acc)
parseStringHelper ('\\':'"':xs) acc = parseStringHelper xs (acc ++ "\"")
parseStringHelper ('\\':'n':xs) acc = parseStringHelper xs (acc ++ "\n")
parseStringHelper ('\\':'\\':xs) acc = parseStringHelper xs (acc ++ "\\")
parseStringHelper ('\\':x:xs) acc = parseStringHelper xs (acc ++ [x])
parseStringHelper ('"':xs) acc = Right (acc, xs)
parseStringHelper (x:xs) acc = parseStringHelper xs (acc ++ [x])

sExprToJSON :: SExpr -> Value
sExprToJSON (Atom atom) = atomToJSON atom
sExprToJSON (List exprs) = Array $ V.fromList $ fmap sExprToJSON exprs


atomToJSON :: SAtom -> Value
atomToJSON (LiteralInt int) = toJSON int
atomToJSON (LiteralDouble double) = toJSON double
atomToJSON (LiteralString str) = toJSON str
atomToJSON (Symbol "true") = Bool True
atomToJSON (Symbol "false") = Bool False
atomToJSON (Symbol "null") = Null
atomToJSON (Symbol sym) = object ["symbol" .= sym]