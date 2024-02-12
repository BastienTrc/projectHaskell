module Parser where

import Data.Char (isSpace, isDigit)
import Data.Bool (Bool(True, False))
import Data.Aeson 
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Map (valid)

data SExpr = Atom SAtom | List [SExpr] deriving (Show)

data SAtom = LiteralInt Int | LiteralDouble Double | Symbol String | LiteralString String deriving (Show)

-- For testing:

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

-- Parse l'input représentant une SExpression et le convertit en JSON
-- Parse l'input représentant une SExpression et le convertit en JSON
parseInput :: String -> Either String Value
parseInput str = case parseSExpressions str of
  Left err -> Left err
  Right (expr, _) -> Right (sExprToJSON expr)

-- Parse l'input représentant une SExpression
parseSExpressions :: String -> Either String (SExpr, String)
parseSExpressions input =
  case parseSExpr input of
    Left err -> Left err
    Right (exp, "") -> Right (exp, "")
    Right (_, rest) -> Left $ "Unparsed input remaining: " ++ rest


-- Parse une seule S-expression
parseSExpr :: String -> Either String (SExpr, String)
parseSExpr [] = Left "Empty input"
parseSExpr (' ':xs) = parseSExpr xs
parseSExpr ('(':xs) = parseList xs []
parseSExpr (')':_) = Left "Unexpected ')'"
parseSExpr ('"':xs) = parseString xs
parseSExpr xs = parseAtom xs


-- Parse une liste de SExpr. Le premier argument est l'expression à parser, le deuxième contient les éléments de la liste déjà parsé
parseList :: String -> [SExpr] -> Either String (SExpr, String)
parseList [] _ = Left "Unfinished list "
parseList (')':xs) exps = Right (List exps, (dropWhile isSpace xs))
parseList xs exps =
  case parseSExpr xs of
    Left err -> Left err
    Right (exp, rest) -> parseList rest (exps ++ [exp])

-- Parse un atome
parseAtom :: String -> Either String (SExpr, String)
parseAtom str = case parseAtomHelper "" str of
  ([], rest) -> Left ("Invalid atom, couldn't parse from : " ++ rest)
  (atom, rest) | all isDigit atom -> Right (Atom $ LiteralInt (read atom), rest)
  (atom, rest) | length (filter (== '.') atom) == 1 && all (\c -> isDigit c || c == '.') atom -> Right (Atom $ LiteralDouble (read atom), rest)
  (atom, _) | length (filter (== '.') atom) > 1 && all (\c -> isDigit c || c == '.') atom -> Left ("Invalid double, couldn't parse from: " ++ atom)
  (atom, rest) | validSymbol atom -> Right (Atom $ Symbol atom, rest)
  (atom, _) -> Left ("Invalid atom, couldn't parse from : " ++ atom)

isValidDouble :: String -> Bool
isValidDouble str = length (filter (== '.') str) <= 1 && all (\c -> isDigit c || c == '.') str


validSymbol :: String -> Bool
validSymbol (x:_) | x == '(' || x == ')' || x == '"' || x == '\\' = False
validSymbol [] = True
validSymbol (_:xs) = validSymbol xs

-- Le but ici est 
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


-- Parse une chaîne de caractère. La fonction va ajouter chaque caractère dans un accumulateur.
-- Une fois la chaîne de caractère terminé, on renvoie l'accumulateur et les caractères non analysés
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
atomToJSON (LiteralInt i) = Number $ fromIntegral i
atomToJSON (LiteralDouble d) = Number $ realToFrac d
atomToJSON (LiteralString s) = String $ T.pack s
atomToJSON (Symbol "true") = Bool True
atomToJSON (Symbol "false") = Bool False
atomToJSON (Symbol "null") = Null
atomToJSON (Symbol sym) = object ["symbol" .= sym]