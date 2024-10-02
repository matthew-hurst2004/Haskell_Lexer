import Data.Char (isSpace, isAlpha, isAlphaNum, isDigit)

-- Define the Token data type
data Token
  = TKeyword String
  | TIdentifier String
  | TOperator String
  | TLiteral String
  | TPunctuation Char
  | TWhiteSpace
  | TUnknown Char
  deriving (Show, Eq)
lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | isSpace x = TWhiteSpace : lexer xs
    | isAlpha x = let (ident, rest) = span isAlphaNum (x:xs) in TIdentifier ident : lexer rest
    | isDigit x = let (num, rest) = span isDigit (x:xs) in TLiteral num : lexer rest
    | x == '<' = case xs of
        '=':rest -> TOperator "<=" : lexer rest
        '<':rest -> TOperator "<<" : lexer rest
        _ -> TOperator "=" : lexer xs
    | x == '>' = case xs of
        '=':rest -> TOperator ">=" : lexer rest
        '>':rest -> TOperator ">>" : lexer rest
        _ -> TOperator ">" : lexer xs
    | x == '=' = case xs of
        '=':rest -> TOperator "==" : lexer rest
        _ -> TOperator "=" : lexer xs
    | x == '!' = case xs of
        '=':rest -> TOperator "!=" : lexer rest
        _ -> TOperator "!" : lexer xs
    | x == '&' = case xs of
        '&':rest -> TOperator "&&" : lexer rest
        _ -> TOperator "&" : lexer xs
    | x == '\\' = case xs of
        'n':rest -> TOperator "\\n" : lexer rest
        _ -> TOperator "\\n" : lexer xs
    | otherwise = case x of
        '+' -> TOperator "+" : lexer xs
        '-' -> TOperator "-" : lexer xs
        '*' -> TOperator "*" : lexer xs
        '/' -> TOperator "/" : lexer xs
        '%' -> TOperator "%" : lexer xs
        '=' -> TOperator "=" : lexer xs
        '<' -> TOperator "<" : lexer xs
        '>' -> TOperator ">" : lexer xs
        '!' -> TOperator "!" : lexer xs
        '&' -> TOperator "&" : lexer xs
        '^' -> TOperator "^" : lexer xs
        ';' -> TPunctuation ';' : lexer xs
        ',' -> TPunctuation ',' : lexer xs
        '{' -> TPunctuation '{' : lexer xs
        '}' -> TPunctuation '}' : lexer xs
        '[' -> TPunctuation '[' : lexer xs
        ']' -> TPunctuation ']' : lexer xs
        '(' -> TPunctuation '(' : lexer xs
        ')' -> TPunctuation ')' : lexer xs
        _ -> TUnknown x : lexer xs
 
main :: IO ()
main = do
    let code = "if (x <= 10) {\n    x = x + 1;\n} else {\n    x = x - 1;\n}"
    let tokens = lexer code
    print tokens