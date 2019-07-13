import Yoda

--newtype Parser a = Parser { parse :: String -> [(String, a)] }

whitespace :: Parser ()
whitespace = many (oneOf [' ', '\t']) *> pure ()

tok :: String -> Parser String
tok xs = whitespace *> string xs <* whitespace

number :: Parser Int
number = undefined
