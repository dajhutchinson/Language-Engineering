module ProblemSheet4LE where
import Text.Yoda
import Data.Maybe

whitespace :: Parser ()
whitespace = skip (many (oneOf [' ', '\t', '\n']))

token :: String -> Parser String
token t = string t <* whitespace

word :: Parser String
word = some (oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

digits :: Parser String
digits = some (oneOf "0123456789") -- parses digits as string

parse' :: Parser a -> String -> [(a,String)]
parse' p s = [(parse p s)!!0]

-- 1.1 a) parse (word <~> number) "hello234 parser" = [(("hello",234), " parser")]

-- 1.1 b) parse (word <~> number) "hello 123" = []

-- 1.1 c) parse (word *> number) "Dog14a" = =[(14,"a")]

-- 1.2 a) parse ((++) <$> word <*> pure " world") "hello parser"
--      = (++) <$> [("hello"," parser")] <*> pure " world"
--      = [((++) "hello"," parser")] <*> pure " world"
--      = [((++) "hello" " world"," parser")]
--      = [("hello world"," parser")]

-- 1.2 b) parse (42 <$ word) "hello world"
--      = [(const 42 "hello", "world")]
--      = [(42, "world")]

-- 1.2 c)
number :: Parser Int
number = (\ s -> read s::Int) <$> digits <* whitespace

-- 1.3 a) parse (word <|> string "abc" <|> pure "7") "abcd" = [("abcd",""), ("abc","d"), (7,"abcd")]

-- 1.3 b)
(<+>) :: Parser a -> Parser b -> Parser (Either a b)
p <+> q = (Left <$> p) <|> (Right <$> q)
-- parse (number <+> word) "hello123" = [(Right "hello","123")]

-- 2
data Robot = Mov Int Robot
           | Rt Robot
           | Lt Robot
           | Stop
           deriving Show

robot :: Parser Robot
robot =  Mov <$ (char 'f' <* optional (char ' ')) <*> number <*> next
     <|> Rt <$ char 'r' <*> next
     <|> Lt <$ char 'l' <*> next
next = (Stop <$ char '.') <|> (string ", " *> robot)

-- 2.1 Mov 10 (Rt (Lt Stop))

-- 2.2 No, a number is required after 'f'

-- 2.3 Yes, Rt (Rt (Rt Stop)) but "l, l, l." is left unconsumed

-- 2.4 "f10." is accepted since ' ' after f is optional. "f 10,l." is not since a space is required after commas.

-- 2.5 parse (robot <* eof) "r, l. f 10." = []

robot' = (
          Mov <$ (char 'f' <* optional (char ' ')) <*> number
      <|> Rt <$ char 'r'
      <|> Lt <$ char 'l'
      ) <*> (string ", " *> robot' <|> Stop <$ char '.')

-- 3.1
--Bran ::= "+" Bran
--       | "-" Bran
--       | ">" Bran
--       | "<" Bran
--       | "[" Bran "]" Bran
--       | varepsilon

-- 3.2
data Bran = Inc Bran
          | Dec Bran
          | Rig Bran
          | Lef Bran
          | Rep Bran Bran
          | Halt
          deriving Show

-- 3.3
branflakes :: Parser Bran
branflakes =  Inc  <$ char '+' <*> branflakes
          <|> Dec  <$ char '-' <*> branflakes
          <|> Rig  <$ char '>' <*> branflakes
          <|> Lef  <$ char '<' <*> branflakes
          <|> between (char '[') (char ']') (Rep <$> branflakes) <*> branflakes
          <|> pure Halt

-- 3.4
data BranOp = Incr | Decr | Ri | Le | Repe [BranOp] deriving Show

-- 3.5
branflakes' :: Parser [BranOp]
branflakes' = catMaybes <$> many px
  where
    px =  Just Incr <$ char '+'
      <|> Just Decr <$ char '-'
      <|> Just Ri   <$ char '>'
      <|> Just Le   <$ char '<'
      <|> between (char '[') (char ']') (Just . Repe <$> branflakes')
      <|> Nothing <$ noneOf "+-><[]"

-- 4.1
between' :: Parser o -> Parser c -> Parser a -> Parser a
between' po pc pa = po *> pa <* pc

-- 4.2 a)
choice :: [Parser a] -> Parser a
choice pxs = foldr (<|>) empty pxs

-- 4.2 b)
oneOf' :: [Char] -> Parser Char
oneOf' cs = satisfy (\ c -> elem c cs)

-- 4.3

-- 4.4 a)
(<::>) :: Parser a -> Parser [a] -> Parser [a]
px <::> pxs = (:) <$> px <*> pxs

-- 4.4 b)
sequence' :: [Parser a] -> Parser [a]
sequence' [] = pure []
sequence' (px:pxs) = px <::> (sequence' pxs)

-- 4.4 c)
traverse' :: (a -> Parser b) -> [a] -> Parser [b]
traverse' f as = sequence (map f as)

-- 4.4 d)
string' :: String -> Parser String
string' s = traverse (\ c -> satisfy (==c)) s
