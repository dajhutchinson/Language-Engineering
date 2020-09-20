-- ghci-8.2.2 == base 4.10.1.0

import Text.Yoda
import Data.Char

{-
  TODO install yoda
  "apt-cache search ghc" shows ghc-8.2.2 is installed
  Thus base 4.10.1.0 is installed (https://wiki.haskell.org/Base_package)
  cd Downloads/yoda-0.1.3.0; runhaskell Setup configure;
  says base 4.10.1.0 is missing
  :(
-}

-- 1.1.1 a)
-- parse (word <X> number) "hello234 parser" = [(("hello", 234), "parser"])

-- 1.1.1 b)
-- parse (word <X> number) "hello 123" = [(("", "hello"), " 123")]

-- 1.1.1 c)
-- parse (word *> number) "Dog14a" = parse number "Dog14a" = [()]

-- 1.1.2 a)
{-
  parse ((++) <$> word <*> pure " world") "hello parser"
= parse (fmap (++) (word <*> pure " world")) "hello parser"
= parse (Parser (\ ts -> [((++) x, ts') | (x,ts') <- (word <*> pure " world") ts])) "hello parser"
= (\ ts -> [((++) x, ts') <- (word <*> pure " world") ts]) "hello parser"
= [((++) x, ts') | (x, ts') <- (word <*> pure " world") "hello parser"]
= [((++) x, ts') | (x, ts') <- [(f x, ts'') | (f, ts') <- word "hello parser"
                                           , (x,ts'') <- pure "world" ts']]
= [((++) x, ts') | (x, ts') <- [("hello" x, ts'') | (f, ts') <- [("hello", " parser")]
                                           , (x,ts'') <- (pure "world") " parser"]]
= [((++) x, ts') | (x, ts') <- [("hello" " world", " parser") | (f, ts') <- [("hello", " parser")]
                                                             , (x,ts'') <- [(" world", " parser")]]]
= [((++) x, ts') | (x, ts') <- [("hello" "world", " parser")]]
= [((++) "hello" " world", " parser")]
= [("hello world", " parser")]
-}

-- 1.1.2 b)
{-
  parse (42 <$ word) "hello world"
= parse (const 42 <$> word) "hello world"
= parse (const fmap 42 word) "hello world"
= parse Parser(\ ts -> [(const 42 x, ts') | (x, ts') <- word ts]) "hello world"
= [(const 42 x, ts') | (x, ts') <- word "hello world"]
= [(const 42 x, ts') | (x, ts') <- ["hello", " world"]]
= [(const 42 "hello", " world")]
= [(42, "world")]
-}

-- 1.1.2 c)
digit :: Parser Char
digit = satisfy isDigit

digits :: Parser String
digits = cull (some digit)

number :: Parser Int
number = read <$> digits

-- 1.1.3 a)
{-
parse (word <|> string "abc" <|> pure "7") "abcd"
  = [("abcd",""), ("abc", "d"), (7, "abcd")]
-}

-- 1.1.3 b)
{-
  parse (number <+> word) "hello123"
= parse ((Left <$> number) <|> (Right <$> word)) "hello123"
= parse ((fmap Left number) <|> (fmap Right word)) "hello123"
= parse ((Parser (\ ts -> [(Left x, ts') | (x,ts') <- number ts])) <|> (Parser (\ ts -> [(Right x, ts') | (x,ts') <- word ts])) "hello123"
= [(Right "hello", "123")]
-}

-- 1.2
data Robot = Mov Int Robot
           | Rt Robot
           | Lt Robot
           | Stop
           deriving (Show)

robot :: Parser Robot
robot = Mov <$ (char 'f' <* optional (char ' ')) <*> number <*> next
    <|> Rt  <$  char 'r' <*> next
    <|> Lt  <$  char 'l' <*> next
next = (Stop <$ char '.') <|> (string ", " *> robot)

-- 1.2.1
rob1 = Mov 10 (Rt (Lt Stop))


-- 1.2.2
rob2 = parse robot "f ,r, l."
--"f ,r, l." not in language as no distance specified after 'f'

-- 1.2.3
rob3 = Rt (Rt (Rt Stop))

-- 1.2.4
-- Second is rejected as a ' ' is required after ',' (next definition)

-- 1.2.5
rob4 = [] -- since robot parser stops after parsing "r, l." to Right(Left Stop) leaving "f 10." to parse. This upsets eof

-- 1.2.6
{-
By the distributivity property of brackets we get
robot ::= "f" [" "] number next
        | "r" next
        | "l" next
We can redine the parser as

robot = (Mov <$ (char 'f' <* optional (char ' ')) <*> number
    <|> Rt <$ char 'r'
    <|> Lt <$ char 'l'
    ) <*> next
next = (Stop <$ char '.') <|> (string ", " *> robot)
-}

--2.1
{-
branFlakes ::= "+" next
             | "-" next
             | ">" next
             | "<" next
             | "[" branFlakes "]" next
next ::=  branFlakes | "."
-}

-- 2.2
data Bran = Inc Bran
          | Dec Bran
          | Rt' Bran
          | Lt' Bran
          | Rep Bran  -- repeat
          | EndRep Bran
          | Halt
          deriving (Show)

-- 2.3
branflakes :: Parser Bran
branflakes = Inc <$ char '+' <*> next'
         <|> Dec <$ char '-' <*> next'
         <|> Rt' <$ char '>' <*> next'
         <|> Lt' <$ char '<' <*> next'
         <|> Rep <$ (char '[') <*> next'
         <|> EndRep <$ char ']' <*> next' -- NOTE sorry
next' = (Halt <$ char '.') <|> branflakes

-- 2.4
data BranOp = Incr
            | Decr
            | Right
            | Left
            | Repeat BranOp

-- 2.5
-- branflakes' :: Parser [BranOp]
