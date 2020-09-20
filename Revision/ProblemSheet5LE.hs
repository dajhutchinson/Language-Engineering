module ProblemSheet5LE where
import ProblemSheet4LE
import Text.Yoda
-- 1.1
data Pred = T
          | F
          | And Pred Pred
          | Not Pred
          deriving Show

-- 1.2
eval :: Pred -> Bool
eval T = True
eval F = False
eval (Not x) = not (eval x)
eval (And x y) | eval x = eval y
               | otherwise = False

-- 1.3 a)
predParser :: Parser Pred
predParser =  T <$ token "t"
          <|> F <$ token "f"
          <|> And <$> predParser <* token "&&" <*> predParser
          <|> Not <$ token "!" <*> predParser

-- 1.3 b) Infinte loop due to left recursion

-- 1.4 a)
-- Pred  ::= "t" Pred' | "f" Pred' | "!" Pred Pred'
-- Pred' ::= "&&" Pred Pred' | varepsilon

-- 1.4 b)
data Pred1 = T' Pred2
           | F' Pred2
           | Not' Pred1 Pred2
           deriving Show

data Pred2 = And' Pred1 Pred2
           | Empty'
           deriving Show

-- 1.4 c)
predParser1 :: Parser Pred1
predParser1 =  T' <$ token "t" <*> predParser2
           <|> F' <$ token "f" <*> predParser2
           <|> Not' <$ token "!" <*> predParser1 <*> predParser2

predParser2 :: Parser Pred2
predParser2 = And' <$ token "&&" <*> predParser1 <*> predParser2
           <|> pure Empty'

-- 1.4 d) All info about a constructor suceeded it

-- 1.5
atom =  T <$ token "t"
    <|> F <$ token "f"
    <|> prefix (Not <$ token "!") atom

predParser' :: Parser Pred
predParser' =  chainr1 atom (And <$ token "&&")

-- 2
data Graph = Empty               -- []
           | Vertex Int          -- [n]
           | Overlay Graph Graph -- +
           | Connect Graph Graph -- ->
           deriving Show

-- 2.1
graphAtom :: Parser Graph
graphAtom = Empty <$ token "[]" <|> Vertex <$> between (token "[") (token "]") number

graphParser :: Parser Graph
graphParser = chainl1  graphAtom (Overlay <$ token "+" <|> Connect <$ token "->")


-- 2.2
-- graph ::= graph "+" graph | connects
-- connects ::= connects "->" connects | atom
-- atom ::= "[" {number} "]"

-- 2.3
graphParser' :: Parser Graph
graphParser' = chainl1 (chainl1 graphAtom (Connect <$ token "+")) (Overlay <$ token "->")

-- 3
data Expr = Add Expr Expr
          | Mul Expr Expr
          | Val Int
          deriving Show

expr1 :: Parser Expr
expr1 = chainl1 term (Add <$ token "+")
  where
    term = chainl1 atom (Mul <$ token "*")
    atom = Val <$> number

expr2 :: Parser Expr
expr2 = chainl1 term (Mul <$ token "*")
  where
    term = chainl1 atom (Add <$ token "+")
    atom = Val <$> number

-- 2.1
-- expr1 = Add (Val 1) (Mul (Val 3) (Val 4)) = 1 + (3 * 4)
-- expr2 = Mul (Add (Val 1) (Val 3)) (Val 4) = (1 + 3) * 4
