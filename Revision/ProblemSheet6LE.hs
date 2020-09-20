import Text.Yoda
import ProblemSheet4LE
-- 1
class Pretty a where
  pretty :: a -> String

-- 1.1
instance Pretty Robot where
  pretty (Mov n r) = "f "++(show n)++" "++(pretty r)
  pretty (Rt r) = ",r "++(pretty r)
  pretty (Lt r) = ",l "++(pretty r)
  pretty (Stop) = "."

robot1 = Mov 10 (Rt (Lt Stop))

-- 1.2
check :: (Eq a, Pretty a) => Parser a -> String -> Bool
check p s = s == pretty (fst (head (parse p s)))

-- 2
type While = Stm
data Stm = Skip | Var ::= Aexp | Stm :> Stm | If Bexp Stm Stm | While Bexp Stm
data Aexp = Num Int | Var Var | Aexp :+ Aexp | Aexp :* Aexp | Aexp :- Aexp
data Bexp = T | F | Aexp := Aexp | Aexp :<= Aexp | Bexp :&& Bexp | Not Bexp
type Var = String

-- 2.1

while :: Parser While
while = stms <* eof

stms :: Parser While
stms = chainr1 stm ((:>) <$ token ";")

stm  :: Parser Stm
stm =  Skip  <$ token "skip"
   <|> (::=) <$> (var <* token "::=") <*> aexp
   <|> If    <$ token "if" <*> (bexp <* token "then") <*> (stm <* token "else") <*> stm
   <|> While <$ token "while" <*> (bexp <* token "do") <*> stm

aexp :: Parser Aexp
aexp =  Num <$> number
    <|> Var <$> var
    <|> chainl1 aexp ((:+) <$ token "+")
    <|> chainl1 aexp ((:*) <$ token "*")
    <|> chainl1 aexp ((:-) <$ token "-")

bexp :: Parser Bexp
bexp =  T <$ token "true"
    <|> F <$ token "false"
    <|> chainl1 aexp ((:=) <$ token "=")
    <|> chainl1 aexp ((:<=) <$ token "<=")
    <|> chainr1 bexp ((:&&) <$ token "&&")
    <|> Not <$ token "!" <*> bexp

var  :: Parser Var
var = word
