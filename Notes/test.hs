type Expr = Int

var :: Int -> Expr
var n = n

add' :: Expr -> Expr -> Expr
add' x y = x + y

add'' :: Int -> Int -> Int
add'' x y = x+y
