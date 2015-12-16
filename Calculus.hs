module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]



lookUp :: Eq a => a -> [(a, b)] -> b
-- look up for an element in a list of tuples (a, b) and return the second
-- element of the tuple (b)
lookUp x ((y, y') : ys)
 | x == y = y'
 | otherwise = lookUp x ys


operators1
 = [(Sin, sin),(Cos, cos),(Log, log),(Neg, negate)]::[(UnOp, Double -> Double)]
-- list of operators with type UnOp
operators2
 = [(Add, (+)), (Mul, (*)), (Div, (/))] :: [(BinOp, Double -> Double -> Double)]
-- list of operators with type BinOp

eval2 :: Exp -> Env -> Double
--another implementation for function eval
eval2 (Val x) e = x
eval2 (Id x) e = lookUp x e
eval2 (UnApp x y) e = (lookUp x operators1) (eval2 y e)
eval2 (BinApp x y z) e = (lookUp x operators2) (eval2 y e) (eval2 z e)



eval :: Exp -> Env -> Double
-- evaluates an expression
eval (Val x) e
 = x
eval (Id x) e
 = lookUp x e
eval (UnApp Neg x) e
 = (-1) * (eval x e)
eval (UnApp Sin x) e
 = sin (eval x e)
eval (UnApp Cos x) e
 = cos (eval x e)
eval (UnApp Log x) e
 = log (eval x e)
eval (BinApp Add x y) e
 = (eval x e) + (eval y e)
eval (BinApp Mul x y) e
 = (eval x e) * (eval y e)
eval (BinApp Div x y) e
 = (eval x e) / (eval y e)



diff :: Exp -> String -> Exp
-- differentiate an expression
diff (Val x) y
 = Val 0.0
diff (Id x) y
 | x == y = Val 1.0
 | otherwise = Val 0.0
diff (UnApp Neg x) y
 = UnApp Neg (diff x y)
diff (UnApp Sin x) y
 = BinApp Mul (UnApp Cos x) (diff x y)
diff (UnApp Cos x) y
 = UnApp Neg (BinApp Mul (UnApp Sin x) (diff x y))
diff (UnApp Log x) y
 = BinApp Div (diff x y) x
diff (BinApp Add x y) z
 = BinApp Add (diff x z) (diff y z)
diff (BinApp Mul x y) z
 = BinApp Add (BinApp Mul x (diff y z)) (BinApp Mul (diff x z) y)
diff (BinApp Div x y) z
 = BinApp Div (BinApp Add (BinApp Mul (diff x z) y)
                         (UnApp Neg (BinApp Mul x (diff y z)))) (BinApp Mul y y)
fact :: Int -> Int
-- computes n!
fact 0 = 1
fact n = n * fact (n - 1)

maclaurin :: Exp -> Double -> Int -> Double
-- computes the nth approximation of maclaurin series for x
maclaurin e x n = eval e [("x", 0)] + maclaurin' e (n - 1)
 where
 maclaurin' :: Exp -> Int -> Double
 maclaurin' ex 0  = 0
 maclaurin' ex nr = eval d [("x", 0)] * (x ^ n') / f + maclaurin' d (nr - 1)
  where
  d = diff' ex "x"
  n' = n - nr
  f = fromIntegral (fact n')

maclaurin2 :: Exp -> Double -> Int -> Double
-- another implementation of maclaurin function
maclaurin2 e x n = maclaurin2' ((a, b, c) : xs)
 where
 maclaurin2' :: [(Exp, Int, Double)] -> Double
 maclaurin2' []
  = 0
 maclaurin2' ((a1, b1, c1) : xs1)
  = (eval a1 [("x", 0)]) * c1 / (fromIntegral b1) + maclaurin2' xs1
 ((a, b, c) : xs)
  = take n (zip3 (iterate (`diff'` "x") e) factorial powers)
  where
  factorial = scanl (*) 1 [1..]
  powers = [x^n | n <- [0,1..]]

showExp :: Exp -> String
-- generates a neat printable representation for expressions
showExp (Id x) = x
showExp (Val x) = show x
showExp (UnApp Neg x) = concat ["-(", showExp x, ")"]
showExp (UnApp Sin x) = concat ["sin(", showExp x, ")"]
showExp (UnApp Cos x) = concat ["cos(", showExp x, ")"]
showExp (UnApp Log x) = concat ["log(", showExp x, ")"]
showExp (BinApp Add x y) = concat ["(", showExp x, "+", showExp y, ")"]
showExp (BinApp Mul x y) = concat ["(", showExp x, "*", showExp y, ")"]
showExp (BinApp Div x y) = concat ["(", showExp x, "/", showExp y, ")"]

diff' :: Exp -> String -> Exp
-- extension of diff function
diff' ex s
 | (diff2 ex s) == Nothing = Val 0
 | otherwise = fromJust (diff2 ex s)
diff2 :: Exp -> String -> Maybe Exp
diff2 (Val x) y
 = Nothing
diff2 (Id x) y
 | x == y = Just 1.0
 | otherwise = Nothing
diff2 (UnApp Neg x) y
 = negate (diff2 x y)
diff2 (UnApp Sin x) y
 = (cos (Just x)) * (diff2 x y)
diff2 (UnApp Cos x) y
 = negate (sin (Just x) * (diff2 x y))
diff2 (UnApp Log x) y
 = (/) (diff2 x y) (Just x)
diff2 (BinApp Add x y) z
 = (diff2 x z) + (diff2 y z)
diff2 (BinApp Mul x y) z
 = (Just x * (diff2 y z)) + ((diff2 x z) * Just y)
diff2 (BinApp Div x y) z
 = (/) (((diff2 x z) * Just y) + (negate (Just x * (diff2 y z))))
                                                               (Just y * Just y)


---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

-- > sin x+5*x ::Exp
e7 = BinApp Add (UnApp Sin (Id "x")) (BinApp Mul (Val 5.0) (Id "x"))

-- > 7*x + (sin x) * (cos x) ::Exp
e8 = BinApp Add (BinApp Mul (Val 7.0) (Id "x")) (BinApp Mul (UnApp Sin (Id "x"))
                                                           (UnApp Cos (Id "x")))

----------------------------------------------------------------------
instance (Eq a, Num a) => Num (Maybe a) where
 Nothing * x = Nothing
 Just 1 * x = x
 x * Nothing = Nothing
 x * Just 1 = x
 Just x * Just y = Just (x * y)
 Nothing + x = x
 x + Nothing = x
 Just x + Just y = Just (x + y)
 negate Nothing = Nothing
 negate (Just x) = Just (negate x)


instance (Eq a, Fractional a) => Fractional (Maybe a) where
 (/) Nothing x = Nothing
 (/) x (Just 1) = x
 (/) (Just x) (Just y) = Just (x / y)


instance (Eq a, Floating a) => Floating (Maybe a) where
 cos (Nothing) = Just 1
 cos (Just x) = Just (cos x)
 sin (Nothing) = Nothing
 sin (Just x) = Just (sin x)
 log (Just 1) = Nothing
 log (Just x) = Just (log x)

class Vars a where
 x, y :: a


instance Num Exp where
 negate = UnApp Neg
 (+) = BinApp Add
 (*) = BinApp Mul
 fromInteger x = Val (fromInteger x)


instance Fractional Exp where
 (/) = BinApp Div
 fromRational x = Val (fromRational x)

instance Floating Exp where
 sin = UnApp Sin
 cos = UnApp Cos
 log = UnApp Log

instance Vars Exp where
 x = Id "x"
 y = Id "y"
                            
