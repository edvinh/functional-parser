module Expr(Expr, T, parse, fromString, value, toString) where

{-
   An expression of type Expr is a representation of an arithmetic expression 
   with integer constants and variables. A variable is a string of upper- 
   and lower case letters. The following functions are exported
   
   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int
   
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.
   
   fromString expects its argument to contain an expression and returns the 
   corresponding Expr. 
  
   toString converts an expression to a string without unneccessary 
   parentheses and such that fromString (toString e) = e.
  
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.  
-}
import Prelude hiding (return, fail)
import Parser hiding (T)
import Data.Maybe
import qualified Dictionary

data Expr = Num Integer | Var String | Add Expr Expr 
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Expo Expr Expr
         deriving Show

type T = Expr

var, num, expo, factor, term, expr :: Parser Expr

term', expr' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> (\ _ -> Mul) !
        lit '/' >-> (\ _ -> Div)

addOp = lit '+' >-> (\ _ -> Add) !
        lit '-' >-> (\ _ -> Sub)

bldOp e (oper,e') = oper e e'

expOp = lit '^' >-> (\_ -> Expo)

expo = num !
       var !
       lit '(' -# expr #- lit ')' !
       err "illegal factor"

factor' e = expOp # expo >-> bldOp e #> factor' ! return e
factor = expo #> factor'

term' e = mulOp # factor >-> bldOp e #> term' ! return e
term = factor #> term'
       
expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)

value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _                 = n
value (Var v) dict              = fromMaybe (error ("Undeclared variable: " ++ v)) (Dictionary.lookup v dict)
value (Add t u) dict            = (value t dict) + (value u dict)
value (Sub t u) dict            = (value t dict) - (value u dict)
value (Mul t u) dict            = (value t dict) * (value u dict)
value (Div t u) dict
        | value u dict == 0     = error ("Division by zero") 
        | otherwise             = (value t dict) `div` (value u dict)
value (Expo base exp) dict      = (value base dict) ^ (value exp dict) 


instance Parse Expr where
    parse = expr
    toString = shw 0
