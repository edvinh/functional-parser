module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T
    | If Expr.T Statement Statement
    | While Expr.T Statement
    | Skip
    | Begin [Statement]
    | Read String
    | Write Expr.T
    | Comment String
    deriving Show

assStmt = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e
skipStmt    = accept "skip" # require ";" >-> \ _ -> Skip
ifStmt      = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> \ ((expr, ifstmt), elsestmt) -> If expr ifstmt elsestmt
whileStmt   = accept "while" -# Expr.parse # require "do" -# parse >-> \ (expr, stmt) -> While expr stmt
readStmt    = accept "read" -# word #- require ";" >-> \ var -> Read var
writeStmt   = accept "write" -# Expr.parse #- require ";" >-> \ expr -> Write expr
beginStmt   = accept "begin" -# iter parse #- require "end" >-> \ stmts -> Begin stmts
commentStmt = accept "--" -# line  #- require "\n" >-> \ cmt-> Comment cmt

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict) > 0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec ((While cond stmt): stmts) dict input =
    if (Expr.value cond dict) > 0
    then exec (stmt: (While cond stmt): stmts) dict input
    else exec stmts dict input
exec (Skip: stmts) dict input                   = exec stmts dict input
exec ((Read var): stmts) dict (val:input)       = exec stmts (Dictionary.insert (var, val) dict) input
exec ((Write expr): stmts) dict input           = (Expr.value expr dict) : exec stmts dict input
exec ((Begin stmt):stmts) dict input            = exec (stmt ++ stmts) dict input
exec ((Comment str): stmts) dict input          = exec stmts dict input 
exec ((Assignment var expr): stmts) dict input  = exec stmts (assToDict var expr dict) input
    where assToDict v e d = (Dictionary.insert (v, Expr.value e d) d)

indent :: Int -> String
indent i = take (2 * i) (repeat ' ') -- 2 spaces for indentation

shw :: Int -> Statement -> String
shw i (Assignment var expr)      = indent i ++ var ++ " := " ++ Expr.toString expr ++ ";\n"
shw i (If cond ifStmt elseStmt)  = indent i ++ "if " ++ Expr.toString cond ++ " then\n" ++ shw (i + 1) ifStmt ++ indent i ++ "else\n" ++ shw (i + 1) elseStmt
shw i (While cond stmts)         = indent i ++ "while " ++ Expr.toString cond ++ " do\n" ++ shw (i + 1) stmts
shw i (Begin stmts)              = indent i ++ "begin\n" ++ concatMap (shw (i + 1)) stmts ++ indent i ++ "end\n"
shw i (Read var)                 = indent i ++ "read " ++ var ++ ";\n"
shw i (Write expr)               = indent i ++ "write " ++ Expr.toString expr ++ ";\n"
shw i (Skip)                     = indent i ++ "skip;\n"
shw i (Comment comment)          = indent i ++ "-- " ++ comment ++ "\n"

instance Parse Statement where
  parse = assStmt ! skipStmt ! ifStmt ! whileStmt ! readStmt ! writeStmt ! beginStmt ! commentStmt
  toString = shw 0