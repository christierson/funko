module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Begin [Statement] |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Comment String
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >->
    \(s, e) -> Assignment s e
ifStatement = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >->
    \((e, x), y) -> If e x y
skipStatement = accept "skip" # require ";" >-> 
    \_ -> Skip
beginStatement = accept "begin" -# iter parse #- require "end" >-> Begin
--    \xs -> Begin xs
whileStatement = accept "while" -# Expr.parse #- require "do" # parse >-> 
    \(e, s) -> While e s
readStatement = accept "read" -# word #- require ";" >-> Read
--    \x -> Read x
writeStatement = accept "write" -# Expr.parse #- require ";" >-> Write
--    \x -> Write x
comment = accept "--" -# newline #- require "\n" >-> Comment

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment varName expr: stmts) dict input = exec stmts updated input
    where updated = Dictionary.insert (varName, Expr.value expr dict) dict
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
        then exec (thenStmts: stmts) dict input
        else exec (elseStmts: stmts) dict input
exec (Skip: stmts) dict input = exec stmts dict input
exec (Begin xs: stmts) dict input = exec (xs ++ stmts) dict input
exec (While cond doStmts: stmts) dict input =
    if (Expr.value cond dict)>0
        then exec (doStmts:(While cond doStmts: stmts)) dict input
        else exec stmts dict input
exec (Read valName: stmts) dict (i:input) = exec stmts updated input
    where updated = Dictionary.insert (valName, i) dict
exec (Write expr: stmts) dict input = Expr.value expr dict : (exec stmts dict input)
exec (Comment cmt: stmts) dict input = exec stmts dict input

instance Parse Statement where
    parse = assignment ! 
        ifStatement ! 
        skipStatement ! 
        beginStatement ! 
        whileStatement ! 
        readStatement ! 
        writeStatement !
        comment
    
    toString = error "Statement.toString not implemented"
