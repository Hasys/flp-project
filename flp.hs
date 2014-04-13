--Loading nessesary libraries.
module Main( main ) where

import System.Environment( getArgs )
import System.IO

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Char

escapeOrStringChar :: Parser Char
escapeOrStringChar = try (string "''" >> return '\'') 
					<|> do 
            c <- noneOf "'"
            return $ c

aelDef = emptyDef
  { commentStart   = "{"
  , commentEnd     = "}"
  , nestedComments = False
  , identStart     = letter <|> char '_'
  , identLetter    = alphaNum <|> char '_'
  , opStart        = oneOf "'=+*-"
  , opLetter       = opStart aelDef
  , reservedOpNames= [ ":=", "=", "+", "*", "-", "/", "<>", ">", "<", ">=", "<=", "(", ")", ".", ":", "\'" ]
  , reservedNames  = [ "writeln", "readln", "while", "do", "if", "then", "else", "begin", "end",
                       "div", "double", "integer", "string", "var", "function" ]
  , caseSensitive  = True
  }

mystringliteral = lexeme $ do   
    str <- between (char '\'') (char '\'' <?> "end of string") (many escapeOrStringChar)
    return str      
					  

lexer = (P.makeTokenParser aelDef) {P.stringLiteral = mystringliteral}
lexeme = P.lexeme lexer
whiteSpace= P.whiteSpace lexer
integer   = P.integer lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
stringConst = P.stringLiteral lexer



aep = do
  whiteSpace
  ast <- many sections
  eof
  return ast
  <?> "aep"

data Command = Empty
  | Assign String Expr
  | Writeln Expr
  | Readln String
  | Seq [ Command ]
  | If BoolExpr Command Command
  | While BoolExpr Command 
  | Program Command
  | Vars
  | Function
  | Main [ Command ]
  deriving Show

sections = do
    reserved "var"
    semi
    return $ Empty
  <|> do 
    reserved "begin"
    program <- mcmd_parser
    reserved "end"
    reservedOp "."
    return $ Program program
  <|> do
    reserved "function"
    reservedOp "("
    reservedOp ")"
    reservedOp ":"
    semi
    return $ Empty
  <?> "sections"

mcmd_parser = 
  do
    x  <- cmd
    xs <- many mcmd
    return $ Seq (x:xs)
    <|> do           
    return $ Empty

  <?> "ERR: multiple command block in main scope"
  
mcmd = do
  semi
  cmd

multipleCmd = 
  do
    reserved "begin"
    seq <- mcmd_parser
    reserved "end"  
    return seq
  <|> do
    c <- cmd
    return c
  <?> "ERR: multiple command"

cmd = do
    semi
    return Empty
  <|> do
    reserved "writeln"
    e <- parens expr
    return $ Writeln e
  <|> do
    reserved "readln"
    reservedOp "("
    i <- identifier
    reservedOp ")"
    return $ Readln i
  <|> do
    i <- identifier
    reservedOp ":="
    e <- expr
    return $ Assign i e
  <|> do
    reserved "if"
    b <- boolExpr
    reserved "then"
    c1 <- multipleCmd
    reserved "else"
    c2 <- multipleCmd
    return $ If b c1 c2
  <|> do
    reserved "while"
    b <- boolExpr
    reserved "do"
    c <- multipleCmd
    return $ While b c
  <?> "command"

data Expr = Const Integer
  | SConst String
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving Show

expr = 
  buildExpressionParser operators term where
    operators = [
        [ op "*" Mult, op "/" Div ],
        [ op "+" Add, op "-" Sub ]
      ]
    op name fun =
      Infix ( do { reservedOp name; return fun } ) AssocLeft

term = do
    i <- integer
    return $ Const $ fromInteger i
  <|> do
    s <- mystringliteral
    return $ SConst s
  <|> do
    v <- identifier
    return $ Var v
  <|> parens expr
  <?> "term"

data BoolExpr = Equal Expr Expr
  | NotEqual Expr Expr
  | Less Expr Expr
  | More Expr Expr
  | LessEqual Expr Expr
  | MoreEqual Expr Expr
  deriving Show

boolExpr = do
    e1 <- expr
    o <- relOp
    e2 <- expr
    return $ o e1 e2
  <?> "boolean expression"
  where
    relOp = ro' "=" Equal
      <|> ro' "<>" NotEqual
      <|> ro' "<" Less
      <|> ro' ">" More
      <|> ro' "<=" LessEqual
      <|> ro' ">=" MoreEqual
      <?> "relational operator"
    ro' name fun = do
      reservedOp name
      return fun

type SymbolTable = [(String, Integer)]

set :: SymbolTable -> String -> Integer -> SymbolTable
set [] var val = [(var, val)]
set (s@(v,_):ss) var val =
  if v == var
    then (var, val):ss
    else s : set ss var val

get :: SymbolTable -> String -> Integer
get [] _ = error "Not found"
get (s@(var, val):ss) v =
  if v == var
    then val
    else get ss v

evaluate :: SymbolTable -> Expr -> Integer
evaluate ts (Const i) = i
evaluate ts (Var v) = get ts v
evaluate ts (Add e1 e2) = (evaluate ts e1) + (evaluate ts e2)
evaluate ts (Sub e1 e2) = (evaluate ts e1) - (evaluate ts e2)
evaluate ts (Mult e1 e2) = (evaluate ts e1) * (evaluate ts e2)

decide :: SymbolTable -> BoolExpr -> Bool
decide ts (Equal a b) = evaluate ts a == evaluate ts b
decide ts (NotEqual a b) = evaluate ts a /= evaluate ts b
decide ts (Less a b) = evaluate ts a < evaluate ts b
decide ts (More a b) = evaluate ts a > evaluate ts b
decide ts (LessEqual a b) = evaluate ts a <= evaluate ts b
decide ts (MoreEqual a b) = evaluate ts a >= evaluate ts b

startInterpret :: SymbolTable -> [Command] -> IO SymbolTable
startInterpret ts cs = do
  --ts' <- interpret ts c
  interpret ts $ Main cs
--startInterpret ts [] = return ts


interpret :: SymbolTable -> Command -> IO SymbolTable
interpret ts (Vars) = return ts

interpret ts (Main []) = return ts
interpret ts (Main (c:cs)) = do
  ts' <- interpret ts c
  interpret ts' $ Main cs


interpret ts (Function) = return ts

interpret ts (Program c) = do  
  interpret ts c

interpret ts (Empty) = return ts

interpret ts (Assign v e) = return $ set ts v $ evaluate ts e

interpret ts (Writeln (SConst s)) = do
  putStrLn s
  return ts

interpret ts (Writeln e) = do
  putStrLn $ show $ evaluate ts e
  return ts

interpret ts (Readln v) = do
  i <- readLn :: IO Integer
  return $ set ts v i

interpret ts (If cond c1 c2) = do
  if decide ts cond
    then interpret ts c1
    else interpret ts c2

interpret ts w@(While cond c) = do
  if decide ts cond
    then do
      ts' <- interpret ts c
      interpret ts' w
    else return ts

interpret ts (Seq []) = return ts
interpret ts (Seq (c:cs)) = do
  ts' <- interpret ts c
  interpret ts' $ Seq cs
  
parseAep input file =
  case parse aep file input of
    Left e -> error $ show e
    Right ast -> ast 

    
main = do
  args <- getArgs
  if length args /= 1
    then error "Specify one input file."
    else do
      let fileName = args!!0
      input <- readFile fileName
      let ast = parseAep input fileName
      startInterpret [] ast 