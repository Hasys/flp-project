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


-- Top-level parser (MAIN)
aep = do
  whiteSpace
  globalVariables <- globalVarParser
  functions <- many functionDeclarationParser
  mainBlock <- mainProgramBlockParser
  eof
  return $ [globalVariables] ++ functions ++ [mainBlock]
  <?> "ERROR: highest level parsing error"

mainProgramBlockParser =
  do
    reserved "begin"
    program <- mcmd_parser
    reserved "end"
    reservedOp "."
    return $ Program program
  <?> "ERROR: parsing main bock error"

-- Parsing global variables section
globalVarParser = 
  do
    reserved "var"
    x <- oneVar
    xs <- many otherVars
    semi
    return $ Seq (x:xs)
  <|> do
    return Empty
  <?> "ERROR: declaration of global variables error"

oneVar = 
  do
    i <- identifier
    reservedOp ":"
    reserved "integer"
    return $ Assign i $ Const $ IntegerValue 0

otherVars = 
  do
    reservedOp ","
    oneVar

-- PARSING FUNCTIONS

-- Parsing function declaration --
functionDeclarationParser =
  do
    reserved "function"
    id <- identifier
    reservedOp "("
    fp <- functionParameters
    reservedOp ")"
    whiteSpace
    reservedOp ":"
    reserved "integer"
    semi
    functionDefinitionParser id fp

  <|> do
    reserved "function"
    id <- identifier
    reservedOp "("
    fp <- functionParameters
    reservedOp ")"
    whiteSpace
    reservedOp ":"
    reserved "integer"
    semi 
    return $ FuncDeclare id fp
  <?> "ERROR: function declaration parsing error"

-- Function parameters in function declaration
functionParameters = 
  do  
    x  <- oneFunctionParameter
    xs <- many multipleFunctionParameters
    return $ (x:xs)
  <|> do                 
    return [] 
  <?> "ERROR: Parameters in fucntion definition error"

oneFunctionParameter =
  do
    id <- identifier
    reservedOp ":"
    reserved "integer"
    return $ Assign id $ Const $ IntegerValue 0
    
multipleFunctionParameters = 
  do
    reservedOp ","
    oneFunctionParameter

-- Parsing function definition --
functionDefinitionParser id fp  = 
  do
    lv <- localVariables
    mc <- multipleCmd
    return $ Function 
  <|> do
    return $ Function 
  <?> "ERROR: function definition parsing error"

-- Local variables in function definition
localVariables =
  do
    reserved "var"   
    x  <- oneLocalVariable
    xs <- many multipleLocalVariables
    semi
    return $ (x:xs)
  <|> do
    return []
  <?> "ERROR: function definition (local variables)"
     
oneLocalVariable =
  do
    i <- identifier
    reservedOp ":"
    reserved "integer"
    
-- dalsi promenne oddelene carkou
multipleLocalVariables = 
  do
    reservedOp ","
    oneLocalVariable


--
--
--
--
--
--

-- Parsing commands
mcmd_parser = 
  do
    x  <- cmd
    xs <- many mcmd
    return $ Seq (x:xs)
    <|> do           
    return $ Empty

  <?> "ERROR: many commands error"

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
  <?> "ERROR: many commands"

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

-- Data structures
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
  | FuncDeclare String [ Command ]
  | Main [ Command ]
  | Assoc String
  deriving Show

data Expr = Const Value
  | SConst String
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving Show

data BoolExpr = Equal Expr Expr
  | NotEqual Expr Expr
  | Less Expr Expr
  | More Expr Expr
  | LessEqual Expr Expr
  | MoreEqual Expr Expr
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
    return $ Const $ IntegerValue $ fromInteger i
  <|> do
    s <- mystringliteral
    return $ SConst s
  <|> do
    v <- identifier
    return $ Var v
  <|> parens expr
  <?> "term"

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

type SymbolTable = [(String, Value)]
type Variable = (String, Value)
data Value = IntegerValue { intVal :: Integer }
            | FunctionValue  {
                              ident :: String,
                              params :: [Command],
                              scope :: Command,
                              lts :: SymbolTable
                            }
            | StringValue { strVal :: String }
            | Undeclared

instance Show Value where
  show (IntegerValue int) = show int
  show (StringValue str) = show str

getType :: Value -> String
getType val = case val of 
                FunctionValue w x y z -> "function"
                Undeclared -> "undeclared"

-- Setting values to symbols table
set :: SymbolTable -> String -> Value -> SymbolTable
set [] var val = [(var, val)]
set (s@(v,_):ss) var val =
  if v == var
    then (var, val):ss
    else s : set ss var val

-- Getting values from symbols table
get :: SymbolTable -> String -> Value
get [] _ = error "Not found"
get (s@(var, val):ss) v =
  if v == var
    then val
    else get ss v

-- Evaluating expressions
evaluate :: SymbolTable -> Expr -> Value
evaluate ts (Const i) = i
evaluate ts (Var v) = get ts v
evaluate ts (Add e1 e2) = addValues (evaluate ts e1) (evaluate ts e2)
evaluate ts (Sub e1 e2) = subValues (evaluate ts e1) (evaluate ts e2)


evaluate ts (Mult e1 e2) = multValues (evaluate ts e1) (evaluate ts e2)

-- Evaluating bool expressions
decide :: SymbolTable -> BoolExpr -> Bool
decide ts (Equal a b) = areEqual (evaluate ts a) (evaluate ts b)
decide ts (NotEqual a b) = areNotEqual (evaluate ts a) (evaluate ts b)
decide ts (Less a b) = isLess (evaluate ts a) (evaluate ts b)
decide ts (More a b) = isMore (evaluate ts a) (evaluate ts b)
decide ts (LessEqual a b) = isLessEqual (evaluate ts a) (evaluate ts b)
decide ts (MoreEqual a b) = isMoreEqual (evaluate ts a) (evaluate ts b)

addValues :: Value -> Value -> Value
addValues (IntegerValue val1) (IntegerValue val2) = IntegerValue (val1 + val2)
--addValues (DoubleValue val1) (DoubleValue val2) = DoubleValue (val1 + val2)
--addValues (DoubleValue val1) (IntegerValue val2) = DoubleValue (val1 + fromIntegral val2)
--addValues (IntegerValue val1) (DoubleValue val2) = DoubleValue (fromIntegral val1 + val2)

subValues :: Value -> Value -> Value
subValues (IntegerValue val1) (IntegerValue val2) = IntegerValue (val1 - val2)

multValues :: Value -> Value -> Value
multValues (IntegerValue val1) (IntegerValue val2) = IntegerValue (val1 * val2)

areEqual :: Value -> Value -> Bool
areEqual (IntegerValue a) (IntegerValue b) = a == b

areNotEqual :: Value -> Value -> Bool
areNotEqual (IntegerValue a) (IntegerValue b) = a /= b

isLess :: Value -> Value -> Bool
isLess (IntegerValue a) (IntegerValue b) = a < b

isMore :: Value -> Value -> Bool
isMore (IntegerValue a) (IntegerValue b) = a > b

isLessEqual :: Value -> Value -> Bool
isLessEqual (IntegerValue a) (IntegerValue b) = a <= b

isMoreEqual :: Value -> Value -> Bool
isMoreEqual (IntegerValue a) (IntegerValue b) = a >= b

startInterpret :: SymbolTable -> [Command] -> IO SymbolTable
startInterpret ts cs = do
  interpret ts $ Main cs

-- Interpreter
interpret :: SymbolTable -> Command -> IO SymbolTable
interpret ts (Vars) = return ts


-- Main block interpreter
interpret ts (Main []) = return ts
interpret ts (Main (c:cs)) = do
  ts' <- interpret ts c
  interpret ts' $ Main cs

-- Fucntion interpreter
interpret ts (Function) = return ts
interpret ts (FuncDeclare id p) = do
  var <- return $ get ts id
  if (getType var) == "undeclared"
    then do
      let func = FunctionValue { ident=id, params=p, scope=Empty, lts=[] }
      return $ set ts id func
    else error "Multiple function declaration"
  return ts

-- All program interpreter
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
  return $ set ts v $ IntegerValue i

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