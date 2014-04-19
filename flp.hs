-- FLP projekt (FLP_2014_Pascal) --
-- Sergii Khunovych xkhuno00     --
-- Kirill Gaevskii xgaevs01      --
-- Maksym Boychuk xboych00       --

module Main( main ) where
import System.Environment( getArgs )
import System.IO
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Char

aelDef = emptyDef
  { commentStart   = "{"
  , commentEnd     = "}"
  , nestedComments = False
  , identStart     = letter <|> char '_'
  , identLetter    = alphaNum <|> char '_'
  , opStart        = oneOf "'=+*-"
  , opLetter       = opStart aelDef
  , reservedOpNames= [ ":=", "=", "+", "*", "-", "div", "<>", ">", "<", ">=", "<=", "(", ")", ".", ":", "\'" ]
  , reservedNames  = [ "writeln", "readln", "while", "do", "if", "then", "else", "begin", "end",
                       "div", "double", "integer", "string", "var", "function" ]
  , caseSensitive  = True
  }

lexer = (P.makeTokenParser aelDef) {P.stringLiteral = mystringliteral}

-- Re-defining string literal
mystringliteral = lexeme $ do   
    str <- between (char '\'') (char '\'' <?> "end of string") (many escapeOrStringChar)
    return str    

lexeme      = P.lexeme lexer
whiteSpace  = P.whiteSpace lexer
integer     = P.integer lexer
naturalOrFloat= P.naturalOrFloat lexer
parens      = P.parens lexer
semi        = P.semi lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
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
    type_of_Variable i
	
type_of_Variable i =
		do
		reserved "integer"
		return $ GVarDec (i, IntegerValue 0)
	  <|> do
		reserved "double"
		return $ GVarDec (i, DoubleValue 0.0)
	  <|> do
		reserved "string"
		return $ GVarDec (i, StringValue "")
	  <?> "ERR: global or function varible type"

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
    rType <- returnType id
    semi
    functionDefinitionParser id fp rType 
  <?> "ERROR: function declaration parsing error"

returnType id =
  do
    reserved "integer"
    return (id, RetValue { retVal = (IntegerValue 0) } )
  <|> do
    reserved "double"
    return (id, RetValue { retVal = (DoubleValue 0.0) } )
  <|> do
    reserved "string"
    return (id, RetValue { retVal = (StringValue "") } )
  <?> "ERR: bad function return type"  

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
    parametr_types id
	
parametr_types i = 
  do
    reserved "integer"
    return $ (i, IntegerValue 0)
  <|> do
    reserved "double"
    return $ (i, DoubleValue 0.0)
  <|> do
    reserved "string"
    return $ (i, StringValue "")
  <?> "ERR: bad function, parameter or varible type"
    
multipleFunctionParameters = 
  do
    reservedOp ","
    oneFunctionParameter

-- Parsing function definition --
functionDefinitionParser id fp rType  = 
  do
    lv <- localVariables
    mc <- multipleCmd
    return $ Function id fp (Seq [mc]) (fp ++ lv ++ [rType])
  <|> do                 
    return $ FuncDeclare id fp
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
    id <- identifier
    reservedOp ":"
    parametr_types id
    
multipleLocalVariables = 
  do
    reservedOp ","
    oneLocalVariable

mcmd_parser = 
  do
    x  <- cmd
    xs <- many mcmd
    return $ Seq (x:xs)
  <|> do           
    return $ Empty

  <?> "ERROR: many commands error"

mcmd = 
  do
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

cmd = 
  do
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

expr = 
  buildExpressionParser operators term where
    operators = [
        [ op "*" Mult, op "div" Div ],
        [ op "+" Add, op "-" Sub ]
      ]
    op name fun =
      Infix ( do { reservedOp name; return fun } ) AssocLeft

term = 
  do
    num <- naturalOrFloat;
		return $ (case num of { Right i -> Const $ DoubleValue i; Left  i -> Const $ IntegerValue (fromInteger i) })
  <|> do
    s <- mystringliteral
    return $ SConst s
  <|> try ( do
    id <- identifier
    reservedOp "("
    ex <- call_params
    reservedOp ")"
    return $ FunctionCall id ex
    ) 
  <|> do
    v <- identifier
    return $ Var v
  <|> parens expr
  <?> "term"

call_params = 
  do
    x  <- expr 
    xs <- many expr_seq
    return $ (x:xs)
  <|> do
    return []

expr_seq = 
  do
    reservedOp ","
    expr

boolExpr = 
  do
    e1 <- expr
    o <- relationOp
    e2 <- expr
    return $ o e1 e2
  <?> "boolean expression"
  where
    relationOp = relationOp' "=" Equal
      <|> relationOp' "<>" NotEqual
      <|> relationOp' "<" Less
      <|> relationOp' ">" More
      <|> relationOp' "<=" LessEqual
      <|> relationOp' ">=" MoreEqual
      <?> "relational operator"
    relationOp' name funOp = do
      reservedOp name
      return funOp

getType :: Value -> String
getType val = case val of 
                IntegerValue x -> "integer"
                DoubleValue x -> "double"
                StringValue x -> "string"
                FunctionValue w x y z -> "function"
                RetValue x -> "return"
                GlobalValue x -> "global"
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
get [] _ = Undeclared
get (s@(var, val):ss) v =
  if v == var
    then val
    else get ss v

-- Checking for quotes inside string
escapeOrStringChar :: Parser Char
escapeOrStringChar = try (string "''" >> return '\'') 
          <|> do 
            c <- noneOf "'"
            return $ c

-- Evaluating expressions
evaluate :: SymbolTable -> Expr -> IO EvalReturn
evaluate ts (Const i) = return $ (i, [])
evaluate ts (Var v) = return $ ((get ts v), [])
evaluate ts (Add e1 e2) = do
  ev1 <- evaluate ts e1
  ev2 <- evaluate ts e2
  return $ ((addValues (fst ev1) (fst ev2)), [])
evaluate ts (Sub e1 e2) = do
  ev1 <- evaluate ts e1
  ev2 <- evaluate ts e2
  return $ ((subValues (fst ev1) (fst ev2)), [])
evaluate ts (Mult e1 e2) = do
  ev1 <- evaluate ts e1
  ev2 <- evaluate ts e2
  return $ ((multValues (fst ev1) (fst ev2)), [])
evaluate ts (Div e1 e2) = do
  ev1 <- evaluate ts e1
  ev2 <- evaluate ts e2
  return $ ((divValues (fst ev1) (fst ev2)), [])
evaluate ts (FunctionCall id e) = do
  fun <- return $ get ts id
  fParams <- return $ params fun
  gParams <- return e
  fScope  <- return $ scope fun

  numOfParams <- return $ length fParams
  givenParams <- return $ length gParams


  if numOfParams /= givenParams
    then do error $ "Different count of  function parameters in function '" ++ id ++ "'"
    else do
        vals <- computeGivenParams ts gParams

        lts   <- return $ lts fun
        lts'  <- setParamValues (lts++ts) fParams vals

        let ft = [ x | x <- ts, (getType (snd x)) == "function" ]
        let nts = ft ++ lts'

        nts' <- interpret (nts) fScope

        let rts = [ x | x <- nts', (getType (snd x)) == "return" ]

        let gv = [ x | x <- nts', (getType (snd x)) == "global"]

        ret <- return $ get rts id
        return $ ((retVal ret), gv)

computeGivenParams :: SymbolTable -> [Expr] -> IO [Value]
computeGivenParams [] [] = return []
computeGivenParams ts [] = return []
computeGivenParams ts [x] = do
  val <- evaluate ts x
  return $ [ fst val ]
  
computeGivenParams ts (x:xs) = do
  val <- evaluate ts x
  ss <- computeGivenParams ts xs
  return $ (fst val) : ss

setParamValues :: SymbolTable -> [Variable] -> [Value] -> IO SymbolTable
setParamValues ts [] [] = return ts
setParamValues ts ((id, value):xs) (y:ys) = do
  val <- checkTypeAssignment value y
  ts' <- return $ set ts id val   
  setParamValues ts' xs ys

checkTypeAssignment :: Value -> Value -> IO Value
checkTypeAssignment (IntegerValue x) (IntegerValue y) = return $ IntegerValue y 
checkTypeAssignment (DoubleValue x) (DoubleValue y) = return $ DoubleValue y
checkTypeAssignment (StringValue x) (StringValue y) = return $ StringValue y 
checkTypeAssignment (DoubleValue x) (IntegerValue y) = return $ DoubleValue $ fromIntegral y
     
checkTypeAssignment r@(RetValue x) y = checkTypeAssignment (retVal r) y

checkTypeAssignment g@(GlobalValue x) h@(GlobalValue y) = checkTypeAssignment g (gVal h)

checkTypeAssignment g@(GlobalValue x) y = do
  var <- return $ gVal g
  val <- checkTypeAssignment var y
  return $ GlobalValue val
  
checkTypeAssignment x h@(GlobalValue y) = do 
  var <- return $ gVal h
  val <- checkTypeAssignment x var
  return val
  
checkTypeAssignment (IntegerValue x) y = error "Incompatible assignment types"
checkTypeAssignment (DoubleValue x) y = error "Incompatible assignment types"
checkTypeAssignment (StringValue x) y = error "Incompatible assignment types"

-- Evaluating bool expressions
decide :: SymbolTable -> BoolExpr -> IO Bool
decide ts (Equal a b) = do
  ev1 <- evaluate ts a
  ev2 <- evaluate ts b
  return $ isEqual (fst ev1) (fst ev2)
decide ts (NotEqual a b) = do
  ev1 <- evaluate ts a
  ev2 <- evaluate ts b
  return $ isNotEqual (fst ev1) (fst ev2)
decide ts (Less a b) = do
  ev1 <- evaluate ts a
  ev2 <- evaluate ts b
  return $ isLess (fst ev1) (fst ev2)
decide ts (More a b) = do
  ev1 <- evaluate ts a
  ev2 <- evaluate ts b
  return $ isMore (fst ev1) (fst ev2)
decide ts (LessEqual a b) = do
  ev1 <- evaluate ts a
  ev2 <- evaluate ts b
  return $ isLessEqual (fst ev1) (fst ev2)
decide ts (MoreEqual a b) = do
  ev1 <- evaluate ts a
  ev2 <- evaluate ts b
  return $ isMoreEqual (fst ev1) (fst ev2)

addValues :: Value -> Value -> Value
addValues (IntegerValue val1) (IntegerValue val2) = IntegerValue (val1 + val2)
addValues (DoubleValue val1) (DoubleValue val2) = DoubleValue (val1 + val2)
addValues (DoubleValue val1) (IntegerValue val2) = DoubleValue (val1 + fromIntegral val2)
addValues (IntegerValue val1) (DoubleValue val2) = DoubleValue (fromIntegral val1 + val2)
addValues (StringValue val1) (StringValue val2) = StringValue (val1 ++ val2)
addValues g@(GlobalValue a) h@(GlobalValue b) = addValues (gVal g) (gVal h) 
addValues g@(GlobalValue a) b = addValues (gVal g) b 
addValues a h@(GlobalValue b) = addValues a (gVal h)

subValues :: Value -> Value -> Value
subValues (IntegerValue val1) (IntegerValue val2) = IntegerValue (val1 - val2)
subValues (DoubleValue val1) (DoubleValue val2) = DoubleValue (val1 - val2)
subValues (DoubleValue val1) (IntegerValue val2) = DoubleValue (val1 - fromIntegral val2)
subValues (IntegerValue val1) (DoubleValue val2) = DoubleValue (fromIntegral val1 - val2)
subValues g@(GlobalValue a) h@(GlobalValue b) = subValues (gVal g) (gVal h) 
subValues g@(GlobalValue a) b = subValues (gVal g) b 
subValues a h@(GlobalValue b) = subValues a (gVal h)

multValues :: Value -> Value -> Value
multValues (IntegerValue val1) (IntegerValue val2) = IntegerValue (val1 * val2)
multValues (DoubleValue val1) (DoubleValue val2) = DoubleValue (val1 * val2)
multValues (DoubleValue val1) (IntegerValue val2) = DoubleValue (val1 * fromIntegral val2)
multValues (IntegerValue val1) (DoubleValue val2) = DoubleValue (fromIntegral val1 * val2)
multValues g@(GlobalValue a) h@(GlobalValue b) = multValues (gVal g) (gVal h) 
multValues g@(GlobalValue a) b = multValues (gVal g) b 
multValues a h@(GlobalValue b) = multValues a (gVal h) 

divValues :: Value -> Value -> Value
divValues (IntegerValue val1) (IntegerValue val2) = IntegerValue (div val1 val2)
divValues g@(GlobalValue a) h@(GlobalValue b) = divValues (gVal g) (gVal h) 
divValues g@(GlobalValue a) b = divValues (gVal g) b 
divValues a h@(GlobalValue b) = divValues a (gVal h) 

isEqual :: Value -> Value -> Bool
isEqual (IntegerValue val1) (IntegerValue val2) = val1 == val2
isEqual (IntegerValue val1) (DoubleValue val2) = (fromIntegral val1) == val2
isEqual (DoubleValue val1) (DoubleValue val2) = val1 == val2
isEqual (DoubleValue val1) (IntegerValue val2) = val1 == (fromIntegral val2)
isEqual (StringValue val1) (StringValue val2) = val1 == val2
isEqual g@(GlobalValue a) h@(GlobalValue b) = isEqual (gVal g) (gVal h) 
isEqual g@(GlobalValue a) b = isEqual (gVal g) b 
isEqual a h@(GlobalValue b) = isEqual a (gVal h)

isNotEqual :: Value -> Value -> Bool
isNotEqual (IntegerValue val1) (IntegerValue val2) = val1 /= val2
isNotEqual (IntegerValue val1) (DoubleValue val2) = (fromIntegral val1) /= val2
isNotEqual (DoubleValue val1) (DoubleValue val2) = val1 /= val2
isNotEqual (DoubleValue val1) (IntegerValue val2) = val1 /= (fromIntegral val2)
isNotEqual (StringValue val1) (StringValue val2) = val1 /= val2
isNotEqual g@(GlobalValue a) h@(GlobalValue b) = isNotEqual (gVal g) (gVal h) 
isNotEqual g@(GlobalValue a) b = isNotEqual (gVal g) b 
isNotEqual a h@(GlobalValue b) = isNotEqual a (gVal h) 

isLess :: Value -> Value -> Bool
isLess (IntegerValue val1) (IntegerValue val2) = val1 < val2
isLess (IntegerValue val1) (DoubleValue val2) = (fromIntegral val1) < val2
isLess (DoubleValue val1) (DoubleValue val2) = val1 < val2
isLess (DoubleValue val1) (IntegerValue val2) = val1 < (fromIntegral val2)
isLess (StringValue val1) (StringValue val2) = val1 < val2
isLess g@(GlobalValue a) h@(GlobalValue b) = isLess (gVal g) (gVal h) 
isLess g@(GlobalValue a) b = isLess (gVal g) b 
isLess a h@(GlobalValue b) = isLess a (gVal h)  

isMore :: Value -> Value -> Bool
isMore (IntegerValue val1) (IntegerValue val2) = val1 > val2
isMore (IntegerValue val1) (DoubleValue val2) = (fromIntegral val1) > val2
isMore (DoubleValue val1) (DoubleValue val2) = val1 > val2
isMore (DoubleValue val1) (IntegerValue val2) = val1 > (fromIntegral val2)
isMore (StringValue val1) (StringValue val2) = val1 > val2
isMore g@(GlobalValue a) h@(GlobalValue b) = isMore (gVal g) (gVal h) 
isMore g@(GlobalValue a) b = isMore (gVal g) b 
isMore a h@(GlobalValue b) = isMore a (gVal h) 

isLessEqual :: Value -> Value -> Bool
isLessEqual (IntegerValue val1) (IntegerValue val2) = val1 <= val2
isLessEqual (IntegerValue val1) (DoubleValue val2) = (fromIntegral val1) <= val2
isLessEqual (DoubleValue val1) (DoubleValue val2) = val1 <= val2
isLessEqual (DoubleValue val1) (IntegerValue val2) = val1 <= (fromIntegral val2)
isLessEqual (StringValue val1) (StringValue val2) = val1 <= val2
isLessEqual g@(GlobalValue a) h@(GlobalValue b) = isLessEqual (gVal g) (gVal h) 
isLessEqual g@(GlobalValue a) b = isLessEqual (gVal g) b 
isLessEqual a h@(GlobalValue b) = isLessEqual a (gVal h)   

isMoreEqual :: Value -> Value -> Bool
isMoreEqual (IntegerValue val1) (IntegerValue val2) = val1 >= val2
isMoreEqual (IntegerValue val1) (DoubleValue val2) = (fromIntegral val1) >= val2
isMoreEqual (DoubleValue val1) (DoubleValue val2) = val1 >= val2
isMoreEqual (DoubleValue val1) (IntegerValue val2) = val1 >= (fromIntegral val2)
isMoreEqual (StringValue val1) (StringValue val2) = val1 >= val2
isMoreEqual g@(GlobalValue a) h@(GlobalValue b) = isMoreEqual (gVal g) (gVal h) 
isMoreEqual g@(GlobalValue a) b = isMoreEqual (gVal g) b 
isMoreEqual a h@(GlobalValue b) = isMoreEqual a (gVal h) 

startInterpret :: SymbolTable -> [Command] -> IO SymbolTable
startInterpret ts cs = do
  interpret ts $ Main cs

-- Interpreter
interpret :: SymbolTable -> Command -> IO SymbolTable
interpret ts (Vars) = return ts


interpret ts (GVarDec v) = do
  name <- return $ fst (v)
  val <- return $ snd (v)
  var <- return $ get ts name
  if (getType var) == "undeclared"
    then do
      let g = GlobalValue { gVal = val }
      return $ set ts name g
    else error "ERR: Multiple variable declaration"


-- Main block interpreter
interpret ts (Main []) = return ts
interpret ts (Main (c:cs)) = do
  ts' <- interpret ts c
  interpret ts' $ Main cs

-- Fucntion interpreter
interpret ts (Function id fp scope lts) = do
  var <- return $ get ts id
  if (getType var) == "function"
      then do
        let f = FunctionValue { ident = id, params = fp, scope = scope, lts = lts }
        return $ set ts id f
      else error $ "Function '" ++ id ++ "' is not declared"

interpret ts (FuncDeclare id fp) = do
  var <- return $ get ts id
  if (getType var) == "undeclared"
    then do
      let func = FunctionValue { ident=id, params=fp, scope=Empty, lts=[] }
      return $ set ts id func
    else error "Multiple function declaration"

-- All program interpreter
interpret ts (Program c) = do  
  interpret ts c

interpret ts (Empty) = return ts

interpret ts (Assign v e) = do

  let tt = [ x | x <- ts, (getType (snd x)) /= "function" ]

  var <- return $ get tt v
  if (getType var) == "undeclared"
    then error ("Cannot assign to undeclared variable" ++ show v )
  else if (getType var) == "return"
    then do
      val <- evaluate ts e
      res <- checkTypeAssignment var (fst val)
      let f = var {retVal=res}
      return $ set ts v f
  else do
    val <- evaluate ts e
    res <- checkTypeAssignment var (fst val)
    nts <- updateGlobalVariables ts (snd val)
    return $ set nts v res

interpret ts (Writeln (SConst s)) = do
  putStrLn s
  return ts

interpret ts (Writeln e) = do
  val <- evaluate ts e
  typ <- return $ getType (fst val)
  let v = if typ == "global"
            then gVal (fst val)
            else (fst val)

  typ <- return $ getType v
  
  if typ == "integer" 
    then putStrLn $ show $ intVal v
  else if typ == "double"
    then putStrLn $ show $ doubleVal v
  else if typ == "string"
    then putStrLn $ strVal v        
  else error "Bad output type for printing"
  
  nts <- updateGlobalVariables ts (snd val)
  return nts

interpret ts (Readln v) = do
  val <- return $ get ts v
  typ <- return $ getType val
  i <- getLine  -- nactu hodnotu ze vstupu
  let ntyp = if typ == "global"
              then getType (gVal val)
              else typ
  let res = if ntyp == "integer" 
              then IntegerValue (read i :: Integer)
            else if ntyp == "double"
              then DoubleValue (read i :: Double)
            else StringValue i
  if typ == "global" 
    then return $ set ts v GlobalValue { gVal = res }
    else return $ set ts v res

interpret ts (If cond c1 c2) = do
  c <- decide ts cond
  if c
    then interpret ts c1
    else interpret ts c2

interpret ts w@(While cond c) = do
  cnd <- decide ts cond
  if cnd
    then do
      ts' <- interpret ts c
      interpret ts' w
    else return ts

interpret ts (Seq []) = return ts
interpret ts (Seq (c:cs)) = do
  ts' <- interpret ts c
  interpret ts' $ Seq cs

updateGlobalVariables :: SymbolTable -> SymbolTable -> IO SymbolTable
updateGlobalVariables ts [] = return ts
updateGlobalVariables ts [(id, value)] = do
  ts' <- return $ set ts id value 
  return ts'
  
updateGlobalVariables ts ((id, value):xs) = do
  ts' <- return $ set ts id value   
  updateGlobalVariables ts' xs

-- Data structures
data Command = Empty
  | GVarDec Variable
  | Assign String Expr
  | Writeln Expr
  | Readln String
  | Seq [ Command ]
  | If BoolExpr Command Command
  | While BoolExpr Command 
  | Program Command
  | Vars
  | Function String [ Variable ] Command SymbolTable
  | FuncDeclare String [ Variable ]
  | Main [ Command ]
  | Assoc String
  deriving Show

data Expr = Const Value
  | SConst String
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | FunctionCall String [Expr]
  | Div Expr Expr
  deriving Show

data BoolExpr = Equal Expr Expr
  | NotEqual Expr Expr
  | Less Expr Expr
  | More Expr Expr
  | LessEqual Expr Expr
  | MoreEqual Expr Expr
  deriving Show

type SymbolTable = [(String, Value)]
type EvalReturn = (Value, SymbolTable)
type Variable = (String, Value)
data Value = IntegerValue { intVal :: Integer }
            | FunctionValue  {
                              ident :: String,
                              params :: [ Variable ],
                              scope :: Command,
                              lts :: SymbolTable
                            }
            | StringValue { strVal :: String }
            | DoubleValue { doubleVal :: Double}
            | RetValue { retVal :: Value }
            | GlobalValue { gVal :: Value }
            | Undeclared
            deriving Show

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