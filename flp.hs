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

-- Re-defining string literal
lexer = (P.makeTokenParser aelDef) {P.stringLiteral = mystringliteral}
mystringliteral = lexeme $ do   
    str <- between (char '\'') (char '\'' <?> "end of string") (many isThereToSingleQuotesInARow)
    return str    

lexeme      = P.lexeme lexer
whiteSpace  = P.whiteSpace lexer
integer     = P.integer lexer
parens      = P.parens lexer
semi        = P.semi lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
stringConst = P.stringLiteral lexer
naturalOrFloat= P.naturalOrFloat lexer

-- TOP-LEVEL PARSER (MAIN) --
aep = 
	do
	   whiteSpace
	   globalVariables <- globalVarParser
	   functions <- many functionDeclarationParser
	   mainBlock <- mainProgramBlockParser
	   eof
	   return $ [globalVariables] ++ functions ++ [mainBlock]
	<?> "ERROR: highest level parsing error"

-- Parsing global variables section
globalVarParser = 
	do
	    reserved "var"
	    x <- oneVar
	    xs <- many otherVars
	    semi
	    return $ Seq (x:xs)
	-- If declaration is empty return nothing
	<|> do
	    return Empty
	<?> "ERROR: declaration of global variables error"

-- Parsing one variable
oneVar = 
	do
	    id <- identifier
	    reservedOp ":"
	    typeOfVariable id

-- Get the type of variable		
typeOfVariable id =
	do
	    reserved "integer"
	    return $ GlovalVariableDeclaration (id, IntegerValue 0)
	<|> do
		reserved "double"
		return $ GlovalVariableDeclaration (id, DoubleValue 0.0)
    <|> do
		reserved "string"
		return $ GlovalVariableDeclaration (id, GlobalStringValue "")
    <?> "ERR: global or function varible type"

-- Many other variables separated by comma
otherVars = 
	do
    	reservedOp ","
    	oneVar

-- PARSING FUNCTIONS --

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
	    returnTypeOfFunction <- returnType id
	    semi
	    functionDefinitionParser id fp returnTypeOfFunction 
	<?> "ERROR: function declaration parsing error"

-- Define the return type of value
returnType id =
	do
	    reserved "integer"
	    return (id, ReturnValue { retVal = (IntegerValue 0) } )
	<|> do
	    reserved "double"
	    return (id, ReturnValue { retVal = (DoubleValue 0.0) } )
    <|> do
	    reserved "string"
	    return (id, ReturnValue { retVal = (GlobalStringValue "") } )
    <?> "ERR: bad function return type"  

-- Function parameters in function declaration
functionParameters = 
	do  
    	x  <- oneFunctionParameter
    	xs <- many multipleFunctionParameters
    	return $ (x:xs)
    <|> do                 
    	return []
    <?> "ERROR: Parameters in function definition error"

-- Parsing one parameter of function
oneFunctionParameter =
	do
    	id <- identifier
    	reservedOp ":"
    	parametrType id

-- Set the origin parameter's value	
parametrType id = 
  	do
    	reserved "integer"
    	return $ (id, IntegerValue 0)
  	<|> do
    	reserved "double"
    	return $ (id, DoubleValue 0.0)
  	<|> do
    	reserved "string"
    	return $ (id, GlobalStringValue "")
  	<?> "ERR: bad function, parameter or varible type"

-- Parsing many function parameters    
multipleFunctionParameters = 
  	do
    	reservedOp ","
    	oneFunctionParameter

-- Parsing function definition --
functionDefinitionParser id fp returnTypeOfFunction  = 
  	do
    	lv <- localVariables
    	mc <- multipleCmd
    	return $ Function id fp (Seq [mc]) (fp ++ lv ++ [returnTypeOfFunction])
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

-- Parsing one local variable     
oneLocalVariable =
  	do
    	id <- identifier
    	reservedOp ":"
    	parametrType id

-- Parsing multiple local variables separated with comma    
multipleLocalVariables = 
    do
    	reservedOp ","
    	oneLocalVariable

-- Commands parser inside blocks
mainProgramBlockParser =
	do
	   reserved "begin"
	   program <- multipleCommandsParser
	   reserved "end"
	   reservedOp "."
	   return $ Program program
    <?> "ERROR: parsing main bock error"

-- Parsing other commands, after first one
multipleCommandsParser = 
 	do
    	x  <- cmd
    	xs <- many manyCommands
    	return $ Seq (x:xs)
  	<|> do           
    	return $ Empty
	<?> "ERROR: Sequence of commands error"

-- Other commands, separeted with comma
manyCommands = 
	do
    	semi
    	cmd

-- Main commands parser (inside block between begin and end)
multipleCmd = 
  	do
    	reserved "begin"
    	seq <- multipleCommandsParser
    	reserved "end"  
    	return seq
  	<|> do
    	c <- cmd
    	return c
  	<?> "ERROR: many commands"

-- Parsing single commands
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
  	<?> "ERROR: error in commads inside block"

-- PARSING EXPRESSIONS
expr = buildExpressionParser operators term where
    operators = [[ op "*" Mult, op "div" Div ],[ op "+" Add, op "-" Sub ]]
    op name fun = Infix ( do { reservedOp name; return fun } ) AssocLeft

-- Single terms - for integer, double, sring and function or variable
term = 
	do
    	num <- naturalOrFloat;
		return $ (case num of { Right i -> Const $ DoubleValue i; Left  i -> Const $ IntegerValue (fromInteger i) })
  	<|> do
    	s <- mystringliteral
    	return $ SConst s
  	<|> try ( 
  		do
    		id <- identifier
    		reservedOp "("
    		ex <- callWithParameters
    		reservedOp ")"
    		return $ FunctionCall id ex ) 
 	<|> do
    	v <- identifier
    	return $ Var v
  	<|> parens expr
  	<?> "ERROR: error in terms"

callWithParameters = 
	do
    	x  <- expr 
    	xs <- many expressionsSequence
    	return $ (x:xs)
  	<|> do
    return []

expressionsSequence = 
  	do
    	reservedOp ","
    	expr

boolExpr = 
	do
    	e1 <- expr
    	o <- relationOp
    	e2 <- expr
    	return $ o e1 e2
  	<?> "ERROR: error in boolean expressions"
  	where
    	relationOp = relationOp' "=" Equal
      		<|> relationOp' "<>" NotEqual
      		<|> relationOp' "<" Less
      		<|> relationOp' ">" More
      		<|> relationOp' "<=" LessEqual
      		<|> relationOp' ">=" MoreEqual
      		<?> "ERROR: error in relations in bool"
    	relationOp' name funOp = do
      		reservedOp name
      		return funOp

-- Get types of values
getType :: Value -> String
getType val = case val of 
	IntegerValue x -> "integer"
	DoubleValue x -> "double"
	GlobalStringValue x -> "string"
	FunctionValue w x y z -> "function"
	ReturnValue x -> "return"
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
isThereToSingleQuotesInARow :: Parser Char
isThereToSingleQuotesInARow = try (string "''" >> return '\'') 
	<|> do 
		c <- noneOf "'"
		return $ c

-- Update global variables, after Assign or Writeln command
updatingOfGlobalVars :: SymbolTable -> SymbolTable -> IO SymbolTable
updatingOfGlobalVars ts [] = return ts
updatingOfGlobalVars ts [(id, value)] = 
	do
		ts' <- return $ set ts id value 
		return ts'
  
updatingOfGlobalVars ts ((id, value):xs) = 
	do
		ts' <- return $ set ts id value   
		updatingOfGlobalVars ts' xs
   
-- Evaluating expressions
evaluate :: SymbolTable -> Expr -> IO EvalReturn
evaluate ts (Const i) = return $ (i, [])
evaluate ts (Var v) = return $ ((get ts v), [])
evaluate ts (Add e1 e2) = 
	do
  		eval1 <- evaluate ts e1
  		eval2 <- evaluate ts e2
  		return $ ((addValues (fst eval1) (fst eval2)), [])
evaluate ts (Sub e1 e2) = 
	do
  		eval1 <- evaluate ts e1
  		eval2 <- evaluate ts e2
  		return $ ((subValues (fst eval1) (fst eval2)), [])
evaluate ts (Mult e1 e2) = 
	do
  		eval1 <- evaluate ts e1
  		eval2 <- evaluate ts e2
  		return $ ((multValues (fst eval1) (fst eval2)), [])
evaluate ts (Div e1 e2) = 
	do
  		eval1 <- evaluate ts e1
  		eval2 <- evaluate ts e2
  		return $ ((divValues (fst eval1) (fst eval2)), [])
-- Evaluating of calling of the function
evaluate ts (FunctionCall id prs) = 
	do
		-- Put function parameters
		globParams  <- return prs
		-- Put function name
  		funName     <- return $ get ts id
  		-- Put paramaters for function in funName
  		funParams   <- return $ params funName
  		-- Put scope of funName function
  		funScope    <- return $ scope funName
  		-- Put number of function's parameters
  		numOfParams <- return $ length funParams
  		-- Put number of parameters
  		givenParams <- return $ length globParams

  		-- Check, if number of parameters that function is waiting
  		-- is the same with number of parameters given
  		if numOfParams /= givenParams
  			
  			-- if not - return error
	    	then do error $ "ERROR: different count of parameters in function" ++ id ++ "'"
	   	 	
	   	 	-- else - call function computeParameters with symbol table
	   	 	else do
	        values <- computeParameters ts globParams

	        -- Put list with new parameters
	        lts   <- return $ lts funName
	        lts'  <- setValuesOfParameters (lts++ts) funParams values

	        -- Get type with "function"
	        let functionTable =  [ x | x <- ts, (getType (snd x)) == "function" ]
	        let newTS = functionTable ++ lts'

	        newTS' <- interpret (newTS) funScope

	        -- Get type with "return"
	        let returns = [ x | x <- newTS', (getType (snd x)) == "return" ]

	        -- Get type with "global"
	        let globals  = [ x | x <- newTS', (getType (snd x)) == "global" ]

	        i <- return $ get returns id
	        return $ ((retVal i), globals)

-- Computing parameters of function
computeParameters :: SymbolTable -> [Expr] -> IO [Value]
computeParameters [] [] = return []
computeParameters ts [] = return []
computeParameters ts [x] = 
	do
  		value <- evaluate ts x
  		return $ [ fst value ]
  
computeParameters ts (x:xs) = 
	do	
  		val <- evaluate ts x
  		ss <- computeParameters ts xs
  		return $ (fst val) : ss

-- Setting values of parameters and putting them to symbol table
setValuesOfParameters :: SymbolTable -> [Variable] -> [Value] -> IO SymbolTable
setValuesOfParameters ts [] [] = return ts
setValuesOfParameters ts ((id, value):xs) (y:ys) = 
	do
		-- Check type before return
  		val <- checkTypeOfAssign value y
  		ts' <- return $ set ts id val   
  		setValuesOfParameters ts' xs ys

-- CHECKING TYPE WHEN ASSIGNING --
checkTypeOfAssign :: Value -> Value -> IO Value

-- for integer
checkTypeOfAssign (IntegerValue x) (IntegerValue y) = return $ IntegerValue y 
-- for double
checkTypeOfAssign (DoubleValue x) (DoubleValue y) = return $ DoubleValue y
-- for string in global
checkTypeOfAssign (GlobalStringValue x) (GlobalStringValue y) = return $ GlobalStringValue y 
-- for integer comparing with double
checkTypeOfAssign (DoubleValue x) (IntegerValue y) = return $ DoubleValue $ fromIntegral y
checkTypeOfAssign r@(ReturnValue x) y = checkTypeOfAssign (retVal r) y

-- Checking types of global values
checkTypeOfAssign g@(GlobalValue x) h@(GlobalValue y) = checkTypeOfAssign g (globalValue h)
checkTypeOfAssign g@(GlobalValue x) y = 
	do
	  	var <- return $ globalValue g
	  	val <- checkTypeOfAssign var y
	  	return $ GlobalValue val
  
checkTypeOfAssign x h@(GlobalValue y) = 
	do 
 		var <- return $ globalValue h
  		val <- checkTypeOfAssign x var
  		return val
  
checkTypeOfAssign (IntegerValue x) y = error "Types are incompatible for assignment"
checkTypeOfAssign (DoubleValue x) y = error "Types are incompatible for assignment"
checkTypeOfAssign (GlobalStringValue x) y = error "Types are incompatible for assignment"

-- EVALUATING BOOL EXPRESSIONS --
decide :: SymbolTable -> BoolExpr -> IO Bool
decide ts (Equal x y) = 
	do
  		eval1 <- evaluate ts x
  		eval2 <- evaluate ts y
  		return $ isEqual (fst eval1) (fst eval2)
decide ts (NotEqual x y) = 
	do
  		eval1 <- evaluate ts x
  		eval2 <- evaluate ts y
  		return $ isNotEqual (fst eval1) (fst eval2)
decide ts (Less x y) = 
	do
  		eval1 <- evaluate ts x
  		eval2 <- evaluate ts y
  		return $ isLess (fst eval1) (fst eval2)
decide ts (More x y) = 
	do
  		eval1 <- evaluate ts x
  		eval2 <- evaluate ts y
  		return $ isMore (fst eval1) (fst eval2)
decide ts (LessEqual x y) = 
	do
  		eval1 <- evaluate ts x
  		eval2 <- evaluate ts y
  		return $ isLessEqual (fst eval1) (fst eval2)
decide ts (MoreEqual x y) = 
	do
  		eval1 <- evaluate ts x
  		eval2 <- evaluate ts y
  		return $ isMoreEqual (fst eval1) (fst eval2)

-- CALCULATING OF EXPRESSIONS --
-- +
addValues :: Value -> Value -> Value
addValues (IntegerValue val1) (IntegerValue val2) = IntegerValue (val1 + val2)
addValues (DoubleValue val1) (DoubleValue val2) = DoubleValue (val1 + val2)
addValues (DoubleValue val1) (IntegerValue val2) = DoubleValue (val1 + fromIntegral val2)
addValues (IntegerValue val1) (DoubleValue val2) = DoubleValue (fromIntegral val1 + val2)
addValues (GlobalStringValue val1) (GlobalStringValue val2) = GlobalStringValue (val1 ++ val2)
addValues x@(GlobalValue val1) y@(GlobalValue val2) = addValues (globalValue x) (globalValue y) 
addValues x@(GlobalValue val1) val2 = addValues (globalValue x) val2 
addValues val1 y@(GlobalValue val2) = addValues val1 (globalValue y)

-- -
subValues :: Value -> Value -> Value
subValues (IntegerValue val1) (IntegerValue val2) = IntegerValue (val1 - val2)
subValues (DoubleValue val1) (DoubleValue val2) = DoubleValue (val1 - val2)
subValues (DoubleValue val1) (IntegerValue val2) = DoubleValue (val1 - fromIntegral val2)
subValues (IntegerValue val1) (DoubleValue val2) = DoubleValue (fromIntegral val1 - val2)
subValues x@(GlobalValue val1) y@(GlobalValue val2) = subValues (globalValue x) (globalValue y) 
subValues x@(GlobalValue val1) val2 = subValues (globalValue x) val2 
subValues val1 y@(GlobalValue val2) = subValues val1 (globalValue y)

-- *
multValues :: Value -> Value -> Value
multValues (IntegerValue val1) (IntegerValue val2) = IntegerValue (val1 * val2)
multValues (DoubleValue val1) (DoubleValue val2) = DoubleValue (val1 * val2)
multValues (DoubleValue val1) (IntegerValue val2) = DoubleValue (val1 * fromIntegral val2)
multValues (IntegerValue val1) (DoubleValue val2) = DoubleValue (fromIntegral val1 * val2)
multValues x@(GlobalValue val1) y@(GlobalValue val2) = multValues (globalValue x) (globalValue y) 
multValues x@(GlobalValue val1) val2 = multValues (globalValue x) val2 
multValues val1 y@(GlobalValue val2) = multValues val1 (globalValue y)

-- /
divValues :: Value -> Value -> Value
divValues (IntegerValue val1) (IntegerValue val2) = IntegerValue (div val1 val2)
divValues x@(GlobalValue val1) y@(GlobalValue val2) = divValues (globalValue x) (globalValue y) 
divValues x@(GlobalValue val1) val2 = divValues (globalValue x) val2 
divValues val1 y@(GlobalValue val2) = divValues val1 (globalValue y)

isEqual :: Value -> Value -> Bool
isEqual (IntegerValue val1) (IntegerValue val2) = val1 == val2
isEqual (IntegerValue val1) (DoubleValue val2) = (fromIntegral val1) == val2
isEqual (DoubleValue val1) (DoubleValue val2) = val1 == val2
isEqual (DoubleValue val1) (IntegerValue val2) = val1 == (fromIntegral val2)
isEqual (GlobalStringValue val1) (GlobalStringValue val2) = val1 == val2
isEqual x@(GlobalValue val1) y@(GlobalValue val2) = isEqual (globalValue x) (globalValue y) 
isEqual x@(GlobalValue val1) val2 = isEqual (globalValue x) val2 
isEqual val1 y@(GlobalValue val2) = isEqual val1 (globalValue y)

-- !=
isNotEqual :: Value -> Value -> Bool
isNotEqual (IntegerValue val1) (IntegerValue val2) = val1 /= val2
isNotEqual (IntegerValue val1) (DoubleValue val2) = (fromIntegral val1) /= val2
isNotEqual (DoubleValue val1) (DoubleValue val2) = val1 /= val2
isNotEqual (DoubleValue val1) (IntegerValue val2) = val1 /= (fromIntegral val2)
isNotEqual (GlobalStringValue val1) (GlobalStringValue val2) = val1 /= val2
isNotEqual x@(GlobalValue val1) y@(GlobalValue val2) = isNotEqual (globalValue x) (globalValue y) 
isNotEqual x@(GlobalValue val1) val2 = isNotEqual (globalValue x) val2 
isNotEqual val1 y@(GlobalValue val2) = isNotEqual val1 (globalValue y)

-- <
isLess :: Value -> Value -> Bool
isLess (IntegerValue val1) (IntegerValue val2) = val1 < val2
isLess (IntegerValue val1) (DoubleValue val2) = (fromIntegral val1) < val2
isLess (DoubleValue val1) (DoubleValue val2) = val1 < val2
isLess (DoubleValue val1) (IntegerValue val2) = val1 < (fromIntegral val2)
isLess (GlobalStringValue val1) (GlobalStringValue val2) = val1 < val2
isLess x@(GlobalValue val1) y@(GlobalValue val2) = isLess (globalValue x) (globalValue y) 
isLess x@(GlobalValue val1) val2 = isLess (globalValue x) val2 
isLess val1 y@(GlobalValue val2) = isLess val1 (globalValue y) 

-- >
isMore :: Value -> Value -> Bool
isMore (IntegerValue val1) (IntegerValue val2) = val1 > val2
isMore (IntegerValue val1) (DoubleValue val2) = (fromIntegral val1) > val2
isMore (DoubleValue val1) (DoubleValue val2) = val1 > val2
isMore (DoubleValue val1) (IntegerValue val2) = val1 > (fromIntegral val2)
isMore (GlobalStringValue val1) (GlobalStringValue val2) = val1 > val2
isMore x@(GlobalValue val1) y@(GlobalValue val2) = isMore (globalValue x) (globalValue y) 
isMore x@(GlobalValue val1) val2 = isMore (globalValue x) val2 
isMore val1 y@(GlobalValue val2) = isMore val1 (globalValue y)

-- <=
isLessEqual :: Value -> Value -> Bool
isLessEqual (IntegerValue val1) (IntegerValue val2) = val1 <= val2
isLessEqual (IntegerValue val1) (DoubleValue val2) = (fromIntegral val1) <= val2
isLessEqual (DoubleValue val1) (DoubleValue val2) = val1 <= val2
isLessEqual (DoubleValue val1) (IntegerValue val2) = val1 <= (fromIntegral val2)
isLessEqual (GlobalStringValue val1) (GlobalStringValue val2) = val1 <= val2
isLessEqual x@(GlobalValue val1) y@(GlobalValue val2) = isLessEqual (globalValue x) (globalValue y) 
isLessEqual x@(GlobalValue val1) val2 = isLessEqual (globalValue x) val2 
isLessEqual val1 y@(GlobalValue val2) = isLessEqual val1 (globalValue y) 

-- >=
isMoreEqual :: Value -> Value -> Bool
isMoreEqual (IntegerValue val1) (IntegerValue val2) = val1 >= val2
isMoreEqual (IntegerValue val1) (DoubleValue val2) = (fromIntegral val1) >= val2
isMoreEqual (DoubleValue val1) (DoubleValue val2) = val1 >= val2
isMoreEqual (DoubleValue val1) (IntegerValue val2) = val1 >= (fromIntegral val2)
isMoreEqual (GlobalStringValue val1) (GlobalStringValue val2) = val1 >= val2
isMoreEqual x@(GlobalValue val1) y@(GlobalValue val2) = isMoreEqual (globalValue x) (globalValue y) 
isMoreEqual x@(GlobalValue val1) val2 = isMoreEqual (globalValue x) val2 
isMoreEqual val1 y@(GlobalValue val2) = isMoreEqual val1 (globalValue y)

-- START OF INTERPRETING --
startInterpret :: SymbolTable -> [Command] -> IO SymbolTable
startInterpret ts cs = 
	do
		interpret ts $ Main cs

-- INTERPRETER --
interpret :: SymbolTable -> Command -> IO SymbolTable
-- Declaration of gloval variables
interpret ts (GlovalVariableDeclaration glvar) = 
	do
		-- Get name of variable
  		name <- return $ fst (glvar)

  		-- Get value of variable
  		value <- return $ snd (glvar)

  		-- Return it's name from symbol table
  		variable <- return $ get ts name

  		-- If it's not declated yet - declare it with value
  		if (getType variable) == "undeclared" then do
			let glob = GlobalValue { globalValue = value }
			return $ set ts name glob
		-- If already declared - put error
  		else error "ERROR: Multiple variable declaration"


-- Main block interpreter
interpret ts (Main []) = return ts
interpret ts (Main (c:cs)) = 
	do
  		ts' <- interpret ts c
  		interpret ts' $ Main cs

-- Function definition interpreter
interpret ts (Function id fp scope lts) = 
	do
  		var <- return $ get ts id
  		if (getType var) == "function" then do
        	let f = FunctionValue { ident = id, params = fp, scope = scope, lts = lts }
        	return $ set ts id f
      		else error $ "Function '" ++ id ++ "' is not declared"

-- Function declaration interpreter
interpret ts (FuncDeclare id fp) = 
	do
  		var <- return $ get ts id
  		if (getType var) == "undeclared" then do
      		let func = FunctionValue { ident=id, params=fp, scope=Empty, lts=[] }
      		return $ set ts id func
    		else error "Multiple function declaration"

-- All program interpreter
interpret ts (Program c) = 
	do  
  		interpret ts c

-- Empty command
interpret ts (Empty) = return ts

-- Assigning values command
interpret ts (Assign v e) = 
	do
		-- Get list of variables from symbol table,
		-- and check for in won't be a function
		let lov = [ x | x <- ts, (getType (snd x)) /= "function" ]

		variable <- return $ get lov v

		-- Put error if variable is undeclared
		if (getType variable) == "undeclared" then error ("ERROR: You are trying to assign to undeclared variable" ++ show v )
		
		-- If type of variable is return - get from symbol table names
		-- then, check types and put value to name
		else if (getType variable) == "return" 
			then do
				value <- evaluate ts e
				result <- checkTypeOfAssign variable (fst value)
				let f = variable {retVal=result}
				return $ set ts v f
			else do
				value <- evaluate ts e
				result <- checkTypeOfAssign variable (fst value)
				updatedTS <- updatingOfGlobalVars ts (snd value)
				return $ set updatedTS v result

-- Writeln command (put string constant)
interpret ts (Writeln (SConst s)) = 
	do
  		putStrLn s
  		return ts

-- Writeln command (put value)
interpret ts (Writeln wl) = 
	do
		-- Get value from symbol table
		value <- evaluate ts wl

		-- Get type of that value
		typeofValue <- return $ getType (fst value)

		-- If it is global - get global value, else - local
		let ofVal = if typeofValue == "global" then globalValue (fst value)
		        	else (fst value)

		typeofValue <- return $ getType ofVal

		-- Checking type of value and returnig function depending on that
		if typeofValue 		== "integer" then putStrLn $ show $ intVal ofVal
		else if typeofValue == "double"  then putStrLn $ show $ doubleVal ofVal
		else if typeofValue == "string"  then putStrLn $ strVal ofVal
		-- If no type founded - error

		else error "Bad output type for 'Writeln'"

		-- Updating symbol table
		updatedTS <- updatingOfGlobalVars ts (snd value)
		return updatedTS

-- Command reading from input
interpret ts (Readln v) = 
	do
		-- Get from symbol table value and put into "value"
		value <- return $ get ts v
		
		-- Get type of that value
		typeofValue <- return $ getType value
		
		-- Get line from input and put it to the "line"
		line <- getLine
		-- Checking, what type is that, global or local
		let typeVal = if typeofValue == "global" then getType (globalValue value) else typeofValue
		let result = if typeVal == "integer" then IntegerValue (read line :: Integer)
		        else if typeVal == "double"  then DoubleValue  (read line :: Double)
		        else GlobalStringValue line
		
		-- If in is gloval - set it to table symbol as global, else - local
		if typeofValue == "global" then return $ set ts v GlobalValue { globalValue = result }
		else return $ set ts v result

-- IF statement command (IF - THEN - ELSE)
interpret ts (If cond x y) = 
	do
  		decideRes <- decide ts cond
  		if decideRes then interpret ts x
			else interpret ts y

-- While cyclus command 
interpret ts w@(While cond x) = 
	do
		condition <- decide ts cond
		if condition then do
 			ts' <- interpret ts x
			interpret ts' w
			else return ts

-- Sequence
interpret ts (Seq []) = return ts
interpret ts (Seq (x:xs)) = 
	do
		ts' <- interpret ts x
		interpret ts' $ Seq xs

-- Data structures
data Command = Empty
	| GlovalVariableDeclaration Variable
	| Assign String Expr
	| Writeln Expr
	| Readln String
	| Seq [ Command ]
	| If BoolExpr Command Command
	| While BoolExpr Command 
	| Program Command
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

data Value = IntegerValue { intVal :: Integer }
	| DoubleValue { doubleVal :: Double}
	| GlobalStringValue { strVal :: String }
	| FunctionValue { ident :: String, params :: [ Variable ], scope :: Command,lts :: SymbolTable}
	| ReturnValue { retVal :: Value }
	| GlobalValue { globalValue :: Value }
	| Undeclared
	deriving Show

-- Types definition
type SymbolTable = [(String, Value)]
type EvalReturn = (Value, SymbolTable)
type Variable = (String, Value)

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