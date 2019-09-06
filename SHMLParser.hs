module SHMLParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Data Structures
data Formula = LVar String
             | TT
             | FF
             | Con Formula Formula
             | Max String Formula
             | Nec Patt BExpr Formula
             deriving Eq

data Patt = Input Var AExpr
          | Output Var AExpr
          deriving Eq

data Var  = BVar String
          | FVar String
          deriving Eq

data AExpr = AVar Var
           | IntConst Integer
           | Neg AExpr
           | ABin ABinOp AExpr AExpr
           deriving Eq

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
            deriving Eq

data BExpr = BoolConst Bool
           | Not BExpr
           | And BExpr BExpr
           | RBin RBinOp AExpr AExpr
           deriving Eq

data RBinOp = Eq
            | Neq
            | Lt 
            | Gt 
            | LtEq 
            | GtEq
            deriving Eq

-- Language Definition
lang :: LanguageDef st
lang =
    emptyDef{ Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.opStart         = oneOf "&~+-*/<>=!?$"
            , Token.opLetter        = oneOf "&~+-*/<>=!?$"
            , Token.reservedOpNames = ["&", "~", "+", "-", "*", "/", "<", ">",
                                       "<=", ">=", "==", "!=", ".", ",", "!", 
                                       "?", "$"]
            , Token.reservedNames   = ["tt", "ff", "max"]
            }


-- Lexer for langauge
lexer = 
    Token.makeTokenParser lang


-- Trivial Parsers
identifier     = Token.identifier lexer
keyword        = Token.reserved lexer
op             = Token.reservedOp lexer
integer        = Token.integer lexer
roundBrackets  = Token.parens lexer
squareBrackets = Token.brackets lexer
whiteSpace     = Token.whiteSpace lexer

-- Main Parser, takes care of trailing whitespaces
formulaParser :: Parser Formula
formulaParser = whiteSpace >> formula

-- Parsing Formulas
formula :: Parser Formula
formula = conFormula
        <|> formulaTerm

-- Conjunction
conFormula :: Parser Formula
conFormula = 
    buildExpressionParser [[Infix (op "&" >> return Con) AssocLeft]] formulaTerm

-- Term in a Formula
formulaTerm :: Parser Formula
formulaTerm = roundBrackets formula
            <|> maxFormula
            <|> necFormula
            <|> ttFormula
            <|> ffFormula
            <|> lvFormula

-- Truth
ttFormula :: Parser Formula
ttFormula = keyword "tt" >> return TT

-- Falsehood
ffFormula :: Parser Formula
ffFormula = keyword "ff" >> return FF

-- Logical Variable
lvFormula :: Parser Formula
lvFormula =
    do  v <- identifier
        return $ LVar v

-- Least Fixed Point 
maxFormula :: Parser Formula
maxFormula = 
    do  keyword "max"
        x <- identifier
        op "."
        phi <- formulaTerm
        return $ Max x phi

-- Necessity 
necFormula :: Parser Formula
necFormula = try condNecFormula
           <|> simpleNecFormula
    
-- Necessity with condition
condNecFormula :: Parser Formula
condNecFormula = 
    do  (p,c) <- squareBrackets condpatt
        phi   <- formulaTerm
        return $ Nec p c phi

-- Inside of conditional pattern
condpatt :: Parser (Patt, BExpr)
condpatt = 
    do  p <- pattern
        op ","
        c <- bExpression
        return (p,c)

-- Necessity without condition
simpleNecFormula :: Parser Formula
simpleNecFormula = 
    do  p <- squareBrackets pattern
        phi <- formulaTerm
        return $ Nec p (BoolConst True) phi

-- Variable
var :: Parser Var
var = bvar <|> fvar

-- Free Variable
fvar :: Parser Var
fvar = 
    do  v <- identifier
        return $ FVar v

-- Bound Variable
bvar :: Parser Var
bvar = 
    do  op "$"
        v <- identifier
        return $ BVar v

-- Pattern
pattern :: Parser Patt
pattern = try inputPattern
        <|> outputPattern

-- Input pattern
inputPattern :: Parser Patt
inputPattern = 
    do  v <- var
        op "?"
        a <- aExpression
        return $ Input v a

-- Output pattern
outputPattern :: Parser Patt
outputPattern = 
    do  v <- var
        op "!"
        a <- aExpression
        return $ Output v a

-- Arithmetic Expressions
aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

aOperators = [ [Prefix (op "-" >> return (Neg          ))          ]
             , [Infix  (op "*" >> return (ABin Multiply)) AssocLeft,
                Infix  (op "/" >> return (ABin Divide  )) AssocLeft]
             , [Infix  (op "+" >> return (ABin Add     )) AssocLeft,
                Infix  (op "-" >> return (ABin Subtract)) AssocLeft]
             ]

aTerm :: Parser AExpr
aTerm = roundBrackets aExpression
      <|> liftM AVar var
      <|> liftM IntConst integer


-- Boolean Expressions
bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

bOperators = [ [ Prefix (op "~" >> return Not)          ]
             , [ Infix  (op "&" >> return And) AssocLeft]
             ]

bTerm :: Parser BExpr
bTerm = roundBrackets bTerm
      <|> (keyword "tt" >> return (BoolConst True))
      <|> (keyword "ff" >> return (BoolConst False))
      <|> rExpression


-- Relational Expressions
rExpression :: Parser BExpr
rExpression = 
    do  a1 <- aExpression
        rel <- relation
        a2 <- aExpression
        return $ RBin rel a1 a2

relation :: Parser RBinOp
relation = (op "==" >> return Eq)
         <|> (op "!=" >> return Neq)
         <|> (op "<"  >> return Lt)
         <|> (op ">"  >> return Gt)
         <|> (op "<=" >> return LtEq)
         <|> (op ">=" >> return GtEq)


-- Parse String Input
parseF :: String -> Formula
parseF s = 
    case ret of
        Left e -> LVar "ErrorParsing"
        Right f -> f
    where 
        ret = parse formulaParser "" s


-- Pretty Outputs (Parse tree)
indent :: Int -> String
indent 0 = "  "
indent 1 = "  └─ "
indent n = "   " ++ indent (n-1)

prettyf :: Formula -> Int -> String
prettyf f n = (indent n) ++ pf
    where 
        pf = 
            case f of
                LVar s -> s ++ " (logical variable)\n"
                TT -> "TT\n"
                FF -> "FF\n"
                Con phi psi -> "∧\n" ++ prettyf phi (n+1)
                               ++ prettyf psi (n+1)
                Max x phi   -> "max " ++ x ++ " .\n" 
                               ++ prettyf phi (n+1)
                Nec p c phi -> "Necessity\n"
                               ++ prettyp p (n+1)
                               ++ prettyb c (n+1)
                               ++ prettyf phi (n+1)

prettyp :: Patt -> Int -> String
prettyp p n =
    case p of
        Input v a   -> (indent n) ++ "Input\n"
                      ++ prettyv v (n+1) ++ "\n"
                      ++ prettya a (n+1)
        Output v a  -> (indent n) ++ "Output\n"
                      ++ prettyv v (n+1) ++ "\n"
                      ++ prettya a (n+1)

prettyv :: Var -> Int -> String
prettyv v n =
    case v of
        BVar v -> (indent n) ++ v ++ " (binding variable)"
        FVar v -> (indent n) ++ v ++ " (free variable)" 


prettya :: AExpr -> Int -> String
prettya a n =
            case a of
                AVar v -> prettyv v n ++ "\n"
                IntConst i -> (indent n) ++ (show i) ++ " (int const)\n"
                Neg a1 ->(indent n) ++ "Negation (-)\n" 
                          ++ prettya a1 (n+1)
                ABin binop a1 a2 -> (indent n) ++ sbinop ++ "\n"
                                    ++ prettya a1 (n+1)
                                    ++ prettya a2 (n+1)
                    where
                        sbinop =
                            case binop of
                                Add -> "+"
                                Subtract -> "-"
                                Multiply -> "×"
                                Divide -> "÷"

prettyb :: BExpr -> Int -> String
prettyb b n = (indent n) ++ pb
    where
        pb =
            case b of
                BoolConst bc -> (show bc) ++ " (bool const)\n"
                Not b1 -> "Negation (¬)\n" 
                          ++ prettyb b1 (n+1)
                And b1 b2 -> "∧\n" ++ prettyb b1 (n+1) 
                             ++ prettyb b2 (n+1)
                RBin rbinop a1 a2 -> sbinop ++ "\n"
                                    ++ prettya a1 (n+1)
                                    ++ prettya a2 (n+1)
                    where
                        sbinop =
                            case rbinop of
                                Eq -> "≟"
                                Neq -> "≠"
                                Lt -> "<"
                                Gt -> ">"
                                LtEq -> "≤"
                                GtEq -> "≥"


-- Output Parse Tree of a given Formula
parseTree :: Formula -> IO ()
parseTree f = putStrLn (prettyf f 0)

-- String to Parse Tree
stringParseTree :: String -> IO ()
stringParseTree s = 
    case ret of
        Left e ->  putStrLn $ "Error: " ++ (show e)
        Right f -> putStrLn $ "Interpreted as:\n" ++ (prettyf f 0)
    where 
        ret = parse formulaParser "" s


-- Normal output (formula)
instance Show Formula where
    showsPrec _ TT = showString "tt"
    showsPrec _ FF = showString "ff"
    showsPrec _ (LVar v) = showString v
    showsPrec p (Con f1 f2) = 
        showParen (p >= 2) $ (showsPrec 2 f1) . (" ∧ " ++) . showsPrec 2 f2
    showsPrec p (Max x f) = 
        showParen (p >= 3) $ (("max " ++ x ++ " . ") ++) . showsPrec 3 f
    showsPrec p (Nec pt c f) = 
        case c of
            BoolConst True -> 
                showParen (p >= 4) $ (("[" ++ show pt ++ "]") ++) . showsPrec 4 f
            _ -> 
                showParen (p >= 4) $ (("[" ++ show pt ++"," ++ show c ++ "]") ++) . showsPrec 4 f

instance Show Patt where
    show (Input v a) = (show v) ++ " ? " ++ (show a)
    show (Output v a) = (show v) ++ " ! " ++ (show a)

instance Show Var where
    show (FVar v) = v
    show (BVar v) = "$" ++ v

instance Show AExpr where
    showsPrec _ (AVar v) = shows v
    showsPrec _ (IntConst i) = shows i
    showsPrec p (ABin op a1 a2) = 
        case op of
            Add ->
                showParen (p >= 5) $ (showsPrec 5 a1) . (" + " ++) . showsPrec 5 a2
            Subtract ->
                showParen (p >= 5) $ (showsPrec 5 a1) . (" - " ++) . showsPrec 5 a2
            Multiply ->
                showParen (p >= 6) $ (showsPrec 6 a1) . (" × " ++) . showsPrec 6 a2
            Divide ->
                showParen (p >= 6) $ (showsPrec 6 a1) . (" ÷ " ++) . showsPrec 6 a2

instance Show BExpr where
    showsPrec _ (BoolConst b) = shows b
    showsPrec _ (Not b) = ("¬" ++) . (shows b)
    showsPrec p (And b1 b2) = (shows b1) . (" ∧ " ++) . (shows b2)
    showsPrec p (RBin op b1 b2) = 
        case op of
            Eq -> 
                (shows b1) . (" ≟ " ++) . (shows b2)
            Neq -> 
                (shows b1) . (" ≠ " ++) . (shows b2)
            Lt -> 
                (shows b1) . (" < " ++) . (shows b2)
            Gt -> 
                (shows b1) . (" > " ++) . (shows b2)
            LtEq -> 
                (shows b1) . (" ≤ " ++) . (shows b2)
            GtEq -> 
                (shows b1) . (" ≥ " ++) . (shows b2)
