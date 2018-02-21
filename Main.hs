module Main where

import Prelude hiding (lookup)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap, fromList, lookup, insert, union, empty)

import Data.List (intercalate, foldl', foldl1', foldr1)

--- Datatypes
--- ---------

--- ### Environments

type Env = HashMap String Val

--- ### Expressions

data Exp = IntExp Integer
         | SymExp String
         | SExp   [Exp]
         deriving (Show, Eq)

--- ### Values

data Val = IntVal Integer
          | SymVal String
          | ExnVal String
          | PrimVal ([Val] -> Val)
          | Closure [String] Exp Env
          | DefVal String Val
          | ConsVal Val Val
          | Macro [String] Exp Env

instance Show Val where
    show (IntVal i)           = show i
    show (SymVal s)           = s
    show (ExnVal s)           = "*** Scheme-Exception: " ++ s ++ " ***"
    show (PrimVal f)          = "*primitive*"
    show (Closure [ss] ex en) = "*closure*"
    show (DefVal name v)      = name
    show (Macro [ss] ex en)   = "*macro*"
    show (ConsVal v1 v2) = "(" ++ (intercalate " " $ aux v1 v2 []) ++ ")"
      where aux v1 (SymVal "nil")  acc = reverse $ (show v1 ++ " "):acc
            aux v1 (ConsVal v3 v4) acc = aux v3 v4 $ (show v1):acc
            aux v1 v2              acc = reverse $ (show v2):".":(show v1):acc

--- Parsing
--- -------

type Parser = ParsecT String () Identity

parseWith :: Parser a -> String -> Either ParseError a
parseWith parser input = parse parser "" input

--- ### Lexicals

adigit :: Parser Char
adigit = oneOf ['0'..'9']

digits :: Parser String
digits = many1 adigit

--- #### Whitespace parser

whitespace :: Parser String
whitespace = many $ oneOf " \n\t"

--- #### Identifier parser

identFirst :: Parser Char
identFirst = oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ "-*+/:'?><=!"

identRest :: Parser Char
identRest = oneOf $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "-*+/:'?><=!"

identifier :: Parser String
identifier = do
             f <- identFirst
             r <- many identRest
             whitespace
             return $ f : r

--- ### Grammaticals

anInt :: Parser Exp
anInt = do d <- digits
           return $ IntExp (read d)

--- #### Parsing symbols

aSym :: Parser Exp
aSym = do s <- identifier
          return $ SymExp s

--- #### Parsing forms

aForm :: Parser Exp
aForm = do
        oneOf ['(']
        r <- many anExp
        oneOf [')']
        return $ SExp r

--- #### Quotes, Quasi-Quotes, and UnQuotes
--- Defined in terms of mkQuote

aQuote :: Parser Exp
aQuote = do
         mkQuote '\'' "quote"

aQQuote :: Parser Exp
aQQuote = do
          mkQuote '`' "quasiquote"

anUnquote :: Parser Exp
anUnquote = do
            mkQuote ',' "unquote"

mkQuote :: Char -> String -> Parser Exp
mkQuote t s = do
              oneOf [t]
              whitespace
              r <- anExp
              whitespace
              return $ SExp $ (SymExp s):[r]

anyQuote :: Parser Exp
anyQuote = do
           whitespace
           r <- aQuote <|> aQQuote <|> anUnquote
           whitespace
           return r

--- #### Expression Parser

anExp :: Parser Exp
anExp = do
        whitespace
        r <- anInt <|> anyQuote <|> aSym <|> aForm
        whitespace
        return r

--- Lifters/Lowerers
--- ----------------

liftbool :: Bool -> Val
liftbool False = SymVal "nil"
liftbool True  = SymVal "t"

lowerbool :: Val -> Bool
lowerbool (SymVal "nil") = False
lowerbool _              = True

liftint :: Integer -> Val
liftint = IntVal

lowerint :: Val -> Integer
lowerint (IntVal i) = i
lowerint _          = error "Cannot lower, not an IntVal!"

--- ### Boolean operations

liftBoolOp :: ([Bool] -> Bool) -> [Val] -> Val
liftBoolOp op = liftbool . op . map lowerbool

--- ### Integer operations

liftIntOp :: (Integer -> Integer -> Integer) -> Integer -> ([Val] -> Val)
liftIntOp _  d [] = liftint d
liftIntOp op _ vals = liftint $ foldl1' op $ lowerint <$> vals

--- ### Comparison operations

liftCompOp :: (Integer -> Integer -> Bool) -> ([Val] -> Val)
liftCompOp op vals = liftbool $ aux op (map lowerint vals) []
  where aux _ [] acc          = and acc
        aux op (v1:[]) acc    = aux op [] (True:acc)
        aux op (v1:v2:vs) acc = aux op (v2:vs) $ (op v1 v2):acc

--- ### List operations

liftList :: [Val] -> Val
liftList []     = SymVal "nil"
liftList (v:vs) = ConsVal v $ liftList vs

lowerList :: Val -> [Val]
lowerList v = aux v []
  where aux (ConsVal a b)  l = aux b $ a : l
        aux (SymVal "nil") l = reverse l
        aux _              l = error "Non-proper cons-list"

--- Runtime
--- -------

runtime :: Env
runtime = foldl union empty [ runtimeArith
                            , runtimeComp
                            , runtimeBool
                            , runtimeUnary
                            , runtimeOther
                            ]

--- ### Arithmetic

runtimeArith :: Env
runtimeArith = fromList [ ("+", PrimVal $ liftIntOp (+) 0)
                        , ("-", PrimVal $ liftIntOp (-) 0)
                        , ("*", PrimVal $ liftIntOp (*) 1)
                        ]

--- ### Comparison

runtimeComp :: Env
runtimeComp = fromList [ (">"  , PrimVal $ liftCompOp (>)  )
                       , ("<"  , PrimVal $ liftCompOp (<)  )
                       , (">=" , PrimVal $ liftCompOp (>=) )
                       , ("<=" , PrimVal $ liftCompOp (<=) )
                       , ("="  , PrimVal $ liftCompOp (==) )
                       , ("!=" , PrimVal $ liftCompOp (/=) )
                       ]

--- ### Boolean Operators

runtimeBool :: Env
runtimeBool = fromList [ ("and", PrimVal $ liftBoolOp and)
                       , ("or",  PrimVal $ liftBoolOp or)
                       ]

--- ### Unary Operators

primNot :: Val -> Val
primNot = liftbool . not . lowerbool

primCar :: Val -> Val
primCar (ConsVal a b) = a
primCar a             = ExnVal $ "Not a cons cell: " ++ show a

primCdr :: Val -> Val
primCdr (ConsVal a b) = b
primCdr _             = ExnVal "Not a cons cell"

liftUnary :: String -> (Val -> Val) -> [Val] -> Val
liftUnary opName op (v:[]) = op v
liftUnary opName op _       = ExnVal $ "`" ++ opName ++ "` is a unary operator."

runtimeUnary :: Env
runtimeUnary = fromList [ ("not", PrimVal $ liftUnary "not" primNot)
                        , ("car", PrimVal $ liftUnary "car" primCar)
                        , ("cdr", PrimVal $ liftUnary "cdr" primCdr)
                        ]

--- ### Other operators

primEq :: [Val] -> Val
primEq [] = SymVal "t"
primEq (init:vv) = aux init vv
  where aux _ [] = SymVal "t"
        aux (IntVal i) ((IntVal j):vs) | i == j    = aux init vs
                                       | otherwise = SymVal "nil"
        aux (SymVal i) ((SymVal j):vs) | i == j    = aux init vs
                                       | otherwise = SymVal "nil"
        aux a@(ConsVal i ii) (b@(ConsVal j jj):vs) | lowerbool (primEq ((lowerList a)++(lowerList b))) = aux init vs
                                                   | otherwise = SymVal "nil"
        aux _ _ = SymVal "nil"

runtimeOther :: Env
runtimeOther = fromList [ ("eq?"  , PrimVal $ primEq  )
                        , ("list" , PrimVal $ liftList)
                        ]

--- Evaluation
--- ----------

--- ### Check parameter names

paramStrs :: [Exp] -> Either String [String]
paramStrs es = aux [] es
  where aux param []              = Right $ reverse param
        aux param ((SymExp s):es) = aux (s:param) es
        aux _ _                  = Left "Must use only `SymExp` for parameter names."


--- ### Quoting, Quasi-Quoting, and Unquoting

quote :: Exp -> Val
quote (SymExp s) = (SymVal s)
quote (IntExp i) = (IntVal i)
quote (SExp exps) = liftList $ map quote exps

quasiquote :: Exp -> Env -> Integer -> Val
quasiquote (SExp [(SymExp "unquote")   , exp]) env 1 = eval exp env
quasiquote (SExp [(SymExp "unquote")   , exp]) env i = liftList [(SymVal "unquote"), quasiquote exp env $ i-1]
quasiquote (SExp [(SymExp "quasiquote"), exp]) env i = liftList [(SymVal "quasiquote"), quasiquote exp env $ i+1]
quasiquote (SExp exps) env i = liftList $ map (\e -> quasiquote e env i) exps
quasiquote exp _ _ = quote exp

unquote :: Val -> Exp
unquote (SymVal s)      = (SymExp s)
unquote (IntVal i)      = (IntExp i)
unquote c@(ConsVal _ _) = (SExp $ map unquote $ lowerList c)


--- ### Evaluator

eval :: Exp -> Env -> Val

--- #### Integer, Symbol, and Empty Forms
eval (IntExp i) _   = (IntVal i)
eval (SymExp s) env = case lookup s env of
                        (Just val) -> val
                        _          -> ExnVal $ "Symbol " ++ s ++ " has no value."
eval (SExp []) _ = SymVal "nil"

--- #### Variable Definition Forms
eval (SExp [(SymExp "def"), (SymExp var), exp]) env = (DefVal var $ eval exp env)

--- #### Function Definition and Lambda Function Forms
eval (SExp [(SymExp "define"), (SymExp f), (SExp param), body]) env =
  case paramStrs param of
    Right ss -> let env' = insert f c env
                    c    = Closure ss body env'
                in         DefVal f c
    Left s   -> ExnVal s

eval (SExp [(SymExp "lambda"), (SExp param), body]) env =
  case paramStrs param of
    Right ss -> Closure ss body env
    Left s   -> ExnVal s


--- #### Conditional Form
eval (SExp [(SymExp "cond"), (SExp [])])       _   = SymVal "nil"
eval (SExp [(SymExp "cond"), (SExp (c:[]))])   _   = SymVal "nil"
eval (SExp [(SymExp "cond"), (SExp (c:e:cs))]) env =
  case eval c env of
    SymVal "t"   -> eval e env
    SymVal "nil" -> eval (SExp [(SymExp "cond"), (SExp cs)]) env

--- #### Let Form
eval (SExp [(SymExp "let"), (SExp ll), body]) env =
  let
      insertList [] [] envir = envir
      insertList (k:keys) (v:values) envir = insertList keys values $ insert k v envir
      listExp     = map (\(SExp s) -> s) ll
      var_exps    = map head listExp
      var_strings = map (\(SymExp s) -> s) var_exps
      exps        = map (head.tail) listExp
      vals        = map ((flip eval) env) exps
      env'        = insertList var_strings vals env
  in  eval body env'

--- #### Cons Form
eval (SExp [(SymExp "cons"), e1, e2]) env = ConsVal (eval e1 env) (eval e2 env)

--- #### Quoting, Quasi-Quoting, and Unquoting Forms
eval (SExp [(SymExp "quote"), exp])      env = quote exp
eval (SExp [(SymExp "quasiquote"), exp]) env = quasiquote exp env 1
eval (SExp [(SymExp "unquote"), exp])    env = ExnVal "Cannot `unquote` more than `quasiquote`."

--- #### Eval Form
eval (SExp [(SymExp "eval"), exp]) env = eval (unquote $ eval exp env) env

--- #### Macro Form
eval (SExp [(SymExp "defmacro"), (SymExp m), (SExp param), body]) env =
  case paramStrs param of
    Right ss -> let env' = insert m c env
                    c    = Macro ss body env'
                in         DefVal m c
    Left s   -> ExnVal s

--- #### Application Form
eval (SExp (e:ee)) env =
  case eval e env of
    (PrimVal f)        -> f $ map (\e -> eval e env) ee
    (Closure ss ex en) -> let vals = map (\e -> eval e env) ee
                              insertList :: [String] -> [Val] -> Env -> Env
                              insertList [] _ envir               = envir
                              insertList (k:keys) (v:values) envir = insertList keys values $ insert k v envir
                              en' = insertList ss vals en
                          in  eval ex en'
    (Macro ss ex en)   -> let vals = map quote ee
                              insertList [] _ envir               = envir
                              insertList (k:keys) (v:values) envir = insertList keys values $ insert k v envir
                              en' = insertList ss vals en
                              m'  = unquote $ eval ex en'
                          in  eval m' env -- check this TODO
    v                  -> v


--- REPL
--- ----

--- ### Generating next environment

nextEnv :: Env -> Val -> Env
nextEnv env (DefVal s v) = insert s v env
nextEnv env _            = env

--- ### REPL

prompt :: String -> IO String
prompt str = hPutStr stdout str >> hFlush stdout >> hGetLine stdin

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: Env -> IO ()
repl env = do input <- prompt "scheme> "
              case input of
                "quit" -> return()
                _      -> do case parseWith anExp input of
                              Left err    -> do printLn "Parse error!"
                                                printLn $ show err
                                                repl env
                              Right parsed -> do printLn . show $ val
                                                 repl $ nextEnv env val
                                                  where val = eval parsed env

--- ### Main function

main :: IO ()
main = do printLn "Welcome to Scheme"
          repl runtime
          printLn "Leaving Scheme"
