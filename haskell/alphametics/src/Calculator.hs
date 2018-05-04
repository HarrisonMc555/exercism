module Calculator (eval) where
-- module Calculator (calc) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import System.IO
  
eval :: String -> Either ParseError Int
eval = fmap fromInteger . parse stmt ""
  where stmt = do e <- expr
                  eof
                  return e

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef { reservedOpNames = ["*","/","+","-","^"] })

-- naturalOrFloat = P.naturalOrFloat lexer
integer        = P.integer lexer
parens         = P.parens lexer
reservedOp     = P.reservedOp lexer

-- expr :: Parser Double
expr :: Parser Integer
expr = buildExpressionParser table factor <?> "expression"
   where
       table = [
           [unary "-" negate, unary "+" id],
           -- [op "**" (**) AssocRight],
           [op "^" (^) AssocRight],
           [op "*" (*) AssocLeft, op "/" div AssocLeft],
           [op "+" (+) AssocLeft, op "-" (-) AssocLeft]
           ]
       op s f assoc = Infix (do{ reservedOp s; return f } <?> "operator") assoc
       unary s f = Prefix (do{ reservedOp s; return f })

-- factor :: Parser Double
factor :: Parser Integer
factor =
   do {
       parens expr;
   } <|> do {
       -- norf <- naturalOrFloat;
       -- case norf of
       --     Left i    -> return $ fromInteger i
       --     Right f    -> return $ f
       i <- integer;
       return $ i
   } <?>
       "factor"

repl :: String -> (String -> Bool) -> (String -> String) -> IO ()
repl prompt bQuit eval = loop
   where
       loop = do
           putStr prompt
           s <- getLine
           if bQuit s
               then return ()
               else putStrLn (eval s) >> loop

calc :: IO ()
calc = repl "> " (== ":q") (tostring . parse stmt "")
   where
       tostring (Right v)    = show v
       tostring (Left err)    = show err
       stmt = do
           e <- expr
           eof
           return e

main = hSetBuffering stdout NoBuffering >> putStrLn "type ':q' to quit." >> calc >> putStrLn "Bye"
