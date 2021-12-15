{-
 -  HOPL/CHECKED/Parser.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the grammatical specification for CHECKED.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.ROUNDABOUT.Lang.Parser
  ( parseToplevel,
    ParseError,
  )
where

import HOPL.ROUNDABOUT.Lang.Lexer
import HOPL.ROUNDABOUT.Lang.Syntax (Exp (..), Pgm (..), BinaryOp (..))
import Text.Parsec (ParseError, choice, eof, many1, parse, sepBy, try)
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)

parseToplevel :: String -> Either ParseError Pgm
parseToplevel = parse (contents toplevel) "<stdin>"

toplevel :: Parser Pgm
toplevel = program

parseExp :: String -> Either ParseError Exp
parseExp = parse (contents expression) "<stdin>"

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

{- Grammar for the PROC language -}

program :: Parser Pgm
program = Pgm <$> expression

expression :: Parser Exp
expression =
  (choice . map try)
    [
      -- Variable declarations
      LetExp
        <$> (reserved "let" >> identifier)
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      -- Control expressions
      IfExp
        <$> (reserved "if" >> expression)
        <*> (reserved "then" >> expression)
        <*> (reserved "else" >> expression),
      -- Function definition
      ProcExp
        <$> (reserved "proc" >> symbol "(" >> identifier)
        <*> (symbol ")" >> expression),
      -- Function call
      CallExp
        <$> (symbol "(" >> expression)
        <*> (expression <* symbol ")"),
      -- Arithmetic operators
      DiffExp
        <$> (reservedOp "-" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      AddExp
        <$> (reservedOp "+" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      DivExp
        <$> (reservedOp "/" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      MultExp
        <$> (reservedOp "*" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      -- Arith Assignment Expressions
      AddAssExp
        <$> (reservedOp "+=" >> symbol "(" >> identifier)
        <*> (symbol "," >> expression <* symbol ")"),
      DiffAssExp
        <$> (reservedOp "-=" >> symbol "(" >> identifier)
        <*> (symbol "," >> expression <* symbol ")"),
      -- Arithmetic/numeric predicates
      IsZeroExp
        <$> (reserved "zero?" >> parens expression),
      uncurry . BinaryExp
        <$> binaryOperator
        <*> parens (sepPairOf expression ","),
      -- Integer literal
      ConstExp
        <$> integer,
      -- Variable reference
      VarExp
        <$> identifier,
      -- TODO: Implement SequenceExp
      SequenceExp
        <$> (symbol "{" >> sepBy expression (symbol ";"))
        <*>  (reserved "return" >> expression <* symbol "}"),
      AssignExp
        <$> (reserved "assign" >> identifier)
        <*> (reservedOp "=" >> expression),
      LoopExp
        <$> (reserved "loop" >> expression)
        <*> (reserved "in" >> expression),
     -- FunctExp
      --  <$> (reserved "functionName" >> identifier )
       -- <*> (),
      ListExp
        <$> (reservedOp "[" >> sepBy expression (symbol ",") <* reservedOp "]"),
      StringExp
        <$> stringLiteral,
      LookupExp
        <$> (reserved "lookup" >> expression <* reservedOp "[")
        <*> ( expression <* reservedOp "]"),
      PrintExp
        <$> (reserved "print" <* symbol "(" >> expression <* symbol ")")
    ]

binaryOperator :: Parser BinaryOp
binaryOperator =
  (choice . map try)
    [ Equal <$ reservedOp "==",
      NotEqual <$ reservedOp "!=",
      Less <$ reservedOp "<",
      LessEqual <$ reservedOp "<=",
      Greater <$ reservedOp ">",
      GreaterEqual <$ reservedOp ">="
    ]

sepPairOf :: Parser a -> String -> Parser (a, a)
sepPairOf p sep = (,) <$> p <*> (symbol sep >> p)