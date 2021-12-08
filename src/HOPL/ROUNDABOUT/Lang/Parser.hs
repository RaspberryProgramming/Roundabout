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

import HOPL.ROUNDABOUT.Type
import HOPL.ROUNDABOUT.Lang.Lexer
import HOPL.ROUNDABOUT.Lang.Syntax (Exp (..), Pgm (..))
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
    [ -- Variable declarations
      LetExp
        <$> (reserved "let" >> identifier)
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      LetrecExp
        <$> (reserved "letrec" >> typeAnnotation)
        <*> identifier
        <*> (symbol "(" >> identifier)
        <*> (symbol ":" >> typeAnnotation <* symbol ")")
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
        <*> (symbol ":" >> typeAnnotation)
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
        <$> (reservedOp "+=" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      DiffAssExp
        <$> (reservedOp "-=" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      -- Arithmetic/numeric predicates
      IsZeroExp
        <$> (reserved "zero?" >> parens expression),
      -- Integer literal
      ConstExp
        <$> integer,
      -- Variable reference
      VarExp
        <$> identifier
      LoopExp
        <$> (reserved "loop" >> expression)
        <*> (reserved "in" >> sepBy expression (symbol ";")
      BoolExp
        <$> (expression, operator, expression),  
      SequenceExp
        <$> (expression)
    

    ]

typeAnnotation :: Parser Type
typeAnnotation =
  (choice . map try)
    [ IntType
        <$ reserved "int",
      BoolType
        <$ reserved "bool",
      ProcType
        <$> (symbol "(" >> typeAnnotation)
        <*> (reservedOp "->" >> typeAnnotation <* symbol ")")
    ]
