{-
 -  HOPL/MUTABLE_PAIRS/Lang/Parser.hs
 -
 -  Reference implementation of the toy language MUTABLE_PAIRS from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the grammatical specification for MUTABLE_PAIRS.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.MUTABLE_PAIRS.Lang.Parser
  ( parseToplevel,
    ParseError,
  )
where

import HOPL.MUTABLE_PAIRS.Lang.Lexer
import HOPL.MUTABLE_PAIRS.Lang.Syntax (Exp (..), Pgm (..))
import Text.Parsec (ParseError, choice, eof, parse, sepBy, try)
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
    [ LetExp
        <$> (reserved "let" >> identifier)
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      LetrecExp
        <$> (reserved "letrec" >> identifier)
        <*> parens identifier
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      IfExp
        <$> (reserved "if" >> expression)
        <*> (reserved "then" >> expression)
        <*> (reserved "else" >> expression),
      CallExp
        <$> (symbol "(" >> expression)
        <*> (expression <* symbol ")"),
      ProcExp
        <$> (reserved "proc" >> parens identifier)
        <*> expression,
      DiffExp
        <$> (reservedOp "-" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      IsZeroExp
        <$> (reserved "zero?" >> parens expression),
      BeginExp
        <$> (reserved "begin" >> sepBy expression (symbol ";") <* reserved "end"),
      AssignExp
        <$> (reserved "set" >> identifier)
        <*> (reservedOp "=" >> expression),
      NewPairExp
        <$> (reserved "newpair" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      LeftExp
        <$> (reserved "left" >> parens expression),
      RightExp
        <$> (reserved "right" >> parens expression),
      SetLeftExp
        <$> (reserved "setleft" >> expression)
        <*> (symbol "=" >> expression),
      SetRightExp
        <$> (reserved "setright" >> expression)
        <*> (symbol "=" >> expression),
      ConstExp
        <$> integer,
      VarExp
        <$> identifier
    ]
