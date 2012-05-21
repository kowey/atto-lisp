{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveDataTypeable, BangPatterns #-}
-- The following is for the ParseList stuff
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances, ScopedTypeVariables, OverlappingInstances, EmptyDataDecls #-}
-- | Efficient parsing and serialisation of S-Expressions (as used by Lisp).
--
-- This module is intended to be imported qualified, e.g.:
--
-- > import qualified Data.AttoLisp.CL as L
--
module Data.AttoLisp.CL
  ( -- * Core Lisp Types
    Lisp(..), nil, isNull,
    -- * Type Conversion
    FromLisp(..), Result(..), fromLisp,
    Failure, Success, Parser,
    parse, parseMaybe, parseEither, typeMismatch,

    ToLisp(..), 

    -- * Constructors and destructors
    mkStruct,  struct,

    -- * Encoding and parsing
    encode, fromLispExpr,
    
    lisp, atom,
  )
where

import Data.AttoLisp.Internal hiding ( skipLispSpace, list_, atom, lisp, fromNumber )
import Control.Applicative
import Data.Attoparsec.Char8 hiding ( Parser, Result, parse, string, double )
import qualified Data.Attoparsec as A
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Unsafe as B

{-

We are using the standard Common Lisp read table.

The following characters are special:

  - whitespace: space, tab, newline, linefeed, return, page
  
  - terminating: ( ) , ` ' " ;

  - escaping: \  and  |

All remaining characters can be part of a symbol.  If a symbol looks
like an number then it is one.  Otherwise it's just a symbol.

-}

-- | Parse an arbitrary lisp expression.
lisp :: A.Parser Lisp
lisp = skipLispSpace *>
  (char '(' *> list_ <|>
   quoted <$> (char '\'' *> char '(' *> list_) <|>
   String <$> (char '"' *> lstring_) <|>
   atom)
 where
  quoted l = List [Symbol "quote", l]

-- | Parse a symbol or a number.  Symbols are expected to be utf8.
--
-- TODO: support escapes in symbols
atom :: A.Parser Lisp
atom = do
  sym <- takeWhile1 (\c -> not (terminatingChar c))
  -- If it looks like a number it is parsed as a number.
  let !w = B.unsafeIndex sym 0
  if (w >= 48 && w <= 57) ||  -- digit
     w == 43 || w == 45       -- '+' or '-'
   then do
     case A.parseOnly number sym of
       Left _  -> pure (Symbol (T.decodeUtf8 sym))
       Right n -> pure (Number n)
   else
     pure (Symbol (T.decodeUtf8 sym))

list_ :: A.Parser Lisp
list_ = do
  skipLispSpace
  elems <- (lisp `sepBy` skipLispSpace) <* skipLispSpace <* char ')'
  return (List elems)

skipLispSpace :: A.Parser ()
skipLispSpace =
  skipSpace >> many (comment >> skipSpace) >> return ()
