{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveDataTypeable, BangPatterns,
             MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances #-}

module Data.AttoLisp.Test where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Attoparsec as A

import Data.AttoLisp
import TestUtil

suite :: Test.Framework.Test
suite = testGroup "Data.AttoLisp" $ map tcase
  [ T "()" (Right $ List [])
  , T "42" (Right $ Number 42)
  , T ";;foo\n42" (Right $ Number 42)
  , T ";;foo\n;;bar\n42" (Right $ Number 42)
  , T "(4 5 6)" (Right $ List [Number 4, Number 5, Number 6])
  , T "(4 5 6 )" (Right $ List [Number 4, Number 5, Number 6])
  , T "(3 (4))" (Right $ List [Number 3, List [Number 4]])
  , T "'(3 4)" (Right $ List [Symbol "quote", List [Number 3, Number 4]])
  , T "\"a; however, b\"" (Right $ String "a; however, b")
  , T "(x ;comment\ny)" (Right $ List [Symbol "x", Symbol "y"])        , T "\"foo\"" (Right (String "foo"))
  , T "foo"     (Right (Symbol "foo"))
  , T "(foo \"bar\" 23)" (Right $ List [Symbol "foo", String "bar", Number 23])
  ]

tcase :: T -> Test.Framework.Test
tcase (T inp out) = testCase (show inp) $ assertEqual (show inp) out out2
 where
  out2 = A.parseOnly (lisp <* A.endOfInput) inp
