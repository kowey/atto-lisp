{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveDataTypeable, BangPatterns,
             MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances #-}

module Main where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Test.HUnit

import qualified Data.Attoparsec as A
import Test.Framework.Providers.HUnit
import Test.Framework

import Data.AttoLisp
import TestUtil
import qualified Data.AttoLisp.Test
import qualified Data.AttoLisp.Test.CL

main :: IO ()
main = defaultMain
  [ Data.AttoLisp.Test.suite
  , Data.AttoLisp.Test.CL.suite
  ]
