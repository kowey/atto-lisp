{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveDataTypeable, BangPatterns,
             MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances #-}

module TestUtil where

import Control.Applicative
import Data.AttoLisp
import qualified Data.Attoparsec as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework

data Msg = Msg T.Text Integer
  deriving (Eq, Show)

instance ToLisp Msg where
  toLisp (Msg t n) = mkStruct "msg" [toLisp t, toLisp n]

instance FromLisp Msg where
  parseLisp e = struct "msg" Msg e

data T = T { tin  :: B.ByteString
           , tout :: Either String Lisp
           }

