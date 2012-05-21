{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveDataTypeable, BangPatterns #-}
-- The following is for the ParseList stuff
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances, ScopedTypeVariables, OverlappingInstances, EmptyDataDecls #-}
-- | Efficient parsing and serialisation of S-Expressions (as used by Lisp).
--
-- This module is intended to be imported qualified, e.g.:
--
-- > import qualified Data.AttoLisp as L
--
module Data.AttoLisp
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

import Data.AttoLisp.Internal
