{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Manifest.Core.Query
  ( Column(..)
  , Op(..)
  , Cond(..)
  , Assign(..)
  , (==.), (/=.), (>.), (<.)
  , (=.)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Manifest.Core.Codec (SqlParam, ToField(..))
import Manifest.Core.Meta (camelToSnake)

-- | A typed reference to column @t@ of table @a@. @#userName :: Column User Text@.
-- The column name is computed from the label via the same camel→snake rule the
-- deriver uses, so labels and metadata agree.
newtype Column a (t :: Type) = Column { colName :: ByteString }

instance (KnownSymbol name) => IsLabel name (Column a t) where
  fromLabel = Column (camelToSnake (symbolVal (Proxy @name)))

-- | Comparison operators supported in SP1.
data Op = OpEq | OpNeq | OpGt | OpLt
  deriving (Eq, Show)

-- | A single condition: @column op value@. A list of conditions is ANDed.
data Cond a = Cond ByteString Op SqlParam
  deriving (Eq, Show)

-- | A single SET assignment in the command path.
data Assign a = Assign ByteString SqlParam
  deriving (Eq, Show)

infix 4 ==., /=., >., <.
(==.), (/=.), (>.), (<.) :: ToField t => Column a t -> t -> Cond a
Column n ==. v = Cond n OpEq  (toField v)
Column n /=. v = Cond n OpNeq (toField v)
Column n >.  v = Cond n OpGt  (toField v)
Column n <.  v = Cond n OpLt  (toField v)

infix 4 =.
(=.) :: ToField t => Column a t -> t -> Assign a
Column n =. v = Assign n (toField v)
