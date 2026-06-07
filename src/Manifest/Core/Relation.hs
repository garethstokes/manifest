{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Manifest.Core.Relation
  ( Card(..)
  , HasRelation(..)
  , RelSpec(..)
  , hasMany
  , hasOpt
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Manifest.Core.Meta (camelToSnake)
import Manifest.Entity (Entity)

-- | Relationship cardinality (promoted to a kind via DataKinds).
data Card = Many | One | Opt
  deriving (Eq, Show)

-- | A relationship named @name@ on entity @a@. SP2 core supports the
-- \"FK on the child references the parent's PK\" shape (Many and Opt).
class (Entity a, KnownSymbol name) => HasRelation a (name :: Symbol) where
  type Target      a name :: Type   -- ^ [Post] / Maybe Profile
  type Cardinality a name :: Card   -- ^ 'Many / 'Opt
  relSpec :: RelSpec (Target a name)

-- | A relationship's runtime spec, indexed by its target type so the loader
-- is type-safe. Each carries the child's 'Entity' dictionary and the child
-- column (the FK) that holds the parent's PK value.
data RelSpec t where
  RelMany :: Entity c => ByteString -> RelSpec [c]
  RelOpt  :: Entity c => ByteString -> RelSpec (Maybe c)

-- | @hasMany #childFk@ — a to-many relationship whose child rows are those
-- with @child_fk = parent_pk@. The child type comes from the 'Target'.
hasMany :: forall c fk. (Entity c, KnownSymbol fk) => Proxy fk -> RelSpec [c]
hasMany _ = RelMany (camelToSnake (symbolVal (Proxy @fk)))

-- | @hasOpt #childFk@ — an optional to-one whose child (if any) has
-- @child_fk = parent_pk@.
hasOpt :: forall c fk. (Entity c, KnownSymbol fk) => Proxy fk -> RelSpec (Maybe c)
hasOpt _ = RelOpt (camelToSnake (symbolVal (Proxy @fk)))
