{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Manifest.Relation
  ( load
  , loadRel
  ) where

import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe)
import Manifest.Core.Codec (SqlParam)
import Manifest.Core.Query (Rel, Cond(..), Op(..))
import Manifest.Core.Relation (HasRelation(..), RelSpec(..))
import Manifest.Core.Sql (renderSelect)
import Manifest.Entity (Entity(..), pkParam)
import Manifest.Session (Db, decodeRowDb, execDb, setBaseline)

-- | Load relation @name@ off a bare value (the A path). Zero type-level
-- tracking; returns the plain 'Target' ([Post] / Maybe Profile).
load :: forall a name. (HasRelation a name) => Rel a name -> a -> Db (Target a name)
load _ = loadRel @a @name

-- | The strategy execution shared by the A and D paths: run a separate SELECT
-- for the children (the @selectin@ strategy) and wrap by cardinality.
loadRel :: forall a name. (HasRelation a name) => a -> Db (Target a name)
loadRel parent = case relSpec @a @name of
  RelMany fk -> selectByFk fk (pkParam parent)
  RelOpt  fk -> listToMaybe <$> selectByFk fk (pkParam parent)

-- | @SELECT <child cols> FROM <child> WHERE <fk> = $1@, decoding each row and
-- registering it in the identity map (so loaded children are managed and flow
-- through snapshot-diff on a later 'Manifest.Session.save').
selectByFk :: forall c. Entity c => ByteString -> SqlParam -> Db [c]
selectByFk fkCol parentPk = do
  let tm  = tableMeta @c
      sql = renderSelect tm [Cond fkCol OpEq parentPk]
  rows <- execDb sql [parentPk]
  mapM (\row -> do child <- decodeRowDb @c row; setBaseline child; pure child) rows
