{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module THSpec (tests) where

import qualified Data.ByteString
import Data.Text (Text)
import Manifest (Key (..), Entity (..), withSession, add, get)
import Manifest.Core.Meta (ColumnMeta (..), SqlType (..), tmTable, tmColumns)
import Manifest.Core.Table (PrimaryKey, Serial)
import Manifest.Postgres (execText, withConnection)
import Manifest.Derive.TH (field, mkEntity)
import Fixtures (withEmptyDb)
import Harness

-- The terse declaration under test. One block generates WidgetT, Widget, and
-- the Entity Widget instance — equivalent to the hand-written UserT in Fixtures.
$(mkEntity "Widget" "widgets"
    [ field "id"   [t| PrimaryKey (Serial Int) |]
    , field "name" [t| Text |]
    , field "size" [t| Maybe Int |]
    ])

widgetsDDL :: Data.ByteString.ByteString
widgetsDDL =
  "CREATE TABLE widgets \
  \( widget_id   BIGSERIAL PRIMARY KEY \
  \, widget_name TEXT NOT NULL \
  \, widget_size BIGINT )"

tests :: [Test]
tests = group "TH"
  [ test "mkEntity generates correct table metadata" $ do
      let tm = tableMeta @Widget
      assertEqual "table name" "widgets" (tmTable tm)
      assertEqual "columns"
        [ ColumnMeta "widget_id"   True  True  SqlBigSerial False
        , ColumnMeta "widget_name" False False SqlText      False
        , ColumnMeta "widget_size" False False SqlBigInt    True
        ]
        (tmColumns tm)
  , test "mkEntity wires primKey to the PrimaryKey field" $
      assertEqual "primKey selects widget_id" 7
        (primKey (Widget { widgetId = 7, widgetName = "x", widgetSize = Nothing } :: Widget))
  , test "generated entity round-trips: add (eager, RETURNING) then get decodes from the DB" $
      withEmptyDb $ \pool -> do
        withConnection pool (\c -> execText c widgetsDDL [])
        w0 <- withSession pool $
          add (Widget { widgetId = 0, widgetName = "gizmo", widgetSize = Just 7 } :: Widget)
        assertBool "add filled the serial PK" (widgetId w0 > 0)
        got <- withSession pool $ get @Widget (Key (widgetId w0))
        assertEqual "name decodes" (Just "gizmo")       (fmap widgetName got)
        assertEqual "size decodes" (Just (Just 7))      (fmap widgetSize got)
        assertEqual "pk decodes"   (Just (widgetId w0)) (fmap widgetId got)
  ]
