{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module SchemaSpec (main) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Manifest hiding (Target)
import Manifest.Postgres (Pool)
import Manifest.Testing (withEphemeralDb)
import Evals.Schema
import Evals.Ids
import Evals.Migrate (migrateAll)

expect :: String -> Bool -> IO ()
expect msg ok = unless ok (ioError (userError ("FAILED: " <> msg)))

main :: IO ()
main = withEphemeralDb $ \pool -> do
  -- migrate twice; the second run is a no-op (empty additive plan)
  _  <- withSession pool migrateAll
  p2 <- withSession pool migrateAll
  expect "second migrate is a no-op (empty additive plan)" (null (planAdditive p2))
  now <- getCurrentTime
  result <- withSession pool $ do
    d <- add (Dataset { id = DatasetId 0, org = OrgId 1, name = "demo", slug = "demo", createdAt = now } :: Dataset)
    v <- add (DatasetVersion { id = DatasetVersionId 0, dataset = d.id, version = 1, note = Nothing, finalizedAt = Just now, createdAt = now } :: DatasetVersion)
    _ <- add (Example { id = ExampleId 0, datasetVersion = v.id, key = "c1"
                      , input = Aeson (object ["q" .= ("2+2" :: Text)])
                      , expected = Just (Aeson (object ["a" .= (4 :: Int)])), meta = Nothing } :: Example)
    got <- get @Dataset (Key d.id)
    pure (fmap (.name) got, v.version)
  expect "dataset round-trips by typed Key" (fst result == Just "demo")
  expect "dataset version is 1" (snd result == 1)

  -- Scenario A: cascade deletes. Manifest cascades are single-level (each parent's
  -- delete issues one DELETE per child rule; they do NOT recurse), so we exercise
  -- both edges: deleting a Run removes its Outputs (Run->Output Cascade), and
  -- deleting an Output removes its Scores (Output->Score Cascade). Rows not owned by
  -- the deleted Run (Example, DatasetVersion) survive.
  cascade' <- expectCascade pool now
  expect "cascade: run's outputs are gone"          (cOutputsGone cascade')
  expect "cascade: output's scores are gone"        (cScoresGone cascade')
  expect "cascade: example survives the run delete"  (cExampleKept cascade')
  expect "cascade: dataset version survives"         (cVersionKept cascade')

  -- Scenario B: deleting a DatasetVersion that a Run references is Restricted.
  restrict' <- expectRestrict pool now
  expect "restrict: delete of referenced version was rejected" (rRejected restrict')
  expect "restrict: referenced dataset version still exists"   (rVersionKept restrict')

  putStrLn "manifest-evals SchemaSpec: migrate + round-trip + cascade + restrict OK"

-- Scenario A ------------------------------------------------------------------

data CascadeResult = CascadeResult
  { cOutputsGone :: Bool
  , cScoresGone  :: Bool
  , cExampleKept :: Bool
  , cVersionKept :: Bool
  }

expectCascade :: Pool -> UTCTime -> IO CascadeResult
expectCascade pool now = withSession pool $ do
  d  <- add (Dataset { id = DatasetId 0, org = OrgId 1, name = "casc", slug = "casc", createdAt = now } :: Dataset)
  v  <- add (DatasetVersion { id = DatasetVersionId 0, dataset = d.id, version = 1, note = Nothing, finalizedAt = Just now, createdAt = now } :: DatasetVersion)
  ex <- add (Example { id = ExampleId 0, datasetVersion = v.id, key = "k1"
                     , input = Aeson (object ["q" .= ("hi" :: Text)]), expected = Nothing, meta = Nothing } :: Example)
  t  <- add (Target { id = TargetId 0, org = OrgId 1, name = "t", createdAt = now } :: Target)
  tv <- add (TargetVersion { id = TargetVersionId 0, target = t.id, version = 1, model = "m", prompt = "p"
                           , params = Aeson (object []), createdAt = now } :: TargetVersion)
  g  <- add (Grader { id = GraderId 0, org = OrgId 1, name = "g", kind = "exact", createdAt = now } :: Grader)
  gv <- add (GraderVersion { id = GraderVersionId 0, grader = g.id, version = 1, config = Aeson (object []), createdAt = now } :: GraderVersion)
  r  <- add (Run { id = RunId 0, org = OrgId 1, datasetVersion = v.id, targetVersion = tv.id, status = "done"
                 , startedAt = Just now, finishedAt = Just now, meta = Nothing, createdAt = now } :: Run)
  -- Output o1 carries a Score and is deleted DIRECTLY (exercises Output->Score).
  o1 <- add (Output { id = OutputId 0, run = r.id, example = ex.id, response = Nothing, text = Just "scored"
                    , error = Nothing, latencyMs = Nothing, tokens = Nothing } :: Output)
  _  <- add (Score { id = ScoreId 0, output = o1.id, graderVersion = gv.id, value = 1.0, passed = Just True
                   , detail = Nothing, createdAt = now } :: Score)
  -- Output o2 is removed by the Run delete (exercises Run->Output).
  _  <- add (Output { id = OutputId 0, run = r.id, example = ex.id, response = Nothing, text = Just "byrun"
                    , error = Nothing, latencyMs = Nothing, tokens = Nothing } :: Output)
  -- Edge 1: Output -> Score cascade (delete the output directly).
  withTransaction $ delete o1
  scores <- selectWhere [ #output ==. o1.id ]
  -- Edge 2: Run -> Output cascade (delete the run; its remaining output goes).
  withTransaction $ delete r
  outs   <- selectWhere [ #run ==. r.id ]
  exs    <- selectWhere [ #datasetVersion ==. v.id ]
  vers   <- get @DatasetVersion (Key v.id)
  pure CascadeResult
    { cOutputsGone = null (outs :: [Output])
    , cScoresGone  = null (scores :: [Score])
    , cExampleKept = length (exs :: [Example]) == 1
    , cVersionKept = maybe False (const True) vers
    }

-- Scenario B ------------------------------------------------------------------

data RestrictResult = RestrictResult
  { rRejected    :: Bool
  , rVersionKept :: Bool
  }

expectRestrict :: Pool -> UTCTime -> IO RestrictResult
expectRestrict pool now = do
  -- Create the dataset version and a Run referencing it.
  vid <- withSession pool $ do
    d  <- add (Dataset { id = DatasetId 0, org = OrgId 1, name = "restr", slug = "restr", createdAt = now } :: Dataset)
    v  <- add (DatasetVersion { id = DatasetVersionId 0, dataset = d.id, version = 1, note = Nothing, finalizedAt = Just now, createdAt = now } :: DatasetVersion)
    t  <- add (Target { id = TargetId 0, org = OrgId 1, name = "t2", createdAt = now } :: Target)
    tv <- add (TargetVersion { id = TargetVersionId 0, target = t.id, version = 1, model = "m", prompt = "p"
                             , params = Aeson (object []), createdAt = now } :: TargetVersion)
    _  <- add (Run { id = RunId 0, org = OrgId 1, datasetVersion = v.id, targetVersion = tv.id, status = "done"
                   , startedAt = Just now, finishedAt = Just now, meta = Nothing, createdAt = now } :: Run)
    pure v.id
  -- Attempt to delete the referenced version; Restrict must reject it.
  res <- (try :: IO a -> IO (Either SomeException a)) $ withSession pool $ do
    v <- get @DatasetVersion (Key vid)
    maybe (pure ()) (withTransaction . delete) v
  -- The version row must survive.
  kept <- withSession pool $ get @DatasetVersion (Key vid)
  pure RestrictResult
    { rRejected    = either (const True) (const False) res
    , rVersionKept = maybe False (const True) kept
    }
