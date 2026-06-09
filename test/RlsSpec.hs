{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RlsSpec (tests) where

import Control.Monad.IO.Class (liftIO)
import Fixtures (User, withEmptyDb)
import Manifest
import Manifest.Core.Rls (PolicyDef (..), policyDef)
import Manifest.Session (execDb)
import Harness

tests :: [Test]
tests = group "Rls"
  [ test "policy DSL renders the USING predicate to SQL" $ do
      let p = policy "org_isolation"
                `using` (\u -> u ^. #userName .== currentSetting "app.current_org")
              :: Policy User
          pd = policyDef p
      assertEqual "name" "org_isolation" (pdName pd)
      assertEqual "using" (Just "user_name = current_setting('app.current_org')") (pdUsing pd)
      assertEqual "check" Nothing (pdCheck pd)
  , test "withRlsContext sets a transaction-local GUC; it is cleared afterward" $
      withEmptyDb $ \pool -> withSession pool $ do
        inside <- withTransaction $ withRlsContext [("app.current_org", "acme")] $ do
          rows <- execDb "SELECT current_setting('app.current_org', true)" []
          pure (head (head rows))
        outside <- withTransaction $ do
          rows <- execDb "SELECT current_setting('app.current_org', true)" []
          pure (head (head rows))
        liftIO $ do
          assertEqual "inside the context" (Just "acme") inside
          -- LOCAL set_config auto-clears the *value* at COMMIT, so the org name
          -- never leaks to the next checkout of this pooled connection. Postgres
          -- keeps the custom-GUC placeholder around as an empty string (not SQL
          -- NULL) once it has been referenced, so the next transaction reads
          -- Just "" — proving the value was cleared without leaking "acme".
          assertEqual "value cleared after the transaction" (Just "") outside
  ]
