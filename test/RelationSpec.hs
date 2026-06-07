{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RelationSpec (tests) where

import Fixtures (User)
import Manifest.Core.Relation (RelSpec (..), relSpec)
import Harness

tests :: [Test]
tests = group "Relation"
  [ test "relSpec for User \"posts\" is RelMany on post_author" $
      case relSpec @User @"posts" of
        RelMany fk -> assertEqual "fk" "post_author" fk
        _          -> assertBool "expected RelMany" False
  , test "relSpec for User \"profile\" is RelOpt on profile_user" $
      case relSpec @User @"profile" of
        RelOpt fk -> assertEqual "fk" "profile_user" fk
        _         -> assertBool "expected RelOpt" False
  ]
