module Test.Feedback (spec) where

import Test.Hspec
import ClaudeVerify.Feedback
import ClaudeVerify.Internal.Types

spec :: Spec
spec = do
    describe "FeedbackDB" $ do
        it "can open and close database" $ do
            db <- openDatabase (Just ":memory:")
            closeDatabase db

        it "can record feedback" $ do
            pending "implement record feedback test"

        it "can query recent feedback" $ do
            pending "implement query test"

    describe "toKnownIssue" $ do
        it "converts feedback to known issue" $ do
            pending "implement conversion test"
