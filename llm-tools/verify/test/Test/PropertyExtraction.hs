module Test.PropertyExtraction (spec) where

import Test.Hspec
import ClaudeVerify.PropertyExtraction
import ClaudeVerify.Internal.AST
import ClaudeVerify.Internal.Types

spec :: Spec
spec = do
    describe "extractVCs" $ do
        it "extracts VCs from an empty module" $ do
            let modul = Module (Annotation Nothing Nothing []) Nothing []
                vcs = extractVCs modul
            vcs `shouldBe` []

        it "extracts bounds check from array index" $ do
            pending "implement array indexing test"

        it "extracts assertion VCs" $ do
            pending "implement assertion test"

    describe "wp" $ do
        it "substitutes variables correctly" $ do
            pending "implement wp test"
