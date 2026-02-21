module Test.EchidnaClient (spec) where

import Test.Hspec
import ClaudeVerify.EchidnaClient

spec :: Spec
spec = do
    describe "Prover" $ do
        it "lists all provers" $ do
            length allProvers `shouldBe` 12

        it "categorizes SMT provers correctly" $ do
            smtProvers `shouldBe` [Z3, CVC5]

        it "categorizes model checkers correctly" $ do
            modelCheckers `shouldBe` [TLC, AlloyAnalyzer]

    describe "PortfolioStrategy" $ do
        it "has a default strategy" $ do
            defaultStrategy `shouldBe` Cascade

        it "has a fast strategy using parallel" $ do
            fastStrategy `shouldBe` Parallel 2
