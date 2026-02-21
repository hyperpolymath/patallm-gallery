module Main (main) where

import Test.Hspec
import qualified Test.PropertyExtraction
import qualified Test.EchidnaClient
import qualified Test.Feedback

main :: IO ()
main = hspec $ do
    describe "PropertyExtraction" Test.PropertyExtraction.spec
    describe "EchidnaClient" Test.EchidnaClient.spec
    describe "Feedback" Test.Feedback.spec
