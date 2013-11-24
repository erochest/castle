

module Main where


import Test.Tasty
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.HUnit as HU
-- import Test.Tasty.Golden as TG
import Test.Tasty.Hspec


specs :: Spec
specs = undefined

tests :: TestTree
tests = testGroup "main"
    [ testCase "something" specs
    ]

main = defaultMain tests

