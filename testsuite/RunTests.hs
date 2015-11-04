module RunTests where

import Distribution.TestSuite.QuickCheck
import Tests.Game

tests :: IO [Test]
tests = return [
  testProperty "Succeeds" True,
  testProperty "Fails" False,
  mayFail
  ]


mayFail :: Test
mayFail = testGroup "May fail" [
  testProperty "Maybe fails" neqNegation,
  testProperty "Probably fails" (not . neqNegation)
  ]

neqNegation :: Int -> Bool
neqNegation x = x /= -x

