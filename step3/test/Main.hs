import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Prelude (IO)

import MyList

pushTest :: Assertion
pushTest = 1 @=? 1

headOfOne :: Assertion
headOfOne = expected @=? actual
    where
        expected = 1
        actual = head (MyList [1])

main :: IO ()
main = defaultMainWithOpts
        [ testCase "push" pushTest
        , testCase "head [1]" headOfOne
        ]
        mempty
