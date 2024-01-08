module MyList where

import Prelude (error)
--import qualified Prelude

newtype MyList a = MyList [a]

append :: MyList a -> a -> MyList a
append (MyList as) a = MyList (a:as)

head :: MyList a -> a
head (MyList (a:_)) = a
head (MyList []) = error "head"

tail :: MyList a -> a
tail (MyList []) = error "tail"
tail (MyList [a]) = a
tail (MyList (_:as)) = tail (MyList as)
