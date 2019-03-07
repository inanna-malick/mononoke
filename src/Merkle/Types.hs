module Merkle.Types where

--------------------------------------------
import           Data.Functor.Const
import qualified Data.Functor.Compose as FC
--------------------------------------------
import           Util.MyCompose
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
--------------------------------------------




type HashIndirect = (,) HashPointer :+ Maybe
type LazyHashTagged m = (,) HashPointer :+ m

pointer :: forall f i x. Term (FC.Compose ((,) HashPointer :+ x) :++ f) i -> HashPointer
pointer (Term (HC (FC.Compose (C (p, _))))) = p

pointer' :: forall f x . Term (FC.Compose ((,) HashPointer :+ x) :++ f) :-> Const HashPointer
pointer' = Const . pointer

derefLayer
  :: forall f m
  . NatM m (Term (FC.Compose (LazyHashTagged m) :++ f))
            (f (Term (FC.Compose (LazyHashTagged m) :++ f)))
derefLayer (Term (HC (FC.Compose (C (_p, m))))) = m

newtype HashPointer = HashPointer { unHashPointer :: String }
  deriving (Eq, Ord)
instance Show HashPointer where
  show (HashPointer x) = "#[" ++ x ++ "]"


-- one-way function
-- (because I'm lazy and don't want or need to write a parser, no fundamental reason)
-- NICE COMPACT STRING REPR FOR CONVENIENCE
mkHashPointer :: Int -> HashPointer
mkHashPointer p = HashPointer $ prefix p ++ f (abs p)
  where
    prefix n | n > 0     = "x"
             | n < 0     = "y"
             | otherwise = "z"
    chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    base = length chars

    f n | n == 0 = ""
        | n < 0 = f $ (-1) * n -- no loss of info, handled via prefix
        | otherwise = chars !! (n `rem` base) : f (n `div` base)
