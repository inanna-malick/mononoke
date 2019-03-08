module Merkle.Types where

--------------------------------------------
import qualified Data.Aeson as AE
import           Data.Functor.Compose
import qualified Data.Hashable as H
--------------------------------------------
import           Util.MyCompose
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
--------------------------------------------

type HashTagged f = Pair (Const HashPointer) f

stripTags :: HFunctor f => Term (HashTagged f) :-> Term f
stripTags = cata (Term . pelem)

type HashIndirect f = HashTagged (Compose Maybe :++ f)

-- handleHI
--   :: (Const HashPointer :-> x)
--   -> (Pair (Const HashPointer) (f g) :-> x)
--   -> HashIndirect f g :-> x
-- handleHI f _ (Pair p (HC (Compose Nothing))) = f p
-- handleHI _ g (Pair p (HC (Compose (Just x)))) = f $ Pair p x


type LazyHashTagged m f = HashTagged (Compose m :++ f)

pointer :: forall f . Term (HashTagged f) :-> Const HashPointer
pointer (Term (Pair p _)) = p

derefLayer
  :: forall f m
   . NatM m (Term (LazyHashTagged m f))
            (f (Term (LazyHashTagged m f)))
derefLayer (Term (Pair _ (HC (Compose m)))) = m

-- | Hash pointer (points to value from which hash was derived),
newtype HashPointer = HashPointer { unHashPointer :: String }
  deriving (Eq, Ord)
instance Show HashPointer where
  show (HashPointer x) = "#[" ++ x ++ "]"

instance H.Hashable HashPointer where
  hashWithSalt i a = i `H.hashWithSalt` (H.hash a)

instance AE.ToJSON HashPointer where
  toJSON (HashPointer x) = AE.toJSON x

instance AE.FromJSON HashPointer where
  parseJSON v = HashPointer <$> AE.parseJSON v

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
