module Main where


import Data.Singletons
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as AE
import Test.Hspec
import Control.Exception (evaluate)
import HGit.Serialization
import HGit.Gen
import HGit.Types.HGit
import Util.HRecursionSchemes
import Merkle.Types


import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


main :: IO ()
main = do
  let dir xs = Term $ Dir xs
      dir' n xs = (n,) . DirEntity $ dir xs
      -- TODO/IDEA: store commit messages as blobs!
      blob body = Term $ Blob body -- todo delete blobtree
      file n  = (n,) . FileEntity . blob
      commit msg r ps  = Term $ Commit msg r ps
      liftHD :: Term HGit :-> Term (HashIndirect HGit)
      liftHD = makeIndirect . hashTag structuralHash

  let roundtrip :: forall i . SingI i => HashIndirectTerm i -> Either String (HashIndirectTerm i)
      roundtrip = AE.eitherDecode . AE.encode


  -- let x' = liftHD $ dir [file "fname" "fblob"]
  -- let x = HashIndirectTerm $ liftHD' $ x'

  let propRoundtrip s =
        property $ do
          dt <- forAll $ (HashIndirectTerm <$> genIndTagged s)
          -- liftIO $ print $ AE.encode dt
          roundtrip dt === Right dt

  checkSequential $ Group "Encoding.RoundTrip" [
        ("dir tag round trip", propRoundtrip SDirTag),
        ("file tag round trip", propRoundtrip SBlobTag),
        ("commit tag round trip", propRoundtrip SCommitTag)
      ]

  pure ()

  hspec $ do
    -- todo: also, add some generator-based properties
    describe "round trip (HashIndirectTerm)" $ do
      it "commit encoding" $ do
        let r1 = dir [file "fname" "fblob", dir' "subdir" [file "f1" "foo", file "f2" "bar"]]
            r2 = dir [("base", DirEntity r1), dir' "tmp" [("bkup", DirEntity r1)]]
            c = commit "commit 2" r1 (pure $ commit "c1" r2 $ pure $ Term NullCommit)
            hidt = HashIndirectTerm $ liftHD c

        print $ AE.encode hidt

        roundtrip hidt `shouldBe` Right hidt

