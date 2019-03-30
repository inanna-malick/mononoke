module Main where


import Data.Singletons
import Data.Functor.Compose
import Control.Monad.IO.Class
import qualified Data.Aeson as AE
import Test.Hspec
import HGit.Diff
import HGit.Merge
import HGit.Diff.Types
import HGit.Types.HGit
import Util.MyCompose
import Util.RecursionSchemes
import Merkle.Functors
import Merkle.Store
import Merkle.Store.Deref
import Merkle.Types
import Data.Map (Map)
import qualified Data.Map as M
import           Control.Monad.Trans.State.Lazy (StateT, gets, get, put, runStateT, modify)
import           Control.Monad.Trans.Except (runExceptT)

import           Hedgehog
import Control.Monad.Fail (MonadFail)


main :: IO ()
main = do
  let ffail :: MonadFail m => Hash x -> Fix (HashTagged x `Compose` m `Compose` x)
      ffail h = Fix $ Compose (h, Compose $ fail "Boom!")
      dir :: Applicative m
          => [NamedFileTreeEntity (Hash Blob) (Fix $ HashTagged (Dir (Hash Blob)) `Compose` m `Compose` Dir (Hash Blob))]
          -> Fix (HashTagged (Dir (Hash Blob)) `Compose` m `Compose` Dir (Hash Blob))
      dir xs =
        let d = Dir xs
            h = hash $ fmap htPointer d
         in Fix $ Compose (h, Compose $ pure d)
      dir' n xs = (n,) . DirEntity $ dir xs
      blobHash body = hash $ Chunk body emptyHash
      file n = (n,) . FileEntity . blobHash

  hspec $ do
    -- todo tests that confirm laziness via boobytrapped branches (via error on eval)
    describe "diff" $ do
      let diffTest r1 r2 expected = do
            diffRes <- diffMerkleDirs r1 r2
            diffRes `shouldBe` expected

      it "modify file" $ do
        let r1 = dir [dir' "foo" [file "bar" "bar.body.v1"]]
            r2 = dir [dir' "foo" [file "bar" "bar.body.v999.final.freeze.01.draft.5"]]
        diffTest r1 r2 [(["foo", "bar"], FileModified)]

      it "add file" $ do -- todo different enums for file/dir created?
        let r1 = dir []
            r2 = dir [dir' "foo" [file "bar" "bar.body"]]
        diffTest r1 r2 [(["foo"], EntityCreated)]

      it "add dir" $ do -- todo make full recursive add/delete diff?
        let r1 = dir []
            r2 = dir [file "bar" "bar.body"]
        diffTest r1 r2 [(["bar"], EntityCreated)]

      it "replace dir with file" $ do
        let r1 = dir [file "foo" "foo.body", dir' "baz" [file "bar" "bar.body"]]
            r2 = dir [dir' "foo" [file "bar" "bar.body"], file "baz" "baz.body"]
        diffTest r1 r2 [(["baz"], DirReplacedWithFile), (["foo"], FileReplacedWithDir)]


    describe "merge" $ do
      it "merge with safely overlapping changes" $ do
        let r1 = dir [ dir' "baz" [ file "bar" "bar.body"
                                  ]
                     , file "bar" "bar.body"
                     ]
            r2 = dir [ dir' "baz" [ file "foo" "foo.body"
                                  ]
                     , file "bar" "bar.body"
                     ]

            expected = dir [ file "bar" "bar.body"
                           , dir' "baz" [ file "bar" "bar.body"
                                        , file "foo" "foo.body"
                                        ]
                           ]

        (strictRes, _) <- flip runStateT M.empty $ do
          Right res <- runExceptT $ mergeMerkleDirs' testStore r1 r2
          strictDeref res

        strictExpected <- strictDeref expected -- janky.. should have lazy & strict branches
        strictRes `shouldBe` strictExpected

      it "merge with file-level conflict" $ do
        let r1 = dir [dir' "baz" [file "bar" "bar.body.b"]]
            r2 = dir [dir' "baz" [file "bar" "bar.body.a"]]

        (Left err, _storeState) <- flip runStateT M.empty
                                 . runExceptT $ mergeMerkleDirs' testStore r1 r2

        err `shouldBe` MergeViolation ["baz", "bar"]

type SSMap f = Map (Hash f) (f (Hash f))

-- TODO: move to Merkle.Store.Test
-- todo: bake 'Maybe' into lookup fn, stores should only have control over, eg, decode parse fail error type
testStore
  :: forall m f
   . Hashable f
  => Functor f
  => MonadIO m
  -- TFW you don't feel like adding lens as a dependency but want lenses anyway
  => Store (StateT (SSMap f) m) f
testStore = Store
  { sDeref = \p -> get >>= lookup' p
  , sUploadShallow = \x -> do
          let p = hash x
          modify (M.insert p x)
          pure p
  }
  where
    lookup' p = maybe (fail "key not found")
                      (pure . fmap (Fix . Compose . (, Compose $ Nothing)))
              . M.lookup p
