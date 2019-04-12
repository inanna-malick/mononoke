module Main where

--------------------------------------------
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.Trans.State.Lazy (runStateT)
import           Data.Functor.Compose
import qualified Data.Map as M
--------------------------------------------
import           Test.Hspec
--------------------------------------------
import           Data.Aeson.Orphans ()
import           HGit.Core.Diff
import           HGit.Core.Merge
import           HGit.Core.Types
import           Merkle.Functors
import           Merkle.Store
import           Merkle.Store.Deref
import           Merkle.Store.Test
import           Merkle.Types
import           Util.RecursionSchemes
--------------------------------------------


main :: IO ()
main = do
  hspec $ do
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

      it "diff lazily without descending into non-conflicting dir branches changes" $ do
        let shared :: forall m. Monad m => Fix (HashAnnotated HashableDir `Compose` m `Compose` HashableDir)
            shared = dir [ dir' "baz" [ file "bar" "bar.body"]
                         , file "bar" "bar.body"
                         ]

            sharedPointer = htPointer (shared @ Maybe) -- just needs some type param... FIXME
            -- ffail is used to booby-trap branches of the lazy merkle tree which shouldn't be derefed
            r1 = dir [ ("shared", DirEntity . ffail $ sharedPointer)
                     , file "foo" "foo.body"]
            r2 = dir [ ("shared", DirEntity . ffail $ sharedPointer)
                     , file "baz" "baz.body"]

        diffTest r1 r2 [(["baz"], EntityCreated), (["foo"], EntityDeleted)]

    describe "merge" $ do
      it "merge lazily without descending into non-conflicting dir branches changes" $ do
        let shared :: forall m. Monad m => Fix (HashAnnotated HashableDir `Compose` m `Compose` HashableDir)
            shared = dir [ dir' "baz" [ file "bar" "bar.body"
                         ]
                         , file "bar" "bar.body"
                         ]

            sharedPointer = htPointer (shared @ Maybe) -- just needs some type param... FIXME
            -- ffail is used to booby-trap branches of the lazy merkle tree which shouldn't be derefed
            r1 = dir [ ("shared", DirEntity . ffail $ sharedPointer)
                     , file "foo" "foo.body"]
            r2 = dir [ ("shared", DirEntity . ffail $ sharedPointer)
                     , file "baz" "baz.body"]

            expected :: forall m. Monad m => Fix (HashAnnotated HashableDir `Compose` m `Compose` HashableDir)
            expected = dir [ file "baz" "baz.body"
                           , file "foo" "foo.body"
                           , ("shared", DirEntity $ shared)
                           ]

        (strictRes, _) <- flip runStateT (M.empty :: SSMap HashableDir) $ do
          -- needs to be derefed from store, so upload
          _ <- strictDeref shared >>= uploadDeep testStore . stripTags
          Right (res, toUpload) <- mergeMerkleDirs r1 r2
          _ <- traverse (sUploadShallow testStore) toUpload

          -- res still has poisoned branches, so deref from pointer (b/c store has shared structure)
          strictDeref $ lazyDeref' testStore $ htPointer res

        strictExpected <- strictDeref expected -- janky.. should have lazy & strict branches

        strictRes `shouldBe` strictExpected

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
          Right (res, _) <- mergeMerkleDirs r1 r2
          strictDeref res

        strictExpected <- strictDeref expected -- janky.. should have lazy & strict branches
        strictRes `shouldBe` strictExpected

      it "merge with file-level conflict" $ do
        let r1 = dir [dir' "baz" [file "bar" "bar.body.b"]]
            r2 = dir [dir' "baz" [file "bar" "bar.body.a"]]

        (Left err, _storeState) <- flip runStateT M.empty $ mergeMerkleDirs r1 r2

        err `shouldBe` MergeViolation ["baz", "bar"]

  where
    ffail :: MonadFail m => Hash x -> Fix (HashAnnotated x `Compose` m `Compose` x)
    ffail h = Fix $ Compose (h, Compose $ fail "Boom!")
    dir :: Applicative m
        => [NamedFileTreeEntity (Hash Blob) (LazyMerkleDir m (Hash Blob))]
        -> LazyMerkleDir m (Hash Blob)
    dir xs =
      let d = Dir xs
          h = hash $ fmap htPointer d
        in Fix $ Compose (h, Compose $ pure d)
    dir' n xs = (n,) . DirEntity $ dir xs
    blobHash body = hash $ Chunk body emptyHash
    file n = (n,) . FileEntity . blobHash
