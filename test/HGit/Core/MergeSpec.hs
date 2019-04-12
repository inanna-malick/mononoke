module HGit.Core.MergeSpec where

--------------------------------------------
import           Control.Monad.Trans.State.Lazy (runStateT)
import qualified Data.Map as M
--------------------------------------------
import           Test.Hspec
--------------------------------------------
import           Data.Aeson.Orphans ()
import           HGit.Core.Merge
import           HGit.Core.TestUtils
import           HGit.Core.Types
import           Merkle.Functors
import           Merkle.Store
import           Merkle.Store.Deref
import           Merkle.Store.Test
import           Merkle.Types
--------------------------------------------

spec :: Spec
spec = describe "merge" $ do
      it "merge lazily without descending into non-conflicting dir branches changes" $ do
        let shared :: forall m. Monad m => LazyMerkleDir m (Hash Blob)
            shared = dir [ dir' "baz" [ file "bar" "bar.body"
                         ]
                         , file "bar" "bar.body"
                         ]

            sharedPointer = htPointer (shared @Maybe) -- fixme
            -- ffail is used to booby-trap branches of the lazy merkle tree which shouldn't be derefed
            r1 = dir [ ("shared", DirEntity . ffail $ sharedPointer)
                     , file "foo" "foo.body"]
            r2 = dir [ ("shared", DirEntity . ffail $ sharedPointer)
                     , file "baz" "baz.body"]

            expected :: forall m. Monad m => LazyMerkleDir m (Hash Blob)
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
