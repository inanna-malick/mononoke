module HGit.Core.DiffSpec where

--------------------------------------------
import           Data.Functor.Compose
--------------------------------------------
import           Test.Hspec
--------------------------------------------
import           Data.Aeson.Orphans ()
import           HGit.Core.Diff
import           HGit.Core.TestUtils
import           HGit.Core.Types
import           Merkle.Functors
import           Util.RecursionSchemes
--------------------------------------------

spec :: Spec
spec = do
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
