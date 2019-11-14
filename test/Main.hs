{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Lens                  ( (%~)
                                               , (&)
                                               , (*~)
                                               , (+~)
                                               , (.~)
                                               , (^..)
                                               , Traversal'
                                               , partsOf
                                               )
import           Data.Char                     ( toUpper )
import           Data.Map.Strict               ( Map )
import qualified Data.Map.Strict        as Map
import           Data.Slice.Lens               ( sliced )
import           Data.Slice.QQ                 ( s
                                               , sd
                                               )
import           Data.Slice.QQ.Internal        ( handle )
import           Data.Slice.TH
import           Data.Tree                     ( Tree
                                               , flatten
                                               , unfoldTree
                                               )
import           Test.Tasty                    ( defaultMain
                                               , testGroup
                                               )
import           Test.Tasty.Hspec

type TupleSlice = (Maybe Int, Maybe Int, Maybe Int)

main :: IO ()
main = defaultMain . testGroup "Data.Slice.Lens" . pure
  =<< testSpec "Tests" spec_lens

spec_lens :: Spec
spec_lens = do
  describe "statically checked overloaded strings" $ do
    let az = ['a'..'z']

    describe "just start" $
      it "should work" $
       az ^.. sliced ($$("23:") :: (Maybe Int, Maybe Int, Maybe Int)) `shouldBe` "xyz"

    describe "just end" $
      it "should work" $
       az ^.. sliced $$(":3") `shouldBe` "abc"

    describe "just end + trailing" $
      it "should work" $
       az ^.. sliced $$(":3:") `shouldBe` "abc"

    describe "start+end" $
      it "should work" $
       az ^.. sliced $$("3:6") `shouldBe` "def"

  describe "some QuasiQuotes" $ do
    let az = ['a'..'z']

    describe "just start" $
      it "should work" $
       az ^.. sliced [s|23:|] `shouldBe` "xyz"

    describe "just end" $
      it "should work" $
       az ^.. sliced [s|:3|] `shouldBe` "abc"

    describe "just end + trailing" $
      it "should work" $
       az ^.. sliced [s|:3:|] `shouldBe` "abc"

    describe "start+end" $
      it "should work" $
       az ^.. sliced [s|3:6|] `shouldBe` "def"

    describe "start+end+trailing colon" $
      it "should work" $
       az ^.. sliced [s|4:7:|] `shouldBe` "efg"

    describe "start+end+step" $
      it "should work" $
       az ^.. sliced [s|23:3:-5|] `shouldBe` "wrmh"

    describe "end+step" $
      it "should work" $
       az ^.. sliced [s|:10:4|] `shouldBe` "aei"

    describe "reversed" $
      it "should work" $
       az ^.. sliced [s|::-1|] `shouldBe` ['z', 'y' .. 'a']

    describe "identity" $
      it "should work" $
        az ^.. sliced [s|::|] `shouldBe` az

    describe "sliced QQ" $
      it "should work alongside everything else" $
        az ^.. [sd|1:10:5|] `shouldBe` "bg"

    describe "step of 0" $
      it "should be rejected at compile time" $
        -- since we can't catch an exception during th ecompile phase this will
        -- have suffice as evidence:
        (handle (const . pure $ ()) "::0") `shouldThrow` anyException

  describe "toListOf (sliced \"::\") on" $ do
    let optic :: Traversable t => Traversal' (t a) a
        optic = sliced "::"

    describe "an empty list" $
      it "should return an empty list" $
        emptyList ^.. optic `shouldBe` emptyList

    describe "an empty map" $
      it "should return an empty list" $
        emptyMap ^.. optic `shouldBe` emptyList

    describe "an empty string" $
      it "should return an empty string" $
        emptyString ^.. optic `shouldBe` emptyString

    describe "a non-empty list" $
      it "should return all of the elements in the list in order" $
        nonEmptyList ^.. optic `shouldBe` nonEmptyList

    describe "a non-empty map" $
      it "should return all of the elements in the map in order" $
        nonEmptyMap ^.. optic `shouldBe` Map.elems nonEmptyMap

    describe "a non-empty string" $
      it "should return the whole non empty string in order" $
        nonEmptyString ^.. optic `shouldBe` nonEmptyString

    describe "a tree" $
      it "should return the whole tree in order" $
        aTree ^.. optic `shouldBe`
          [ 5, 4, 3, 2, 1, 0, 0, 1, 0, 0, 2, 1, 0, 0, 1, 0, 0, 3, 2, 1, 0
          , 0, 1, 0, 0, 2, 1, 0, 0, 1, 0, 0, 4, 3, 2, 1, 0, 0, 1, 0, 0, 2
          , 1, 0, 0, 1, 0, 0, 3, 2, 1, 0, 0, 1, 0, 0, 2, 1, 0, 0, 1, 0, 0
          ]

  describe "toListOf \":3\" on" $ do
    let optic :: Traversable t => Traversal' (t a) a
        optic = sliced ":3"

    describe "an empty list" $
      it "should return an empty list" $
        emptyList ^.. optic `shouldBe` emptyList

    describe "an empty map" $
      it "should return an empty list" $
        emptyMap ^.. optic `shouldBe` emptyList

    describe "an empty string" $
      it "should return an empty string" $
        emptyString ^.. optic `shouldBe` emptyString

    describe "a non-empty list" $
      it "should return the first three elements of the list" $
        nonEmptyList ^.. optic `shouldBe` take 3 nonEmptyList

    describe "a non-empty map" $
      it "should return the first three elements of the map" $
        nonEmptyMap ^.. optic `shouldBe` [0, 9, 17]

    describe "a non-empty string" $
      it "should return the first three characters of the string" $
        nonEmptyString ^.. optic `shouldBe` "foo"

    describe "a tree" $
      it "should return the first three nodes of the tree" $
        aTree ^.. optic `shouldBe` [5,4,3]

  describe "toListOf \"::2\" on" $ do
    let optic :: Traversable t => Traversal' (t a) a
        optic = sliced "::2"

    describe "an empty list" $
      it "should return an empty list" $
        emptyList ^.. optic `shouldBe` emptyList

    describe "an empty map" $
      it "should return an empty list" $
        emptyMap ^.. optic `shouldBe` emptyList

    describe "an empty string" $
      it "should return an empty string" $
        emptyString ^.. optic `shouldBe` emptyString

    describe "a non-empty list" $
      it "should return every other element from the list" $
        nonEmptyList ^.. optic `shouldBe` [1, 3, 5]

    describe "a non-empty map" $
      it "should return every other element from the map" $
        nonEmptyMap ^.. optic `shouldBe` [0, 17, 212]

    describe "a non-empty string" $
      it "should return every other character from the string" $
        nonEmptyString ^.. optic `shouldBe` "foa"

    describe "a tree" $
      it "should return every other node in the tree" $
        aTree ^.. optic `shouldBe`
          [ 5, 3, 1, 0, 0, 2, 0, 1, 0, 2, 0, 1, 0, 1, 0, 0
          , 4, 2, 0, 1, 0, 1, 0, 0, 3, 1, 0, 0, 2, 0, 1, 0
          ]

  describe "toListOf \"::-1\" on" $ do
    let optic :: Traversable t => Traversal' (t a) a
        optic = sliced "::-1"

    describe "an empty list" $
      it "should return an empty list" $
        emptyList ^.. optic `shouldBe` emptyList

    describe "an empty map" $
      it "should return an empty list" $
        emptyMap ^.. optic `shouldBe` emptyList

    describe "an empty string" $
      it "should return an empty string" $
        emptyString ^.. optic `shouldBe` emptyString

    describe "a non-empty list" $
      it "should return the whole non empty list reversed" $
        nonEmptyList ^.. optic `shouldBe` reverse nonEmptyList

    describe "a non-empty map" $
      it "should return all of the elements in the map reversed" $
        nonEmptyMap ^.. optic `shouldBe` reverse (Map.elems nonEmptyMap)

    describe "a non-empty string" $
      it "should return the whole non empty string reversed" $
        nonEmptyString ^.. optic `shouldBe` reverse nonEmptyString

    describe "a tree" $
      it "should return every node in the tree reversed" $
        aTree ^.. optic `shouldBe`
          [ 0, 0, 1, 0, 0, 1, 2, 0, 0, 1, 0, 0, 1, 2, 3, 0, 0, 1, 0, 0, 1
          , 2, 0, 0, 1, 0, 0, 1, 2, 3, 4, 0, 0, 1, 0, 0, 1, 2, 0, 0, 1, 0
          , 0, 1, 2, 3, 0, 0, 1, 0, 0, 1, 2, 0, 0, 1, 0, 0, 1, 2, 3, 4, 5
          ]

  describe "toListOf \"3::-1\" on" $ do
    let optic :: Traversable t => Traversal' (t a) a
        optic = sliced "3::-1"

    describe "an empty list" $
      it "should return an empty list" $
        emptyList ^.. optic `shouldBe` emptyList

    describe "an empty map" $
      it "should return an empty map" $
        emptyMap ^.. optic `shouldBe` emptyList

    describe "an empty string" $
      it "should return an empty string" $
        emptyString ^.. optic `shouldBe` emptyString

    describe "a non-empty list" $
      it "should return the first three elements of the list reversed" $
        nonEmptyList ^.. optic `shouldBe` [3, 2, 1]

    describe "a non-empty map" $
      it "should return the first three elements of the map reversed" $
        nonEmptyMap ^.. optic `shouldBe` [17, 9, 0]

    describe "a non-empty string" $
      it "should return the first three characters of the string reversed" $
        nonEmptyString ^.. optic `shouldBe` "oof"

    describe "a tree" $
      it "should return the first three nodes of the tree reversed" $
        aTree ^.. optic `shouldBe` [3,4,5]

  describe "toListOf \"':3:-1'\" on" $ do
    let optic :: Traversable t => Traversal' (t a) a
        optic = sliced ":3:-1"

    describe "an empty list" $
      it "should return an empty list" $
        emptyList ^.. optic `shouldBe` emptyList

    describe "an empty map" $
      it "should return an empty map" $
        emptyMap ^.. optic `shouldBe` emptyList

    describe "an empty string" $
      it "should return an empty string" $
        emptyString ^.. optic `shouldBe` emptyString

    describe "a non-empty list" $
      it "should return the last three elements of the list reversed" $
        nonEmptyList ^.. optic `shouldBe` [6, 5, 4]

    describe "a non-empty map" $
      it "should return the last two(?) elements of map reversed" $
        nonEmptyMap ^.. optic `shouldBe` [400, 212, 32]

    describe "a non-empty string" $
      it "should return the last three characters of the string reversed" $
        nonEmptyString ^.. optic `shouldBe` "rab"

    describe "a tree" $
      it "should return every node but the last three reversed" $
        aTree ^.. optic `shouldBe`
          [ 0, 0, 1, 0, 0, 1, 2, 0, 0, 1, 0, 0, 1, 2, 3, 0, 0, 1, 0, 0
          , 1, 2, 0, 0, 1, 0, 0, 1, 2, 3, 4, 0, 0, 1, 0, 0, 1, 2, 0, 0
          , 1, 0, 0, 1, 2, 3, 0, 0, 1, 0, 0, 1, 2, 0, 0, 1, 0, 0, 1, 2
          ]

  describe "toListOf \"6:0:-1\" on" $ do
    let optic :: Traversable t => Traversal' (t a) a
        optic = sliced "6:0:-1"

    describe "an empty list" $
      it "should return an empty list" $
        emptyList ^.. optic `shouldBe` emptyList

    describe "an empty map" $
      it "should return an empty map" $
        emptyMap ^.. optic `shouldBe` emptyList

    describe "an empty string" $
      it "should return an empty string" $
        emptyString ^.. optic `shouldBe` emptyString

    describe "a non-empty list" $
      it "should return last six elements of the non-empty list reversed" $
        nonEmptyList ^.. optic `shouldBe` reverse nonEmptyList

    describe "a non-empty map" $
      it "should return non-empty map reversed" $
        nonEmptyMap ^.. optic `shouldBe` reverse (Map.elems nonEmptyMap)

    describe "a non-empty string" $
      it "should return non-empty string reversed" $
        nonEmptyString ^.. optic `shouldBe` reverse nonEmptyString

    describe "a tree" $
      it "should return six nodes reversed" $
        aTree ^.. optic `shouldBe` [ 0, 1, 2, 3, 4, 5 ]

  describe "toListOf \"2:-2\" on" $ do
    let optic :: Traversable t => Traversal' (t a) a
        optic = sliced "2:-2"

    describe "an empty list" $
      it "should return an empty list" $
        emptyList ^.. optic `shouldBe` emptyList

    describe "an empty map" $
      it "should return an empty map" $
        emptyMap ^.. optic `shouldBe` emptyList

    describe "an empty string" $
      it "should return an empty string" $
        emptyString ^.. optic `shouldBe` emptyString

    describe "a non-empty list" $
      it "should return middle two elements of the non-empty list" $
        nonEmptyList ^.. optic `shouldBe` (take 2 . drop 2) nonEmptyList

    describe "a non-empty map" $
      it "should return non-empty map reversed" $
        nonEmptyMap ^.. optic `shouldBe`
          (take 2 . drop 2 . Map.elems $ nonEmptyMap)

    describe "a non-empty string" $
      it "should return non-empty string reversed" $
        nonEmptyString ^.. optic `shouldBe` (take 2 . drop 2) nonEmptyString

    describe "a tree" $
      it "should return everything but first and last two reversed" $
        aTree ^.. optic `shouldBe`
          [ 3, 2, 1, 0, 0, 1, 0, 0, 2, 1, 0, 0, 1, 0, 0, 3, 2, 1, 0, 0
          , 1, 0, 0, 2, 1, 0, 0, 1, 0, 0, 4, 3, 2, 1, 0, 0, 1, 0, 0, 2
          , 1, 0, 0, 1, 0, 0, 3, 2, 1, 0, 0, 1, 0, 0, 2, 1, 0, 0, 1
          ]

  describe "slice \"::\" *~ 10" $ do
    let optic :: Traversable t => Traversal' (t a) a
        optic = sliced "::"

    describe "an empty list" $
      it "should return an empty list" $
        (emptyList & optic *~ 10) `shouldBe` emptyList

    describe "an empty map" $
      it "should return an empty map" $
        (emptyMap & optic *~ 10) `shouldBe` emptyMap

    describe "a non-empty list" $
      it "should return middle two elements of the non-empty list" $
        (nonEmptyList & optic *~ 10) `shouldBe` nonEmptyListTimes10

    describe "a non-empty map" $
      it "should return non-empty map reversed" $
        (nonEmptyMap & optic *~ 10) `shouldBe` nonEmptyMapTimes10

  describe "slice \"5:1:-1\" +~ 1" $ do
    let optic :: Traversable t => Traversal' (t a) a
        optic = sliced "5:1:-1"

    describe "an empty list" $
      it "should return an empty list" $
        (emptyList & optic +~ 1) `shouldBe` emptyList

    describe "an empty map" $
      it "should return an empty map" $
        (emptyMap & optic +~ 1) `shouldBe` emptyMap

    describe "a non-empty list" $
      it "should return middle two elements of the non-empty list" $
        (nonEmptyList & optic +~ 1) `shouldBe` [ 1, 3, 4, 5, 6, 6 ]

    describe "a non-empty map" $
      it "should return 1:5 reversed" $
        (nonEmptyMap & optic +~ 1) `shouldBe`
          (Map.fromList
            [ ("a",           0)
            , ("b",           10)
            , ("bazbif",      18)
            , ("bimbop",      33)
            , ("foobar",      213)
            , ("four hundred",400)
            ])

    describe "a tree" $
      it "should return the tree incremented in 5-1" $
        flatten (aTree & optic +~ 1) `shouldBe`
          [ 5, 5, 4, 3, 2, 0, 0, 1, 0, 0, 2, 1, 0, 0, 1, 0, 0, 3, 2, 1, 0
          , 0, 1, 0, 0, 2, 1, 0, 0, 1, 0, 0, 4, 3, 2, 1, 0, 0, 1, 0, 0, 2
          , 1, 0, 0, 1, 0, 0, 3, 2, 1, 0, 0, 1, 0, 0, 2, 1, 0, 0, 1, 0, 0
          ]

  describe "slice \"::\" %~ toUpper" $ do
    let optic :: Traversable t => Traversal' (t a) a
        optic = sliced "6:0:-1"

    describe "an empty string" $
      it "should return an empty string" $
        (emptyString & optic %~ toUpper) `shouldBe` emptyString

    describe "a non-empty string" $
      it "should return non-empty string uppered appropriately" $
        (nonEmptyString & optic %~ toUpper) `shouldBe` map toUpper nonEmptyString

  describe "partsOf (slice \"5:1:-1\") .~ ['A'..]" $ do
    let optic :: Traversable t => Traversal' (t a) a
        optic = sliced "5:1:-1"

    describe "an empty string" $
      it "should return an empty string" $
        (emptyString & partsOf optic .~ ['A'..]) `shouldBe` emptyString

    describe "a non-empty string" $
      it "should return the string with the alphabet backwards in the middle" $
        (nonEmptyString & partsOf optic .~ ['A'..]) `shouldBe` "fDCBAr"

    describe "a tree" $
      it "should end up with a tree full of 100's somewhere" $
        flatten (aTree & partsOf optic .~ repeat 100) `shouldBe`
          [ 5, 100, 100, 100, 100, 0, 0, 1, 0, 0, 2, 1, 0, 0, 1, 0, 0
          , 3, 2, 1, 0, 0, 1, 0, 0, 2, 1, 0, 0, 1, 0, 0, 4, 3, 2, 1, 0
          , 0, 1, 0, 0, 2, 1, 0, 0, 1, 0, 0, 3, 2, 1, 0, 0, 1, 0, 0, 2
          , 1, 0, 0, 1, 0, 0
          ]

  describe "Tuple of three Maybe Ints" $ do
    let optic :: (Maybe Int, Maybe Int, Maybe Int)
        optic = (Just 5, Nothing, Just (-1))

    it "should work as well" $
      "Slice of Py" ^.. sliced optic `shouldBe` "ecilS"

  describe "Tuple of three Ints" $ do
    let optic :: (Int, Int, Int)
        optic = (2, (-2), 2)

    it "should work as well" $
      "Slice of Py" ^.. sliced optic `shouldBe` "ieo "

aTree :: Tree Int
aTree = unfoldTree buildExampleTree 5
  where
    buildExampleTree x = (x, filter (>=0) [x-1, x-1])

emptyList :: [Int]
emptyList = []

emptyMap :: Map String Int
emptyMap = mempty

emptyString :: String
emptyString = ""

nonEmptyList :: [Int]
nonEmptyList = [1..6]

nonEmptyListTimes10 :: [Int]
nonEmptyListTimes10 = fmap (*10) nonEmptyList

nonEmptyMap :: Map String Int
nonEmptyMap = Map.fromList
  [ ("foobar"      , 212)
  , ("bazbif"      , 17)
  , ("bimbop"      , 32)
  , ("four hundred", 400)
  , ("a"           , 0)
  , ("b"           , 9)
  ]

nonEmptyMapTimes10 :: Map String Int
nonEmptyMapTimes10 = fmap (*10) nonEmptyMap

nonEmptyString :: String
nonEmptyString = "foobar"
