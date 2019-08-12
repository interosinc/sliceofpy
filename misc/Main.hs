{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens    ( makeLenses
                       , toListOf
                       , view
                       )
import Data.Char       ( isSpace )
import Data.List       ( dropWhileEnd
                       , partition
                       )
import Data.Slice.Lens ( sliced )
import System.Process  ( proc
                       , readCreateProcess
                       )

data Result = Result
  { _sliceStr :: String
  , _pyResult :: String
  , _hsResult :: String
  } deriving (Eq, Ord, Read, Show)

makeLenses ''Result

exampleStr :: String
exampleStr = "Slice of Py"

main :: IO ()
main = mapM execute examples >>= writeExamples

examples :: [String]
examples =
  [ "::"
  , ":3"
  , "3:"
  , "::2"
  , "::-1"
  , "::-2"
  , "2:-2"
  , "1:2"
  , "2:1"
  , "2:1:-1"
  , "1:-1"
  , "1:2:-1"
  , "2::-1"
  , "2::-2"
  , "10::-2"
  , "11::-2"
  , "0:9"
  , "0:10"
  , "0:11"
  , "0:12"
  , "12:0:-1"
  , "11:0:-1"
  , "10:0:-1"
  , "9:0:-1"
  ]

execute :: String -> IO Result
execute slice = Result slice <$> runPython slice <*> pure (runHaskell slice)

runHaskell :: String -> String
runHaskell slice = trim $ toListOf (sliced slice) exampleStr

runPython :: String -> IO String
runPython slice = trim <$> readCreateProcess (proc "python3" [])
  (unlines [ str, "quit()" ])
  where
    str = "print(\"" ++ exampleStr ++ "\"[" ++ slice ++ "])"

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

writeExamples :: [Result] -> IO ()
writeExamples executed = do
  writeFile "misc/same.md" (makeTable same)
  writeFile "misc/diff.md" (makeTable diff)
  where
    (same, diff) = partition match executed

match :: Result -> Bool
match = (==) <$> view pyResult <*> view hsResult

makeTable :: [Result] -> String
makeTable results = unlines $ header:divider:rows
  where
    header   = wrap " Python" " Haskell"
    divider  = wrap line line
    rows     = map (init . unlines . map (uncurry wrap) . uncurry zip) rendered
    rendered = map render results
    line     = replicate w '-'
    w        = 1 + maximum (8:map maxWidth rendered)
    wrap x y = concat ["|", pad x, "|", pad y, "|"]
    pad s    = s ++ replicate (w - length s) ' '

    render :: Result -> ([String], [String])
    render r = ( [ pyCall (view sliceStr r) ++ "<br>"
                   ++  " \"" ++ view pyResult r ++ q
                 ]
               , [ hsCall (view sliceStr r) ++ "<br>"
                   ++ " \"" ++ view hsResult r ++ q
                 ]
               )

pyCall :: String -> String
pyCall s = " >>> " ++ q ++ exampleStr ++ q ++ "[" ++ s ++ "]"

hsCall :: String -> String
hsCall s = " λ " ++ q ++ exampleStr ++ q ++ " ^.. sliced " ++ q ++ s ++ q

maxWidth :: ([String], [String]) -> Int
maxWidth (x, y) = maximum $ map length x ++ map length y

q :: String
q = "\""
