{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Slice.QQ.Internal where

import Data.Slice.Parser         ( parseSlice )
import Language.Haskell.TH.Quote ( QuasiQuoter( QuasiQuoter )
                                 , quoteDec
                                 , quoteExp
                                 , quotePat
                                 , quoteType
                                 )

s :: QuasiQuoter
s = QuasiQuoter
  { quoteExp  = handle
      (\exp' -> [| (exp' :: (Maybe Int, Maybe Int, Maybe Int)) |])
  , quotePat  = unsupported "pattern"
  , quoteDec  = unsupported "declaration"
  , quoteType = unsupported "type"
  }

sd :: QuasiQuoter
sd = QuasiQuoter
  { quoteExp  = handle
      (\exp' -> [| sliced (exp' :: (Maybe Int, Maybe Int, Maybe Int)) |])
  , quotePat  = unsupported "pattern"
  , quoteDec  = unsupported "declaration"
  , quoteType = unsupported "type"
  }

handle :: Monad m => ((Maybe Int, Maybe Int, Maybe Int) -> m a) -> String -> m a
handle handler str = case parseSlice str of
  Just (_, _, Just 0) -> fail "Invalid step: 0"
  Just exp'           -> handler exp'
  Nothing             -> fail "invalid slice expr"

unsupported :: Monad m => String -> m a
unsupported ctx = fail
  $ "Unsupported operation: this QuasiQuoter can not be used in a "
    ++ ctx ++ " context."
