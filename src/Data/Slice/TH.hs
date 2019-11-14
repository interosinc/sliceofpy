{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Slice.TH where

import Data.Slice.Lens.Internal   ( tupleSliceFromString )
import Data.String                ( IsString(..) )
import Language.Haskell.TH.Syntax ( Q
                                  , TExp(..)
                                  )
import Language.Haskell.TH.Syntax ( Exp )

-- TODO use a different type to avoid orphan instances / conflicts
instance IsString (Q (TExp (Maybe Int, Maybe Int, Maybe nt))) where
  fromString = fmap TExp . qify . tupleSliceFromString

qify :: (Maybe Int, Maybe Int, Maybe Int) -> Q Exp
qify x = [| pure x |]

-- instance IsString (Q (TExp (Path Rel Dir))) where
--   fromString = fmap TExp . mkRelDir
-- instance IsString (Q (TExp (Path Abs Dir))) where
--   fromString = fmap TExp . mkAbsDir
-- instance IsString (Q (TExp (Path Rel File))) where
--   fromString = fmap TExp . mkRelFile
-- instance IsString (Q (TExp (Path Abs File))) where
--   fromString = fmap TExp . mkAbsFile
