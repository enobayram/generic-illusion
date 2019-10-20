{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenericIllusion
       ( MergeRecords (..)
       ) where

import GHC.Generics
import           Data.Coerce                    ( Coercible, coerce )

data MergeRecords a b = MR a b

type GIMetaData = 'MetaData "Generic_Illusion" "Generic_Illusion" "Generic_Illusion" 'False
type GIMetaCons = 'MetaCons "MR" 'PrefixI 'True

type SimpleRecord a = (Generic a, Coercible (Rep a) (Strip (Rep a)))

instance (SimpleRecord a, SimpleRecord b) => Generic (MergeRecords a b) where
    type Rep (MergeRecords a b) =
      D1 GIMetaData (C1 GIMetaCons (Strip (Rep a) :*: (Strip (Rep b))))

    from :: forall x. MergeRecords a b -> Rep (MergeRecords a b) x
    from (MR a b) = coerce
        $ (coerce $ (from a :: Rep a x) :: Strip (Rep a) x)
      :*: (coerce $ (from b :: Rep b x) :: Strip (Rep b) x)

    to :: forall x. Rep (MergeRecords a b) x -> MergeRecords a b
    to (M1 (M1 (ra :*: rb))) = MR
      (to (coerce ra :: Rep a x))
      (to (coerce rb :: Rep b x))

type family Strip (r :: * -> *) :: * -> * where
  Strip (D1 _ (C1 _ fields)) = fields
