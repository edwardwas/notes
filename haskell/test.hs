{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

import           Data.Singletons.TH

$(singletons [d|
  data Peano = Z | S Peano
  |])

data SVector (n :: Peano) (a :: *) where
  EmptyVector :: SVector Z a
  (:*:) :: a -> SVector n a -> SVector (S n) a

infixr 5 :*:

instance Functor (SVector n) where
  fmap _ EmptyVector = EmptyVector
  fmap f (a :*: as)  = f a :*: (f <$> as)

instance SingI n => Applicative (SVector n) where
    pure =
        let helper :: Sing x -> a -> SVector x a
            helper SZ _     = EmptyVector
            helper (SS n) a = a :*: helper n a
        in withSing helper
    fVec <*> aVec =
        let helper :: SVector x (a -> b) -> SVector x a -> SVector x b
            helper EmptyVector EmptyVector = EmptyVector
            helper (f :*: fs) (a :*: as)   = f a :*: helper fs as
        in helper fVec aVec
