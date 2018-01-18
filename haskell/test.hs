{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

import           Data.Singletons.Prelude
import           Data.Singletons.TH
import           Data.Type.Coercion
import           Data.Type.Equality
import           GHC.TypeLits

$(singletons [d|
  data Peano = Z | S Peano
      deriving (Eq,Ord)
  |])

data SVector (n :: Peano) (a :: *) where
  EmptyVector :: SVector Z a
  (:*:) :: a -> SVector n a -> SVector (S n) a

infixr 5 :*:

instance Show a => Show (SVector n a) where
  show EmptyVector = "EmptyVector"
  show (a :*: as)  = show a ++ " :*: " ++ show as

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

expand ::
     forall m n a. (SingI m, SingI n, (n :<= m) ~ True)
  => SVector n a
  -> a
  -> SVector m a
expand v a =
  let helper :: forall x . SingI x => SVector x a -> SVector m a
      helper vec =
        case (sing :: Sing x) %~ (sing :: Sing m) of
          Disproved _ -> helper (a :*: vec)
          Proved eqal -> coerceWith (repr $ apply (apply Refl eqal) Refl) vec
  in helper v

t :: forall m n a . (SingI n, SingI m) => SVector n a -> a -> Maybe (SVector m a)
t v a = case (sing :: Sing n) %:<= (sing :: Sing m) of
  SFalse -> Nothing
  STrue  -> Just $ expand v a

