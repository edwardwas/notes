Singletons
===========

Although GHC supports some type level programing, types are erased at run time. This is a right pain in the arse, meaning some things are impossible or difficult to do without hacks. The singleton pattern is one such hack. Let's take a definition Peano numbers.

```Haskell
data Peano = Z | S Peano
```

Here, `Z` represents 0 and `S` represents adding 1 to a number. Using DataKinds, we can use this definition on the type level and index types by a number. Let's do that now to produce a Sized Vector.

```Haskell
data SVector (n :: Peano) (a :: *) where
  EmptyVector :: SVector Z a
  (:*:) :: a -> SVector n a -> SVector (S n) a

infixr 5 :*:

instance Functor (SVector n) where
  fmap _ EmptyVector = EmptyVector
  fmap f (a :*: as) = f a :*: (f <$> as)
```

`SVector n a` is a vector of elements `a` with exactly `n` elements. We can see that we can write a nice Functor instance, but it would be nice if we could write and applicative one as well. `SVector` is similar too a list, so there are two applicative instances we could choose: either the default Haskell one or a ZipList. However, the first requires  a change in the length of the list, so we choose the second. `pure` corresponds to a list of a simple element, and `<*>` to combining the vectors element by element.

```haskell
instance Applicative (SVector n) where
  EmptyVector <*> EmptyVector = EmptyVector
  (f :*: fs) <*> (a :*: as) = f a :*: (fs <*> as)
  pure a = ???
```

What do we write for the definition of `pure`? It depends of the value of `n` which we can't know in the body of pure. Without singletons, we can write it as follows.

```haskell
instance Applicative (SVector Z) where
  EmptyVector <*> EmptyVector = EmptyVector
  pure _ = EmptyVector

instance Applicative (SVector n) => Applicative (SVector (S n)) where
  (f :*: fs) <*> (a :*: as) = f a :*: (fs <*> as)
  pure a = a :*: pure a
```

Although this works, it has a problem if we ever want to use the instance. Although we have written an instance for every possible value of `n`, the compiler has no way of knowing this. If we wish to use the instance we must provide an `Applicative (SVector n)` instance. However, there is another way.

Singletons provides a data family `Sing`. `Sing` has a kind `k -> *`, so it produces a concrete type from a kind. We can define `Sing` for our `Peano` as follows.

```haskell
data instance Sing (a :: Peano) where
  SZ :: a ~ Z' => Sing a
  SS :: a ~ S' n => Sing n -> Sing a
```

At first glance this appears pretty pointless. We've expressed the same information as `Peano`, but in a more confusing way. However, it now allows us to pattern match on this new information and recover type information. We have, in a way, conserved type information at run time. Let's write a better pure.

```haskell
pureHelper :: Sing n -> a -> SVector n a
pureHelper SZ _ = EmptyVector
pureHelper (SS s) a = a :*: pureHelper s a
```

Here, we pattern match on the `Sing` to bring type level information to the value level. When we match on `SZ`, we know that we must be in the case where `n ~ Z`, so we can treat it appropriately.

The last part of out pure puzzle is the `SingI` class. This is defined as

```haskell
class SingI (a :: k) where
  sing :: Sing a
```

It allows us to get a `Sing` easily. We can define it for our `Peano` as

```haskell
instance SingI Z where
  sing = SZ

instance SingI n => SingI (S n) where
  sing = SS sing
```

Singletons introduces the function `withSing :: SingI a => (Sing a -> b) -> b`. Using this, we can finally write our Applicative instance

```haskell
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
```

`helper` in `pure` is `pureHelper`, and we use `withSing` to generate the `Sing`. Not the type signature: we have to use a different variable for the size as opposed to the top level as we are recursing down. Otherwise, we would have to proved that all n have a `SingI` instance. This is why our definition for `<*>` has to use a helper function as well.

Defining all of out singleton definitions can become painful, so there is a template haskell function provided. If we write our peano definition as follows, it is all derived for us.

```hasekll
$(singletons [d|
  data Peano = Z | S Peano
  |])
```

Equality and coercion
---------

Sometimes we wish to check for equality on the type level and run time. Run time type erasure can make this very tricky, but luckily singletons is here to help us.

Let's define a function which allows us to expand out a vector to a new size, padding with a supplied value.

```haskell
expand :: SVector n a -> a -> SVector m a
```

Although we can define this by hand easily enough, it will be hard to convince the compiler. To do so, we need to use type level equality. We redefine `Peano` as follows. 

```hasekll
$(singletons [d|
  data Peano = Z | S Peano
      deriving (Eq,Show,Ord)
  |])
```

By defining the Eq and Ord instances inside the singletons QuasiQuote, singletons derives promoted (`POrd` and `PEq`) and singleton (`SEq`, `SDecide` and `SOrd`) instances for us automatically. Promoted instances work on type level instances and singleton versions on the Sing type. The type we're interested in is `SDecide`, which allows us to prove to the compiler than two sings are equal.

`SDecide` is defined as follows.

```haskell 
class SDecide k where
  (%~) :: Sing (a :: k) -> Sing (b :: k) -> Decision (a :~: b)
```

Note that this a *kind* class: it works of a kind (such as \'Peano) not a type (such as Peano). Decision is defined as

```haskell
data Decision a = Proved a | Disproved (Refuted a)
type Refuted a = a -> Void
```

`Decision` either produces a proof of something, or a function that make `Void`. Finally, we can look at `(:~:)`. `(:~:)` is a witness to the proof that two types are equal. We can use the constructor `Refl` to get `a :~: a`, or proof that the same two type are equal. `Data.Type.Equality` contains a number of functions to manipulate these such as`apply :: (f :~: g) -> (a :~: b) -> f a :~: g b`. These can be used to coerce types that are equal with `repr :: (a :~: b) -> Coercion a b` and `coerceWith :: Coercion a b -> a -> b`. We can now write out expand

```haskell
expand :: forall m n a . (SingI m, SingI n) 
  => SVector n a
  -> a
  -> SVector m a
expand v a = case (sing :: Sing n) %~ (sing :: Sing m) of
  Disproved _ -> expand (a :*: v) a
  Proved equal -> coerceWith (rep $ apply (apply Refl equal)) v
```
