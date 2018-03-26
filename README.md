# `hashcons`: Hash-consing and memoisation for Haskell

This library provides hash-consing (a.k.a. interning) and memoisation, with a
mostly-clean interface. This library does all the dirty tricks so you don't have
to.


## Quick start

1. Make instances of `HashCons` (and its superclasses `Eq` and `Hashable`) for
   your types. Be aware that `HashCons` instances can't have type variables or
   contexts.
2. Wrap types which might have large values (`Text`, ASTs, etc.) in `HC`.
3. Use `memo` (`memo2`, `memo3`, `memo4`) to memoise functions.


## Tutorial

Imagine you have some recursive datatype like this one:

```haskell
type Id = Text

data Expr =
    Var !Id
  | App !Expr !Expr
  | Lam !Id   !Expr
  deriving (Eq, Generic, Hashable)
```

Clearly, checking equality of an expression might require traversing the whole
tree. We may also have duplicates of large data structures taking up lots of
memory.

### Hash-consing

We can solve both of these problems at once by storing all `Expr` values in a
global table and tagging them, so that equality of tags coincides with equality
of the values. This is what the `HC` type does. Using `HC` requires the inner
type to be an instance of `HashCons`, which in turn requires `Eq` and `Hashable`

```haskell
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, PatternSynonyms, ViewPatterns #-}

import Data.HashCons (HashCons, HC, hc, getVal)

type Id = HC Text

type Expr = HC Expr'

data Expr' =
    Var' !Id
  | App' !Expr !Expr
  | Lam' !Id   !Expr
  deriving (Eq, Generic, Hashable, HashCons)

{-# COMPLETE Var, App, Lam #-}
pattern Var :: Id -> Expr
pattern Var x <- Var' (getVal -> x) -- getVal :: HC a -> a
  where Var x =  Var' $ hc x        -- hc     :: HashCons a => a -> HC a
-- and similarly for App & Lam
```

Equality of `Expr` (i.e., `HC Expr'`) now amounts to checking the equality of
two `Int`s, and likewise for hashing. Notice also that the subterms of an
`Expr'` are individually hash-consed, so that:

- the benefits of hash-consing are kept while traversing a term;
- constructing an `Expr` only requires hashing at most three atoms—one for the
  constructor, and one for each field—rather than a whole tree.

### Memoisation

Now you've started to use your datatype in some recursive functions:

```haskell
subst :: Expr -> Id -> Expr -> Expr
subst a@(Var y)   x e = if x == y then e else a
subst   (App s t) x e = App (subst s x e) (subst t x e)
subst a@(Lam y s) x e = if x == y then a else Lam y (subst s x e)
```

You can also use this library to _memoise_ functions. This keeps the results of
previous function calls, so they can be reused. Memoising a function is simple:

```haskell
import Data.HashCons.Memo (memo3)

subst :: Expr -> Id -> Expr -> Expr
subst = memo3 $ \a x e -> case a of
  Var y   -> if x == y then e else a
  App s t -> App (subst s x e) (subst t x e)
  Lam y s -> if x == y then a else Lam y (subst s x e)
```

Functions `memo`, `memo2`, up to `memo4` are provided by the library. Functions
with higher numbers of arguments can be memoised by chaining the existing
functions.

The type of `memo` might look a little strange:

```haskell
memo :: (MemoArg a, CanFinalize a ~ True) => (a -> b) -> a -> b
```

The memo table uses finalisers to prune old entries from the memo table. The
type family `CanFinalize`, part of `MemoArg`, is used to ensure that the
argument actually can run finalisers reliably, since most datatypes can't. (The
only data type currently declared to run finalisers is `HC`.) If you don't want
this check, and don't mind if the memo table continues to grow forever, you can
use the `uncheckedMemo` family of functions, which doesn't care about the value
of `CanFinalize`.

The `memoN` functions only check the first argument—as long as it can run
finalisers, the table will be pruned.


## Requested contributions

I didn't really know what instances of `HashCons` would be useful. If you need
an instance for a type from a package this one depends on, open an issue.
