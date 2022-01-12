{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- |
-- Module      : Data.Boltzmann.Specifiable
-- Description :
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.Specifiable
  ( SpecifiableType (..),
    TypeDef,
    Cons (..),
    Specifiable (..),
    listTypeName,
  )
where

import GHC.Generics
  ( C1,
    Constructor (conName),
    D1,
    Datatype (datatypeName, moduleName),
    Generic (Rep, from),
    K1,
    R,
    S1,
    U1,
    type (:*:),
    type (:+:),
  )
import Language.Haskell.TH.Syntax (Lift)

-- | Specifiable, algebraic type definitions given
--   as a list of respective type constructors.
type TypeDef = [Cons]

data Cons = Cons
  { name :: String,
    args :: [SpecifiableType]
  }
  deriving (Eq, Show, Lift)

data SpecifiableType
  = forall a. (Specifiable a) => SpecifiableType a

instance Show SpecifiableType where
  show (SpecifiableType x) = typeName x

instance Eq SpecifiableType where
  x == y = show x == show y

instance Ord SpecifiableType where
  x <= y = show x <= show y

instance Lift SpecifiableType

class GWithTypeName f where
  gtypeName :: f a -> String

instance (Datatype d) => GWithTypeName (D1 d f) where
  gtypeName x = moduleName x ++ "." ++ datatypeName x

class Specifiable a where
  typedef :: a -> TypeDef
  default typedef :: (Generic a, GSpecifiable (Rep a)) => a -> TypeDef
  typedef = gtypedef . from

  typeName :: a -> String
  default typeName :: (Generic a, GWithTypeName (Rep a)) => a -> String
  typeName = gtypeName . from

bracket :: String -> String
bracket s = "[" ++ s ++ "]"

listTypeName :: Specifiable a => a -> String
listTypeName = bracket . typeName

-- handle lists.
instance {-# OVERLAPS #-} (Generic a, Specifiable a) => Specifiable [a] where
  typeName _ = "[" ++ typeName (undefined :: a) ++ "]"

class GSpecifiable f where
  gtypedef :: f a -> TypeDef

instance (Datatype d, GSpecifiable' f) => GSpecifiable (D1 d f) where
  gtypedef x = gtypedef' (moduleName x) (undefined :: f a)

type ModuleName = String

class GSpecifiable' f where
  gtypedef' :: ModuleName -> f a -> TypeDef

instance (GSpecifiable' f, GSpecifiable' g) => GSpecifiable' (f :+: g) where
  gtypedef' moduleName (_ :: (f :+: g) a) =
    gtypedef' moduleName (undefined :: f a) ++ gtypedef' moduleName (undefined :: g a)

instance (Constructor c, GConsArg f) => GSpecifiable' (C1 c f) where
  gtypedef' moduleName x = [Cons (qualifier ++ conName x) (gargs (undefined :: f a))]
    where
      qualifier = moduleName ++ "."

class GConsArg f where
  gargs :: f a -> [SpecifiableType]

instance GConsArg U1 where
  gargs _ = []

instance (Specifiable a) => GConsArg (K1 R a) where
  gargs _ = [SpecifiableType (undefined :: a)]

instance (GConsArg f) => GConsArg (S1 s f) where
  gargs _ = gargs (undefined :: f a)

instance (GConsArg f, GConsArg g) => GConsArg (f :*: g) where
  gargs (_ :: (f :*: g) a) =
    gargs (undefined :: f a) ++ gargs (undefined :: g a)
