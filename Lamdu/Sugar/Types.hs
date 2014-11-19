{-# LANGUAGE KindSignatures, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, RankNTypes, DeriveGeneric, TypeFamilies #-}
module Lamdu.Sugar.Types
  ( Definition(..), drName, drGuid, drBody
  , DefinitionBody(..), _DefinitionBodyExpression, _DefinitionBodyBuiltin
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions
  , DefinitionExpression(..), deContent, deTypeInfo
  , AcceptNewType(..)
  , DefinitionTypeInfo(..)
    , _DefinitionExportedTypeInfo
    , _DefinitionNewType
  , DefinitionContent(..)
    , dDepParams, dParams, dBody, dWhereItems, dAddFirstParam, dAddInnermostWhereItem
  , DefinitionBuiltin(..)
  , WrapAction(..)
  , SetToHole(..), _SetToHole, _AlreadyAHole
  , SetToInnerExpr(..), _SetToInnerExpr, _NoInnerExpr
  , Actions(..)
    , wrap, setToHole, setToInnerExpr, cut
  , Body(..)
    , _BodyLam, _BodyApply, _BodyGetVar, _BodyGetField, _BodyHole
    , _BodyCollapsed, _BodyLiteralInteger
    , _BodyList, _BodyRecord
  , Payload(..), plGuid, plInferredType, plActions, plData
  , ExpressionP(..), rBody, rPayload
  , NameSource(..), NameCollision(..), Name(..), MStoredName
  , DefinitionN, DefinitionU
  , Expression, ExpressionN
  , BodyN
  , WhereItem(..), wiValue, wiGuid, wiName, wiActions, wiInferredType
  , ListItem(..), liMActions, liExpr
  , ListActions(..), List(..)
  , RecordField(..), rfMItemActions, rfTag, rfExpr
  , Record(..), rItems, rMAddFirstItem
  , GetField(..), gfRecord, gfTag
  , GetVarType(..)
  , GetVar(..), gvIdentifier, gvName, gvJumpTo, gvVarType
  , GetParams(..), gpDefGuid, gpDefName, gpJumpTo
  , SpecialArgs(..), _NoSpecialArgs, _ObjectArg, _InfixArgs
  , AnnotatedArg(..), aaTag, aaTagExprGuid, aaExpr
  , Apply(..), aFunc, aSpecialArgs, aAnnotatedArgs
  , Lam(..), lParam, lResult
  , FuncParamType(..)
  , FuncParam(..)
    , fpName, fpGuid, fpId, fpAltIds, fpVarKind, fpInferredType, fpMActions
  , Unwrap(..), _UnwrapMAction, _UnwrapTypeMismatch
  , HoleArg(..), haExpr, haExprPresugared, haUnwrap
  , HoleInferred(..), hiSuggestedValue, hiType, hiMakeConverted
  , Hole(..)
    , holeMActions, holeMArg, holeInferred
  , ScopeItem
  , Scope(..), scopeLocals, scopeGlobals, scopeTags, scopeGetParams
  , HoleActions(..)
    , holeScope, holePaste, holeInferExprType
  , HoleResult(..)
    , holeResultInferred
    , holeResultConverted
    , holeResultPick
    , holeResultHasHoles
  , PickedResult(..), prMJumpTo, prIdTranslation
  , TagG(..), tagGName, tagVal, tagInstance
  , Collapsed(..), cFuncGuid, cCompact, cFullExpression, cFullExprHasInfo
  , MStorePoint, ExprStorePoint
  -- Input types:
  , InputPayload(..), ipGuid, ipInferred, ipStored, ipData
  , InputExpr
  , Stored
  , NameProperty(..)
    , npName, npSetName

  , ReadOnly(..)
  , Writable(..)
  ) where

import Control.Applicative (Applicative(..))
import Data.Binary (Binary)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid(..))
import Data.Monoid.Generic (def_mempty, def_mappend)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Lamdu.Expr.Scheme (Scheme)
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val)
import Lamdu.Sugar.Types.Internal (T, Stored)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Types.Internal as TypesInternal
import qualified System.Random as Random

-- Stored expressions are Writable and have a Writable wrapper around
-- their mutating actions.

-- Fabricated expressions (e.g: Hole suggested results) are ReadOnly
-- and cannot be modified, so their mutating actions are wrapped in a
-- ReadOnly.

newtype Writable a = Writable { getWritable :: a } deriving (Functor, Foldable, Traversable)
data    ReadOnly a = ReadOnly   deriving (Functor, Foldable, Traversable)

instance Lens.Wrapped (Writable a) where
  type Unwrapped (Writable a) = a
  _Wrapped' = Lens.iso getWritable Writable

instance Applicative Writable where
  pure = Writable
  Writable f <*> Writable x = Writable (f x)

instance Applicative ReadOnly where
  pure _ = ReadOnly
  ReadOnly <*> ReadOnly = ReadOnly

data InputPayload rw m a
  = InputPayload
    { _ipGuid :: Guid
    , _ipInferred :: Infer.Payload
    , _ipStored :: rw (Stored m)
    , _ipData :: a -- TODO: Extract to tuple
    }
Lens.makeLenses ''InputPayload

type InputExpr rw m a = Val (InputPayload rw m a)

data WrapAction m
  = WrapperAlready -- I'm an apply-of-hole, no need to wrap
  | WrappedAlready Guid -- I'm an arg of apply-of-hole (Guid of apply), no need to wrap
  | WrapNotAllowed -- I'm already wrapped or a tag or a hole
  | WrapAction (T m Guid) -- Wrap me!

data SetToHole m
  = SetToHole (T m Guid)
  | AlreadyAHole -- or already an arg of one

data SetToInnerExpr m = SetToInnerExpr (T m Guid) | NoInnerExpr

data Actions m = Actions
  { _wrap :: WrapAction m
  , _setToHole :: SetToHole m
  , _setToInnerExpr :: SetToInnerExpr m
  , _cut :: T m Guid
  }

data Payload m a = Payload
  { _plInferredType :: Type
  -- This must be embedded in the expression AST and not as a separate
  -- function so that AddNames can correct the "name" here in the
  -- right context.
  , _plActions :: Maybe (Actions m)
  , _plGuid :: Guid
  , _plData :: a
  } deriving (Functor, Foldable, Traversable)

-- When fabricating a new hole result involving a stored argument,
-- this Maybe varies between Nothing and Just in the same expression
type MStorePoint m a = (Maybe (TypesInternal.StorePoint (Tag m)), a)

type ExprStorePoint m a = Val (MStorePoint m a)

data ExpressionP name m pl = Expression
  { _rBody :: Body name m (ExpressionP name m pl)
  , _rPayload :: pl
  } deriving (Functor, Foldable, Traversable)

data NameSource = NameSourceAutoGenerated | NameSourceStored
  deriving (Show)
data NameCollision = NoCollision | Collision {-Disambiguator:-} Int
  deriving (Show)
data Name = Name
  { nNameSource :: NameSource
  , nNameCollisionSuffix :: NameCollision
  , nName :: String
  } deriving (Show)

-- This funny type-alias exists because MStoredName is an extremely
-- common type parameter of virtually all sugar types. Using (Maybe
-- StoredName) or (Maybe String) directly adds a lot of noise.
type MStoredName = Maybe String

type Expression name m a = ExpressionP name m (Payload m a)
type ExpressionN m a = Expression Name m a

type BodyN m a = Body Name m (ExpressionN m a)

data ListItemActions m = ListItemActions
  { _itemAddNext :: T m Guid
  , _itemDelete :: T m Guid
  }

newtype FuncParamActions m = FuncParamActions
  { _fpListItemActions :: ListItemActions m
  }

data FuncParamType = FuncParameter | FuncFieldParameter

data NameProperty name m = NameProperty
  { _npName :: name
  , _npSetName :: String -> T m ()
  }

-- TODO:
-- FuncParam for lambda needs GetExample, but not ListItemActions
-- FuncParam for pi needs neither
-- FuncParam for definition needs both
-- So separate the types properly
data FuncParam name m = FuncParam
  { -- non-unique (e.g: tag guid). Name attached here:
    _fpGuid :: Guid
  , _fpId :: Guid
  , _fpAltIds :: [Guid]
  , _fpVarKind :: FuncParamType
  , _fpName :: NameProperty name m
  , _fpInferredType :: Type
  , _fpMActions :: Maybe (FuncParamActions m)
  }

data Lam name m expr = Lam
  { _lParam :: FuncParam name m
  , _lResult :: expr
  } deriving (Functor, Foldable, Traversable)

data PickedResult = PickedResult
  { _prMJumpTo :: Maybe Guid
  , -- pairs of ids from converted expression and written expression.
    _prIdTranslation :: [(Guid, Guid)]
  }

data HoleResult name m a = HoleResult
  { _holeResultInferred :: Val Infer.Payload
  , _holeResultConverted :: Expression name m a
  , _holeResultPick :: T m PickedResult
  , _holeResultHasHoles :: Bool
  } deriving (Functor, Foldable, Traversable)

type ScopeItem a = (a, Val ())

data Scope name m = Scope
  { _scopeLocals    :: [ScopeItem (GetVar name m)]
  , _scopeGlobals   :: [ScopeItem (GetVar name m)]
  , _scopeTags      :: [(TagG name m, T.Tag)]
  , _scopeGetParams :: [ScopeItem (GetParams name m)]
  } deriving (Generic)
instance Monoid (Scope name m) where
  mempty = def_mempty
  mappend = def_mappend

data HoleActions name m = HoleActions
  { _holeScope :: T m (Scope name m)
  , -- Infer expression "on the side" (not in the hole position),
    -- but with the hole's scope.
    -- If given expression does not type check on its own, returns Nothing.
    -- (used by HoleEdit to suggest variations based on type)
    _holeInferExprType :: Val () -> T m (Maybe Type)
  , holeResult ::
      forall a.
      (Binary a, Ord a, Monoid a) =>
      (Guid -> Random.StdGen) -> -- for consistent guids
      Val (Maybe (TypesInternal.StorePoint (Tag m)), a) ->
      T m (Maybe (HoleResult name m a))
  , _holePaste :: Maybe (T m Guid)
  }

data Unwrap m
  = UnwrapMAction (Maybe (T m Guid))
  | UnwrapTypeMismatch

data HoleArg m expr = HoleArg
  { _haExpr :: expr
  , _haExprPresugared :: ExprStorePoint m ()
  , _haUnwrap :: Unwrap m
  } deriving (Functor, Foldable, Traversable)

data HoleInferred name m = HoleInferred
  { _hiSuggestedValue :: Val ()
  , _hiType :: Type
  -- The Sugar Expression of the WithVarsValue
  , _hiMakeConverted :: Random.StdGen -> T m (Expression name m ())
  }

data Hole name m expr = Hole
  { _holeMActions :: Maybe (HoleActions name m)
  , _holeInferred :: HoleInferred name m
  , _holeMArg :: Maybe (HoleArg m expr)
  } deriving (Functor, Foldable, Traversable)

data Collapsed name m expr = Collapsed
  { _cFuncGuid :: Guid
  , _cCompact :: GetVar name m
  , _cFullExpression :: expr
    -- If the full expr has info (non-hole args) we want to leave it
    -- expanded:
  , _cFullExprHasInfo :: Bool
  } deriving (Functor, Foldable, Traversable)

-- TODO: Do we want to store/allow-access to the implicit type params (nil's type, each cons type?)
data ListItem m expr = ListItem
  { _liMActions :: Maybe (ListItemActions m)
  , _liExpr :: expr
  } deriving (Functor, Foldable, Traversable)

data ListActions m = ListActions
  { addFirstItem :: T m Guid
  , replaceNil :: T m Guid
  }

data List m expr = List
  { lValues :: [ListItem m expr]
  , lMActions :: Maybe (ListActions m)
  , -- Nil guid stays consistent when adding items.
    -- (Exposed for consistent animations)
    lNilGuid :: Guid
  } deriving (Functor, Foldable, Traversable)

data RecordField name m expr = RecordField
  { _rfMItemActions :: Maybe (ListItemActions m)
  , _rfTag :: TagG name m
  , _rfExpr :: expr -- field type or val
  } deriving (Functor, Foldable, Traversable)

data Record name m expr = Record
  { _rItems :: [RecordField name m expr]
  , _rMAddFirstItem :: Maybe (T m Guid)
  } deriving (Functor, Foldable, Traversable)

data GetField name m expr = GetField
  { _gfRecord :: expr
  , _gfTag :: TagG name m
  } deriving (Functor, Foldable, Traversable)

data GetVarType = GetDefinition | GetFieldParameter | GetParameter
  deriving (Eq, Ord)

data GetVar name m = GetVar
  { _gvIdentifier :: Guid
  , _gvName :: NameProperty name m
  , _gvJumpTo :: T m Guid
  , _gvVarType :: GetVarType
  }

data GetParams name m = GetParams
  { _gpDefGuid :: Guid
  , _gpDefName :: NameProperty name m
  , _gpJumpTo :: T m Guid
  }

data TagG name m = TagG
  { _tagInstance :: Guid -- Unique across different uses of a tag
  , _tagVal :: T.Tag
  , _tagGName :: NameProperty name m
  }

data SpecialArgs expr
  = NoSpecialArgs
  | ObjectArg expr
  | InfixArgs expr expr
  deriving (Functor, Foldable, Traversable)

data AnnotatedArg name m expr = AnnotatedArg
  { _aaTag :: TagG name m
  , -- Used for animation ids consistent with record.
    _aaTagExprGuid :: Guid
  , _aaExpr :: expr
  } deriving (Functor, Foldable, Traversable)

data Apply name m expr = Apply
  { _aFunc :: expr
  , _aSpecialArgs :: SpecialArgs expr
  , _aAnnotatedArgs :: [AnnotatedArg name m expr]
  } deriving (Functor, Foldable, Traversable)

data Body name m expr
  = BodyLam (Lam name m expr)
  | BodyApply (Apply name m expr)
  | BodyHole (Hole name m expr)
  | BodyCollapsed (Collapsed name m expr)
  | BodyLiteralInteger Integer
  | BodyList (List m expr)
  | BodyRecord (Record name m expr)
  | BodyGetField (GetField name m expr)
  | BodyGetVar (GetVar name m)
  | BodyGetParams (GetParams name m)
  deriving (Functor, Foldable, Traversable)

instance Show (FuncParam name m) where
  show fp =
    concat ["(", show (_fpGuid fp), ":", show (_fpInferredType fp), ")"]

instance Show expr => Show (Body name m expr) where
  show (BodyLam (Lam paramType resultType)) =
    "_:" ++ show paramType ++ " -> " ++ show resultType
  show BodyHole {} = "Hole"
  show BodyCollapsed {} = "Collapsed"
  show (BodyLiteralInteger i) = show i
  show (BodyList (List items _ _)) =
    concat
    [ "["
    , List.intercalate ", " $ map (show . _liExpr) items
    , "]"
    ]
  show BodyApply {} = "LabelledApply:TODO"
  show BodyRecord {} = "Record:TODO"
  show BodyGetField {} = "GetField:TODO"
  show BodyGetVar {} = "GetVar:TODO"
  show BodyGetParams {} = "GetParams:TODO"

data WhereItem name m expr = WhereItem
  { _wiValue :: DefinitionContent name m expr
  , _wiInferredType :: Type
  , _wiGuid :: Guid
  , _wiName :: NameProperty name m
  , _wiActions :: Maybe (ListItemActions m)
  } deriving (Functor, Foldable, Traversable)

-- Common data for definitions and where-items
data DefinitionContent name m expr = DefinitionContent
  { _dDepParams :: [FuncParam name m]
  , _dParams :: [FuncParam name m]
  , _dBody :: expr
  , _dWhereItems :: [WhereItem name m expr]
  , _dAddFirstParam :: T m Guid
  , _dAddInnermostWhereItem :: T m Guid
  } deriving (Functor, Foldable, Traversable)

data AcceptNewType m = AcceptNewType
  { antOldType :: Definition.ExportedType
  , antNewType :: Scheme
  , antAccept :: T m ()
  }

data DefinitionTypeInfo m
  = DefinitionExportedTypeInfo Scheme
  | DefinitionNewType (AcceptNewType m)

data DefinitionExpression name m expr = DefinitionExpression
  { _deTypeInfo :: DefinitionTypeInfo m
  , _deContent :: DefinitionContent name m expr
  } deriving (Functor, Foldable, Traversable)

data DefinitionBuiltin m = DefinitionBuiltin
  { biName :: Definition.FFIName
  , biSetName :: Definition.FFIName -> T m ()
  , biType :: Definition.ExportedType
  }

data DefinitionBody name m expr
  = DefinitionBodyExpression (DefinitionExpression name m expr)
  | DefinitionBodyBuiltin (DefinitionBuiltin m)
  deriving (Functor, Foldable, Traversable)

data Definition name m expr = Definition
  { _drGuid :: Guid
  , _drName :: NameProperty name m
  , _drBody :: DefinitionBody name m expr
  } deriving (Functor, Foldable, Traversable)

type DefinitionN m a = Definition Name m (Expression Name m a)
type DefinitionU m a = Definition MStoredName m (Expression MStoredName m a)

Lens.makeLenses ''Actions
Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''Apply
Lens.makeLenses ''Body
Lens.makeLenses ''Collapsed
Lens.makeLenses ''Definition
Lens.makeLenses ''DefinitionContent
Lens.makeLenses ''DefinitionExpression
Lens.makeLenses ''ExpressionP
Lens.makeLenses ''FuncParam
Lens.makeLenses ''FuncParamActions
Lens.makeLenses ''GetField
Lens.makeLenses ''GetParams
Lens.makeLenses ''GetVar
Lens.makeLenses ''Hole
Lens.makeLenses ''HoleActions
Lens.makeLenses ''HoleArg
Lens.makeLenses ''HoleInferred
Lens.makeLenses ''HoleResult
Lens.makeLenses ''Lam
Lens.makeLenses ''ListItem
Lens.makeLenses ''ListItemActions
Lens.makeLenses ''Payload
Lens.makeLenses ''PickedResult
Lens.makeLenses ''Record
Lens.makeLenses ''RecordField
Lens.makeLenses ''Scope
Lens.makeLenses ''TagG
Lens.makeLenses ''WhereItem
Lens.makeLenses ''NameProperty
Lens.makePrisms ''Body
Lens.makePrisms ''DefinitionBody
Lens.makePrisms ''DefinitionTypeInfo
Lens.makePrisms ''SpecialArgs
Lens.makePrisms ''Unwrap
Lens.makePrisms ''SetToHole
Lens.makePrisms ''SetToInnerExpr
