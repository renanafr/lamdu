{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.Expr.Utils
  ( makeApply
  , makePi, makeLambda, makeLam
  , pureApply
  , pureHole
  , pureRecord
  , pureLam
  , pureGetField
  , pureVLiteralInteger
  , pureIntegerType
  , pureTagType
  , pureType
  , pureExpr
  , randomizeExpr
  , randomizeParamIds
  , randomizeParamIdsG
  , randomizeExprAndParams
  , NameGen(..), onNgMakeName
  , randomNameGen, debugNameGen
  , matchBody, matchBodyA
  , matchExpr, matchExprG
  , subExprs, subExprsWithout
  , isDependentPi, exprHasGetVar
  , curriedFuncArguments
  , ApplyFormAnnotation(..), applyForms
  , recordValForm, structureForType
  , alphaEq
  , subst, substGetPar
  , showBodyExpr, showsPrecBodyExpr
  , isTypeConstructorType
  , addExprContexts
  , addBodyContexts
  , PiWrappers(..), piWrappersDepParams, piWrappersMIndepParam, piWrappersResultType
  , getPiWrappers
  , TypePosition(..), _Negative, _Positive
  , annotateTypePositions
  ) where

import Lamdu.Expr

import Control.Applicative (Applicative(..), liftA2, (<$>), (<$))
import Control.Lens (Context(..))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Lens.Utils (addListContexts, addTuple2Contexts, getPrism)
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State (evalState, state)
import Data.Map (Map)
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (Any)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable(..), sequenceA)
import Data.Void (absurd)
import System.Random (Random, RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Expr.Lens as ExprLens
import qualified System.Random as Random

data PiWrappers def par a = PiWrappers
  { _piWrappersDepParams :: [(par, Expr def par a)]
  , _piWrappersMIndepParam :: Maybe (par, Expr def par a)
  , _piWrappersResultType :: Expr def par a
  }
Lens.makeLenses ''PiWrappers

data NameGen par pl = NameGen
  { ngSplit :: (NameGen par pl, NameGen par pl)
  , ngMakeName :: par -> pl -> (par, NameGen par pl)
  }

onNgMakeName ::
  (NameGen par b ->
   (par -> a -> (par, NameGen par b)) ->
   par -> b -> (par, NameGen par b)) ->
  NameGen par a -> NameGen par b
onNgMakeName onMakeName =
  go
  where
    go nameGen =
      result
      where
        result =
          nameGen
          { ngMakeName =
            ngMakeName nameGen
            & Lens.mapped . Lens.mapped . Lens._2 %~ go
            & onMakeName result
          , ngSplit =
            ngSplit nameGen
            & Lens.both %~ go
          }

getPiWrappers :: Eq par => Expr def par a -> PiWrappers def par a
getPiWrappers expr =
  case expr ^? ExprLens.exprLam of
  Just (Lam KType param paramType resultType)
    | isDependentPi expr ->
      getPiWrappers resultType & piWrappersDepParams %~ (p :)
    | otherwise ->
        PiWrappers
        { _piWrappersDepParams = []
        , _piWrappersMIndepParam = Just p
        , _piWrappersResultType = resultType
        }
    where
      p = (param, paramType)
  _ -> PiWrappers [] Nothing expr

alphaEq :: (Ord par, Eq def) => Expr def par a -> Expr def par a -> Bool
alphaEq x y =
  isJust $ matchExpr
  ((const . const . Just) ())
  ((const . const) Nothing)
  x y

-- Useful functions:
substGetPar ::
  Eq par =>
  par ->
  Expr def par a ->
  Expr def par a ->
  Expr def par a
substGetPar from =
  subst (ExprLens.exprParameterRef . Lens.filtered (== from))

subst ::
  Lens.Getting Any (Expr def par a) b ->
  Expr def par a ->
  Expr def par a ->
  Expr def par a
subst lens to expr
  | Lens.has lens expr = to
  | otherwise = expr & eBody . traverse %~ subst lens to

recordValForm :: Expr a par () -> Maybe (Expr b par ())
recordValForm paramType =
  paramType ^? ExprLens.exprKindedRecordFields KType
  <&> replaceFieldTypesWithHoles
  where
    replaceFieldTypesWithHoles fields =
      fields
      & Lens.traversed . Lens._2 .~ pureHole
      & (ExprLens.pureExpr . _VRec . ExprLens.kindedRecordFields KVal # )

data ApplyFormAnnotation =
  Untouched | DependentParamAdded | IndependentParamAdded
  deriving Eq

addApply :: ann -> Expr a par ann -> Expr b par () -> Expr a par ann
addApply ann func paramType =
  Expr (makeApply func arg) ann
  where
    arg = ann <$ fromMaybe pureHole (recordValForm paramType)

-- Transform expression to expression applied with holes,
-- with all different sensible levels of currying.
applyForms :: Eq par => Expr a par () -> Expr b par () -> [Expr b par ApplyFormAnnotation]
applyForms exprType rawExpr
  | Lens.has (ExprLens.exprLam . lamKind . _KVal) expr = [expr]
  | otherwise = reverse withAllAppliesAdded
  where
    expr = Untouched <$ rawExpr
    withDepAppliesAdded =
      foldl (addApply DependentParamAdded) expr depParamTypes
    withAllAppliesAdded =
      scanl (addApply IndependentParamAdded) withDepAppliesAdded $
      indepParamTypes ++ assumeHoleIsPi
    depParamTypes = snd <$> depParams
    indepParamTypes = mNonDepParam ^.. Lens._Just . Lens._2
    assumeHoleIsPi
      | Lens.has ExprLens.exprHole resultType = [pureHole]
      | otherwise = []
    PiWrappers
      { _piWrappersDepParams = depParams
      , _piWrappersMIndepParam = mNonDepParam
      , _piWrappersResultType = resultType
      } = getPiWrappers exprType

structureForType :: Expr def par () -> Expr def par ()
structureForType =
  (eBody %~) $
  const (ExprLens.bodyHole # ())
  & Lens.outside (ExprLens.bodyKindedRecordFields KType) .~
    (ExprLens.bodyKindedRecordFields KVal # ) . (traverse . Lens._2 %~ structureForType)
  & Lens.outside (ExprLens.bodyKindedLam KType) .~
    (ExprLens.bodyKindedLam KVal # ) . (Lens._3 %~ structureForType)

randomizeExprAndParams ::
  (RandomGen gen, Random par, Random r, Ord par) =>
  gen -> Expr def par (r -> a) -> Expr def par a
randomizeExprAndParams gen = randomizeParamIds paramGen . randomizeExpr exprGen
  where
    (exprGen, paramGen) = Random.split gen

randomizeExpr :: (RandomGen gen, Random r) => gen -> Expr def par (r -> a) -> Expr def par a
randomizeExpr gen (Expr body pl) =
  (`evalState` gen) $ do
    r <- state random
    newBody <- body & traverse %%~ randomizeSubexpr
    return . Expr newBody $ pl r
  where
    randomizeSubexpr subExpr = do
      localGen <- state Random.split
      return $ randomizeExpr localGen subExpr

randomNameGen :: (Random par, RandomGen g) => g -> NameGen par dummy
randomNameGen g = NameGen
  { ngSplit = Random.split g & Lens.both %~ randomNameGen
  , ngMakeName = const . const $ random g & Lens._2 %~ randomNameGen
  }

debugNameGen :: NameGen Guid dummy
debugNameGen = ng names ""
  where
    names = (:[]) <$> ['a'..'z']
    ng [] _ = error "TODO: Infinite list of names"
    ng st@(l:ls) suffix =
      NameGen
      { ngSplit = (ng st "_0", ng st "_1")
      , ngMakeName = const . const $ (Guid.fromString (l++suffix), ng ls suffix)
      }

randomizeParamIds ::
  (Ord par, Random par, RandomGen g) => g -> Expr def par a -> Expr def par a
randomizeParamIds gen = randomizeParamIdsG id (randomNameGen gen) Map.empty $ \_ _ a -> a

randomizeParamIdsG ::
  Ord par =>
  (a -> n) ->
  NameGen par n -> Map par par ->
  (NameGen par n -> Map par par -> a -> b) ->
  Expr def par a -> Expr def par b
randomizeParamIdsG preNG gen initMap convertPL =
  (`evalState` gen) . (`runReaderT` initMap) . go
  where
    go (Expr v s) = do
      parMap <- Reader.ask
      newGen <- lift $ state ngSplit
      (`Expr` convertPL newGen parMap s) <$>
        case v of
        VAbs (Lam k oldParamId paramType body) -> do
          newParamId <- lift . state $ makeName oldParamId s
          fmap VAbs $ liftA2 (Lam k newParamId) (go paramType) .
            Reader.local (Map.insert oldParamId newParamId) $ go body
        VLeaf (VVar (ParameterRef par)) ->
          pure $ ExprLens.bodyParameterRef #
          fromMaybe par (Map.lookup par parMap)
        x@VLeaf {}     -> traverse go x
        x@VApp {}      -> traverse go x
        x@VGetField {} -> traverse go x
        x@VRec {}      -> traverse go x
    makeName oldParamId s nameGen =
      ngMakeName nameGen oldParamId $ preNG s

matchLeaf ::
  Eq def =>
  (p -> q -> r) ->
  Leaf def p ->
  Leaf def q ->
  Maybe (Leaf def r)
matchLeaf matchGetPar leaf0 leaf1 =
  case (getPrism ExprLens.parameterRef leaf0, getPrism ExprLens.parameterRef leaf1) of
  (Left p0, Left p1) -> Just $ ExprLens.parameterRef # matchGetPar p0 p1
  (Right x, Right y)
    | x == y -> Just $ fmap absurd x
  _ -> Nothing

{-# INLINE matchBody #-}
matchBody ::
  Eq def =>
  (p -> q -> a -> b -> (r, c)) ->  -- ^ Match lam param ids and results
  (a -> b -> c) ->                 -- ^ Match same-scoped subexpressions
  (p -> q -> r) ->                 -- ^ Match get-params
  Body def p a ->
  Body def q b ->
  Maybe (Body def r c)
matchBody matchLam matchSubexpr matchGetPar body0 body1 =
  case body0 of
  VAbs (Lam k0 p0 pt0 r0) -> do
    Lam k1 p1 pt1 r1 <- body1 ^? _VAbs
    guard $ k0 == k1
    let (p, r) = matchLam p0 p1 r0 r1
    Just . VAbs $ Lam k0 p (matchSubexpr pt0 pt1) r
  VApp (Apply f0 a0) -> do
    Apply f1 a1 <- body1 ^? _VApp
    Just . VApp $ Apply (matchSubexpr f0 f1) (matchSubexpr a0 a1)
  VRec (Record k0 fs0) -> do
    Record k1 fs1 <- body1 ^? _VRec
    guard $ k0 == k1
    matchedPairs <- sequenceA =<< ListUtils.match matchPair fs0 fs1
    Just . VRec $ Record k0 matchedPairs
    where
      matchPair (t0, v0) (t1, v1)
        | t0 == t1 = Just (t0, matchSubexpr v0 v1)
        | otherwise = Nothing
  VGetField (GetField r0 t0) -> do
    GetField r1 t1 <- body1 ^? _VGetField
    guard $ t0 == t1
    Just . VGetField $ GetField (matchSubexpr r0 r1) t0
  VLeaf leaf0 -> do
    leaf1 <- body1 ^? _VLeaf
    VLeaf <$> matchLeaf matchGetPar leaf0 leaf1

{-# INLINE matchBodyA #-}
matchBodyA ::
  (Applicative f, Eq def) =>
  (p -> q -> a -> b -> (f r, f c)) ->  -- ^ Lam/Pi result match
  (a -> b -> f c) ->                   -- ^ Ordinary structural match (Apply components, param type)
  (p -> q -> f (Maybe r)) ->           -- ^ Match ParameterRef's
  Body def p a -> Body def q b -> f (Maybe (Body def r c))
matchBodyA matchLamResult matchOther matchGetPar body0 body1 =
  matchBody matchLam' matchOther matchGetPar body0 body1
  <&> ExprLens.bodyNTraverse pure id id & sequenceA
  <&> (>>= Lens.sequenceAOf ExprLens.bodyPar)
  where
    matchLam' =
      matchLamResult
      & Lens.mapped . Lens.mapped . Lens.mapped . Lens.mapped .
        Lens._1 . Lens.mapped %~ Just

-- The returned expression gets the same guids as the left
-- expression
{-# INLINE matchExpr #-}
matchExpr ::
  (Ord par, Eq def, Applicative f) =>
  (a -> b -> f c) ->
  (Expr def par a -> Expr def par b -> f (Expr def par c)) ->
  Expr def par a -> Expr def par b -> f (Expr def par c)
matchExpr = matchExprG . const . const $ pure ()

{-# INLINE matchExprG #-}
matchExprG ::
  (Ord par, Eq def, Applicative f) =>
  (par -> par -> f ()) -> -- ^ Left expr par overrides right expr par
  (a -> b -> f c) ->
  (Expr def par a -> Expr def par b -> f (Expr def par c)) ->
  Expr def par a -> Expr def par b -> f (Expr def par c)
matchExprG overridePars onMatch onMismatch =
  go Map.empty
  where
    go scope e0@(Expr body0 pl0) e1@(Expr body1 pl1) =
      case
        matchBody matchLamResult matchOther matchGetPar body0 body1
        >>= Lens.sequenceAOf ExprLens.bodyPar
      of
      Nothing ->
        onMismatch e0 $
        (ExprLens.exprLeaves . ExprLens.parameterRef %~ lookupPar) e1
      Just bodyMatched -> Expr <$> sequenceA bodyMatched <*> onMatch pl0 pl1
      where
        matchGetPar p0 p1 = p0 <$ guard (p0 == lookupPar p1)
        matchLamResult p0 p1 r0 r1 =
          ( Just p0
          , overridePars p0 p1 *> go (Map.insert p1 p0 scope) r0 r1
          )
        matchOther = go scope
        lookupPar par = fromMaybe par $ Map.lookup par scope

subExprs :: Expr def par a -> [Expr def par a]
subExprs x =
  x : Foldable.concatMap subExprs (x ^. eBody)

subExprsWithout ::
  Lens.Traversal' (Expr def par (Bool, a)) (Expr def par (Bool, a)) ->
  Expr def par a -> [Expr def par a]
subExprsWithout group =
  map (fmap snd) .
  filter (fst . (^. ePayload)) .
  subExprs .
  (group . ePayload . Lens._1 .~ False) .
  fmap ((,) True)

isDependentPi :: Eq par => Expr def par a -> Bool
isDependentPi =
  Lens.has (ExprLens.exprKindedLam KType . Lens.filtered f)
  where
    f (g, _, resultType) = exprHasGetVar g resultType

parameterRefs :: Lens.Fold (Expr def par a) par
parameterRefs = Lens.folding subExprs . ExprLens.exprParameterRef

exprHasGetVar :: Eq par => par -> Expr def par a -> Bool
exprHasGetVar g = Lens.anyOf parameterRefs (== g)

curriedFuncArguments :: Expr def par a -> [Expr def par a]
curriedFuncArguments =
  (^.. ExprLens.exprLam . ExprLens.kindedLam KVal . Lens.folding f)
  where
    f (_, paramType, body) = paramType : curriedFuncArguments body

pureIntegerType :: Expr def par ()
pureIntegerType = ExprLens.pureExpr . ExprLens.bodyIntegerType # ()

pureTagType :: Expr def par ()
pureTagType = ExprLens.pureExpr . ExprLens.bodyTagType # ()

pureType :: Expr def par ()
pureType = ExprLens.pureExpr . ExprLens.bodyType # ()

pureVLiteralInteger :: Integer -> Expr def par ()
pureVLiteralInteger = (ExprLens.pureExpr . ExprLens.bodyVLiteralInteger # )

pureApply :: Expr def par () -> Expr def par () -> Expr def par ()
pureApply f x = ExprLens.pureExpr . _VApp # Apply f x

pureHole :: Expr def par ()
pureHole = ExprLens.pureExpr . ExprLens.bodyHole # ()

pureRecord :: Kind -> [(Tag, Expr def par ())] -> Expr def par ()
pureRecord k fields = ExprLens.pureExpr . ExprLens.bodyKindedRecordFields k # fields

pureLam :: Kind -> par -> Expr def par () -> Expr def par () -> Expr def par ()
pureLam k param paramType result =
  ExprLens.pureExpr . ExprLens.bodyKindedLam k # (param, paramType, result)

pureGetField :: Expr def par () -> Tag -> Expr def par ()
pureGetField record field =
  ExprLens.pureExpr . _VGetField # GetField record field

-- TODO: Deprecate below here:
pureExpr :: Body def par (Expr def par ()) -> Expr def par ()
pureExpr = (ExprLens.pureExpr # )

makeApply :: expr -> expr -> Body def par expr
makeApply func arg = VApp $ Apply func arg

makeLam :: Kind -> par -> expr -> expr -> Body def par expr
makeLam k argId argType resultType =
  VAbs $ Lam k argId argType resultType

-- TODO: Remove the kind-passing wrappers
makePi :: par -> expr -> expr -> Body def par expr
makePi = makeLam KType

makeLambda :: par -> expr -> expr -> Body def par expr
makeLambda = makeLam KVal

isTypeConstructorType :: Expr def par a -> Bool
isTypeConstructorType expr =
  case expr ^. eBody of
  VLeaf Type -> True
  VAbs (Lam KType _ _ res) -> isTypeConstructorType res
  _ -> False

-- Show isntances:
showsPrecBody ::
  (Show def, Show par, Show expr) => (par -> expr -> Bool) ->
  Int -> Body def par expr -> ShowS
showsPrecBody mayDepend prec body =
  case body of
  VAbs (Lam KVal paramId paramType result) ->
    paren 0 $
    showChar '\\' . shows paramId . showChar ':' .
    showsPrec 11 paramType . showString "==>" .
    shows result
  VAbs (Lam KType paramId paramType resultType) ->
    paren 0 $
    paramStr . showString "->" . shows resultType
    where
      paramStr
        | dependent =
          showString "(" . shows paramId . showString ":" . showsPrec 11 paramType . showString ")"
        | otherwise = showsPrec 1 paramType
      dependent = mayDepend paramId resultType
  VApp (Apply func arg) ->
    paren 10 $
    showsPrec 10 func . showChar ' ' . showsPrec 11 arg
  VRec (Record k fields) ->
    paren 11 $ showString recStr
    where
      recStr =
        concat ["Rec", recType k, "{", List.intercalate ", " (map showField fields), "}"]
      showField (field, typ) =
        unwords [show field, sep k, show typ]
      sep KVal = "="
      sep KType = ":"
      recType KVal = "V"
      recType KType = "T"
  VGetField (GetField r t) ->
    paren 8 $ showsPrec 8 r . showChar '.' . showsPrec 9 t
  VLeaf leaf -> showsPrec prec leaf
  where
    paren innerPrec = showParen (prec > innerPrec)

showsPrecBodyExpr :: (Show def, Show par, Show a, Eq par) => Int -> BodyExpr def par a -> ShowS
showsPrecBodyExpr = showsPrecBody exprHasGetVar

showBodyExpr :: BodyExpr String String String -> String
showBodyExpr = flip (showsPrecBodyExpr 0) ""

instance (Show def, Show par, Show expr) => Show (Body def par expr) where
  showsPrec = showsPrecBody mayDepend
    where
      -- We are polymorphic on any expr, so we cannot tell...
      mayDepend _ _ = True

instance (Eq par, Show def, Show par, Show a) => Show (Expr def par a) where
  showsPrec prec (Expr body payload) =
    showsPrecBodyExpr bodyPrec body .
    showString showPayload
    where
      (bodyPrec, showPayload) =
        case show payload of
        "" -> (prec, "")
        "()" -> (prec, "")
        str -> (11, "{" ++ str ++ "}")

addBodyContexts ::
  (a -> b) -> Context (Body def par a) (Body def par b) container ->
  Body def par (Context a b container)
addBodyContexts tob (Context intoContainer body) =
  afterSetter %~ intoContainer $
  case body of
  VAbs (Lam k paramId func arg) ->
    Lam k paramId
    (Context (flip (Lam k paramId) (tob arg)) func)
    (Context (Lam k paramId (tob func)) arg)
    & VAbs
    & afterSetter %~ VAbs
  VApp (Apply func arg) ->
    Apply
    (Context (`Apply` tob arg) func)
    (Context (tob func `Apply`) arg)
    & VApp
    & afterSetter %~ VApp
  VRec (Record k fields) ->
    (Record k .
     map addTuple2Contexts .
     addListContexts (_2 %~ tob))
    (Context (Record k) fields)
    & VRec
    & afterSetter %~ VRec
  VGetField (GetField record t) ->
    GetField
    (Context (`GetField` t) record)
    t
    & VGetField
    & afterSetter %~ VGetField
  VLeaf leaf -> VLeaf leaf
  where
    afterSetter = Lens.mapped . Lens.mapped

addExprContexts ::
  (a -> b) ->
  Context (Expr def par a) (Expr def par b) container ->
  Expr def par (Context a (Expr def par b) container)
addExprContexts atob (Context intoContainer (Expr body a)) =
  Expr newBody (Context intoContainer a)
  where
    newBody =
      addExprContexts atob <$>
      addBodyContexts (fmap atob) bodyPtr
    bodyPtr =
      Context (intoContainer . (`Expr` atob a)) body

data TypePosition = Negative | Positive
Lens.makePrisms ''TypePosition

negPos :: TypePosition -> TypePosition
negPos Positive = Negative
negPos Negative = Positive

annotateTypePositions :: Expr def par a -> Expr def par (TypePosition, a)
annotateTypePositions = annotateTypePositionsH Positive

annotateTypePositionsH :: TypePosition -> Expr def par a -> Expr def par (TypePosition, a)
annotateTypePositionsH pos (Expr body pl) =
  (`Expr` (pos, pl)) $
  case body of
  VAbs (Lam k paramId paramType result) ->
    VAbs $
    Lam k paramId
    (annotateTypePositionsH (negPos pos) paramType)
    (annotateTypePositionsH pos result)
  x ->
    x & Lens.traverse %~ annotateTypePositionsH pos
