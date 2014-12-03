{-# LANGUAGE ConstraintKinds, OverloadedStrings, RankNTypes #-}

module Lamdu.Sugar.Convert.Hole
  ( convert, convertPlain, orderedInnerHoles
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Arrow ((&&&))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), evalStateT, evalState, state)
import Control.MonadA (MonadA)
import Data.Maybe.Utils(unsafeUnjust)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.String (IsString(..))
import Data.Traversable (traverse)
import Lamdu.Expr.IRef (DefIM)
import Lamdu.Expr.Type (Type(..))
import Lamdu.Expr.Val (Val(..))
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import Lamdu.Sugar.Types.Internal
import Lamdu.Suggest (suggestValueWith)
import System.Random.Utils (genFromHashable)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Infer as SugarInfer
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.InputExpr as InputExpr
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified System.Random as Random

type T = Transaction

convert ::
  (MonadA m, Monoid a) =>
  InputPayload m a -> ConvertM m (ExpressionU m a)
convert exprPl =
  convertPlain exprPl
  <&> rPayload . plActions . Lens._Just . setToHole .~ AlreadyAHole

convertPlain ::
  (MonadA m, Monoid a) =>
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertPlain exprPl =
  mkHole exprPl
  <&> BodyHole
  >>= ConvertExpr.make exprPl
  <&> rPayload . plActions . Lens._Just . wrap .~ WrapNotAllowed

mkPaste ::
  MonadA m => ExprIRef.ValIProperty m -> ConvertM m (Maybe (T m EntityId))
mkPaste exprP = do
  clipboardsP <- ConvertM.codeAnchor Anchors.clipboards
  let
    mClipPop =
      case Property.value clipboardsP of
      [] -> Nothing
      (clip : clips) -> Just (clip, Property.set clipboardsP clips)
  return $ doPaste (Property.set exprP) <$> mClipPop
  where
    doPaste replacer (clipDefI, popClip) = do
      clipDef <- Transaction.readIRef clipDefI
      let
        clip =
          case clipDef of
          Definition.Body (Definition.ContentExpr defExpr) _ -> defExpr
          _ -> error "Clipboard contained a non-expression definition!"
      Transaction.deleteIRef clipDefI
      ~() <- popClip
      ~() <- replacer clip
      return $ EntityId.ofValI clip

inferOnTheSide ::
  (MonadA m) =>
  ConvertM.Context m -> Infer.Scope -> Val () ->
  T m (Maybe Type)
-- token represents the given holeInferContext
inferOnTheSide sugarContext scope val =
  runMaybeT . (`evalStateT` (sugarContext ^. ConvertM.scInferContext)) $
  SugarInfer.loadInferScope scope val
  <&> (^. V.payload . Lens._1 . Infer.plType)

-- Value for holeResultNewTag
newTag :: T.Tag
newTag = "newTag"

mkWritableHoleActions ::
  (MonadA m) =>
  InputPayload m dummy -> ExprIRef.ValIProperty m ->
  ConvertM m (HoleActions Guid m)
mkWritableHoleActions exprPl stored = do
  sugarContext <- ConvertM.readContext
  mPaste <- mkPaste stored
  let globals =
        Property.value $ Anchors.globals $ sugarContext ^. ConvertM.scCodeAnchors
  let inferredScope = inferred ^. Infer.plScope
  pure HoleActions
    { _holePaste = mPaste
    , _holeScope =
      mconcat . concat <$> sequence
      [ mapM (getScopeElement sugarContext) $ Map.toList $ Infer.scopeToTypeMap inferredScope
      , mapM getGlobal globals
      ]
    , _holeInferExprType = inferOnTheSide sugarContext inferredScope
    , holeResult = mkHoleResult sugarContext exprPl stored
    , _holeResultNewTag = newTag
    , _holeGuid = UniqueId.toGuid $ ExprIRef.unValI $ Property.value stored
    }
  where
    inferred = exprPl ^. ipInferred

-- Ignoring alpha-renames:
consistentExprIds :: EntityId -> Val (Guid -> EntityId -> a) -> Val a
consistentExprIds holeEntityId val =
  EntityId.randomizeExprAndParams gen val
  where
    gen =
      genFromHashable
        ( holeEntityId
        , void $ InputExpr.randomizeParamIds (genFromHashable holeEntityId) val
        )

mkHoleSuggested :: MonadA m => EntityId -> Infer.Payload -> ConvertM m (HoleSuggested Guid m)
mkHoleSuggested holeEntityId inferred = do
  sugarContext <- ConvertM.readContext
  (inferredIVal, newCtx) <-
    SugarInfer.loadInferScope (inferred ^. Infer.plScope) suggestedVal
    & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
    & runMaybeT
    <&> unsafeUnjust "Inference on inferred val must succeed"
    & ConvertM.liftTransaction
  let
    mkConverted =
      inferredIVal
      <&> mkInputPayload . fst
      & consistentExprIds holeEntityId
      & ConvertM.convertSubexpression
      & ConvertM.run (sugarContext & ConvertM.scInferContext .~ newCtx)
  pure HoleSuggested
    { _hsValue = suggestedVal
    , _hsMakeConverted = mkConverted
    }
  where
    suggestedVal =
      (`evalState` (0 :: Int)) $
      suggestValueWith mkVar
      (inferred ^. Infer.plType)
    mkVar = do
      i <- State.get
      State.modify (+1)
      return . fromString $ "var" ++ show i
    mkInputPayload i guid entityId = InputPayload
      { _ipEntityId = entityId
      , _ipGuid = guid
      , _ipInferred = i
      , _ipStored = Nothing
      , _ipData = ()
      }

mkHole ::
  (MonadA m, Monoid a) =>
  InputPayload m a -> ConvertM m (Hole Guid m (ExpressionU m a))
mkHole exprPl = do
  mActions <- traverse (mkWritableHoleActions exprPl) (exprPl ^. ipStored)
  suggested <- mkHoleSuggested (exprPl ^. ipEntityId) $ exprPl ^. ipInferred
  pure Hole
    { _holeMActions = mActions
    , _holeSuggested = suggested
    , _holeMArg = Nothing
    }

getScopeElement ::
  MonadA m => ConvertM.Context m ->
  (V.Var, Type) -> T m (Scope Guid m)
getScopeElement sugarContext (par, typeExpr) = do
  scopePar <- mkGetPar
  mconcat . (scopePar :) <$>
    mapM onScopeField
    (typeExpr ^.. ExprLens._TRecord . ExprLens.compositeTags)
  where
    mkGetPar =
      case Map.lookup par recordParamsMap of
      Just (ConvertM.RecordParamsInfo defName jumpTo) ->
        pure mempty
          { _scopeGetParams = [
            ( GetParams
              { _gpDefName = defName
              , _gpJumpTo = jumpTo
              }
            , getParam )
          ] }
      Nothing -> do
        pure mempty
          { _scopeLocals = [
            ( GetVar
              { _gvName = ConvertExpr.makeNameProperty par
              , _gvJumpTo = errorJumpTo
              , _gvVarType = GetParameter
              }
            , getParam )
          ] }
    recordParamsMap = sugarContext ^. ConvertM.scRecordParamsInfos
    errorJumpTo = error "Jump to on scope item??"
    getParam = P.var par
    onScopeField tag = do
      pure mempty
        { _scopeLocals = [
          ( GetVar
            { _gvName = ConvertExpr.makeNameProperty tag
            , _gvJumpTo = errorJumpTo
            , _gvVarType = GetFieldParameter
            }
          , P.getField getParam tag
          )
        ] }

-- TODO: Put the result in scopeGlobals in the caller, not here?
getGlobal :: MonadA m => DefIM m -> T m (Scope Guid m)
getGlobal defI = do
  pure mempty
    { _scopeGlobals = [
      ( GetVar
        { _gvName = ConvertExpr.makeNameProperty defI
        , _gvJumpTo = errorJumpTo
        , _gvVarType = GetDefinition
        }
      , P.global $ ExprIRef.globalId defI
      )
      ] }
  where
    errorJumpTo = error "Jump to on scope item??"

writeConvertTypeChecked ::
  (MonadA m, Monoid a) =>
  EntityId -> ConvertM.Context m -> ExprIRef.ValIProperty m ->
  Val (Infer.Payload, MStorePoint m a) ->
  T m
  ( ExpressionU m a
  , Val (InputPayload m a)
  , Val (ExprIRef.ValIProperty m, InputPayload m a)
  )
writeConvertTypeChecked holeEntityId sugarContext holeStored inferredVal = do
  -- With the real stored guids:
  writtenExpr <-
    inferredVal
    <&> intoStorePoint
    & writeExprMStored holeEntityId (Property.value holeStored)
    <&> ExprIRef.addProperties (Property.set holeStored)
    <&> fmap toPayload
  let
    -- Replace the guids with consistent ones:

    -- The sugar convert must apply *inside* the forked transaction
    -- upon the *written* expr because we actually make use of the
    -- resulting actions (e.g: press ',' on a list hole result).
    -- However, the written expr goes crazy with new guids every time.
    --
    -- So, we do something a bit odd: Take the written expr with its
    -- in-tact stored allowing actions to be built correctly but
    -- replace the ipGuid/ipEntityId with determinstic/consistent
    -- pseudo-random generated ones that preserve proper animations
    -- and cursor navigation.

    makeConsistentPayload (False, (_, pl)) guid entityId = pl
      & ipEntityId .~ entityId
      & ipGuid .~ guid
    makeConsistentPayload (True, (_, pl)) _ _ = pl
    consistentExpr =
      writtenExpr
      <&> makeConsistentPayload
      & consistentExprIds holeEntityId
  converted <-
    consistentExpr
    & ConvertM.convertSubexpression
    & ConvertM.run sugarContext
  return
    ( converted
    , consistentExpr
    , writtenExpr <&> snd
    )
  where
    intoStorePoint (inferred, (mStorePoint, a)) =
      (mStorePoint, (inferred, Lens.has Lens._Just mStorePoint, a))
    toPayload (stored, (inferred, wasStored, a)) = (,) wasStored $ (,) stored InputPayload
      { _ipEntityId = EntityId.ofValI $ Property.value stored
      , _ipGuid = IRef.guid $ ExprIRef.unValI $ Property.value stored
      , _ipInferred = inferred
      , _ipStored = Just stored
      , _ipData = a
      }

resultComplexityScore :: Val Infer.Payload -> [Int]
resultComplexityScore expr =
  [ length . show $ expr ^. V.payload . Infer.plType
  , length $ Foldable.toList expr
  ]

idTranslations ::
  Val EntityId ->
  Val EntityId ->
  [(EntityId, EntityId)]
idTranslations src dest
  | V.alphaEq (void src) (void dest)
    = concat
      [ pairUp V.payload
      , pairUp params
      , pairUpTags ExprLens._BRecExtend EntityId.ofRecExtendTag
      , pairUpTags ExprLens._BGetField EntityId.ofGetFieldTag
      , pairUp getLambdaTagParams
      ]
  | otherwise = error "idTranslations of mismatching expressions"
  where
    pairUp l = zip (src ^.. ExprLens.subExprs . l) (dest ^.. ExprLens.subExprs . l)
    pairUpTags prism toEntityId =
      pairUp $
      Lens.filtered (Lens.has (V.body . prism)) . V.payload . Lens.to toEntityId
    getLambdaTagEntityIds (V.GetField (V.Val _ (V.BLeaf (V.LVar var))) tag) =
      [EntityId.ofLambdaTagParam var tag]
    getLambdaTagEntityIds _ = []
    getLambdaTagParams =
      V.body . ExprLens._BGetField . Lens.folding getLambdaTagEntityIds
    params =
      V.body . ExprLens._BAbs . V.lamParamId .
      Lens.to EntityId.ofLambdaParam

mkHoleResult ::
  (MonadA m, Monoid a) =>
  ConvertM.Context m ->
  InputPayload m dummy -> ExprIRef.ValIProperty m ->
  Val (MStorePoint m a) ->
  T m (Maybe (HoleResult Guid m a))
mkHoleResult sugarContext exprPl stored val =
  runMaybeT $ do
    (inferredVal, ctx) <-
      (`runStateT` (sugarContext ^. ConvertM.scInferContext))
      (SugarInfer.loadInferInto (exprPl ^. ipInferred) val)
    let newSugarContext = sugarContext & ConvertM.scInferContext .~ ctx
    ((fConverted, fConsistentExpr, fWrittenExpr), forkedChanges) <-
      lift $ Transaction.fork $
        writeConvertTypeChecked (exprPl ^. ipEntityId)
        newSugarContext stored inferredVal
    return $ HoleResult
      { _holeResultComplexityScore = resultComplexityScore $ fst <$> inferredVal
      , _holeResultConverted = fConverted
      , _holeResultPick = mkPickedResult fConsistentExpr fWrittenExpr <$ Transaction.merge forkedChanges
      , _holeResultHasHoles = not . null $ orderedInnerHoles val
      }
  where
    mkPickedResult consistentExpr writtenExpr =
      PickedResult
      { _prMJumpTo =
        (orderedInnerHoles writtenExpr ^? Lens.traverse . V.payload . _2)
        <&> (^. ipGuid) &&& (^. ipEntityId)
      , _prIdTranslation =
        idTranslations
        (consistentExpr <&> (^. ipEntityId))
        (writtenExpr <&> EntityId.ofValI . Property.value . fst)
      }

randomizeNonStoredParamIds ::
  Random.StdGen -> ExprStorePoint m a -> ExprStorePoint m a
randomizeNonStoredParamIds gen =
  InputExpr.randomizeParamIdsG id nameGen Map.empty $ \_ _ pl -> pl
  where
    nameGen = InputExpr.onNgMakeName f $ InputExpr.randomNameGen gen
    f n _        prevEntityId (Just _, _) = (prevEntityId, n)
    f _ prevFunc prevEntityId pl@(Nothing, _) = prevFunc prevEntityId pl

writeExprMStored ::
  MonadA m =>
  EntityId ->
  ExprIRef.ValIM m ->
  ExprStorePoint m a ->
  T m (Val (ExprIRef.ValIM m, a))
writeExprMStored holeEntityId exprIRef exprMStorePoint = do
  key <- Transaction.newKey
  exprMStorePoint
    & randomizeNonStoredParamIds (genFromHashable key)
    & newTags %%~ const (state InputExpr.randomTag)
    & (`evalState` genFromHashable holeEntityId)
    & Lens.mapped . Lens._1 . Lens._Just %~ unStorePoint
    & ExprIRef.writeValWithStoredSubexpressions exprIRef
  where
    newTags = ExprLens.valTags . Lens.filtered (== newTag)

orderedInnerHoles :: Val a -> [Val a]
orderedInnerHoles e =
  case e ^. V.body of
  V.BLeaf V.LHole -> [e]
  V.BApp (V.Apply func@(V.Val _ (V.BLeaf V.LHole)) arg) ->
      orderedInnerHoles arg ++ [func]
  body -> Foldable.concatMap orderedInnerHoles body
