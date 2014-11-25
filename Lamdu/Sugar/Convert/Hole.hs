{-# LANGUAGE ConstraintKinds #-}

module Lamdu.Sugar.Convert.Hole
  ( convert, convertPlain, orderedInnerHoles
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Maybe (listToMaybe)
import Data.Maybe.Utils(unsafeUnjust)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
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
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
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
  clipboards <- ConvertM.getP clipboardsP
  let
    mClipPop =
      case clipboards of
      [] -> Nothing
      (clip : clips) -> Just (clip, Transaction.setP clipboardsP clips)
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

mkWritableHoleActions ::
  (MonadA m) =>
  InputPayloadP (ExprIRef.ValIProperty m) () ->
  ConvertM m (HoleActions MStoredName m)
mkWritableHoleActions exprPlStored = do
  sugarContext <- ConvertM.readContext
  mPaste <- mkPaste $ exprPlStored ^. ipStored
  globals <-
    ConvertM.liftTransaction . Transaction.getP . Anchors.globals $
    sugarContext ^. ConvertM.scCodeAnchors
  tags <-
    ConvertM.liftTransaction . Transaction.getP . Anchors.tags $
    sugarContext ^. ConvertM.scCodeAnchors
  let inferredScope = inferred ^. Infer.plScope
  pure HoleActions
    { _holePaste = mPaste
    , _holeScope =
      mconcat . concat <$> sequence
      [ mapM (getScopeElement sugarContext) $ Map.toList $ Infer.scopeToTypeMap inferredScope
      , mapM getGlobal globals
      , mapM (getTag (exprPlStored ^. ipEntityId)) tags
      ]
    , _holeInferExprType = inferOnTheSide sugarContext inferredScope
    , holeResult = mkHoleResult sugarContext exprPlStored
    , _holeGuid = UniqueId.toGuid $ ExprIRef.unValI $ Property.value $ exprPlStored ^. ipStored
    }
  where
    inferred = exprPlStored ^. ipInferred

mkHoleInferred :: MonadA m => Infer.Payload -> ConvertM m (HoleInferred MStoredName m)
mkHoleInferred inferred = do
  sugarContext <- ConvertM.readContext
  iVal <-
      suggestValueWith (ConvertM.liftTransaction UniqueId.new)
      (inferred ^. Infer.plType)
  (inferredIVal, newCtx) <-
    SugarInfer.loadInferScope (inferred ^. Infer.plScope) iVal
    & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
    & runMaybeT
    <&> unsafeUnjust "Inference on inferred val must succeed"
    & ConvertM.liftTransaction
  let
    mkConverted gen =
      inferredIVal
      <&> mkInputPayload . fst
      & EntityId.randomizeExprAndParams gen
      & ConvertM.convertSubexpression
      & ConvertM.run (sugarContext & ConvertM.scInferContext .~ newCtx)
  pure HoleInferred
    { _hiSuggestedValue = iVal
    , _hiType = inferred ^. Infer.plType
    , _hiMakeConverted = mkConverted
    }
  where
    mkInputPayload i entityId = InputPayload
      { _ipEntityId = entityId
      , _ipInferred = i
      , _ipStored = Nothing
      , _ipData = ()
      }

mkHole ::
  (MonadA m, Monoid a) =>
  InputPayloadP (Maybe (ExprIRef.ValIProperty m)) a ->
  ConvertM m (Hole MStoredName m (ExpressionU m a))
mkHole exprPl = do
  mActions <-
    exprPl
    & ipData .~ ()
    & Lens.sequenceOf ipStored
    & traverse mkWritableHoleActions
  inferred <- mkHoleInferred $ exprPl ^. ipInferred
  pure Hole
    { _holeMActions = mActions
    , _holeInferred = inferred
    , _holeMArg = Nothing
    }

getScopeElement ::
  MonadA m => ConvertM.Context m ->
  (V.Var, Type) -> T m (Scope MStoredName m)
getScopeElement sugarContext (par, typeExpr) = do
  scopePar <- mkGetPar
  mconcat . (scopePar :) <$>
    mapM onScopeField
    (typeExpr ^.. ExprLens._TRecord . ExprLens.compositeTags)
  where
    mkGetPar =
      case Map.lookup par recordParamsMap of
      Just (ConvertM.RecordParamsInfo defName jumpTo) -> do
        pure mempty
          { _scopeGetParams = [
            ( GetParams
              { _gpDefName = defName
              , _gpJumpTo = jumpTo
              }
            , getParam )
          ] }
      Nothing -> do
        parName <- ConvertExpr.makeStoredNameProperty par
        pure mempty
          { _scopeLocals = [
            ( GetVar
              { _gvName = parName
              , _gvJumpTo = errorJumpTo
              , _gvVarType = GetParameter
              }
            , getParam )
          ] }
    recordParamsMap = sugarContext ^. ConvertM.scRecordParamsInfos
    errorJumpTo = error "Jump to on scope item??"
    getParam = P.var par
    onScopeField tag = do
      name <- ConvertExpr.makeStoredNameProperty tag
      pure mempty
        { _scopeLocals = [
          ( GetVar
            { _gvName = name
            , _gvJumpTo = errorJumpTo
            , _gvVarType = GetFieldParameter
            }
          , P.getField getParam tag
          )
        ] }

-- TODO: Put the result in scopeGlobals in the caller, not here?
getGlobal :: MonadA m => DefIM m -> T m (Scope MStoredName m)
getGlobal defI = do
  name <- ConvertExpr.makeStoredNameProperty defI
  pure mempty
    { _scopeGlobals = [
      ( GetVar
        { _gvName = name
        , _gvJumpTo = errorJumpTo
        , _gvVarType = GetDefinition
        }
      , P.global $ ExprIRef.globalId defI
      )
      ] }
  where
    errorJumpTo = error "Jump to on scope item??"

getTag :: MonadA m => EntityId -> T.Tag -> T m (Scope MStoredName m)
getTag ctxEntityId tag = do
  name <- ConvertExpr.makeStoredNameProperty tag
  let
    tagG = TagG
      { _tagInstance = EntityId.augment (show (UniqueId.toGuid tag)) ctxEntityId
      , _tagVal = tag
      , _tagGName = name
      }
  pure mempty { _scopeTags = [(tagG, tag)] }

writeConvertTypeChecked ::
  (MonadA m, Monoid a) => Random.StdGen ->
  ConvertM.Context m -> ExprIRef.ValIProperty m ->
  Val (Infer.Payload, MStorePoint m a) ->
  T m
  ( ExpressionU m a
  , Val (InputPayloadP (ExprIRef.ValIProperty m) a)
  , Val (InputPayloadP (ExprIRef.ValIProperty m) a)
  )
writeConvertTypeChecked gen sugarContext holeStored inferredVal = do
  -- With the real stored guids:
  writtenExpr <-
    fmap toPayload .
    ExprIRef.addProperties (Property.set holeStored) <$>
    writeExprMStored (Property.value holeStored) (intoStorePoint <$> inferredVal)
  let
    -- Replace the guids with consistently fake ones
    makeConsistentPayload (False, pl) entityId = pl & ipEntityId .~ entityId
    makeConsistentPayload (True, pl) _ = pl
    consistentExpr =
      writtenExpr
      <&> makeConsistentPayload
      & EntityId.randomizeExprAndParams gen
  converted <-
    consistentExpr
    <&> ipStored %~ Just
    & ConvertM.convertSubexpression
    & ConvertM.run sugarContext
  return
    ( converted
    , consistentExpr
    , snd <$> writtenExpr
    )
  where
    intoStorePoint (inferred, (mStorePoint, a)) =
      (mStorePoint, (inferred, Lens.has Lens._Just mStorePoint, a))
    toPayload (stored, (inferred, wasStored, a)) = (,) wasStored InputPayload
      { _ipEntityId = EntityId.ofValI $ Property.value stored
      , _ipInferred = inferred
      , _ipStored = stored
      , _ipData = a
      }

mkHoleResult ::
  (MonadA m, Binary a, Monoid a) =>
  ConvertM.Context m ->
  InputPayloadP (ExprIRef.ValIProperty m) () ->
  (EntityId -> Random.StdGen) ->
  Val (MStorePoint m a) ->
  T m (Maybe (HoleResult MStoredName m a))
mkHoleResult sugarContext (InputPayload guid inferPayload stored ()) mkGen val = do
  (mResult, forkedChanges) <-
    Transaction.fork $ runMaybeT $ do
      (inferredVal, ctx) <-
        (`runStateT` (sugarContext ^. ConvertM.scInferContext)) $
        SugarInfer.loadInferInto inferPayload val
      lift $ writeConvertTypeChecked (mkGen guid) (sugarContext & ConvertM.scInferContext .~ ctx) stored inferredVal

  return $ mkResult (Transaction.merge forkedChanges) <$> mResult
  where
    mkResult unfork (fConverted, fConsistentExpr, fWrittenExpr) =
      HoleResult
        { _holeResultInferred = inferredExpr
        , _holeResultConverted = fConverted
        , _holeResultPick = unfork *> mkPickedResult fConsistentExpr fWrittenExpr
        , _holeResultHasHoles =
          not . null . uninferredHoles $ (,) () <$> inferredExpr
        }
        where
          inferredExpr = (^. ipInferred) <$> fWrittenExpr
    mkPickedResult _consistentExpr writtenExpr = do
      let
        f payload = (payload ^. ipEntityId, payload ^. ipInferred)
        mNextHole = listToMaybe . orderedInnerHoles $ f <$> writtenExpr
      pure
        PickedResult
        { _prMJumpTo = (^. V.payload . Lens._1) <$> mNextHole
        , _prIdTranslation = []
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
  ExprIRef.ValIM m ->
  ExprStorePoint m a ->
  T m (Val (ExprIRef.ValIM m, a))
writeExprMStored exprIRef exprMStorePoint = do
  key <- Transaction.newKey
  randomizeNonStoredParamIds (genFromHashable key) exprMStorePoint
    & Lens.mapped . Lens._1 . Lens._Just %~ unStorePoint
    & ExprIRef.writeValWithStoredSubexpressions exprIRef

orderedInnerHoles :: Val (a, Infer.Payload) -> [Val (a, Infer.Payload)]
orderedInnerHoles e =
  case e ^. V.body of
  V.BApp (V.Apply func arg)
    | isHole func ->
      -- This is a "type-error wrapper".
      -- Skip the conversion hole
      -- and go to inner holes in the expression first.
      uninferredHoles arg ++ [func]
  _ -> uninferredHoles e
  where
    isHole (V.Val _ (V.BLeaf V.LHole)) = True
    isHole _ = False

-- Also skip param types, those can usually be inferred later, so less
-- useful to fill immediately
uninferredHoles :: Val a -> [Val a]
uninferredHoles e =
  case e ^. V.body of
  V.BLeaf V.LHole -> [e]
  body -> Foldable.concatMap uninferredHoles body
