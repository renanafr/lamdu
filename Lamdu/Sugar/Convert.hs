{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types, PatternGuards #-}
module Lamdu.Sugar.Convert
  ( convertDefI
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens (LensLike, Lens')
import Control.Lens.Operators
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (traverse)
import Data.Typeable (Typeable1)
import Lamdu.Expr.IRef (DefIM)
import Lamdu.Infer.Load (LoadedDef(..), ldDef)
import Lamdu.Sugar.Convert.Monad (ConvertM, Context(..))
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import Lamdu.Sugar.Types.Internal
import System.Random (RandomGen)
import System.Random.Utils (genFromHashable)
import qualified Control.Lens as Lens
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr as Expr
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Utils as ExprUtil
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Deref as InferDeref
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Sugar.Convert.Apply as ConvertApply
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.Sugar.Convert.Infer as SugarInfer
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.InputExpr as InputExpr
import qualified Lamdu.Sugar.RemoveTypes as SugarRemoveTypes

onMatchingSubexprs ::
  MonadA m => (a -> m ()) ->
  (Expr.Expr def par a -> Bool) ->
  Expr.Expr def par a -> m ()
onMatchingSubexprs action predicate =
  traverse_ (action . (^. Expr.ePayload)) .
  filter predicate . ExprUtil.subExprs

toHole :: MonadA m => Stored m -> T m ()
toHole = void . DataOps.setToHole

isGetParamOf :: Eq par => par -> Expr.Expr def par a -> Bool
isGetParamOf = Lens.anyOf ExprLens.exprParameterRef . (==)

isGetFieldParam :: Eq par => par -> Guid -> Expr.Expr def par a -> Bool
isGetFieldParam param tagG =
  p . (^? ExprLens.exprGetField)
  where
    p Nothing = False
    p (Just (Expr.GetField record (Expr.Tag tagGuid))) =
      tagGuid == tagG &&
      Lens.anyOf ExprLens.exprParameterRef (== param) record

deleteParamRef ::
  (Eq par, MonadA m) => par -> Expr.Expr def par (Stored m) -> T m ()
deleteParamRef =
  onMatchingSubexprs toHole . isGetParamOf

deleteFieldParamRef ::
  (Eq par, MonadA m) => par -> Guid -> Expr.Expr def par (Stored m) -> T m ()
deleteFieldParamRef param tagG =
  onMatchingSubexprs toHole $ isGetFieldParam param tagG

lambdaWrap :: MonadA m => Stored m -> T m Guid
lambdaWrap stored =
  f <$> DataOps.lambdaWrap stored
  where
    f (newParam, newLamI) = Guid.combine (ExprIRef.exprGuid newLamI) newParam

fakeExample :: Guid -> ExpressionP name0 m0 (Payload name1 m1 ())
fakeExample guid =
  Expression (BodyAtom "NotImplemented") Payload
  { _plGuid = Guid.augment "EXAMPLE" guid
  , _plInferredTypes = []
  , _plActions = Nothing
  , _plData = ()
  }

mkPositionalFuncParamActions ::
  MonadA m => Guid -> Stored m -> Expr.Expr def Guid (Stored m) ->
  FuncParamActions MStoredName m
mkPositionalFuncParamActions param lambdaProp body =
  FuncParamActions
  { _fpListItemActions =
    ListItemActions
    { _itemDelete = do
        deleteParamRef param body
        SugarInfer.replaceWith lambdaProp $ body ^. Expr.ePayload
    , _itemAddNext = lambdaWrap $ body ^. Expr.ePayload
    }
  , _fpGetExample = pure $ fakeExample param
  }

getStoredNameS :: MonadA m => Guid -> ConvertM m MStoredName
getStoredNameS = ConvertM.liftTransaction . ConvertExpr.getStoredName

addFuncParamName ::
  MonadA m => FuncParam MStoredName f expr ->
  ConvertM m (FuncParam MStoredName f expr)
addFuncParamName fp = do
  name <- getStoredNameS $ fp ^. fpGuid
  pure fp { _fpName = name }

getInferredVal :: InputExpr m a -> LoadedExpr m ()
getInferredVal x =
  x ^? Expr.ePayload . ipInferred . Lens._Just . InferDeref.dValue
  <&> void
  & fromMaybe ExprUtil.pureHole

convertPositionalFuncParam ::
  (Typeable1 m, MonadA m, Monoid a) => Expr.Lam Guid (InputExpr m a) ->
  InputPayload m a ->
  ConvertM m (FuncParam MStoredName m (ExpressionU m a))
convertPositionalFuncParam (Expr.Lam _k paramGuid paramType body) lamExprPl = do
  paramTypeS <- ConvertM.convertSubexpression paramType
  addFuncParamName FuncParam
    { _fpName = Nothing
    , _fpGuid = paramGuid
    , _fpVarKind = FuncParameter
    , _fpId = Guid.combine lamGuid paramGuid
    , _fpAltIds = [paramGuid] -- For easy jumpTo
    , _fpType = SugarRemoveTypes.successfulType paramTypeS
    , _fpInferredType = getInferredVal paramType
    , _fpMActions =
      mkPositionalFuncParamActions paramGuid
      <$> lamExprPl ^. ipStored
      <*> traverse (^. ipStored) body
    }
  where
    lamGuid = lamExprPl ^. ipGuid

convertLam ::
  (MonadA m, Typeable1 m, Monoid a) =>
  Expr.Lam Guid (InputExpr m a) ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertLam lam@(Expr.Lam k paramGuid _paramType result) exprPl = do
  param <- convertPositionalFuncParam lam exprPl
  resultS <- ConvertM.convertSubexpression result
  BodyLam
    Lam
    { _lParam =
        param
        & fpMActions .~ Nothing
    , _lResultType = resultS
    , _lKind = k
    , _lIsDep = isDep
    }
    & ConvertExpr.make exprPl
    <&> rPayload . plActions . Lens._Just . mSetToInnerExpr .~ do
      guard $ Lens.nullOf ExprLens.exprHole result
      bodyStored <- traverse (^. ipStored) result
      stored <- exprPl ^. ipStored
      return $ do
        deleteParamRef paramGuid bodyStored
        ExprIRef.exprGuid <$>
          DataOps.setToWrapper (Property.value (bodyStored ^. Expr.ePayload)) stored
  where
    isDep =
      case k of
      KVal -> SugarInfer.isPolymorphicFunc exprPl
      KType -> ExprUtil.exprHasGetVar paramGuid result

convertParameterRef ::
  (MonadA m, Typeable1 m) => Guid ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertParameterRef parGuid exprPl = do
  recordParamsMap <- (^. ConvertM.scRecordParamsInfos) <$> ConvertM.readContext
  case Map.lookup parGuid recordParamsMap of
    Just (ConvertM.RecordParamsInfo defGuid jumpTo) -> do
      defName <- getStoredNameS defGuid
      ConvertExpr.make exprPl $ BodyGetParams GetParams
        { _gpDefGuid = defGuid
        , _gpDefName = defName
        , _gpJumpTo = jumpTo
        }
    Nothing -> do
      parName <- getStoredNameS parGuid
      ConvertExpr.make exprPl .
        BodyGetVar $ GetVar
        { _gvName = parName
        , _gvIdentifier = parGuid
        , _gvJumpTo = pure parGuid
        , _gvVarType = GetParameter
        }

jumpToDefI ::
  MonadA m => Anchors.CodeProps m -> DefIM m -> T m Guid
jumpToDefI cp defI = IRef.guid defI <$ DataOps.newPane cp defI

convertVVar ::
  (MonadA m, Typeable1 m) =>
  Expr.VariableRef (Infer.LoadedDef (DefIM m)) Guid ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertVVar varRef exprPl = do
  cp <- (^. ConvertM.scCodeAnchors) <$> ConvertM.readContext
  case varRef of
    Expr.ParameterRef parGuid -> convertParameterRef parGuid exprPl
    Expr.DefinitionRef defL -> do
      let
        defI = defL ^. ldDef
        defGuid = IRef.guid defI
      defName <- getStoredNameS defGuid
      ConvertExpr.make exprPl .
        BodyGetVar $ GetVar
        { _gvName = defName
        , _gvIdentifier = defGuid
        , _gvJumpTo = jumpToDefI cp defI
        , _gvVarType = GetDefinition
        }

convertVLiteralInteger ::
  (MonadA m, Typeable1 m) => Integer ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertVLiteralInteger i exprPl = ConvertExpr.make exprPl $ BodyLiteralInteger i

convertTag :: (MonadA m, Typeable1 m) => Expr.Tag -> ConvertM m (TagG MStoredName)
convertTag (Expr.Tag tagGuid) = TagG tagGuid <$> getStoredNameS tagGuid

convertAtom ::
  (MonadA m, Typeable1 m) => String ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertAtom str exprPl =
  ConvertExpr.make exprPl $ BodyAtom str

sideChannel :: Monad m => Lens' s a -> LensLike m s (side, s) a (side, a)
sideChannel lens f s = (`runStateT` s) . Lens.zoom lens $ StateT f

-- writeRecordFields ::
--   MonadA m =>
--   ExprIRef.ExprIM m -> result ->
--   ( [(Expr.Tag, ExprIRef.ExprIM m)] ->
--     T m
--     ( result
--     , [(Expr.Tag, ExprIRef.ExprIM m)]
--     )
--   ) ->
--   T m result
-- writeRecordFields iref def f = do
--   oldBody <- ExprIRef.readExprBody iref
--   case oldBody ^? Expr._VRec of
--     Nothing -> return def
--     Just oldRecord -> do
--       (res, newRecord) <- sideChannel Expr.recordFields f oldRecord
--       ExprIRef.writeExprBody iref $ Expr.VRec newRecord
--       return res

-- recordFieldActions ::
--   MonadA m => Guid -> ExprIRef.ExprIM m -> ExprIRef.ExprIM m ->
--   ListItemActions m
-- recordFieldActions defaultGuid exprIRef iref =
--   ListItemActions
--   { _itemDelete = action delete
--   , _itemAddNext = action addNext
--   }
--   where
--     action f = writeRecordFields iref defaultGuid $ splitFields f
--     addNext (prevFields, field, nextFields) = do
--       tagHole <- DataOps.newHole
--       exprHole <- DataOps.newHole
--       return
--         ( ExprIRef.exprGuid tagHole
--         , prevFields ++ field : (tagHole, exprHole) : nextFields
--         )
--     delete (prevFields, _, nextFields) =
--       return
--       ( case nextFields ++ reverse prevFields of
--         [] -> defaultGuid
--         ((nextTagExpr, _) : _) -> ExprIRef.exprGuid nextTagExpr
--       , prevFields ++ nextFields
--       )
--     splitFields f oldFields =
--       case break ((== exprIRef) . fst) oldFields of
--       (prevFields, field : nextFields) -> f (prevFields, field, nextFields)
--       _ -> return (defaultGuid, oldFields)

convertField ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Maybe (ExprIRef.ExprIM m) -> Guid ->
  (Expr.Tag, InputExpr m a) ->
  ConvertM m (RecordField MStoredName m (ExpressionU m a))
convertField mIRef defaultGuid (tag, expr) = do
  tagS <- convertTag tag
  exprS <- ConvertM.convertSubexpression expr
  return RecordField
    { _rfMItemActions = error "TODO: rfMItemActions"
 --      recordFieldActions defaultGuid <$> tagExpr ^? SugarInfer.exprIRef <*> mIRef
    , _rfTag = tagS
    , _rfExpr = exprS
    }

convertRecord ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Expr.Record (InputExpr m a) ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertRecord (Expr.Record k fields) exprPl = do
  sFields <- mapM (convertField (exprPl ^? SugarInfer.plIRef) defaultGuid) fields
  ConvertExpr.make exprPl $ BodyRecord
    Record
    { _rKind = k
    , _rFields =
        FieldList
        { _flItems = sFields
        , _flMAddFirstItem = error "TODO: _flMAddFirstItem" -- addField <$> exprPl ^? SugarInfer.plIRef
        }
    }
  where
    defaultGuid = exprPl ^. ipGuid
    -- addField iref =
    --   writeRecordFields iref defaultGuid $ \recordFields -> do
    --     tag <- Expr.Tag <$> Transaction.newKey
    --     holeExpr <- DataOps.newHole
    --     return
    --       ( ExprIRef.exprGuid holeTagExpr
    --       , (tag, holeExpr) : recordFields
    --       )

convertGetField ::
  (MonadA m, Typeable1 m, Monoid a) =>
  Expr.GetField (InputExpr m a) ->
  InputPayload m a ->
  ConvertM m (ExpressionU m a)
convertGetField (Expr.GetField recExpr gTag@(Expr.Tag tagGuid)) exprPl = do
  tagParamInfos <- (^. ConvertM.scTagParamInfos) <$> ConvertM.readContext
  let
    mkGetVar (tag, jumpTo) = do
      name <- getStoredNameS tag
      pure GetVar
        { _gvName = name
        , _gvIdentifier = tag
        , _gvJumpTo = pure jumpTo
        , _gvVarType = GetFieldParameter
        }
  mVar <- traverse mkGetVar $ do
    paramInfo <- Map.lookup tagGuid tagParamInfos
    param <- recExpr ^? ExprLens.exprParameterRef
    guard $ param == ConvertM.tpiFromParameters paramInfo
    return (tagGuid, ConvertM.tpiJumpTo paramInfo)
  ConvertExpr.make exprPl =<<
    case mVar of
    Just var ->
      return $ BodyGetVar var
    Nothing ->
      traverse ConvertM.convertSubexpression
      GetField
      { _gfRecord = recExpr
      , _gfTag = gTag
      }
      <&> gfTag %~
          ( (rPayload . plActions . Lens._Just . wrap .~ WrapNotAllowed) -- Tag cannot be wrapped
          . SugarRemoveTypes.successfulType
          )
      <&> BodyGetField

removeRedundantSubExprTypes :: Expression n m a -> Expression n m a
removeRedundantSubExprTypes =
  rBody %~
  ( Lens.traversed %~ removeRedundantTypes
    & Lens.outside _BodyHole .~
      BodyHole . (Lens.traversed %~ removeRedundantSubExprTypes)
  ) .
  (_BodyApply . aFunc %~ remSuc) .
  (_BodyGetField . gfRecord %~ remSuc) .
  (_BodyLam . lResultType %~ remSuc) .
  (_BodyRecord %~
    (fields . rfTag %~ remSuc) .
    (Lens.filtered ((== KType) . (^. rKind)) . fields . rfExpr %~ remSuc)
  )
  where
    fields = rFields . flItems . Lens.traversed
    remSuc =
      Lens.filtered (Lens.nullOf (rBody . _BodyHole)) %~
      SugarRemoveTypes.successfulType

removeRedundantTypes :: Expression name m a -> Expression name m a
removeRedundantTypes =
  removeRedundantSubExprTypes .
  (Lens.filtered cond %~ SugarRemoveTypes.successfulType)
  where
    cond e =
      Lens.anyOf (rBody . _BodyGetVar) ((/= GetDefinition) . (^. gvVarType)) e ||
      Lens.has (rBody . _BodyRecord) e

convertExpressionI ::
  (Typeable1 m, MonadA m, Monoid a) =>
  InputExpr m a -> ConvertM m (ExpressionU m a)
convertExpressionI ee =
  ($ ee ^. Expr.ePayload) $
  case ee ^. Expr.eBody of
  Expr.VAbs x -> convertLam x
  Expr.VApp x -> ConvertApply.convert x
  Expr.VRec x -> convertRecord x
  Expr.VGetField x -> convertGetField x
  Expr.VLeaf (Expr.VVar x) -> convertVVar x
  Expr.VLeaf (Expr.VLiteralInteger x) -> convertVLiteralInteger x
  Expr.VLeaf Expr.VHole -> ConvertHole.convert
  Expr.VLeaf Expr.Type -> convertAtom "Type"
  Expr.VLeaf Expr.IntegerType -> convertAtom "Int"
  Expr.VLeaf Expr.TagType -> convertAtom "Tag"

-- Check no holes
isCompleteType :: Expr.Expr def par a -> Bool
isCompleteType =
  Lens.nullOf (Lens.folding ExprUtil.subExprs . ExprLens.exprHole)

mkContext ::
  (MonadA m, Typeable1 m) =>
  Anchors.Code (Transaction.MkProperty m) (Tag m) ->
  InferContext m -> InferContext m -> InferContext m ->
  T m (Context m)
mkContext cp holeInferContext structureInferContext withVarsInferContext = do
  specialFunctions <- Transaction.getP $ Anchors.specialFunctions cp
  return Context
    { _scHoleInferContext = holeInferContext
    , _scStructureInferContext = structureInferContext
    , _scWithVarsInferContext = withVarsInferContext
    , _scCodeAnchors = cp
    , _scSpecialFunctions = specialFunctions
    , _scTagParamInfos = mempty
    , _scRecordParamsInfos = mempty
    , scConvertSubexpression = convertExpressionI
    }

convertExpressionPure ::
  (MonadA m, Typeable1 m, RandomGen g, Monoid a) =>
  Anchors.CodeProps m -> g ->
  LoadedExpr m a -> CT m (ExpressionU m a)
convertExpressionPure cp gen res = do
  context <-
    lift $ mkContext cp
    (err "holeInferContext")
    (err "structureInferContext")
    (err "withVarsInferContext")
  fmap removeRedundantTypes .
    ConvertM.run context .
    ConvertM.convertSubexpression $
    InputExpr.makePure gen res
  where
    err x = error $ "convertExpressionPure: " ++ x

data ConventionalParams m a = ConventionalParams
  { cpTags :: [Guid]
  , cpParamInfos :: Map Guid ConvertM.TagParamInfo
  , cpRecordParamsInfos :: Map Guid (ConvertM.RecordParamsInfo m)
  , cpParams :: [FuncParam MStoredName m (ExpressionU m a)]
  , cpAddFirstParam :: T m Guid
  , cpHiddenPayloads :: [InputPayload m a]
  }

data FieldParam m a = FieldParam
  { fpTagGuid :: Guid
  , fpFieldType :: InputExpr m a
  }

mkRecordParams ::
  (MonadA m, Typeable1 m, Monoid a) =>
  ConvertM.RecordParamsInfo m -> Guid -> [FieldParam m a] ->
  InputExpr m a ->
  Maybe (ExprIRef.ExprIM m) ->
  Maybe (LoadedExpr m (Stored m)) ->
  ConvertM m (ConventionalParams m a)
mkRecordParams recordParamsInfo paramGuid fieldParams lambdaExprI mParamTypeI mBodyStored = do
  params <- traverse mkParam fieldParams
  pure ConventionalParams
    { cpTags = fpTagGuid <$> fieldParams
    , cpParamInfos = mconcat $ mkParamInfo <$> fieldParams
    , cpRecordParamsInfos = Map.singleton paramGuid recordParamsInfo
    , cpParams = params
    , cpAddFirstParam =
      addFirstFieldParam lamGuid $
      fromMaybe (error "Record param type must be stored!") mParamTypeI
    , cpHiddenPayloads = [lambdaExprI ^. Expr.ePayload]
    }
  where
    lamGuid = lambdaExprI ^. SugarInfer.exprGuid
    mLambdaP = lambdaExprI ^. Expr.ePayload . ipStored
    tagExprGuid = Guid.combine lamGuid . fpTagGuid
    mkParamInfo fp =
      Map.singleton (fpTagGuid fp) . ConvertM.TagParamInfo paramGuid $ tagExprGuid fp
    mkParam fp = do
      typeS <- ConvertM.convertSubexpression $ fpFieldType fp
      let guid = fpTagGuid fp
      addFuncParamName FuncParam
        { _fpName = Nothing
        , _fpGuid = guid
        , _fpId = -- TOOD: Is this supposed to be the same?
                  -- It used to be different: "Guid.combine lamGuid guid"
                  tagExprGuid fp
        , _fpAltIds = [] -- TODO: fpAltIds still needed?
        , _fpVarKind = FuncFieldParameter
        , _fpType = SugarRemoveTypes.successfulType typeS
        , _fpInferredType = getInferredVal $ fpFieldType fp
        , _fpMActions =
          fpActions (tagExprGuid fp)
          <$> mLambdaP <*> mParamTypeI <*> mBodyStored
        }
    fpActions tagExprGuid lambdaP paramTypeI bodyStored =
      FuncParamActions
      { _fpListItemActions = ListItemActions
        { _itemAddNext = addFieldParamAfter lamGuid tagExprGuid paramTypeI
        , _itemDelete =
          delFieldParam tagExprGuid paramTypeI paramGuid lambdaP bodyStored
        }
      , _fpGetExample = pure $ fakeExample tagExprGuid
      }

type ExprField m = (Expr.Tag, ExprIRef.ExprIM m)
rereadFieldParamTypes ::
  MonadA m =>
  Guid -> ExprIRef.ExprIM m ->
  ([ExprField m] -> ExprField m -> [ExprField m] -> T m Guid) ->
  T m Guid
rereadFieldParamTypes tagExprGuid paramTypeI f = do
  paramType <- ExprIRef.readExprBody paramTypeI
  let
    mBrokenFields =
      paramType ^? Expr._VRec . ExprLens.kindedRecordFields KType .
      (Lens.to . break) ((tagExprGuid ==) . ExprIRef.exprGuid . fst)
  case mBrokenFields of
    Just (prevFields, theField : nextFields) -> f prevFields theField nextFields
    _ -> return tagExprGuid

rewriteFieldParamTypes ::
  MonadA m => ExprIRef.ExprIM m -> [ExprField m] -> T m ()
rewriteFieldParamTypes paramTypeI fields =
  ExprIRef.writeExprBody paramTypeI . Expr.VRec $
  Expr.Record KType fields

addFieldParamAfter :: MonadA m => Guid -> Guid -> ExprIRef.ExprIM m -> T m Guid
addFieldParamAfter lamGuid tagExprGuid paramTypeI =
  rereadFieldParamTypes tagExprGuid paramTypeI $
  \prevFields theField nextFields -> do
    fieldGuid <- Transaction.newKey
    holeTypeI <- DataOps.newHole
    rewriteFieldParamTypes paramTypeI $
      prevFields ++ theField : (Expr.Tag fieldGuid, holeTypeI) : nextFields
    pure $ Guid.combine lamGuid fieldGuid

delFieldParam ::
  MonadA m => Guid -> ExprIRef.ExprIM m -> Guid ->
  Stored m -> LoadedExpr m (Stored m) -> T m Guid
delFieldParam tagExprGuid paramTypeI paramGuid lambdaP bodyStored =
  rereadFieldParamTypes tagExprGuid paramTypeI $
  \prevFields (tag@(Expr.Tag tagG), _) nextFields -> do
    deleteFieldParamRef paramGuid tagG bodyStored
    case prevFields ++ nextFields of
      [] -> error "We were given fewer than 2 field params, which should never happen"
      [(Expr.Tag fieldTagGuid, fieldTypeI)] -> do
        ExprIRef.writeExprBody (Property.value lambdaP) $
          ExprUtil.makeLambda fieldTagGuid fieldTypeI bodyI
        deleteParamRef paramGuid bodyStored
        let
          toGetParam iref =
            ExprIRef.writeExprBody iref $
            ExprLens.bodyParameterRef # fieldTagGuid
        onMatchingSubexprs (toGetParam . Property.value)
          (isGetFieldParam paramGuid fieldTagGuid) bodyStored
        pure $ Guid.combine lamGuid fieldTagGuid
      newFields -> do
        rewriteFieldParamTypes paramTypeI newFields
        pure $ dest prevFields nextFields
  where
    lamGuid = ExprIRef.exprGuid $ Property.value lambdaP
    bodyI = bodyStored ^. Expr.ePayload . Property.pVal
    dest prevFields nextFields =
      fromMaybe (ExprIRef.exprGuid bodyI) . listToMaybe $
      map (getParamGuidFromTagExprI . fst) $ nextFields ++ reverse prevFields
    getParamGuidFromTagExprI (Expr.Tag tagGuid) = Guid.combine lamGuid tagGuid

-- | Convert a (potentially stored) param type hole with inferred val
-- to the unstored inferred val
paramTypeExprMM ::
  Monoid a => InputExpr m a -> InputExpr m a
paramTypeExprMM origParamType
  | Lens.has ExprLens.exprHole origParamType
  , Just inferredParamType <- origParamType ^. SugarInfer.exprInferred
  = paramTypeOfInferredVal $ inferredParamType ^. InferDeref.dValue
  | otherwise = origParamType
  where
    paramTypeGen = genFromHashable $ origParamType ^. SugarInfer.exprGuid
    paramTypeOfInferredVal iVal =
      iVal
      & Lens.mapped .~ mempty
      -- Copy the paramType payload to top-level of inferred val so it
      -- isn't lost:
      & Expr.ePayload .~ origParamType ^. SugarInfer.exprData
      & InputExpr.makePure paramTypeGen

convertDefinitionParams ::
  (MonadA m, Typeable1 m, Monoid a) =>
  ConvertM.RecordParamsInfo m -> [Guid] -> InputExpr m a ->
  ConvertM m
  ( [FuncParam MStoredName m (ExpressionU m a)]
  , ConventionalParams m a
  , InputExpr m a
  )
convertDefinitionParams recordParamsInfo usedTags expr =
  case expr ^. Expr.eBody of
  Expr.VAbs lambda@(Expr.Lam KVal paramGuid origParamType body) -> do
    param <-
      convertPositionalFuncParam lambda (expr ^. Expr.ePayload)
      -- Slightly strange but we mappend the hidden lambda's
      -- annotation into the param type:
      <&> fpType . rPayload . plData <>~ expr ^. SugarInfer.exprData
    let paramType = paramTypeExprMM origParamType
    if SugarInfer.isPolymorphicFunc $ expr ^. Expr.ePayload
      then do -- Dependent:
        (depParams, convParams, deepBody) <- convertDefinitionParams recordParamsInfo usedTags body
        return (param : depParams, convParams, deepBody)
      else -- Independent:
      case paramType ^. Expr.eBody of
      Expr.VRec (Expr.Record KType fields)
        | ListUtils.isLengthAtLeast 2 fields
        , all ((`notElem` usedTags) . fpTagGuid) fieldParams -> do
          convParams <-
            mkRecordParams recordParamsInfo paramGuid fieldParams
            expr (origParamType ^? SugarInfer.exprIRef)
            (traverse (^. ipStored) body)
          return ([], convParams, body)
        where
          fieldParams = map makeFieldParam fields
      _ ->
        pure
        ( []
        , singleConventionalParam stored param paramGuid origParamType body
        , body
        )
  _ -> return ([], emptyConventionalParams stored, expr)
  where
    stored =
      fromMaybe (error "Definition body is always stored!") $
      expr ^. SugarInfer.exprStored
    makeFieldParam (Expr.Tag fieldTagGuid, typeExpr) =
      FieldParam
      { fpTagGuid = fieldTagGuid
      , fpFieldType = typeExpr
      }

singleConventionalParam ::
  MonadA m =>
  Stored m -> FuncParam MStoredName m (ExpressionU m a) ->
  Guid -> InputExpr m a -> InputExpr m a -> ConventionalParams m a
singleConventionalParam lamProp existingParam existingParamGuid existingParamType body =
  ConventionalParams
  { cpTags = []
  , cpParamInfos = Map.empty
  , cpRecordParamsInfos = Map.empty
  , cpParams =
    [ existingParam & fpMActions . Lens._Just . fpListItemActions . itemAddNext .~
      addSecondParam (\old new -> [old, new])
    ]
  , cpAddFirstParam = addSecondParam (\old new -> [new, old])
  , cpHiddenPayloads = []
  }
  where
    existingParamTag = Expr.Tag existingParamGuid
    existingParamTypeIRef =
      fromMaybe (error "Only stored record param type is converted as record") $
      existingParamType ^? SugarInfer.exprIRef
    bodyWithStored =
      fromMaybe (error "Definition body should be stored") $
      traverse (^. ipStored) body
    addSecondParam mkFields = do
      let existingParamField = (existingParamTag, existingParamTypeIRef)
      (newTag, newParamField) <- newField
      newParamTypeI <-
        ExprIRef.newExprBody . Expr.VRec . Expr.Record KType $
        mkFields existingParamField newParamField
      newParamsGuid <- Transaction.newKey
      ExprIRef.writeExprBody (Property.value lamProp) $
        ExprUtil.makeLambda newParamsGuid newParamTypeI .
        Property.value $ bodyWithStored ^. Expr.ePayload
      let
        toGetField iref = do
          recordRef <- ExprIRef.newExprBody $ ExprLens.bodyParameterRef # newParamsGuid
          tagRef <- ExprIRef.newExprBody existingParamTag
          ExprIRef.writeExprBody iref $
            Expr.VGetField Expr.GetField
            { Expr._getFieldRecord = recordRef
            , Expr._getFieldTag = tagRef
            }
      onMatchingSubexprs (toGetField . Property.value)
        (isGetParamOf existingParamGuid) bodyWithStored
      let lamGuid = ExprIRef.exprGuid $ Property.value lamProp
      pure $ Guid.combine lamGuid $ newTag ^. Lens.from Expr.tag

emptyConventionalParams :: MonadA m => ExprIRef.ExprProperty m -> ConventionalParams m a
emptyConventionalParams stored = ConventionalParams
  { cpTags = []
  , cpParamInfos = Map.empty
  , cpRecordParamsInfos = Map.empty
  , cpParams = []
  , cpAddFirstParam = lambdaWrap stored
  , cpHiddenPayloads = []
  }

data ExprWhereItem m a = ExprWhereItem
  { ewiBody :: LoadedExpr m a
  , ewiParamGuid :: Guid
  , ewiArg :: LoadedExpr m a
  , ewiHiddenPayloads :: [a]
  , ewiInferredType :: LoadedExpr m ()
  }

mExtractWhere :: InputExpr m a -> Maybe (ExprWhereItem m (InputPayload m a))
mExtractWhere expr = do
  Expr.Apply func arg <- expr ^? ExprLens.exprApply
  (paramGuid, paramType, body) <- func ^? ExprLens.exprKindedLam KVal
  -- paramType has to be Hole for this to be sugarred to Where
  paramType ^? ExprLens.exprHole
  Just ExprWhereItem
    { ewiBody = body
    , ewiParamGuid = paramGuid
    , ewiArg = arg
    , ewiHiddenPayloads = (^. Expr.ePayload) <$> [expr, func, paramType]
    , ewiInferredType = getInferredVal paramType
    }

convertWhereItems ::
  (MonadA m, Typeable1 m, Monoid a) =>
  [Guid] ->
  InputExpr m a ->
  ConvertM m ([WhereItem MStoredName m (ExpressionU m a)], InputExpr m a)
convertWhereItems usedTags expr =
  case mExtractWhere expr of
  Nothing -> return ([], expr)
  Just ewi -> do
    let
      defGuid = ewiParamGuid ewi
      recordParamsInfo =
        ConvertM.RecordParamsInfo defGuid $ pure defGuid
    value <-
      convertDefinitionContent recordParamsInfo usedTags $
      ewiArg ewi
    let
      mkWIActions topLevelProp bodyStored =
        ListItemActions
        { _itemDelete = do
             deleteParamRef defGuid bodyStored
             SugarInfer.replaceWith topLevelProp $ bodyStored ^. Expr.ePayload
        , _itemAddNext = fst <$> DataOps.redexWrap topLevelProp
        }
    name <- getStoredNameS defGuid
    let
      hiddenData = ewiHiddenPayloads ewi ^. Lens.traversed . ipData
      item = WhereItem
        { _wiValue =
            value
            & dBody . rPayload . plData <>~ hiddenData
        , _wiGuid = defGuid
        , _wiActions =
            mkWIActions <$>
            expr ^. SugarInfer.exprStored <*>
            traverse (^. ipStored) (ewiBody ewi)
        , _wiName = name
        , _wiInferredType = ewiInferredType ewi
        }
    (nextItems, whereBody) <- convertWhereItems usedTags $ ewiBody ewi
    return (item : nextItems, whereBody)

newField :: MonadA m => T m (Expr.Tag, ExprIRef.ExprIM m)
newField = (,) <$> (Expr.Tag <$> Transaction.newKey) <*> DataOps.newHole

addFirstFieldParam :: MonadA m => Guid -> ExprIRef.ExprIM m -> T m Guid
addFirstFieldParam lamGuid recordI = do
  recordBody <- ExprIRef.readExprBody recordI
  case recordBody ^? Expr._VRec . ExprLens.kindedRecordFields KType of
    Just fields -> do
      (newTag, field) <- newField
      ExprIRef.writeExprBody recordI $
        Expr.VRec . Expr.Record KType $ field : fields
      pure $ Guid.combine lamGuid $ newTag ^. Lens.from Expr.tag
    _ -> pure $ ExprIRef.exprGuid recordI

assertedGetProp ::
  String ->
  Expr.Expr def par (InputPayloadP inferred (Maybe stored) a) -> stored
assertedGetProp _
  Expr.Expr
  { Expr._ePayload =
    InputPayload { _ipStored = Just prop } } = prop
assertedGetProp msg _ = error msg

convertDefinitionContent ::
  (MonadA m, Typeable1 m, Monoid a) =>
  ConvertM.RecordParamsInfo m -> [Guid] -> InputExpr m a ->
  ConvertM m (DefinitionContent MStoredName m (ExpressionU m a))
convertDefinitionContent recordParamsInfo usedTags expr = do
  (depParams, convParams, funcBody) <-
    convertDefinitionParams recordParamsInfo usedTags expr
  ConvertM.local
    ((ConvertM.scTagParamInfos <>~ cpParamInfos convParams) .
     (ConvertM.scRecordParamsInfos <>~ cpRecordParamsInfos convParams)) $ do
      (whereItems, whereBody) <-
        convertWhereItems (usedTags ++ cpTags convParams) funcBody
      bodyS <- ConvertM.convertSubexpression whereBody
      return DefinitionContent
        { _dDepParams = depParams
        , _dParams = cpParams convParams
        , _dBody =
          bodyS
          & rPayload . plData <>~
            cpHiddenPayloads convParams ^. Lens.traversed . ipData
        , _dWhereItems = whereItems
        , _dAddFirstParam = cpAddFirstParam convParams
        , _dAddInnermostWhereItem =
          fmap fst . DataOps.redexWrap $
          assertedGetProp "Where must be stored" whereBody
        }

convertDefIBuiltin ::
  (Typeable1 m, MonadA m) => Anchors.CodeProps m ->
  Definition.Builtin -> DefIM m ->
  LoadedExpr m (Stored m) ->
  CT m (DefinitionBody MStoredName m (ExpressionU m [Guid]))
convertDefIBuiltin cp (Definition.Builtin name) defI defType =
  DefinitionBodyBuiltin <$> do
    defTypeS <-
      defType
      & void
      & convertExpressionPure cp iDefTypeGen
      <&> Lens.mapped . Lens.mapped .~ mempty
    pure DefinitionBuiltin
      { biName = name
      , biMSetName = Just setName
      , biType = defTypeS
      }
  where
    defGuid = IRef.guid defI
    iDefTypeGen = ConvertExpr.mkGen 1 3 defGuid
    typeIRef = Property.value (defType ^. Expr.ePayload)
    setName =
      Transaction.writeIRef defI .
      (`Definition.Body` typeIRef) .
      Definition.ContentBuiltin . Definition.Builtin

makeTypeInfo ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Anchors.CodeProps m -> DefIM m ->
  LoadedExpr m (Stored m, a) ->
  LoadedExpr m a ->
  CT m (DefinitionTypeInfo m (ExpressionU m a))
makeTypeInfo cp defI defType inferredType = do
  defTypeS <- conv iDefTypeGen $ snd <$> defType
  case () of
    ()
      | not (isCompleteType inferredType) ->
        DefinitionIncompleteType <$> do
          inferredTypeS <- conv iNewTypeGen inferredType
          pure ShowIncompleteType
            { sitOldType = defTypeS
            , sitNewIncompleteType = inferredTypeS
            }
      | typesMatch -> pure $ DefinitionExportedTypeInfo defTypeS
      | otherwise ->
        DefinitionNewType <$> do
          inferredTypeS <- conv iNewTypeGen inferredType
          pure AcceptNewType
            { antOldType = defTypeS
            , antNewType = inferredTypeS
            , antAccept =
              inferredType
              & ExprLens.exprDef %~ (^. ldDef)
              & void
              & ExprIRef.newExpr
              >>= Property.set (defType ^. Expr.ePayload . Lens._1)
            }
  where
    conv = convertExpressionPure cp
    iDefTypeGen = ConvertExpr.mkGen 1 3 defGuid
    iNewTypeGen = ConvertExpr.mkGen 2 3 defGuid
    typesMatch = (snd <$> defType) `ExprUtil.alphaEq` inferredType
    defGuid = IRef.guid defI

convertDefIExpr ::
  (MonadA m, Typeable1 m) => Anchors.CodeProps m ->
  ExprIRef.ExprM m (Load.ExprPropertyClosure (Tag m)) ->
  DefIM m ->
  LoadedExpr m (Stored m) ->
  CT m (DefinitionBody MStoredName m (ExpressionU m [Guid]))
convertDefIExpr cp exprLoaded defI defType = do
  ilr@SugarInfer.InferredWithImplicits
    { SugarInfer._iwiExpr = iwiExpr
    } <-
    SugarInfer.inferAddImplicits inferLoadedGen defI
    exprLoaded initialContext rootNode
  let
    inferredType =
      iwiExpr ^. SugarInfer.exprInferred . InferDeref.dType & void
    addStoredGuids lens x = (x, ExprIRef.exprGuid . Property.value <$> x ^.. lens)
    guidIntoPl (pl, x) = pl & ipData %~ \() -> x
  typeInfo <-
    makeTypeInfo cp defI (addStoredGuids id <$> defType) (mempty <$ inferredType)
  context <-
    lift $ mkContext cp
    (ilr ^. SugarInfer.iwiBaseInferContext)
    (ilr ^. SugarInfer.iwiStructureInferContext)
    (ilr ^. SugarInfer.iwiInferContext)
  ConvertM.run context $ do
    content <-
      iwiExpr
      <&> guidIntoPl . addStoredGuids (ipStored . Lens._Just)
      & traverse . ipInferred %~ Just
      & convertDefinitionContent recordParamsInfo []
    return $ DefinitionBodyExpression DefinitionExpression
      { _deContent = removeRedundantTypes <$> content
      , _deTypeInfo = typeInfo
      }
  where
    (rootNode, initialContext) = initialInferContext defI
    defGuid = IRef.guid defI
    recordParamsInfo = ConvertM.RecordParamsInfo defGuid $ jumpToDefI cp defI
    inferLoadedGen = ConvertExpr.mkGen 0 3 defGuid

convertDefI ::
  (MonadA m, Typeable1 m) =>
  Anchors.CodeProps m ->
  -- TODO: Use DefinitionClosure?
  Definition.Definition
    (ExprIRef.ExprM m (Load.ExprPropertyClosure (Tag m)))
    (ExprIRef.DefIM m) ->
  CT m (Definition (Maybe String) m (ExpressionU m [Guid]))
convertDefI cp (Definition.Definition (Definition.Body bodyContent typeLoaded) defI) = do
  let
    typeLoadedFake =
      typeLoaded
      -- TODO: HACK HACK HACK, fix this!!
      & ExprLens.exprDef %~ (`LoadedDef` error "Tried accessing ref of a def of defType")
  bodyS <- convertDefContent bodyContent $ Load.exprPropertyOfClosure <$> typeLoadedFake
  name <- lift $ ConvertExpr.getStoredName defGuid
  return Definition
    { _drGuid = defGuid
    , _drName = name
    , _drBody = bodyS
    }
  where
    defGuid = IRef.guid defI
    convertDefContent (Definition.ContentBuiltin builtin) =
      convertDefIBuiltin cp builtin defI
    convertDefContent (Definition.ContentExpr exprLoaded) =
      convertDefIExpr cp exprLoaded defI
