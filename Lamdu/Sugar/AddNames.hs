{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, TemplateHaskell, RankNTypes, DeriveGeneric #-}
module Lamdu.Sugar.AddNames
  ( addToDef
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad ((<=<))
import Control.Monad.Trans.Reader (Reader, runReader)
import Control.Monad.Trans.State (runState, evalState)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Monoid.Generic (def_mempty, def_mappend)
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable, traverse)
import GHC.Generics (Generic)
import Lamdu.Expr.Type (Type)
import Lamdu.Sugar.AddNames.CPS (CPS(..))
import Lamdu.Sugar.AddNames.NameGen (NameGen)
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Sugar.AddNames.NameGen as NameGen

type CPSNameConvertor m rw tm = Guid -> NameProperty (OldName m) rw tm -> CPS m (NameProperty (NewName m) rw tm)
type NameConvertor m rw tm = Guid -> NameProperty (OldName m) rw tm -> m (NameProperty (NewName m) rw tm)

newtype RunMonad m = RunMonad (forall a. m a -> a)

class MonadA m => MonadNaming m where
  type OldName m
  type NewName m
  opRun :: m (RunMonad m)

  opWithParamName :: NameGen.IsFunction -> CPSNameConvertor m rw tm
  opWithWhereItemName :: NameGen.IsFunction -> CPSNameConvertor m rw tm
  opWithDefName :: CPSNameConvertor m rw tm
  opWithTagName :: CPSNameConvertor m rw tm
  opGetDefName :: NameConvertor m rw tm
  opGetTagName :: NameConvertor m rw tm
  opGetParamName :: NameConvertor m rw tm
  opGetHiddenParamsName :: NameConvertor m rw tm

newtype SetList a = SetList { getSetList :: [a] }
  deriving (Show)
instance Eq a => Monoid (SetList a) where
  mempty = SetList []
  SetList xs `mappend` SetList ys = SetList $ xs ++ filter (`notElem` xs) ys

type StoredName = String
newtype NameGuidMap = NameGuidMap (Map StoredName (SetList Guid))
  deriving Show

type instance Lens.Index NameGuidMap = StoredName
type instance Lens.IxValue NameGuidMap = SetList Guid

-- ghc-7.7.20131205 fails deriving these instances on its own.
instance Lens.Ixed NameGuidMap where
  ix k f (NameGuidMap m) = NameGuidMap <$> Lens.ix k f m
  {-# INLINE ix #-}
instance Lens.At NameGuidMap where
  at k f (NameGuidMap m) = NameGuidMap <$> Lens.at k f m
  {-# INLINE at #-}

instance Monoid NameGuidMap where
  mempty = NameGuidMap Map.empty
  NameGuidMap x `mappend` NameGuidMap y =
    NameGuidMap $ Map.unionWith mappend x y

nameGuidMapSingleton :: StoredName -> Guid -> NameGuidMap
nameGuidMapSingleton name guid = NameGuidMap . Map.singleton name $ SetList [guid]

data StoredNamesWithin = StoredNamesWithin
  { _snwGuidMap :: NameGuidMap
  -- Names of tags and defs: considered conflicted if used in two
  -- different meanings anywhere in the whole definition:
  , _snwGlobalNames :: NameGuidMap
  } deriving (Generic)
Lens.makeLenses ''StoredNamesWithin
instance Monoid StoredNamesWithin where
  mempty = def_mempty
  mappend = def_mappend

-- Pass 0:
data StoredNames = StoredNames
  { storedName :: Maybe StoredName
  , storedNamesWithin :: StoredNamesWithin
  }
newtype Pass0M a = Pass0M (Writer StoredNamesWithin a)
  deriving (Functor, Applicative, Monad)
p0TellStoredNames :: StoredNamesWithin -> Pass0M ()
p0TellStoredNames = Pass0M . Writer.tell
p0ListenStoredNames :: Pass0M a -> Pass0M (a, StoredNamesWithin)
p0ListenStoredNames (Pass0M act) = Pass0M $ Writer.listen act
runPass0M :: Pass0M a -> (a, StoredNamesWithin)
runPass0M (Pass0M act) = runWriter act

data NameScope = Local | Global

instance MonadNaming Pass0M where
  type OldName Pass0M = MStoredName
  type NewName Pass0M = StoredNames
  opRun = pure $ RunMonad $ fst . runPass0M
  opWithParamName _ = p0cpsNameConvertor Local
  opWithWhereItemName _ = p0cpsNameConvertor Local
  opWithDefName = p0cpsNameConvertor Local
  opWithTagName = p0cpsNameConvertor Local
  opGetParamName = p0nameConvertor Local
  opGetHiddenParamsName = p0nameConvertor Local
  opGetTagName = p0nameConvertor Global
  opGetDefName = p0nameConvertor Global

pass0Result ::
  NameScope -> Guid -> NameProperty MStoredName rw tm ->
  Pass0M (StoredNamesWithin -> NameProperty StoredNames rw tm)
pass0Result scope guid nameProp =
  nameProp
  & npName %%~ go
  <&> Lens.sequenceAOf npName
  where
    go mName = do
      p0TellStoredNames myStoredNamesWithin
      pure $ \storedNamesUnder -> StoredNames
        { storedName = mName
        , storedNamesWithin = myStoredNamesWithin `mappend` storedNamesUnder
        }
      where
        myStoredNamesWithin =
          maybe mempty (buildStoredNamesWithin . (`nameGuidMapSingleton` guid)) mName
        buildStoredNamesWithin myNameGuidMap =
          StoredNamesWithin myNameGuidMap $
          globalNames myNameGuidMap
        globalNames myNameGuidMap =
          case scope of
          Local -> mempty
          Global -> myNameGuidMap

p0nameConvertor :: NameScope -> NameConvertor Pass0M rw tm
p0nameConvertor scope guid nameProp =
  ($ mempty) <$> pass0Result scope guid nameProp

p0cpsNameConvertor :: NameScope -> CPSNameConvertor Pass0M rw tm
p0cpsNameConvertor scope guid mName = CPS $ \k -> do
  result <- pass0Result scope guid mName
  (res, storedNamesBelow) <- p0ListenStoredNames k
  pure (result storedNamesBelow, res)

-- Pass 1:
data P1Env = P1Env
  { _p1NameGen :: NameGen Guid
  , _p1StoredNameSuffixes :: Map Guid Int
  , _p1StoredNames :: Set String
  }
Lens.makeLenses ''P1Env

newtype Pass1M a = Pass1M (Reader P1Env a)
  deriving (Functor, Applicative, Monad)
runPass1M :: P1Env -> Pass1M a -> a
runPass1M initial (Pass1M act) = runReader act initial
p1GetEnv :: Pass1M P1Env
p1GetEnv = Pass1M Reader.ask
p1WithEnv :: (P1Env -> P1Env) -> Pass1M a -> Pass1M a
p1WithEnv f (Pass1M act) = Pass1M $ Reader.local f act

instance MonadNaming Pass1M where
  type OldName Pass1M = StoredNames
  type NewName Pass1M = Name
  opRun = (\x -> RunMonad (runPass1M x)) <$> p1GetEnv
  opWithDefName = p1cpsNameConvertorGlobal "def_"
  opWithTagName = p1cpsNameConvertorGlobal "tag_"
  opWithParamName = p1cpsNameConvertorLocal
  opWithWhereItemName = p1cpsNameConvertorLocal
  opGetParamName guid nameProp =
    nameProp & npName %%~
    \(StoredNames name storedNamesUnder) ->
    case name of
      Just str ->
        makeStoredName str storedNamesUnder guid <$> p1GetEnv
      Nothing ->
        do
          nameGen <- (^. p1NameGen) <$> p1GetEnv
          pure . Name NameSourceAutoGenerated NoCollision $
            evalState (NameGen.existingName guid) nameGen
  opGetHiddenParamsName _ nameProp =
    nameProp & npName %%~
    \(StoredNames mName _) ->
    pure $ maybe (Name NameSourceAutoGenerated NoCollision "params") (Name NameSourceStored NoCollision) mName
  opGetTagName = p1nameConvertor "tag_"
  opGetDefName = p1nameConvertor "def_"

makeStoredName :: StoredName -> StoredNamesWithin -> Guid -> P1Env -> Name
makeStoredName storedName storedNamesBelow guid env =
  fst $ makeStoredNameEnv storedName storedNamesBelow guid env

compose :: [a -> a] -> a -> a
compose = foldr (.) id

makeStoredNameEnv ::
  StoredName -> StoredNamesWithin -> Guid -> P1Env -> (Name, P1Env)
makeStoredNameEnv storedName storedNamesBelow guid env =
  (Name NameSourceStored collision storedName, newEnv)
  where
    (collision, newEnv) =
      case (mSuffixFromAbove, collidingGuids) of
        (Just suffix, _) -> (Collision suffix, env)
        (Nothing, []) -> (NoCollision, envWithName [])
        (Nothing, otherGuids) -> (Collision 0, envWithName (guid:otherGuids))
    envWithName guids = env
      & p1StoredNames %~ Set.insert storedName
      -- This name is first occurence, so we get suffix 0
      & p1StoredNameSuffixes %~ compose ((Lens.itraversed %@~ flip Map.insert) guids)
    mSuffixFromAbove =
      Map.lookup guid $ env ^. p1StoredNameSuffixes
    collidingGuids =
      maybe [] (filter (/= guid) . getSetList) $
      storedNamesBelow ^. snwGuidMap . Lens.at storedName

p1cpsNameConvertor ::
  Guid -> NameProperty StoredNames rw tm ->
  (StoredNamesWithin -> P1Env -> (Name, P1Env)) ->
  CPS Pass1M (NameProperty Name rw tm)
p1cpsNameConvertor guid (NameProperty storedNames setName) nameMaker =
  CPS $ \k -> do
    oldEnv <- p1GetEnv
    let
      (name, newEnv) =
        case storedNames of
        StoredNames (Just storedName) storedNamesBelow ->
          makeStoredNameEnv storedName storedNamesBelow guid oldEnv
        StoredNames Nothing storedNamesBelow ->
          nameMaker storedNamesBelow oldEnv
    res <- p1WithEnv (const newEnv) k
    return (NameProperty name setName, res)

p1cpsNameConvertorGlobal :: String -> CPSNameConvertor Pass1M rw tm
p1cpsNameConvertorGlobal prefix guid nameProp =
  p1cpsNameConvertor guid nameProp $
  \_ p1env -> (makeGuidName prefix guid, p1env)

p1cpsNameConvertorLocal :: NameGen.IsFunction -> CPSNameConvertor Pass1M rw tm
p1cpsNameConvertorLocal isFunction guid nameProp =
  p1cpsNameConvertor guid nameProp $
  \storedNamesBelow p1env ->
  (`runState` p1env) . Lens.zoom p1NameGen $
    let
      conflict name =
        Lens.has (snwGuidMap . Lens.at name . Lens._Just) storedNamesBelow ||
        (p1env ^. p1StoredNames . Lens.contains name)
    in
      Name NameSourceAutoGenerated NoCollision <$>
      NameGen.newName (not . conflict) isFunction guid

p1nameConvertor :: String -> NameConvertor Pass1M rw tm
p1nameConvertor prefix guid (NameProperty (StoredNames mName storedNamesBelow) setName) =
  (`NameProperty` setName) <$>
  case mName of
  Just str -> makeStoredName str storedNamesBelow guid <$> p1GetEnv
  Nothing -> pure $ makeGuidName prefix guid

makeGuidName :: Show guid => String -> guid -> Name
makeGuidName prefix guid = Name NameSourceAutoGenerated NoCollision $ prefix ++ show guid

isFunctionType :: Type -> NameGen.IsFunction
isFunctionType T.TFun {} = NameGen.Function
isFunctionType _ = NameGen.NotFunction

withFuncParam ::
  (MonadA tm, MonadNaming m) =>
  FuncParam (OldName m) rw tm ->
  CPS m (FuncParam (NewName m) rw tm)
withFuncParam fp@FuncParam{..} = CPS $ \k -> do
  (name, res) <-
    case _fpVarKind of
    FuncParameter ->
      runCPS
      (opWithParamName (isFunctionType _fpInferredType) _fpGuid _fpName) k
    FuncFieldParameter ->
      runCPS (opWithTagName _fpGuid _fpName) k
  pure
    ( fp { _fpName = name }
    , res
    )

toLam ::
  (MonadA tm, MonadNaming m, Traversable rw) =>
  Lam (OldName m) rw tm (Expression (OldName m) rw tm a) ->
  m (Lam (NewName m) rw tm (Expression (NewName m) rw tm a))
toLam lam@Lam {..} = do
  (param, result) <- runCPS (withFuncParam _lParam) $ toExpression _lResult
  pure lam { _lParam = param, _lResult = result }

toTagG :: MonadNaming m => TagG (OldName m) rw tm -> m (TagG (NewName m) rw tm)
toTagG tagG@TagG {..} = tagGName (opGetTagName (UniqueId.toGuid _tagVal)) tagG

toRecordField ::
  (MonadA tm, MonadNaming m, Traversable rw) =>
  RecordField (OldName m) rw tm (Expression (OldName m) rw tm a) ->
  m (RecordField (NewName m) rw tm (Expression (NewName m) rw tm a))
toRecordField recordField@RecordField {..} = do
  tag <- toTagG _rfTag
  expr <- toExpression _rfExpr
  pure recordField
    { _rfTag = tag
    , _rfExpr = expr
    }

toRecord ::
  (MonadA tm, MonadNaming m, Traversable rw) =>
  Record (OldName m) rw tm (Expression (OldName m) rw tm a) ->
  m (Record (NewName m) rw tm (Expression (NewName m) rw tm a))
toRecord record@Record {..} = do
  items <- traverse toRecordField _rItems
  pure record { _rItems = items }

toGetField ::
  (MonadA tm, MonadNaming m, Traversable rw) =>
  GetField (OldName m) rw tm (Expression (OldName m) rw tm a) ->
  m (GetField (NewName m) rw tm (Expression (NewName m) rw tm a))
toGetField getField@GetField {..} = do
  record <- toExpression _gfRecord
  tag <- toTagG _gfTag
  pure getField { _gfRecord = record, _gfTag = tag }

toScope :: MonadNaming m => Scope (OldName m) rw tm -> m (Scope (NewName m) rw tm)
toScope (Scope l g t p) =
  Scope
  <$> (traverse . Lens._1) toGetVar l
  <*> (traverse . Lens._1) toGetVar g
  <*> (traverse . Lens._1) toTag t
  <*> (traverse . Lens._1) toGetParams p

toHoleActions ::
  (MonadA tm, MonadNaming m) =>
  HoleActions (OldName m) tm ->
  m (HoleActions (NewName m) tm)
toHoleActions ha@HoleActions {..} = do
  RunMonad run <- opRun
  pure ha
    { _holeScope =
      fmap (run . toScope) _holeScope
    , holeResult =
      (fmap . fmap . fmap . fmap) (run . holeResultConverted toExpression) holeResult
    }

toInferred ::
  (MonadA tm, MonadNaming m) =>
  HoleInferred (OldName m) tm ->
  m (HoleInferred (NewName m) tm)
toInferred inferred = do
  RunMonad run <- opRun
  inferred
    & hiMakeConverted . Lens.mapped . Lens.mapped %~ run . toExpression
    & pure

toHole ::
  (MonadA tm, MonadNaming m, Traversable rw) =>
  Hole (OldName m) rw tm (Expression (OldName m) rw tm a) ->
  m (Hole (NewName m) rw tm (Expression (NewName m) rw tm a))
toHole hole@Hole {..} = do
  mActions <- _holeMActions & Lens.traversed %%~ toHoleActions
  inferred <- toInferred _holeInferred
  mArg <- _holeMArg & Lens._Just . Lens.traversed %%~ toExpression
  pure hole
    { _holeMActions = mActions
    , _holeMArg = mArg
    , _holeInferred = inferred
    }

toCollapsed ::
  (MonadA tm, MonadNaming m, Traversable rw) =>
  Collapsed (OldName m) rw tm (Expression (OldName m) rw tm a) ->
  m (Collapsed (NewName m) rw tm (Expression (NewName m) rw tm a))
toCollapsed Collapsed {..} = do
  compact <- toGetVar _cCompact
  fullExpression <- toExpression _cFullExpression
  pure Collapsed { _cCompact = compact, _cFullExpression = fullExpression, .. }

toTag ::
  MonadNaming m => TagG (OldName m) rw tm ->
  m (TagG (NewName m) rw tm)
toTag (TagG inst tag oldName) = do
  name <- opGetTagName (UniqueId.toGuid tag) oldName
  pure $ TagG inst tag name

toGetVar ::
  MonadNaming m => GetVar (OldName m) rw tm ->
  m (GetVar (NewName m) rw tm)
toGetVar getVar@GetVar{..} =
  gvName (f _gvIdentifier) getVar
  where
    f =
      case _gvVarType of
      GetParameter -> opGetParamName
      GetFieldParameter -> opGetTagName
      GetDefinition -> opGetDefName

toGetParams ::
  MonadNaming m => GetParams (OldName m) rw tm ->
  m (GetParams (NewName m) rw tm)
toGetParams getParams@GetParams{..} =
  gpDefName (opGetDefName _gpDefGuid) getParams

toApply ::
  (MonadNaming m, MonadA tm, Traversable rw) =>
  Apply (OldName m) rw tm (Expression (OldName m) rw tm a) ->
  m (Apply (NewName m) rw tm (Expression (NewName m) rw tm a))
toApply la@Apply{..} = do
  func <- toExpression _aFunc
  specialArgs <- traverse toExpression _aSpecialArgs
  annotatedArgs <- traverse (aaTag toTag <=< aaExpr toExpression) _aAnnotatedArgs
  pure la
    { _aFunc = func
    , _aSpecialArgs = specialArgs
    , _aAnnotatedArgs = annotatedArgs
    }

traverseToExpr ::
  (MonadA tm, MonadNaming m, Traversable t, Traversable rw) =>
  (t (Expression (NewName m) rw tm a) -> b) -> t (Expression (OldName m) rw tm a) ->
  m b
traverseToExpr cons body = cons <$> traverse toExpression body

toBody ::
  (MonadA tm, MonadNaming m, Traversable rw) =>
  Body (OldName m) rw tm (Expression (OldName m) rw tm a) ->
  m (Body (NewName m) rw tm (Expression (NewName m) rw tm a))
toBody (BodyList x)           = traverseToExpr BodyList x
toBody (BodyLiteralInteger x) = pure $ BodyLiteralInteger x
--
toBody (BodyGetField x) = BodyGetField <$> toGetField x
toBody (BodyRecord x) = BodyRecord <$> toRecord x
toBody (BodyLam x) = BodyLam <$> toLam x
toBody (BodyApply x) = BodyApply <$> toApply x
toBody (BodyHole x) = BodyHole <$> toHole x
toBody (BodyCollapsed x) = BodyCollapsed <$> toCollapsed x
toBody (BodyGetVar x) = BodyGetVar <$> toGetVar x
toBody (BodyGetParams x) = BodyGetParams <$> toGetParams x

toExpression ::
  (MonadA tm, MonadNaming m, Traversable rw) => Expression (OldName m) rw tm a ->
  m (Expression (NewName m) rw tm a)
toExpression = rBody toBody

withWhereItem ::
  (MonadA tm, MonadNaming m, Traversable rw) =>
  WhereItem (OldName m) rw tm (Expression (OldName m) rw tm a) ->
  CPS m (WhereItem (NewName m) rw tm (Expression (NewName m) rw tm a))
withWhereItem item@WhereItem{..} = CPS $ \k -> do
  (name, (value, res)) <-
    runCPS (opWithWhereItemName (isFunctionType _wiInferredType) _wiGuid _wiName) $
    (,) <$> toDefinitionContent _wiValue <*> k
  pure (item { _wiValue = value, _wiName = name }, res)

toDefinitionContent ::
  (MonadA tm, MonadNaming m, Traversable rw) =>
  DefinitionContent (OldName m) rw tm (Expression (OldName m) rw tm a) ->
  m (DefinitionContent (NewName m) rw tm (Expression (NewName m) rw tm a))
toDefinitionContent def@DefinitionContent{..} = do
  (depParams, (params, (whereItems, body))) <-
    runCPS (traverse withFuncParam _dDepParams) .
    runCPS (traverse withFuncParam _dParams) .
    runCPS (traverse withWhereItem _dWhereItems) $
    toExpression _dBody
  pure def
    { _dDepParams = depParams
    , _dParams = params
    , _dBody = body
    , _dWhereItems = whereItems
    }

toDefinitionBody ::
  (MonadA tm, MonadNaming m, Traversable rw) =>
  DefinitionBody (OldName m) rw tm (Expression (OldName m) rw tm a) ->
  m (DefinitionBody (NewName m) rw tm (Expression (NewName m) rw tm a))
toDefinitionBody (DefinitionBodyBuiltin bi) =
  pure (DefinitionBodyBuiltin bi)
toDefinitionBody
  (DefinitionBodyExpression (DefinitionExpression typeInfo content)) =
    DefinitionBodyExpression <$>
    (DefinitionExpression typeInfo <$> toDefinitionContent content)

toDef ::
  (MonadA tm, MonadNaming m, Traversable rw) =>
  Definition (OldName m) rw tm (Expression (OldName m) rw tm a) ->
  m (Definition (NewName m) rw tm (Expression (NewName m) rw tm a))
toDef def@Definition {..} = do
  (name, body) <-
    runCPS (opWithDefName _drGuid _drName) $ toDefinitionBody _drBody
  pure def { _drName = name, _drBody = body }

addToDef :: (MonadA m, Traversable rw) => DefinitionU rw m a -> DefinitionN rw m a
addToDef =
  pass1 . runPass0M . toDef
  where
    emptyP1Env (NameGuidMap globalNamesMap) = P1Env
      { _p1NameGen = NameGen.initial
      , _p1StoredNames = mempty
      , _p1StoredNameSuffixes =
        mconcat .
        map Map.fromList . filter (ListUtils.isLengthAtLeast 2) .
        map ((`zip` [0..]) . getSetList) . Map.elems $ globalNamesMap
      }
    pass1 (def, storedNamesBelow) =
      runPass1M (emptyP1Env (storedNamesBelow ^. snwGlobalNames)) $ toDef def
