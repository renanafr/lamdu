module Lamdu.Infer.Trigger
  ( module Lamdu.Infer.Trigger.Types
  , add, checkOrAdd, updateRefData
  ) where

import Control.Applicative ((<$))
import Control.Lens.Operators
import Control.Lens.Utils (_fromJust)
import Control.Monad (filterM, when)
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Lamdu.Infer.Context (Context)
import Lamdu.Infer.Monad (Infer)
import Lamdu.Infer.RefData (RefData, scopeNormalizeParamRefs)
import Lamdu.Infer.RefTags (ExprRef, ParamRef)
import Lamdu.Infer.Rule.Types (RuleRef)
import Lamdu.Infer.Trigger.Types
  ( Trigger(..), ParameterRefEvent(..)
  , Fired(..), _FiredDirectlyTag, _FiredKnownBody, _FiredParameterRef, _FiredUnify
  )
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.OpaqueRef as OR
import qualified Data.Set as Set
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Expr as Expr
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Infer.Context as Context
import qualified Lamdu.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Infer.Monad as InferM
import qualified Lamdu.Infer.RefData as RefData
import qualified Lamdu.Infer.Rule.Types as Rule

remember ::
  MonadA m =>
  ExprRef def -> [RefData.Restriction def] ->
  RefData def -> Trigger def -> RuleRef def ->
  StateT (Context def) m ()
remember rep restrictions refData trigger ruleId = do
  Lens.zoom Context.ufExprs . UFData.writeRep rep $
    refData
    & RefData.rdTriggers . Lens.at ruleId <>~ Just (Set.singleton trigger)
    & RefData.rdRestrictions %~ (restrictions ++)
  Context.ruleMap . Rule.rmMap . Lens.at ruleId .
    _fromJust "Trigger.remember to missing rule" .
    Rule.ruleTriggersIn <>= OR.refSetSingleton rep

checkParameterRef :: ParamRef def -> RefData def -> Infer def (Maybe (Fired def))
checkParameterRef triggerGuidRef refData
  | Lens.nullOf (RefData.rdScope . RefData.scopeParamRefs) refData =
    -- Scope is empty so this cannot be a parameter Ref
    answer triggerGuidRef TheParameterOutOfScope
  | otherwise = do
    triggerGuidRep <- InferM.liftGuidAliases $ GuidAliases.find triggerGuidRef
    -- Our caller must hand us a normalized scope
    if triggerGuidRep `notElem` (refData ^.. RefData.rdScope . RefData.scopeParamRefs)
      then answer triggerGuidRep TheParameterOutOfScope
      else
        case refData ^. RefData.rdBody of
        Expr.VLeaf (Expr.VVar (Expr.ParameterRef guid)) -> do
          guidRep <- InferM.liftGuidAliases $ GuidAliases.getRep guid
          answer triggerGuidRep $
            if triggerGuidRep == guidRep
            then IsTheParameterRef
            else NotTheParameterRef
        Expr.VLeaf Expr.VHole -> return Nothing
        _ -> answer triggerGuidRep NotTheParameterRef
  where
    answer ref = return . Just . FiredParameterRef ref

{-# INLINE checkKnownBody #-}
checkKnownBody :: RefData def -> Maybe (Fired def)
checkKnownBody refData
  | Lens.has ExprLens.bodyHole body = Nothing
  | otherwise = Just . FiredKnownBody $ body & ExprLens.bodyDef .~ ()
  where
    body = refData ^. RefData.rdBody

-- | Must be called with RefData with normalized scope
{-# INLINE checkTrigger #-}
checkTrigger :: RefData def -> Trigger def -> Infer def (Maybe (Fired def))
checkTrigger refData trigger =
  case trigger of
  OnKnownBody -> return $ checkKnownBody refData
  OnParameterRef triggerGuidRef -> checkParameterRef triggerGuidRef refData
  OnUnify -> return Nothing -- unification trigger is handled in unify

-- | Must be called with RefData with normalized scope
handleTrigger :: ExprRef def -> RefData def -> RuleRef def -> Trigger def -> Infer def Bool
handleTrigger rep refData ruleId trigger = do
  mRes <- checkTrigger refData trigger
  case mRes of
    Nothing -> return True
    Just fired -> False <$ InferM.ruleTrigger ruleId rep fired

-- | Must be called with RefData with normalized scope
updateRefData :: ExprRef def -> RefData def -> Infer def (RefData def)
updateRefData rep refData =
  refData &
  RefData.rdTriggers %%~
  fmap (OR.refMapFilter (not . Set.null)) .
  Lens.itraverse onTriggers
  where
    onTriggers ruleId =
      fmap Set.fromList .
      filterM (handleTrigger rep refData ruleId) .
      Set.toList

{-# INLINE checkOrAdd #-}
checkOrAdd ::
  [RefData.Restriction def] -> Trigger def -> RuleRef def -> ExprRef def ->
  Infer def (Maybe (Fired def))
checkOrAdd restrictions trigger ruleId ref = do
  rep <- InferM.liftUFExprs $ UFData.find ref
  refData <- InferM.liftUFExprs . State.gets $ UFData.readRep rep
  -- TODO: The tests pass even with the un-normalized Scope. Is there
  -- some guarantee that when we're called here, scope is always
  -- already normalized?
  refDataNorm <- refData & RefData.rdScope %%~ InferM.liftGuidAliases . scopeNormalizeParamRefs
  mFired <- checkTrigger refDataNorm trigger
  when (Lens.has Lens._Nothing mFired) .
    InferM.liftContext $ remember rep restrictions refData trigger ruleId
  return mFired

{-# INLINE add #-}
add ::
  [RefData.Restriction def] -> Trigger def -> RuleRef def -> ExprRef def ->
  Infer def ()
add restrictions trigger ruleId ref = do
  rep <- InferM.liftUFExprs $ UFData.find ref
  mFired <- checkOrAdd restrictions trigger ruleId rep
  mFired & Lens.traverseOf_ Lens._Just (InferM.ruleTrigger ruleId rep)
