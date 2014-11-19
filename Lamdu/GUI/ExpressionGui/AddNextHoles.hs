{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Lamdu.GUI.ExpressionGui.AddNextHoles
  ( addToDef, addToExpr
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Applicative.Utils (when)
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad.Trans.State (State, evalState)
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Sugar.Types as Sugar

addToDef ::
  MonadA m =>
  Sugar.Definition name m (Sugar.Expression name rw m ExprGuiM.Payload) ->
  Sugar.Definition name m (Sugar.Expression name rw m ExprGuiM.Payload)
addToDef def =
  def
  & Lens.mapped %~ addJumpToExprMarkers
  & (`evalState` Nothing) . defExprs addPrevHoles
  & (`evalState` Nothing) . defExprs addNextHoles
  & Lens.mapped . Lens.mapped . Sugar.plData %~ snd

-- | The exprs of the Definition that we want to include in the
-- next/prev hole traversals (e.g: don't include the def's FuncParams)
defExprs :: Lens.Traversal' (Sugar.Definition name rw m expr) expr
defExprs =
  Sugar.drBody . Sugar._DefinitionBodyExpression .
  Sugar.deContent . defContentExprs

defContentExprs :: Lens.Traversal' (Sugar.DefinitionContent name rw m expr) expr
defContentExprs f defContent =
  mkDefContent
  <$> f (defContent ^. Sugar.dBody)
  <*> (Lens.traverse . Sugar.wiValue . defContentExprs) f (defContent ^. Sugar.dWhereItems)
  where
    mkDefContent newBody newWhereItems =
      defContent
      & Sugar.dBody .~ newBody
      & Sugar.dWhereItems .~ newWhereItems

addToExpr ::
  MonadA m =>
  Sugar.Expression name rw m ExprGuiM.Payload ->
  Sugar.Expression name rw m ExprGuiM.Payload
addToExpr expr =
  expr
  & addJumpToExprMarkers
  & (`evalState` Nothing) . addPrevHoles
  & (`evalState` Nothing) . addNextHoles
  & Lens.mapped . Sugar.plData %~ snd

addNextHoles ::
  Sugar.Expression name rw m (Bool, ExprGuiM.Payload) ->
  State (Maybe Guid) (Sugar.Expression name m (Bool, ExprGuiM.Payload))
addNextHoles = Lens.backwards Lens.traverse %%~ setGuid ExprGuiM.hgMNextHole

addPrevHoles ::
  Sugar.Expression name rw m (Bool, ExprGuiM.Payload) ->
  State (Maybe Guid) (Sugar.Expression name rw m (Bool, ExprGuiM.Payload))
addPrevHoles = Lens.traverse %%~ setGuid ExprGuiM.hgMPrevHole

addJumpToExprMarkers ::
  Sugar.Expression name rw m ExprGuiM.Payload ->
  Sugar.Expression name rw m (Bool, ExprGuiM.Payload)
addJumpToExprMarkers expr =
  expr
  & Lens.mapped . Sugar.plData %~ (,) False
  & jumpToExprPayloads . Lens._1 .~ True
  & Lens.mapped %~ removeNonStoredMarks
  where
    removeNonStoredMarks pl =
      case pl ^. Sugar.plActions of
      Nothing -> pl & Sugar.plData . Lens._1 .~ False
      Just _ -> pl

jumpToExprPayloads :: Lens.Traversal' (Sugar.Expression name rw m a) a
jumpToExprPayloads f expr =
  case expr ^. Sugar.rBody of
  Sugar.BodyHole _ -> mark
  Sugar.BodyCollapsed _ -> pure expr
  Sugar.BodyLam _
    -> (Sugar.rBody . Sugar._BodyLam . Sugar.lResult) (jumpToExprPayloads f) expr
  _ -> (Sugar.rBody . Lens.traverse) (jumpToExprPayloads f) expr
  where
    mark = (Sugar.rPayload . Sugar.plData) f expr

setGuid ::
  Lens' ExprGuiM.HoleGuids (Maybe Guid) ->
  Sugar.Payload m (Bool, ExprGuiM.Payload) ->
  State (Maybe Guid) (Sugar.Payload m (Bool, ExprGuiM.Payload))
setGuid lens pl =
  setIt <$>
  State.get <*
  when (Lens.anyOf Sugar.plData fst pl)
    (State.put (Just (pl ^. Sugar.plGuid)))
  where
    setIt x = pl & Sugar.plData . Lens._2 . ExprGuiM.plHoleGuids . lens .~ x
