{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.ApplyEdit
  ( make
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (traverse, sequenceA)
import Lamdu.GUI.ExpressionGui (ExpressionGui, ParentPrecedence(..), MyPrecedence(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

infixPrecedence :: ExpressionGui.Precedence
infixPrecedence = 5

prefixPrecedence :: ExpressionGui.Precedence
prefixPrecedence = 10

make ::
  MonadA m => ParentPrecedence ->
  Sugar.Apply Sugar.Name (ExprGuiM.SugarExpr m) ->
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make (ParentPrecedence parentPrecedence) (Sugar.Apply func specialArgs annotatedArgs) pl myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    maybeOverrideHoleWrap
      | null annotatedArgs = id
      | otherwise = overrideModifyEventMap
    overrideModifyEventMap =
      ExpressionGui.egWidget %~
      Widget.strongerEvents
      (maybe mempty (ExprEventMap.modifyEventMap [] config) (pl ^. Sugar.plActions))
  case specialArgs of
    Sugar.NoSpecialArgs ->
      mk prefixPrecedence $
      overrideModifyEventMap <$> ExprGuiM.makeSubexpression 0 func
    Sugar.ObjectArg arg ->
      mk prefixPrecedence $ ExpressionGui.hboxSpaced <$> sequenceA
      [ maybeOverrideHoleWrap <$> ExprGuiM.makeSubexpression (prefixPrecedence+1) func
      , ExprGuiM.makeSubexpression prefixPrecedence arg
      ]
    Sugar.InfixArgs l r ->
      mk infixPrecedence $ ExpressionGui.hboxSpaced <$> sequenceA
      [ ExprGuiM.makeSubexpression (infixPrecedence+1) l
      , -- TODO: What precedence to give when it must be atomic?:
        overrideModifyEventMap <$> ExprGuiM.makeSubexpression 20 func
      , ExprGuiM.makeSubexpression (infixPrecedence+1) r
      ]
  where
    destGuid = func ^. Sugar.rPayload . Sugar.plGuid
    mk precedence mkFuncRow =
      mkMBoxed pl
      (ParentPrecedence parentPrecedence)
      (MyPrecedence precedence)
      destGuid mkFuncRow annotatedArgs myId

assignCursorGuid :: MonadA m => Widget.Id -> Guid -> ExprGuiM m a -> ExprGuiM m a
assignCursorGuid myId = ExprGuiM.assignCursor myId . WidgetIds.fromGuid

makeTagView :: MonadA m => Guid -> Sugar.TagG Sugar.Name -> ExprGuiM m (ExpressionGui m)
makeTagView tagExprGuid tagG =
  TagEdit.makeView tagG . Widget.toAnimId $
  WidgetIds.fromGuid tagExprGuid

makeArgRows ::
  MonadA m =>
  Sugar.AnnotatedArg Sugar.Name (ExprGuiM.SugarExpr m) ->
  ExprGuiM m [[(Grid.Alignment, ExprGuiM.WidgetT m)]]
makeArgRows arg = do
  argTagEdit <- makeTagView (arg ^. Sugar.aaTagExprGuid) (arg ^. Sugar.aaTag)
  argValEdit <- ExprGuiM.makeSubexpression 0 $ arg ^. Sugar.aaExpr
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    scaleTag =
      ExpressionGui.egWidget %~
      Widget.scale (realToFrac <$> Config.fieldTagScaleFactor config)
  pure
    [ [ (0.5, (BWidgets.vspaceWidget . realToFrac . Config.spaceBetweenAnnotatedArgs) config)
      -- TODO: This is a workaround for GridEdit bug that
      -- differing-length'd rows break navigation
      , (0.5, Spacer.empty)
      , (0.5, Spacer.empty)
      ]
    , ExpressionGui.makeRow
      [ (0, scaleTag argTagEdit)
      , (0.5, space)
      , (0, argValEdit)
      ]
    ]
  where
    space = ExpressionGui.fromValueWidget BWidgets.stdSpaceWidget

mkMBoxed ::
  MonadA m =>
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  ParentPrecedence ->
  ExpressionGui.MyPrecedence ->
  Guid -> ExprGuiM m (ExpressionGui m) ->
  [Sugar.AnnotatedArg Sugar.Name (ExprGuiM.SugarExpr m)] ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
mkMBoxed pl (ParentPrecedence parentPrecedence) (MyPrecedence myPrecedence) destGuid mkFuncRow annotatedArgs =
  ExpressionGui.stdWrapParentExpr pl $ \myId ->
  assignCursorGuid myId destGuid $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    grid <-
      Grid.toWidget . Grid.make . concat <$> traverse makeArgRows annotatedArgs
    let
      mBox
        | isBoxed =
          ExpressionGui.withBgColor (Config.layerLabeledApplyBG (Config.layers config))
          (Config.labeledApplyBGColor config) (Widget.toAnimId myId ++ ["bg"]) .
          ExpressionGui.pad (realToFrac <$> Config.labeledApplyPadding config)
        | otherwise = id
    mBox . ExpressionGui.addBelow 0 [(0, grid)] <$> mkFuncRow
  where
    isBoxed = myPrecedence <= parentPrecedence || (not . null) annotatedArgs
