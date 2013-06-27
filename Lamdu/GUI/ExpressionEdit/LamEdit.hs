{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LamEdit
  ( make, makeParamNameEdit, makeParamEdit
  ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.Parens as Parens
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

paramFDConfig :: FocusDelegator.Config
paramFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename parameter"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEsc]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Done renaming"]
  }

makeParamNameEdit ::
  MonadA m =>
  ExprGuiM.HoleGuids -> Sugar.Name -> Guid -> Widget.Id ->
  ExprGuiM m (WidgetT m)
makeParamNameEdit hg name ident myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  isSelected <- ExprGuiM.widgetEnv $ WE.isSubCursor myId
  jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap [] hg
  ExprGuiM.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
    (ExprGuiM.withFgColor (Config.paramOriginColor config) .
     ExpressionGui.makeNameEdit name ident) myId
    <&> if isSelected
        then Widget.weakerEvents jumpHolesEventMap
        else id

compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- exported for use in definition sugaring.
makeParamEdit ::
  MonadA m =>
  ExprGuiM.HoleGuids -> Widget.Id ->
  Sugar.FuncParam Sugar.Name m (ExprGuiM.SugarExpr m) ->
  ExprGuiM m (ExpressionGui m)
makeParamEdit hg prevId param =
  assignCursor $ do
    paramTypeEdit <- ExprGuiM.makeSubexpression 0 $ param ^. Sugar.fpType
    paramNameEdit <- makeParamNameEdit hg name (param ^. Sugar.fpGuid) myId
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      paramAddNextEventMap =
        maybe mempty
        (Widget.keysEventMapMovesCursor (Config.addNextParamKeys config)
         (E.Doc ["Edit", "Add next parameter"]) .
         fmap (FocusDelegator.delegatingId . WidgetIds.fromGuid) .
         (^. Sugar.fpListItemActions . Sugar.itemAddNext))
        mActions
      paramEventMap = mconcat
        [ paramDeleteEventMap (Config.delForwardKeys config) "" id
        , paramDeleteEventMap (Config.delBackwardKeys config) " backwards" (const prevId)
        , paramAddNextEventMap
        ]
    return .
      (ExpressionGui.egWidget %~ Widget.weakerEvents paramEventMap) .
      ExpressionGui.addType config ExpressionGui.HorizLine myId
      [paramTypeEdit ^. ExpressionGui.egWidget] $
      ExpressionGui.fromValueWidget paramNameEdit
  where
    name = param ^. Sugar.fpName
    assignGuidToMe = (`ExprGuiM.assignCursor` myId) . WidgetIds.fromGuid
    assignCursor = compose . map assignGuidToMe $ param ^. Sugar.fpAltIds
    myId = WidgetIds.fromGuid $ param ^. Sugar.fpId
    mActions = param ^. Sugar.fpMActions
    paramDeleteEventMap keys docSuffix onId =
      maybe mempty
      (Widget.keysEventMapMovesCursor keys (E.Doc ["Edit", "Delete parameter" ++ docSuffix]) .
       fmap (onId . WidgetIds.fromGuid) .
       (^. Sugar.fpListItemActions . Sugar.itemDelete))
      mActions

rightArrowTextSize :: Sugar.Kind -> Config.Config -> Int
rightArrowTextSize Sugar.KVal = Config.lambdaArrowTextSize
rightArrowTextSize Sugar.KType = Config.piArrowTextSize

rightArrowColor :: Sugar.Kind -> Config.Config -> Draw.Color
rightArrowColor Sugar.KVal = Config.lambdaArrowColor
rightArrowColor Sugar.KType = Config.piArrowColor

make ::
  MonadA m =>
  ExpressionGui.ParentPrecedence ->
  Sugar.Lam Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make parentPrecedence (Sugar.Lam k param _isDep body) pl =
  ExpressionGui.stdWrapParenify pl parentPrecedence (ExpressionGui.MyPrecedence 0)
  Parens.addHighlightedTextParens $ \myId ->
  ExprGuiM.assignCursor myId typeId $ do
    (resultTypeEdit, usedVars) <-
      ExprGuiM.listenUsedVariables $
      ExprGuiM.makeSubexpression 0 body
    let
      paramUsed = k == Sugar.KVal || paramGuid `elem` usedVars
      redirectCursor cursor
        | paramUsed = cursor
        | otherwise =
          case Widget.subId paramId cursor of
          Nothing -> cursor
          Just _ -> typeId
    ExprGuiM.localEnv (WE.envCursor %~ redirectCursor) $ do
      paramTypeEdit <- ExprGuiM.makeSubexpression 1 $ param ^. Sugar.fpType
      paramEdit <-
        if paramUsed
        then do
          paramNameEdit <-
            makeParamNameEdit
            (param ^. Sugar.fpType . Sugar.rPayload . Sugar.plData . ExprGuiM.plHoleGuids)
            name paramGuid paramId
          colonLabel <- ExprGuiM.widgetEnv . BWidgets.makeLabel ":" $ Widget.toAnimId paramId
          return $ ExpressionGui.hbox
            [ ExpressionGui.fromValueWidget paramNameEdit
            , ExpressionGui.fromValueWidget colonLabel
            , paramTypeEdit
            ]
        else return paramTypeEdit
      config <- ExprGuiM.widgetEnv WE.readConfig
      rightArrowLabel <-
        ExprGuiM.localEnv
        (WE.setTextSizeColor
         (rightArrowTextSize k config)
         (rightArrowColor k config)) .
        ExprGuiM.widgetEnv . BWidgets.makeLabel "→" $ Widget.toAnimId myId
      let
        addBg
          | k == Sugar.KType && paramUsed =
              ExpressionGui.egWidget %~
              Widget.backgroundColor
              (Config.layerCollapsedExpandedBG (Config.layers config))
              (mappend (Widget.toAnimId paramId) ["polymorphic bg"])
              (Config.collapsedExpandedBGColor config)
          | otherwise = id
        paramAndArrow =
          addBg $
          ExpressionGui.hboxSpaced
          [paramEdit, ExpressionGui.fromValueWidget rightArrowLabel]
      return $ ExpressionGui.hboxSpaced [paramAndArrow, resultTypeEdit]
  where
    name = param ^. Sugar.fpName
    paramGuid = param ^. Sugar.fpGuid
    paramId = WidgetIds.fromGuid $ param ^. Sugar.fpId
    typeId =
      WidgetIds.fromGuid $ param ^. Sugar.fpType . Sugar.rPayload . Sugar.plGuid
