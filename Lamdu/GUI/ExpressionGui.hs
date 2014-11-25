{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Lamdu.GUI.ExpressionGui
  ( ExpressionGui(..), egWidget, egAlignment
  -- General:
  , fromValueWidget
  , scaleFromTop
  , pad
  , hbox, hboxSpaced, addBelow
  , makeRow
  -- Lifted widgets:
  , makeLabel, makeColoredLabel
  , makeFocusableView
  , makeNameView
  , makeNameEdit
  , withBgColor
  -- Info adding
  , TypeStyle(..), addType -- TODO: s/type/info
  -- Expression wrapping
  , MyPrecedence(..), ParentPrecedence(..), Precedence
  , parenify
  -- | stdWrap/stdPostProcess means addTypes and wrapExprEventMap
  , stdWrap
  , stdWrapDelegated
  , stdWrapParentExpr
  , stdWrapParenify
  , addInferredTypes
  , wrapExprEventMap
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Function (on)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Animation (AnimId, Layer)
import Graphics.UI.Bottle.Widget (Widget)
import Graphics.UI.Bottle.Widgets.Box (KBox)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, HolePickers)
import Lamdu.GUI.ExpressionGui.Types (WidgetT, MyPrecedence(..), ParentPrecedence(..), Precedence, ExpressionGui(..), egWidget, egAlignment)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

fromValueWidget :: WidgetT m -> ExpressionGui m
fromValueWidget widget = ExpressionGui widget 0.5

-- | Scale the given ExpressionGui without moving its top alignment
-- point:
scaleFromTop :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
scaleFromTop ratio (ExpressionGui widget alignment) =
  ExpressionGui (Widget.scale ratio widget) (alignment / (ratio ^. Lens._2))

pad :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
pad padding (ExpressionGui widget alignment) =
  ExpressionGui newWidget $
  (padding ^. Lens._2 + alignment * widget ^. height) / newWidget ^. height
  where
    height = Widget.wSize . Lens._2
    newWidget = Widget.pad padding widget

hbox :: [ExpressionGui m] -> ExpressionGui m
hbox guis =
  ExpressionGui (Box.toWidget box) $
  case box ^. Box.boxContent of
  ((_, x) : _) -> x ^. Grid.elementAlign . Lens._2
  _ -> error "hbox must not get empty list :("
  where
    box = Box.make Box.horizontal $ map f guis
    f (ExpressionGui widget alignment) = (Vector2 0.5 alignment, widget)

hboxSpaced :: [ExpressionGui m] -> ExpressionGui m
hboxSpaced = hbox . List.intersperse (fromValueWidget BWidgets.stdSpaceWidget)

fromBox :: KBox Bool (Transaction m) -> ExpressionGui m
fromBox box =
  ExpressionGui (Box.toWidget box) alignment
  where
    alignment =
      maybe (error "True disappeared from box list?!")
        (^. Grid.elementAlign . Lens._2) .
      lookup True $ box ^. Box.boxContent

addBelow ::
  Widget.R ->
  [(Box.Alignment, WidgetT m)] ->
  ExpressionGui m ->
  ExpressionGui m
addBelow egHAlign ws eg =
  fromBox . Box.makeKeyed Box.vertical $
  (True, (Vector2 egHAlign (eg ^. egAlignment), eg ^. egWidget)) :
  map ((,) False) ws

data TypeStyle = HorizLine | Background

wWidth :: Lens' (Widget f) Widget.R
wWidth = Widget.wSize . Lens._1

addType ::
  Config ->
  TypeStyle ->
  Widget.Id ->
  [WidgetT m] ->
  ExpressionGui m ->
  ExpressionGui m
addType _ _ _ [] eg = eg
addType config style exprId typeEdits eg =
  addBelow 0.5 items eg
  where
    items = middleElement : [(0.5, annotatedTypes)]
    middleElement =
      case style of
      HorizLine -> (0.5, Spacer.makeHorizLine underlineId (Vector2 width 1))
      Background -> (0.5, Spacer.makeWidget 5)
    annotatedTypes =
      addBackground . (wWidth .~ width) $
      Widget.translate (Vector2 ((width - typesBox ^. wWidth)/2) 0) typesBox
    width = on max (^. wWidth) (eg ^. egWidget) typesBox
    typesBox = Box.vboxCentered typeEdits
    isError = length typeEdits >= 2
    bgAnimId = Widget.toAnimId exprId ++ ["type background"]
    addBackground = maybe id (Widget.backgroundColor (Config.layerTypes (Config.layers config)) bgAnimId) bgColor
    bgColor
      | isError = Just $ Config.inferredTypeErrorBGColor config
      | otherwise = do
        Background <- Just style
        return $ Config.inferredTypeBGColor config
    underlineId = WidgetIds.underlineId $ Widget.toAnimId exprId

parentExprFDConfig :: Config -> FocusDelegator.Config
parentExprFDConfig config = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = Config.enterSubexpressionKeys config
  , FocusDelegator.startDelegatingDoc = EventMap.Doc ["Navigation", "Enter subexpression"]
  , FocusDelegator.stopDelegatingKeys = Config.leaveSubexpressionKeys config
  , FocusDelegator.stopDelegatingDoc = EventMap.Doc ["Navigation", "Leave subexpression"]
  }

-- ExprGuiM GUIs (TODO: Move to Monad.hs?)

disallowedNameChars :: [(Char, EventMap.IsShifted)]
disallowedNameChars =
  EventMap.anyShiftedChars "[]\\`()" ++
  [ ('0', EventMap.Shifted)
  , ('9', EventMap.Shifted)
  ]

makeBridge ::
  MonadA m =>
  (Widget.Id -> WE.WidgetEnvT m (Widget f)) ->
  (Widget.Id -> WE.WidgetEnvT m (Widget f)) ->
  Widget.Id -> WE.WidgetEnvT m (Widget f)
makeBridge mkFocused mkUnfocused myId = do
  isFocused <- WE.isSubCursor myId
  (if isFocused then mkFocused else mkUnfocused) myId

nameSrcTint :: Config -> Sugar.NameSource -> Widget f -> Widget f
nameSrcTint config Sugar.NameSourceAutoGenerated = Widget.tint $ Config.autoGeneratedNameTint config
nameSrcTint _ Sugar.NameSourceStored = id

makeNameEdit ::
  MonadA m =>
  Sugar.NameProperty Sugar.Name m -> Widget.Id -> ExprGuiM m (WidgetT m)
makeNameEdit (Sugar.NameProperty (Sugar.Name nameSrc nameCollision name) _ setName) myId = do
  collisionSuffixes <-
    ExprGuiM.widgetEnv . makeCollisionSuffixLabels nameCollision $
    Widget.toAnimId myId
  config <- ExprGuiM.widgetEnv WE.readConfig
  nameEdit <-
    fmap (nameSrcTint config nameSrc) .
    ExprGuiM.widgetEnv .
    WE.localEnv (WE.envTextStyle . TextEdit.sEmptyFocusedString .~ "") $
    makeEditor nameProp
  return . Box.hboxCentered $ nameEdit : collisionSuffixes
  where
    nameProp = Property storedName setName
    storedName =
      case nameSrc of
      Sugar.NameSourceAutoGenerated -> ""
      Sugar.NameSourceStored -> name
    makeEditor property =
      makeBridge (makeWordEdit property) (BWidgets.makeFocusableTextView name) myId
    makeWordEdit =
      BWidgets.makeWordEdit <&>
      Lens.mapped . Lens.mapped . Widget.wEventMap %~
      EventMap.filterSChars (curry (`notElem` disallowedNameChars))

stdWrap ::
  MonadA m => Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (ExpressionGui m) ->
  ExprGuiM m (ExpressionGui m)
stdWrap pl mkGui = wrapExprEventMap pl $ addInferredTypes pl =<< mkGui

stdWrapDelegated ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  FocusDelegator.Config -> FocusDelegator.IsDelegating ->
  (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
stdWrapDelegated pl fdConfig isDelegating f =
  stdWrap pl . ExprGuiM.wrapDelegated fdConfig isDelegating (egWidget %~) f

stdWrapParentExpr ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
stdWrapParentExpr pl f myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  stdWrapDelegated pl (parentExprFDConfig config) FocusDelegator.Delegating f myId

makeLabel ::
  MonadA m => String -> Widget.Id -> ExprGuiM m (WidgetT f)
makeLabel text myId = ExprGuiM.widgetEnv . BWidgets.makeLabel text $ Widget.toAnimId myId

makeColoredLabel ::
  MonadA m => Int -> Draw.Color -> String -> Widget.Id -> ExprGuiM m (WidgetT f)
makeColoredLabel textSize color text myId =
  ExprGuiM.localEnv (WE.setTextSizeColor textSize color) $
  makeLabel text myId

makeFocusableView ::
  (MonadA m, MonadA n) => Widget.Id -> ExpressionGui n -> ExprGuiM m (ExpressionGui n)
makeFocusableView myId gui =
  ExprGuiM.widgetEnv $
  egWidget (BWidgets.makeFocusableView myId) gui

parenify ::
  MonadA m =>
  ParentPrecedence -> MyPrecedence ->
  (Widget.Id -> ExpressionGui f -> ExprGuiM m (ExpressionGui f)) ->
  (Widget.Id -> ExprGuiM m (ExpressionGui f)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui f)
parenify (ParentPrecedence parent) (MyPrecedence prec) addParens mkWidget myId
  | parent > prec = addParens myId =<< mkWidget myId
  | otherwise = mkWidget myId

stdWrapParenify ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  ParentPrecedence -> MyPrecedence ->
  (Widget.Id -> ExpressionGui m -> ExprGuiM m (ExpressionGui m)) ->
  (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
stdWrapParenify pl parentPrec prec addParens =
  stdWrapParentExpr pl . parenify parentPrec prec addParens

withBgColor :: Layer -> Draw.Color -> AnimId -> ExpressionGui m -> ExpressionGui m
withBgColor layer color animId =
  egWidget %~ Widget.backgroundColor layer animId color

makeRow :: [(Widget.R, ExpressionGui m)] -> [(Vector2 Widget.R, WidgetT m)]
makeRow =
  map item
  where
    item (halign, ExpressionGui widget alignment) =
      (Vector2 halign alignment, widget)

makeNameView :: MonadA m => Sugar.Name -> AnimId -> WE.WidgetEnvT m (Widget f)
makeNameView (Sugar.Name nameSrc collision name) animId = do
  label <- BWidgets.makeLabel name animId
  suffixLabels <- makeCollisionSuffixLabels collision $ animId ++ ["suffix"]
  config <- WE.readConfig
  return .
    nameSrcTint config nameSrc .
    Box.hboxCentered $ label : suffixLabels

makeCollisionSuffixLabels ::
  MonadA m => Sugar.NameCollision -> AnimId -> WE.WidgetEnvT m [Widget f]
makeCollisionSuffixLabels Sugar.NoCollision _ = return []
makeCollisionSuffixLabels (Sugar.Collision suffix) animId = do
  config <- WE.readConfig
  let
    onSuffixWidget =
      Widget.backgroundColor (Config.layerNameCollisionBG (Config.layers config))
        (animId ++ ["bg"]) (Config.collisionSuffixBGColor config) .
      Widget.scale (realToFrac <$> Config.collisionSuffixScaleFactor config)
  BWidgets.makeLabel (show suffix) animId
    & (WE.localEnv . WE.setTextColor . Config.collisionSuffixTextColor) config
    <&> (:[]) . onSuffixWidget

wrapExprEventMap ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (ExpressionGui m) -> ExprGuiM m (ExpressionGui m)
wrapExprEventMap pl action = do
  (res, resultPickers) <- ExprGuiM.listenResultPickers action
  addExprEventMap pl resultPickers res

addExprEventMap ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload -> HolePickers m ->
  ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addExprEventMap pl resultPickers gui = do
  exprEventMap <-
    ExprEventMap.make (gui ^. egWidget . Widget.wIsFocused)
    resultPickers pl
  gui & egWidget %~ Widget.weakerEvents exprEventMap & return

addInferredTypes ::
  MonadA m =>
  Sugar.Payload m a ->
  ExpressionGui m ->
  ExprGuiM m (ExpressionGui m)
addInferredTypes _exprPl = return -- eg = do
  -- config <- ExprGuiM.widgetEnv WE.readConfig
  -- typeEdits <-
  --   exprPl ^. Sugar.plInferredTypes
  --   & Lens.traversed . Lens.mapped . Lens.mapped .~
  --     ExprGuiM.emptyPayload
  --   & Lens.traversed (ExprGuiM.makeSubexpression 0)
  --   <&>
  --     map
  --     ( Widget.tint (Config.inferredTypeTint config)
  --     . Widget.scale (realToFrac <$> Config.typeScaleFactor config)
  --     . (^. egWidget)
  --     )
  -- return $ addType config Background exprId typeEdits eg
  -- where
  --   exprId = WidgetIds.fromGuid $ exprPl ^. Sugar.plEntityId
