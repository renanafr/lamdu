module Lamdu.GUI.ExpressionEdit.EventMap
  ( make
  , modifyEventMap
  , applyOperatorEventMap
  , cutEventMap
  , jumpHolesEventMap, jumpHolesEventMapIfSelected
  , replaceOrComeToParentEventMap
  ) where

import Control.Applicative ((<$>), Applicative(..), liftA2)
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.CharClassification (operatorChars)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState(..), setHoleStateAndJump)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, HolePickers, holePickersAddDocPrefix, holePickersAction)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

make ::
  MonadA m => Bool -> HolePickers m -> Sugar.Payload Maybe m ExprGuiM.Payload ->
  ExprGuiM m (EventHandlers (T m))
make isFocused holePickers pl =
  mconcat <$> sequenceA
  [ maybe (return mempty)
    (actionsEventMap holePickers (pl ^. Sugar.plGuid))
    (pl ^. Sugar.plActions)
  , jumpHolesEventMapIfSelected holePickers pl
  , replaceOrComeToParentEventMap isFocused pl
  ]

mkEventMap ::
  (Functor f, Functor g) =>
  [E.ModKey] -> E.Doc ->
  (f Widget.Id -> g Widget.Id) ->
  f Guid -> EventHandlers g
mkEventMap keys doc f =
  Widget.keysEventMapMovesCursor keys doc .
  f . fmap WidgetIds.fromGuid

mkEventMapWithPickers ::
  (Functor f, MonadA m) =>
  HolePickers m ->
  [E.ModKey] -> E.Doc ->
  (f Widget.Id -> T m Widget.Id) ->
  f Guid -> EventHandlers (T m)
mkEventMapWithPickers holePickers keys doc f =
  E.keyPresses keys doc .
  liftA2 mappend (holePickersAction holePickers) .
  fmap Widget.eventResultFromCursor . f .
  fmap WidgetIds.fromGuid

isExprSelected :: MonadA m => Sugar.Payload Maybe f a -> ExprGuiM m Bool
isExprSelected pl =
  ExprGuiM.widgetEnv . WE.isSubCursor . WidgetIds.fromGuid $
  pl ^. Sugar.plGuid

jumpHolesEventMap ::
  MonadA m => HolePickers m ->
  ExprGuiM.HoleGuids ->
  ExprGuiM m (EventHandlers (T m))
jumpHolesEventMap holePickers hg = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    jumpEventMap keys dirStr lens =
      maybe mempty
      (mkEventMapWithPickers holePickers (keys config)
       (E.Doc ["Navigation", jumpDoc dirStr]) id . pure) $
      hg ^. lens
  pure $
    mconcat
    [ jumpEventMap Config.jumpToNextHoleKeys "next" ExprGuiM.hgMNextHole
    , jumpEventMap Config.jumpToPrevHoleKeys "previous" ExprGuiM.hgMPrevHole
    ]
  where
    jumpDoc dirStr =
      holePickersAddDocPrefix holePickers $ "Jump to " ++ dirStr ++ " hole"

jumpHolesEventMapIfSelected ::
  MonadA m => HolePickers m ->
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (EventHandlers (T m))
jumpHolesEventMapIfSelected holePickers pl = do
  isSelected <- isExprSelected pl
  if isSelected
    then jumpHolesEventMap holePickers $ pl ^. Sugar.plData . ExprGuiM.plHoleGuids
    else pure mempty

cutEventMap :: Functor m => Config -> Sugar.Actions m -> EventHandlers (T m)
cutEventMap config actions =
  mkEventMap (Config.cutKeys config) (E.Doc ["Edit", "Cut"]) id $
  actions ^. Sugar.cut

replaceOrComeToParentEventMap ::
  MonadA m =>
  Bool ->
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (EventHandlers (T m))
replaceOrComeToParentEventMap isFocused pl =
  do
    isSelected <- isExprSelected pl
    config <- ExprGuiM.widgetEnv WE.readConfig
    let delKeys = Config.replaceKeys config ++ Config.delKeys config
    return $ if isSelected
      then maybe mempty (replaceEventMap config) $ pl ^. Sugar.plActions
      else
        if isFocused
        then
          mkEventMap delKeys (E.Doc ["Navigation", "Select parent"])
          (fmap FocusDelegator.notDelegatingId) . return $
          pl ^. Sugar.plGuid
        else mempty

actionsEventMap ::
  MonadA m =>
  HolePickers m -> Guid ->
  Sugar.Actions m ->
  ExprGuiM m (EventHandlers (T m))
actionsEventMap holePickers guid actions = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  return $ mconcat
    [ wrapEventMap holePickers config
    , applyOperatorEventMap holePickers guid
    , cutEventMap config
    ] actions

applyOperatorEventMap ::
  MonadA m => HolePickers m -> Guid -> Sugar.Actions m -> EventHandlers (T m)
applyOperatorEventMap holePickers guid actions =
  case actions ^. Sugar.wrap of
  Sugar.WrapAction wrap -> action wrap
  Sugar.WrapperAlready -> action $ return guid
  Sugar.WrappedAlready applyGuid -> action $ return applyGuid
  Sugar.WrapNotAllowed -> mempty
  where
    doc = E.Doc ["Edit", holePickersAddDocPrefix holePickers "Apply operator"]
    action wrap =
      E.charGroup "Operator" doc operatorChars $ \c _isShifted ->
        mappend
        <$> holePickersAction holePickers
        <*>
          (Widget.eventResultFromCursor <$>
           (setHoleStateAndJump (HoleState [c]) =<< wrap))

wrapEventMap ::
  MonadA m =>
  HolePickers m -> Config ->
  Sugar.Actions m -> EventHandlers (T m)
wrapEventMap holePickers config actions =
  case actions ^. Sugar.wrap of
  Sugar.WrapAction wrap ->
    mkEventMapWithPickers holePickers
    (Config.wrapKeys config)
    (E.Doc ["Edit", holePickersAddDocPrefix holePickers "Wrap"])
    (fmap FocusDelegator.delegatingId) wrap
  Sugar.WrapperAlready -> mempty
  Sugar.WrappedAlready _ -> mempty
  Sugar.WrapNotAllowed -> mempty

replaceEventMap :: MonadA m => Config -> Sugar.Actions m -> EventHandlers (T m)
replaceEventMap config actions =
  mconcat
  [ actionEventMap (Sugar.setToInnerExpr . Sugar._SetToInnerExpr)
    "Replace with inner expression" $ Config.delKeys config
  , actionEventMap (Sugar.setToHole . Sugar._SetToHole)
    "Replace expression" delKeys
  ]
  where
    actionEventMap l doc keys =
      maybe mempty (mkEventMap keys (E.Doc ["Edit", doc]) id) $
      actions ^? l
    delKeys = Config.replaceKeys config ++ Config.delKeys config

modifyEventMap ::
  MonadA m => HolePickers m -> Config ->
  Guid -> Sugar.Actions m -> EventHandlers (T m)
modifyEventMap holePickers config guid =
  mconcat
  [ wrapEventMap holePickers config
  , applyOperatorEventMap holePickers guid
  , replaceEventMap config
  ]
