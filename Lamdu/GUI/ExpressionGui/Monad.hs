{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ConstraintKinds, TypeFamilies #-}
module Lamdu.GUI.ExpressionGui.Monad
  ( ExprGuiM, WidgetT
  , widgetEnv
  , StoredEntityIds(..), Injected(..)
  , HoleEntityIds(..), hgMNextHole, hgMPrevHole
  , emptyHoleEntityIds
  , Payload(..), plStoredEntityIds, plInjected, plHoleEntityIds
  , emptyPayload
  , SugarExpr

  , transaction, localEnv, withFgColor
  , getP, assignCursor, assignCursorPrefix
  , wrapDelegated
  --
  , makeSubexpression
  --
  , readSettings, readCodeAnchors
  , getCodeAnchor, mkPrejumpPosSaver
  --
  , HolePickers, holePickersAddDocPrefix, holePickersAction
  , addResultPicker, listenResultPickers

  , run
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS (RWST, runRWST)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.GUI.CodeEdit.Settings (Settings)
import Lamdu.GUI.ExpressionGui.Types (ExpressionGui, WidgetT, ParentPrecedence(..), Precedence)
import Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.Char as Char
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

type HolePickers m = [T m Widget.EventResult]

holePickersAddDocPrefix :: HolePickers m -> E.Subtitle -> E.Subtitle
holePickersAddDocPrefix [] doc = doc
holePickersAddDocPrefix (_:_) doc =
  doc
  & Lens.element 0 %~ Char.toLower
  & ("Pick result and " ++)

holePickersAction :: MonadA m => HolePickers m -> T m Widget.EventResult
holePickersAction = fmap mconcat . sequence

newtype Output m = Output
  { oHolePickers :: HolePickers m
  } deriving (Monoid)

newtype StoredEntityIds = StoredEntityIds [Sugar.EntityId]
  deriving (Monoid)

newtype Injected = Injected [Bool]
  deriving (Monoid, Binary, Eq, Ord)

data HoleEntityIds = HoleEntityIds
  { _hgMNextHole :: Maybe Sugar.EntityId
  , _hgMPrevHole :: Maybe Sugar.EntityId
  }
Lens.makeLenses ''HoleEntityIds

emptyHoleEntityIds :: HoleEntityIds
emptyHoleEntityIds = HoleEntityIds Nothing Nothing

-- GUI input payload on sugar exprs
data Payload = Payload
  { _plStoredEntityIds :: [Sugar.EntityId]
  , _plInjected :: [Bool]
  , _plHoleEntityIds :: HoleEntityIds
  }
Lens.makeLenses ''Payload

emptyPayload :: Payload
emptyPayload = Payload
  { _plStoredEntityIds = []
  , _plInjected = []
  , _plHoleEntityIds = emptyHoleEntityIds
  }

type SugarExpr m = Sugar.ExpressionN m Payload

data Askable m = Askable
  { _aSettings :: Settings
  , _aMakeSubexpression ::
    ParentPrecedence -> SugarExpr m ->
    ExprGuiM m (ExpressionGui m)
  , _aCodeAnchors :: Anchors.CodeProps m
  }

newtype ExprGuiM m a = ExprGuiM
  { _exprGuiM :: RWST (Askable m) (Output m) () (WidgetEnvT (T m)) a
  }
  deriving (Functor, Applicative, Monad)

Lens.makeLenses ''Askable
Lens.makeLenses ''ExprGuiM

-- TODO: To lens
localEnv :: MonadA m => (WE.Env -> WE.Env) -> ExprGuiM m a -> ExprGuiM m a
localEnv = (exprGuiM %~) . RWS.mapRWST . WE.localEnv

withFgColor :: MonadA m => Draw.Color -> ExprGuiM m a -> ExprGuiM m a
withFgColor = localEnv . WE.setTextColor

readSettings :: MonadA m => ExprGuiM m Settings
readSettings = ExprGuiM $ Lens.view aSettings

readCodeAnchors :: MonadA m => ExprGuiM m (Anchors.CodeProps m)
readCodeAnchors = ExprGuiM $ Lens.view aCodeAnchors

mkPrejumpPosSaver :: MonadA m => ExprGuiM m (T m ())
mkPrejumpPosSaver =
  DataOps.savePreJumpPosition <$> readCodeAnchors <*> widgetEnv WE.readCursor

-- TODO: makeSubexpresSion
makeSubexpression ::
  MonadA m => Precedence -> SugarExpr m -> ExprGuiM m (ExpressionGui m)
makeSubexpression parentPrecedence expr = do
  maker <- ExprGuiM $ Lens.view aMakeSubexpression
  maker (ParentPrecedence parentPrecedence) expr

run ::
  MonadA m =>
  (ParentPrecedence -> SugarExpr m -> ExprGuiM m (ExpressionGui m)) ->
  Anchors.CodeProps m -> Settings -> ExprGuiM m a ->
  WidgetEnvT (T m) a
run makeSubexpr codeAnchors settings (ExprGuiM action) =
  f <$> runRWST action
  Askable
  { _aSettings = settings
  , _aMakeSubexpression = makeSubexpr
  , _aCodeAnchors = codeAnchors
  }
  ()
  where
    f (x, (), _output) = x

widgetEnv :: MonadA m => WidgetEnvT (T m) a -> ExprGuiM m a
widgetEnv = ExprGuiM . lift

transaction :: MonadA m => T m a -> ExprGuiM m a
transaction = widgetEnv . lift

getP :: MonadA m => Transaction.MkProperty m a -> ExprGuiM m a
getP = transaction . Transaction.getP

getCodeAnchor ::
  MonadA m => (Anchors.CodeProps m -> Transaction.MkProperty m b) -> ExprGuiM m b
getCodeAnchor anchor = getP . anchor =<< readCodeAnchors

assignCursor :: MonadA m => Widget.Id -> Widget.Id -> ExprGuiM m a -> ExprGuiM m a
assignCursor x y = localEnv $ WE.envAssignCursor x y

assignCursorPrefix :: MonadA m => Widget.Id -> Widget.Id -> ExprGuiM m a -> ExprGuiM m a
assignCursorPrefix x y = localEnv $ WE.envAssignCursorPrefix x y

wrapDelegated ::
  (MonadA f, MonadA m) =>
  FocusDelegator.Config -> FocusDelegator.IsDelegating ->
  ((Widget f -> Widget f) -> a -> b) ->
  (Widget.Id -> ExprGuiM m a) ->
  Widget.Id -> ExprGuiM m b
wrapDelegated =
  BWidgets.wrapDelegatedWith (widgetEnv WE.readCursor) (widgetEnv WE.readConfig)
  (localEnv . (WE.envCursor %~))

-- Used vars:

listener :: MonadA m => (Output m -> b) -> ExprGuiM m a -> ExprGuiM m (a, b)
listener f =
  exprGuiM %~ RWS.listen
  & Lens.mapped . Lens.mapped . Lens._2 %~ f

listenResultPickers :: MonadA m => ExprGuiM m a -> ExprGuiM m (a, HolePickers m)
listenResultPickers = listener oHolePickers

addResultPicker :: MonadA m => T m Widget.EventResult -> ExprGuiM m ()
addResultPicker picker = ExprGuiM $ RWS.tell mempty { oHolePickers = [picker] }
