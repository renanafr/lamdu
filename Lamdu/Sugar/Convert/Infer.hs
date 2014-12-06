module Lamdu.Sugar.Convert.Infer
  ( ExpressionSetter

  , loadInferScope -- TODO: is this sensible to export here?
  , loadInferInto
  , loadInfer
  ) where

import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), mapStateT)
import Control.MonadA (MonadA)
import Data.Maybe (fromMaybe)
import Data.Store.Transaction (Transaction)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Infer (Infer)
import Lamdu.Infer.Load (Loader(..))
import Lamdu.Infer.Memo (Memo)
import Lamdu.Infer.Unify (unify)
import Lamdu.Infer.Update (updateInferredVal)
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Memo as InferMemo
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

type ExpressionSetter def = Val () -> Val ()

loader :: MonadA m => Loader (MaybeT (T m))
loader =
  Loader loadType
  where
    loadType globalId = do
      defBody <- lift $ Transaction.readIRef $ ExprIRef.defI globalId
      case defBody ^. Definition.bodyType of
        Definition.NoExportedType -> mzero -- Reference to global with non-exported type!
        Definition.ExportedType scheme -> return scheme

eitherToMaybeT :: Monad m => Either l a -> MaybeT m a
eitherToMaybeT (Left _) = MaybeT $ return Nothing
eitherToMaybeT (Right x) = MaybeT $ return $ Just x

type M m = StateT Infer.Context (MaybeT (T m))

liftInfer :: Monad m => Infer a -> M m a
liftInfer = mapStateT eitherToMaybeT . Infer.run

loadInferScope ::
  MonadA m => Memo -> Infer.Scope -> Val a -> M m (Val (Infer.Payload, a))
loadInferScope memo scope val = do
  inferAction <- lift $ InferMemo.loadInfer loader memo scope val
  liftInfer inferAction

loadInferInto ::
  MonadA m => Memo -> Infer.Payload -> Val a -> M m (Val (Infer.Payload, a))
loadInferInto memo pl val = do
  inferredVal <- loadInferScope memo (pl ^. Infer.plScope) val
  let inferredType = inferredVal ^. V.payload . _1 . Infer.plType
  liftInfer $ do
    unify inferredType (pl ^. Infer.plType)
    updateInferredVal inferredVal

loadInfer ::
  MonadA m => Memo -> Val (Load.ExprPropertyClosure m) ->
  T m (Val (Sugar.InputPayload m ()), Infer.Context)
loadInfer memo val =
  loadInferScope memo Infer.emptyScope val
  & (`runStateT` Infer.initialContext)
  & runMaybeT
  <&> fromMaybe (error "Type inference failed")
  <&> _1 . Lens.mapped . _2 %~ Load.exprPropertyOfClosure
  <&> _1 . Lens.mapped %~ mkInputPayload
  where
    mkInputPayload (inferPl, stored) = Sugar.InputPayload
      { Sugar._ipEntityId = EntityId.ofValI $ Property.value stored
      , Sugar._ipInferred = inferPl
      , Sugar._ipStored = Just stored
      , Sugar._ipData = ()
      , Sugar._ipGuid = IRef.guid $ ExprIRef.unValI $ Property.value stored
      }
