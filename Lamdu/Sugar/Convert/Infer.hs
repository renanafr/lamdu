{-# LANGUAGE ConstraintKinds #-}
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
import Lamdu.Expr.Val (Val(..))
import Lamdu.Infer (Infer)
import Lamdu.Infer.Load (Loader(..))
import Lamdu.Infer.Unify (unify)
import Lamdu.Infer.Update (updateInferredVal)
import Lamdu.Sugar.Types.Internal
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Load as InferLoad
import qualified Lamdu.Sugar.Types as Sugar

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
  MonadA m => Infer.Scope -> Val a -> M m (Val (Infer.Payload, a))
loadInferScope scope val = do
  inferAction <- lift $ InferLoad.loadInfer loader scope val
  liftInfer inferAction

loadInferInto ::
  MonadA m => Infer.Payload -> Val a -> M m (Val (Infer.Payload, a))
loadInferInto pl val = do
  inferredVal <- loadInferScope (pl ^. Infer.plScope) val
  let inferredType = inferredVal ^. V.payload . _1 . Infer.plType
  liftInfer $ do
    unify inferredType (pl ^. Infer.plType)
    updateInferredVal inferredVal

loadInfer ::
  MonadA m => Val (Stored m) ->
  T m (Val (Sugar.InputPayload Sugar.Writable m ()), Infer.Context)
loadInfer val =
  loadInferScope Infer.emptyScope val
  & (`runStateT` Infer.initialContext)
  & runMaybeT
  <&> fromMaybe (error "Type inference failed")
  <&> _1 . Lens.mapped %~ mkInputPayload
  where
    mkInputPayload (inferPl, stored) = Sugar.InputPayload
      { Sugar._ipGuid = ExprIRef.epGuid stored
      , Sugar._ipInferred = inferPl
      , Sugar._ipStored = Sugar.Writable stored
      , Sugar._ipData = ()
      }
