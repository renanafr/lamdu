module Lamdu.Sugar.Internal
  ( BodyU, ExpressionU
  , replaceWith
  ) where

import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Lamdu.Sugar.Types
import Lamdu.Sugar.Types.Internal
import qualified Data.Store.Property as Property
import qualified Lamdu.Expr.IRef as ExprIRef

type BodyU rw m a = Body MStoredName rw m (ExpressionU rw m a)
type ExpressionU rw m a = Expression MStoredName rw m a

replaceWith :: MonadA m => Stored m -> Stored m -> T m Guid
replaceWith parentP replacerP = do
  Property.set parentP replacerI
  return $ ExprIRef.valIGuid replacerI
  where
    replacerI = Property.value replacerP
