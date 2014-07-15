{-# OPTIONS -fno-warn-orphans #-} -- Arbitrary Expression
{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Lamdu.Data.Arbitrary (Name(..)) where

import Control.Applicative (Applicative(..), (<$>), (<*))
import Control.Lens ((%~))
import Control.Monad (replicateM, join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Derive.Arbitrary (makeArbitrary)
import Data.DeriveTH (derive)
import Data.Maybe (maybeToList)
import Data.Store.Guid (Guid)
import Lamdu.Expr (Kind(..))
import Test.QuickCheck (Arbitrary(..), Gen, choose)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString as BS
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Expr as Expr
import qualified Test.QuickCheck.Gen as Gen

data Env def par = Env
  { _envScope :: [par]
  , __envMakeDef :: Maybe (Gen def)
  }
Lens.makeLenses ''Env

type GenExpr def par = ReaderT (Env def par) (StateT [par] Gen)

liftGen :: Gen a -> GenExpr def par a
liftGen = lift . lift

next :: GenExpr def par par
next = lift $ State.gets head <* State.modify tail

arbitraryLambda :: Arbitrary a => GenExpr def par (Expr.Lam par (Expr.Expr def par a))
arbitraryLambda = do
  par <- next
  flip Expr.Lam par <$> liftGen arbitrary <*> arbitraryExpr <*>
    Reader.local (envScope %~ (par :)) arbitraryExpr

listOf :: GenExpr def par a -> GenExpr def par [a]
listOf gen = do
  terminate <- liftGen $ Gen.frequency [(1, return True), (2, return False)]
  if terminate
    then return []
    else (:) <$> gen <*> listOf gen

arbitraryRecord :: Arbitrary a => GenExpr def par (Expr.Record (Expr.Expr def par a))
arbitraryRecord =
  Expr.Record
  <$> liftGen arbitrary
  <*> listOf ((,) <$> liftGen arbitrary <*> arbitraryExpr)

arbitraryGetField :: Arbitrary a => GenExpr def par (Expr.GetField (Expr.Expr def par a))
arbitraryGetField = Expr.GetField <$> arbitraryExpr <*> liftGen arbitrary

arbitraryApply :: Arbitrary a => GenExpr def par (Expr.Apply (Expr.Expr def par a))
arbitraryApply = Expr.Apply <$> arbitraryExpr <*> arbitraryExpr

arbitraryLeaf :: GenExpr def par (Expr.Leaf def par)
arbitraryLeaf = do
  Env scope mGenDefI <- Reader.ask
  join . liftGen . Gen.elements $
    [ Expr.VLiteralInteger <$> liftGen arbitrary
    , pure Expr.Type
    , pure Expr.IntegerType
    , pure Expr.VHole
    ] ++
    map (pure . Expr.VVar . Expr.ParameterRef) scope ++
    map (fmap (Expr.VVar . Expr.DefinitionRef) . liftGen)
      (maybeToList mGenDefI)

arbitraryBody :: Arbitrary a => GenExpr def par (Expr.BodyExpr def par a)
arbitraryBody =
  join . liftGen . Gen.frequency . (Lens.mapped . Lens._2 %~ pure) $
  [ weight 2  $ Expr.VAbs         <$> arbitraryLambda
  , weight 2  $ Expr.VRec         <$> arbitraryRecord
  , weight 2  $ Expr.VGetField    <$> arbitraryGetField
  , weight 5  $ Expr.VApp         <$> arbitraryApply
  , weight 17 $ Expr.VLeaf        <$> arbitraryLeaf
  ]
  where
    weight = (,)

arbitraryExpr :: Arbitrary a => GenExpr def par (Expr.Expr def par a)
arbitraryExpr = Expr.Expr <$> arbitraryBody <*> liftGen arbitrary

class Name n where
  names :: [n]

exprGen :: (Arbitrary a, Name par) => Maybe (Gen def) -> Gen (Expr.Expr def par a)
exprGen makeDefI =
  (`evalStateT` names) .
  (`runReaderT` Env [] makeDefI) $
  arbitraryExpr

instance Name Guid where
  names = Guid.fromString . (: []) <$> ['a'..]

instance Arbitrary Guid where
  arbitrary = Guid.make . BS.pack <$> replicateM Guid.length arbitrary

instance Arbitrary Expr.Tag where
  arbitrary = Expr.Tag <$> arbitrary

-- TODO: This instance doesn't know which Definitions exist in the
-- world so avoids DefinitionRef and only has valid ParameterRefs to
-- its own lambdas.
instance (Name par, Arbitrary a) => Arbitrary (Expr.Expr def par a) where
  arbitrary = exprGen Nothing

derive makeArbitrary ''Expr.Kind
