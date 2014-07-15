{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Infer.Trigger.Types
  ( Trigger(..)
  , ParameterRefEvent(..)
  , Fired(..), _FiredDirectlyTag, _FiredKnownBody, _FiredParameterRef, _FiredUnify
  ) where

import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.Guid (Guid)
import Lamdu.Expr.Utils () -- Show instance for Expr.Body
import Lamdu.Infer.RefTags (ParamRef, ExprRef)
import qualified Control.Lens as Lens
import qualified Lamdu.Expr as Expr

-- Triggers are alive as long as their truthfulness is yet
-- unknown. When they become known, they fire (see Fired) below and
-- disappear.
data Trigger def
  = OnKnownBody
  | OnParameterRef (ParamRef def)
  | OnUnify
  deriving (Eq, Ord, Show)
derive makeBinary ''Trigger

data ParameterRefEvent
  = TheParameterOutOfScope
  | NotTheParameterRef
  | IsTheParameterRef
  deriving (Eq, Ord, Show)

data Fired def
  = FiredDirectlyTag Bool
  | FiredKnownBody (Expr.Body () Guid (ExprRef def))
  | FiredParameterRef (ParamRef def) ParameterRefEvent
  | FiredUnify (ExprRef def)
  deriving (Show)
Lens.makePrisms ''Fired
