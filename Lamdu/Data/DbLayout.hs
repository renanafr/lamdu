{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Lamdu.Data.DbLayout
  ( DbM, runDbTransaction
  , ViewM, runViewTransaction
  , CodeMkProps, codeMkProps
  , CodeProps, codeProps
  , codeIRefs
  , RevisionProps, revisionProps, revisionIRefs
  , module Lamdu.Data.Anchors
  ) where

import Control.Applicative (Applicative)
import Control.Lens.Operators
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Char8 ()
import Data.Functor.Identity (Identity(..))
import Data.Store.Db (Db)
import Data.Store.IRef (IRef, Tag)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Lamdu.Data.Anchors (Code(..), Revision(..), assocNameRef, SpecialFunctions(..))
import qualified Data.Store.Db as Db
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors

type T = Transaction

newtype DbM a = DbM { dbM :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

newtype ViewM a = ViewM { viewM :: T DbM a }
  deriving (Functor, Applicative, Monad)

runDbTransaction :: Db -> T DbM a -> IO a
runDbTransaction db = dbM . Transaction.run (Transaction.onStoreM DbM (Db.store db))

runViewTransaction :: View (Tag DbM) -> T ViewM a -> T DbM a
runViewTransaction v = viewM . (Transaction.run . Transaction.onStoreM ViewM . View.store) v

codeIRefs :: Code (IRef (Tag ViewM)) (Tag ViewM)
codeIRefs = Code
  { panes = IRef.anchor "panes"
  , clipboards = IRef.anchor "clipboards"
  , globals = IRef.anchor "globals"
  , specialFunctions = IRef.anchor "specialFuncs"
  , preJumps = IRef.anchor "prejumps"
  , preCursor = IRef.anchor "precursor"
  , postCursor = IRef.anchor "postcursor"
  , tags = IRef.anchor "tags"
  }

revisionIRefs :: Revision (IRef t) t
revisionIRefs = Revision
  { branches = IRef.anchor "branches"
  , currentBranch = IRef.anchor "currentBranch"
  , cursor = IRef.anchor "cursor"
  , redos = IRef.anchor "redos"
  , view = IRef.anchor "view"
  }

type CodeMkProps = Anchors.CodeMkProps ViewM
type CodeProps = Anchors.CodeProps ViewM
type RevisionProps = Anchors.RevisionProps DbM

codeMkProps :: CodeMkProps
codeMkProps =
  runIdentity $
  Anchors.onCode (Identity . Transaction.mkPropertyFromIRef) codeIRefs

codeProps :: T ViewM CodeProps
codeProps =
  Anchors.onCode (^. Transaction.mkProperty) codeMkProps

revisionProps :: RevisionProps
revisionProps = Anchors.onRevision Transaction.mkPropertyFromIRef revisionIRefs
