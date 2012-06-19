module Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker) where

import Editor.Anchors(ViewTag)
import Editor.OTransaction(TWidget)
import qualified Editor.CodeEdit.Sugar as Sugar

type ExpressionEditMaker m = Sugar.ExpressionRef m -> TWidget ViewTag m
