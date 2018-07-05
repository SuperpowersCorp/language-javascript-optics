{-# LANGUAGE LambdaCase #-}

module Language.JavaScript.Optics where

import Language.JavaScript.Prelude

import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST

_JSAST :: Prism' String JSAST
_JSAST = prism renderToString f
  where
    f src = case parse src "src" of
      Left _err -> Left src
      Right ast -> Right ast

_JSAstProgram :: Prism' JSAST ([JSStatement], JSAnnot)
_JSAstProgram = prism (uncurry JSAstProgram) f
  where
    f (JSAstProgram a b) = Right (a, b)
    f other              = Left other

_JSAstStatement :: Prism' JSAST (JSStatement, JSAnnot)
_JSAstStatement = prism (uncurry JSAstStatement) f
  where
    f (JSAstStatement a b) = Right (a, b)
    f other                = Left other

_JSAstExpression :: Prism' JSAST (JSExpression, JSAnnot)
_JSAstExpression = prism (uncurry JSAstExpression) f
  where
    f (JSAstExpression a b) = Right (a, b)
    f other                 = Left other

_JSAstLiteral :: Prism' JSAST (JSExpression, JSAnnot)
_JSAstLiteral = prism (uncurry JSAstLiteral) f
  where
    f (JSAstLiteral a b) = Right (a, b)
    f other              = Left other

_JSStatementBlock :: Prism' JSStatement (JSAnnot, [JSStatement], JSAnnot, JSSemi)
_JSStatementBlock = prism g s
  where
    g (a, b, c, d) = JSStatementBlock a b c d
    s = \case
      JSStatementBlock a b c d -> Right (a, b, c, d)
      other                    -> Left other

_JSBreak :: Prism' JSStatement (JSAnnot, JSIdent, JSSemi)
_JSBreak = prism g s
  where
    g (a, b, c) = JSBreak a b c
    s = \case
      JSBreak a b c -> Right (a, b, c)
      other         -> Left other

_JSConstant :: Prism' JSStatement (JSAnnot, (JSCommaList JSExpression), JSSemi)
_JSConstant = prism g s
  where
    g (a, b, c) = JSConstant a b c
    s = \case
      JSConstant a b c -> Right (a, b, c)
      other            -> Left other

_JSContinue :: Prism' JSStatement (JSAnnot, JSIdent, JSSemi)
_JSContinue = prism g s
  where
    g (a, b, c) = JSContinue a b c
    s = \case
      JSContinue a b c -> Right (a, b, c)
      other            -> Left other

_JSDoWhile :: Prism' JSStatement (JSAnnot, JSStatement, JSAnnot, JSAnnot, JSExpression, JSAnnot, JSSemi)
_JSDoWhile = prism g s
  where
    g (a, b, c, d, e, f, h) = JSDoWhile a b c d e f h
    s = \case
      JSDoWhile a b c d e f h -> Right (a, b, c, d, e, f, h)
      other                   -> Left other

_JSFor :: Prism' JSStatement (JSAnnot, JSAnnot, (JSCommaList JSExpression), JSAnnot, (JSCommaList JSExpression), JSAnnot, (JSCommaList JSExpression), JSAnnot, JSStatement)
_JSFor = prism g s
  where
    g (a, b, c, d, e, f, h, i, j) = JSFor a b c d e f h i j
    s = \case
      JSFor a b c d e f h i j -> Right (a, b, c, d, e, f, h, i, j)
      other                   -> Left other

_JSForIn :: Prism' JSStatement (JSAnnot, JSAnnot, JSExpression, JSBinOp, JSExpression, JSAnnot, JSStatement)
_JSForIn = prism g s
  where
    g (a, b, c, d, e, f, h) = JSForIn a b c d e f h
    s = \case
      JSForIn a b c d e f h -> Right (a, b, c, d, e, f, h)
      other                 -> Left other

_JSForVar :: Prism' JSStatement (JSAnnot, JSAnnot, JSAnnot, (JSCommaList JSExpression), JSAnnot, (JSCommaList JSExpression), JSAnnot, (JSCommaList JSExpression), JSAnnot, JSStatement)
_JSForVar = prism g s
  where
    g (a, b, c, d, e, f, h, i, j, k) = JSForVar a b c d e f h i j k
    s = \case
      JSForVar a b c d e f h i j k -> Right (a, b, c, d, e, f, h, i, j, k)
      other                        -> Left other

_JSForVarIn :: Prism' JSStatement (JSAnnot, JSAnnot, JSAnnot, JSExpression, JSBinOp, JSExpression, JSAnnot, JSStatement)
_JSForVarIn = prism g s
  where
    g (a, b, c, d, e, f, h, i) = JSForVarIn a b c d e f h i
    s = \case
      JSForVarIn a b c d e f h i -> Right (a, b, c, d, e, f, h, i)
      other                      -> Left other

_JSFunction :: Prism' JSStatement (JSAnnot, JSIdent, JSAnnot, (JSCommaList JSIdent), JSAnnot, JSBlock, JSSemi)
_JSFunction = prism g s
  where
    g (a, b, c, d, e, f, h) = JSFunction a b c d e f h
    s = \case
      JSFunction a b c d e f h -> Right (a, b, c, d, e, f, h)
      other                    -> Left other

_JSIf :: Prism' JSStatement (JSAnnot, JSAnnot, JSExpression, JSAnnot, JSStatement)
_JSIf = prism g s
  where
    g (a, b, c, d, e) = JSIf a b c d e
    s = \case
      JSIf a b c d e -> Right (a, b, c, d, e)
      other          -> Left other

_JSIfElse :: Prism' JSStatement (JSAnnot, JSAnnot, JSExpression, JSAnnot, JSStatement, JSAnnot, JSStatement)
_JSIfElse = prism g s
  where
    g (a, b, c, d, e, f, h) = JSIfElse a b c d e f h
    s = \case
      JSIfElse a b c d e f h -> Right (a, b, c, d, e, f, h)
      other                  -> Left other

_JSLabelled :: Prism' JSStatement (JSIdent, JSAnnot, JSStatement)
_JSLabelled = prism g s
  where
    g (a, b, c) = JSLabelled a b c
    s = \case
      JSLabelled a b c -> Right (a, b, c)
      other            -> Left other

_JSEmptyStatement :: Prism' JSStatement JSAnnot
_JSEmptyStatement = prism JSEmptyStatement $ \case
  JSEmptyStatement x -> Right x
  other              -> Left other

_JSExpressionStatement :: Prism' JSStatement (JSExpression, JSSemi)
_JSExpressionStatement = prism g s
  where
    g (a, b) = JSExpressionStatement a b
    s = \case
      JSExpressionStatement a b -> Right (a, b)
      other                     -> Left other

_JSAssignStatement :: Prism' JSStatement (JSExpression, JSAssignOp, JSExpression, JSSemi)
_JSAssignStatement = prism g s
  where
    g (a, b, c, d) = JSAssignStatement a b c d
    s = \case
      JSAssignStatement a b c d -> Right (a, b, c, d)
      other                     -> Left other

_JSMethodCall :: Prism' JSStatement (JSExpression, JSAnnot, (JSCommaList JSExpression), JSAnnot, JSSemi)
_JSMethodCall = prism g s
  where
    g (a, b, c, d, e) = JSMethodCall a b c d e
    s = \case
      JSMethodCall a b c d e -> Right (a, b, c, d, e)
      other                  -> Left other

_JSReturn :: Prism' JSStatement (JSAnnot, (Maybe JSExpression), JSSemi)
_JSReturn = prism g s
  where
    g (a, b, c) = JSReturn a b c
    s = \case
      JSReturn a b c -> Right (a, b, c)
      other          -> Left other

_JSSwitch :: Prism' JSStatement (JSAnnot, JSAnnot, JSExpression, JSAnnot, JSAnnot, [JSSwitchParts], JSAnnot, JSSemi)
_JSSwitch = prism g s
  where
    g (a, b, c, d, e, f, h, i) = JSSwitch a b c d e f h i
    s = \case
      JSSwitch a b c d e f h i -> Right (a, b, c, d, e, f, h, i)
      other                    -> Left other

_JSThrow :: Prism' JSStatement (JSAnnot, JSExpression, JSSemi)
_JSThrow = prism g s
  where
    g (a, b, c) = JSThrow a b c
    s = \case
      JSThrow a b c -> Right (a, b, c)
      other         -> Left other

_JSTry :: Prism' JSStatement (JSAnnot, JSBlock, [JSTryCatch], JSTryFinally)
_JSTry = prism g s
  where
    g (a, b, c, d) = JSTry a b c d
    s = \case
      JSTry a b c d -> Right (a, b, c, d)
      other         -> Left other

_JSVariable :: Prism' JSStatement (JSAnnot, (JSCommaList JSExpression), JSSemi)
_JSVariable = prism g s
  where
    g (a, b, c) = JSVariable a b c
    s = \case
      JSVariable a b c -> Right (a, b, c)
      other            -> Left other

_JSWhile :: Prism' JSStatement (JSAnnot, JSAnnot, JSExpression, JSAnnot, JSStatement)
_JSWhile = prism g s
  where
    g (a, b, c, d, e) = JSWhile a b c d e
    s = \case
      JSWhile a b c d e -> Right (a, b, c, d, e)
      other             -> Left other

_JSWith :: Prism' JSStatement (JSAnnot, JSAnnot, JSExpression, JSAnnot, JSStatement, JSSemi)
_JSWith = prism g s
  where
    g (a, b, c, d, e, f) = JSWith a b c d e f
    s = \case
      JSWith a b c d e f -> Right (a, b, c, d, e, f)
      other              -> Left other

_JSIdentifier :: Prism' JSExpression (JSAnnot, String)
_JSIdentifier = prism g s
  where
    g (a, b) = JSIdentifier a b
    s = \case
      JSIdentifier a b -> Right (a, b)
      other            -> Left other

_JSDecimal :: Prism' JSExpression (JSAnnot, String)
_JSDecimal = prism g s
  where
    g (a, b) = JSDecimal a b
    s = \case
      JSDecimal a b -> Right (a, b)
      other         -> Left other

_JSLiteral :: Prism' JSExpression (JSAnnot, String)
_JSLiteral = prism g s
  where
    g (a, b) = JSLiteral a b
    s = \case
      JSLiteral a b -> Right (a, b)
      other         -> Left other

_JSHexInteger :: Prism' JSExpression (JSAnnot, String)
_JSHexInteger = prism g s
  where
    g (a, b) = JSHexInteger a b
    s = \case
      JSHexInteger a b -> Right (a, b)
      other            -> Left other

_JSOctal :: Prism' JSExpression (JSAnnot, String)
_JSOctal = prism g s
  where
    g (a, b) = JSOctal a b
    s = \case
      JSOctal a b -> Right (a, b)
      other       -> Left other

_JSStringLiteral :: Prism' JSExpression (JSAnnot, String)
_JSStringLiteral = prism g s
  where
    g (a, b) = JSStringLiteral a b
    s = \case
      JSStringLiteral a b -> Right (a, b)
      other               -> Left other

_JSRegEx :: Prism' JSExpression (JSAnnot, String)
_JSRegEx = prism g s
  where
    g (a, b) = JSRegEx a b
    s = \case
      JSRegEx a b -> Right (a, b)
      other       -> Left other

_JSArrayLiteral :: Prism' JSExpression (JSAnnot, [JSArrayElement], JSAnnot)
_JSArrayLiteral = prism g s
  where
    g (a, b, c) = JSArrayLiteral a b c
    s = \case
      JSArrayLiteral a b c -> Right (a, b, c)
      other                -> Left other

_JSAssignExpression :: Prism' JSExpression (JSExpression, JSAssignOp, JSExpression)
_JSAssignExpression = prism g s
  where
    g (a, b, c) = JSAssignExpression a b c
    s = \case
      JSAssignExpression a b c -> Right (a, b, c)
      other                    -> Left other

_JSCallExpression :: Prism' JSExpression (JSExpression, JSAnnot, (JSCommaList JSExpression), JSAnnot)
_JSCallExpression = prism g s
  where
    g (a, b, c, d) = JSCallExpression a b c d
    s = \case
      JSCallExpression a b c d -> Right (a, b, c, d)
      other                    -> Left other

_JSCallExpressionDot :: Prism' JSExpression (JSExpression, JSAnnot, JSExpression)
_JSCallExpressionDot = prism g s
  where
    g (a, b, c) = JSCallExpressionDot a b c
    s = \case
      JSCallExpressionDot a b c -> Right (a, b, c)
      other                     -> Left other

_JSCallExpressionSquare :: Prism' JSExpression (JSExpression, JSAnnot, JSExpression, JSAnnot)
_JSCallExpressionSquare = prism g s
  where
    g (a, b, c, d) = JSCallExpressionSquare a b c d
    s = \case
      JSCallExpressionSquare a b c d -> Right (a, b, c, d)
      other                          -> Left other

_JSCommaExpression :: Prism' JSExpression (JSExpression, JSAnnot, JSExpression)
_JSCommaExpression = prism g s
  where
    g (a, b, c) = JSCommaExpression a b c
    s = \case
      JSCommaExpression a b c -> Right (a, b, c)
      other                   -> Left other

_JSExpressionBinary :: Prism' JSExpression (JSExpression, JSBinOp, JSExpression)
_JSExpressionBinary = prism g s
  where
    g (a, b, c) = JSExpressionBinary a b c
    s = \case
      JSExpressionBinary a b c -> Right (a, b, c)
      other                    -> Left other

_JSExpressionParen :: Prism' JSExpression (JSAnnot, JSExpression, JSAnnot)
_JSExpressionParen = prism g s
  where
    g (a, b, c) = JSExpressionParen a b c
    s = \case
      JSExpressionParen a b c -> Right (a, b, c)
      other                   -> Left other

_JSExpressionPostfix :: Prism' JSExpression (JSExpression, JSUnaryOp)
_JSExpressionPostfix = prism g s
  where
    g (a, b) = JSExpressionPostfix a b
    s = \case
      JSExpressionPostfix a b -> Right (a, b)
      other                   -> Left other

_JSExpressionTernary :: Prism' JSExpression (JSExpression, JSAnnot, JSExpression, JSAnnot, JSExpression)
_JSExpressionTernary = prism g s
  where
    g (a, b, c, d, e) = JSExpressionTernary a b c d e
    s = \case
      JSExpressionTernary a b c d e -> Right (a, b, c, d, e)
      other                         -> Left other

_JSFunctionExpression :: Prism' JSExpression (JSAnnot, JSIdent, JSAnnot, (JSCommaList JSIdent), JSAnnot, JSBlock)
_JSFunctionExpression = prism g s
  where
    g (a, b, c, d, e, f) = JSFunctionExpression a b c d e f
    s = \case
      JSFunctionExpression a b c d e f -> Right (a, b, c, d, e, f)
      other                            -> Left other

_JSMemberDot :: Prism' JSExpression (JSExpression, JSAnnot, JSExpression)
_JSMemberDot = prism g s
  where
    g (a, b, c) = JSMemberDot a b c
    s = \case
      JSMemberDot a b c -> Right (a, b, c)
      other             -> Left other

_JSMemberExpression :: Prism' JSExpression (JSExpression, JSAnnot, (JSCommaList JSExpression), JSAnnot)
_JSMemberExpression = prism g s
  where
    g (a, b, c, d) = JSMemberExpression a b c d
    s = \case
      JSMemberExpression a b c d -> Right (a, b, c, d)
      other                      -> Left other

_JSMemberNew :: Prism' JSExpression (JSAnnot, JSExpression, JSAnnot, (JSCommaList JSExpression), JSAnnot)
_JSMemberNew = prism g s
  where
    g (a, b, c, d, e) = JSMemberNew a b c d e
    s = \case
      JSMemberNew a b c d e -> Right (a, b, c, d, e)
      other                 -> Left other

_JSMemberSquare :: Prism' JSExpression (JSExpression, JSAnnot, JSExpression, JSAnnot)
_JSMemberSquare = prism g s
  where
    g (a, b, c, d) = JSMemberSquare a b c d
    s = \case
      JSMemberSquare a b c d -> Right (a, b, c, d)
      other                  -> Left other

_JSNewExpression :: Prism' JSExpression (JSAnnot, JSExpression)
_JSNewExpression = prism g s
  where
    g (a, b) = JSNewExpression a b
    s = \case
      JSNewExpression a b -> Right (a, b)
      other               -> Left other

_JSObjectLiteral :: Prism' JSExpression (JSAnnot, JSObjectPropertyList, JSAnnot)
_JSObjectLiteral = prism g s
  where
    g (a, b, c) = JSObjectLiteral a b c
    s = \case
      JSObjectLiteral a b c -> Right (a, b, c)
      other                 -> Left other

_JSUnaryExpression :: Prism' JSExpression (JSUnaryOp, JSExpression)
_JSUnaryExpression = prism g s
  where
    g (a, b) = JSUnaryExpression a b
    s = \case
      JSUnaryExpression a b -> Right (a, b)
      other                 -> Left other

_JSVarInitExpression :: Prism' JSExpression (JSExpression, JSVarInitializer)
_JSVarInitExpression = prism g s
  where
    g (a, b) = JSVarInitExpression a b
    s = \case
      JSVarInitExpression a b -> Right (a, b)
      other                   -> Left other

_JSAnnot :: Prism' JSAnnot (TokenPosn, [CommentAnnotation])
_JSAnnot = prism g s
  where
    g (a, b) = JSAnnot a b
    s = \case
      JSAnnot a b -> Right (a, b)
      other       -> Left other

_JSAnnotSpace :: Prism' JSAnnot ()
_JSAnnotSpace = prism (const JSAnnotSpace) $ \case
  JSAnnotSpace -> Right ()
  other        -> Left other

_JSNoAnnot :: Prism' JSAnnot ()
_JSNoAnnot = prism (const JSNoAnnot) $ \case
  JSNoAnnot -> Right ()
  other     -> Left other

_JSSemi :: Prism' JSSemi JSAnnot
_JSSemi = prism JSSemi $ \case
  JSSemi a -> Right a
  other    -> Left other

_JSSemiAuto :: Prism' JSSemi ()
_JSSemiAuto = prism (const JSSemiAuto) $ \case
  JSSemiAuto -> Right ()
  other      -> Left other

_JSCatch :: Prism' JSTryCatch (JSAnnot, JSAnnot, JSExpression, JSAnnot, JSBlock)
_JSCatch = prism g s
  where
    g (a, b, c, d, e) = JSCatch a b c d e
    s = \case
      JSCatch a b c d e -> Right (a, b, c, d, e)
      other             -> Left other

_JSCatchIf :: Prism' JSTryCatch (JSAnnot, JSAnnot, JSExpression, JSAnnot, JSExpression, JSAnnot, JSBlock)
_JSCatchIf = prism g s
  where
    g (a, b, c, d, e, f, h) = JSCatchIf a b c d e f h
    s = \case
      JSCatchIf a b c d e f h -> Right (a, b, c, d, e, f, h)
      other                   -> Left other

_JSFinally :: Prism' JSTryFinally (JSAnnot, JSBlock)
_JSFinally = prism g s
  where
    g (a, b) = JSFinally a b
    s = \case
      JSFinally a b -> Right (a, b)
      other         -> Left other

_JSNoFinally :: Prism' JSTryFinally ()
_JSNoFinally = prism (const JSNoFinally) $ \case
  JSNoFinally -> Right ()
  other       -> Left other

_JSBlock :: Prism' JSBlock (JSAnnot, [JSStatement], JSAnnot)
_JSBlock = prism g s
  where
    g (a, b, c)       = JSBlock a b c
    s (JSBlock a b c) = Right (a, b, c)

_JSCase :: Prism' JSSwitchParts (JSAnnot, JSExpression, JSAnnot, [JSStatement])
_JSCase = prism g s
  where
    g (a, b, c, d) = JSCase a b c d
    s = \case
      JSCase a b c d -> Right (a, b, c, d)
      other          -> Left other

_JSDefault :: Prism' JSSwitchParts (JSAnnot, JSAnnot, [JSStatement])
_JSDefault = prism g s
  where
    g (a, b, c) = JSDefault a b c
    s = \case
      JSDefault a b c -> Right (a, b, c)
      other           -> Left other

_JSPropertyAccessor :: Prism' JSObjectProperty (JSAccessor, JSPropertyName, JSAnnot, [JSExpression], JSAnnot, JSBlock)
_JSPropertyAccessor = prism g s
  where
    g (a, b, c, d, e, f) = JSPropertyAccessor a b c d e f
    s = \case
      JSPropertyAccessor a b c d e f -> Right (a, b, c, d, e, f)
      other                          -> Left other

_JSPropertyNameandValue :: Prism' JSObjectProperty (JSPropertyName, JSAnnot, [JSExpression])
_JSPropertyNameandValue = prism g s
  where
    g (a, b, c) = JSPropertyNameandValue a b c
    s = \case
      JSPropertyNameandValue a b c -> Right (a, b, c)
      other                        -> Left other

_JSPropertyIdent :: Prism' JSPropertyName (JSAnnot, String)
_JSPropertyIdent = prism g s
  where
    g (a, b) = JSPropertyIdent a b
    s = \case
      JSPropertyIdent a b -> Right (a, b)
      other               -> Left other

_JSPropertyString :: Prism' JSPropertyName (JSAnnot, String)
_JSPropertyString = prism g s
  where
    g (a, b) = JSPropertyString a b
    s = \case
      JSPropertyString a b -> Right (a, b)
      other               -> Left other

_JSPropertyNumber :: Prism' JSPropertyName (JSAnnot, String)
_JSPropertyNumber = prism g s
  where
    g (a, b) = JSPropertyNumber a b
    s = \case
      JSPropertyNumber a b -> Right (a, b)
      other                -> Left other

_JSAccessorGet :: Prism' JSAccessor JSAnnot
_JSAccessorGet = prism g s
  where
    g = JSAccessorGet
    s = \case
      (JSAccessorGet x) -> Right x
      other             -> Left other

_JSAccessorSet :: Prism' JSAccessor JSAnnot
_JSAccessorSet = prism g s
  where
    g = JSAccessorSet
    s = \case
      (JSAccessorSet x) -> Right x
      other             -> Left other

_JSVarInit :: Prism' JSVarInitializer (JSAnnot, JSExpression)
_JSVarInit = prism g s
  where
    g (a, b) = JSVarInit a b
    s = \case
      JSVarInit a b -> Right (a, b)
      other         -> Left other

_JSVarInitNone :: Prism' JSVarInitializer ()
_JSVarInitNone = prism (const JSVarInitNone) $ \case
  JSVarInitNone -> Right ()
  other         -> Left other

_JSArrayElement :: Prism' JSArrayElement JSExpression
_JSArrayElement = prism g s
  where
    g = JSArrayElement
    s = \case
      JSArrayElement x -> Right x
      other            -> Left other

_JSArrayComma :: Prism' JSArrayElement JSAnnot
_JSArrayComma = prism g s
  where
    g = JSArrayComma
    s = \case
      JSArrayComma x -> Right x
      other          -> Left other

_JSLCons :: Prism' (JSCommaList a) (JSCommaList a, JSAnnot, a)
_JSLCons = prism g s
  where
    g (a, b, c) = JSLCons a b c
    s = \case
      JSLCons a b c -> Right (a, b, c)
      other         -> Left other

_JSLOne :: Prism' (JSCommaList a) a
_JSLOne = prism JSLOne $ \case
  JSLOne x -> Right x
  other    -> Left other

_JSLNil :: Prism' (JSCommaList a) ()
_JSLNil = prism (const JSLNil) $ \case
  JSLNil -> Right ()
  other  -> Left other

_JSCTLComma :: Prism' (JSCommaTrailingList a) (JSCommaList a, JSAnnot)
_JSCTLComma = prism g s
  where
    g (a, b) = JSCTLComma a b
    s = \case
      JSCTLComma a b -> Right (a, b)
      other          -> Left other

_JSCTLNone :: Prism' (JSCommaTrailingList a) (JSCommaList a)
_JSCTLNone = prism JSCTLNone $ \case
  JSCTLNone x -> Right x
  other       -> Left other

-- _JSEmptyStatement :: Prism' JSStatement JSAnnot
-- _JSEmptyStatement = prism JSEmptyStatement $ \case
--   JSEmptyStatement x -> Right x
--   other              -> Left other

_JSBinOpAnd :: Prism' JSBinOp JSAnnot
_JSBinOpAnd = prism JSBinOpAnd $ \case
  JSBinOpAnd x -> Right x
  other        -> Left other

_JSBinOpBitAnd :: Prism' JSBinOp JSAnnot
_JSBinOpBitAnd = prism JSBinOpBitAnd $ \case
  JSBinOpBitAnd x -> Right x
  other           -> Left other

_JSBinOpBitOr :: Prism' JSBinOp JSAnnot
_JSBinOpBitOr = prism JSBinOpBitOr $ \case
  JSBinOpBitOr x -> Right x
  other          -> Left other

_JSBinOpBitXor :: Prism' JSBinOp JSAnnot
_JSBinOpBitXor = prism JSBinOpBitXor $ \case
  JSBinOpBitXor x -> Right x
  other           -> Left other

_JSBinOpDivide :: Prism' JSBinOp JSAnnot
_JSBinOpDivide = prism JSBinOpDivide $ \case
  JSBinOpDivide x -> Right x
  other           -> Left other

_JSBinOpEq :: Prism' JSBinOp JSAnnot
_JSBinOpEq = prism JSBinOpEq $ \case
  JSBinOpEq x -> Right x
  other       -> Left other

_JSBinOpGe :: Prism' JSBinOp JSAnnot
_JSBinOpGe = prism JSBinOpGe $ \case
  JSBinOpGe x -> Right x
  other       -> Left other

_JSBinOpGt :: Prism' JSBinOp JSAnnot
_JSBinOpGt = prism JSBinOpGt $ \case
  JSBinOpGt x -> Right x
  other       -> Left other

_JSBinOpIn :: Prism' JSBinOp JSAnnot
_JSBinOpIn = prism JSBinOpIn $ \case
  JSBinOpIn x -> Right x
  other       -> Left other

_JSBinOpInstanceOf :: Prism' JSBinOp JSAnnot
_JSBinOpInstanceOf = prism JSBinOpInstanceOf $ \case
  JSBinOpInstanceOf x -> Right x
  other               -> Left other

_JSBinOpLe :: Prism' JSBinOp JSAnnot
_JSBinOpLe = prism JSBinOpLe $ \case
  JSBinOpLe x -> Right x
  other       -> Left other

_JSBinOpLsh :: Prism' JSBinOp JSAnnot
_JSBinOpLsh = prism JSBinOpLsh $ \case
  JSBinOpLsh x -> Right x
  other        -> Left other

_JSBinOpLt :: Prism' JSBinOp JSAnnot
_JSBinOpLt = prism JSBinOpLt $ \case
  JSBinOpLt x -> Right x
  other       -> Left other

_JSBinOpMinus :: Prism' JSBinOp JSAnnot
_JSBinOpMinus = prism JSBinOpMinus $ \case
  JSBinOpMinus x -> Right x
  other          -> Left other

_JSBinOpMod :: Prism' JSBinOp JSAnnot
_JSBinOpMod = prism JSBinOpMod $ \case
  JSBinOpMod x -> Right x
  other        -> Left other

_JSBinOpNeq :: Prism' JSBinOp JSAnnot
_JSBinOpNeq = prism JSBinOpNeq $ \case
  JSBinOpNeq x -> Right x
  other        -> Left other

_JSBinOpOr :: Prism' JSBinOp JSAnnot
_JSBinOpOr = prism JSBinOpOr $ \case
  JSBinOpOr x -> Right x
  other       -> Left other

_JSBinOpPlus :: Prism' JSBinOp JSAnnot
_JSBinOpPlus = prism JSBinOpPlus $ \case
  JSBinOpPlus x -> Right x
  other         -> Left other

_JSBinOpRsh :: Prism' JSBinOp JSAnnot
_JSBinOpRsh = prism JSBinOpRsh $ \case
  JSBinOpRsh x -> Right x
  other        -> Left other

_JSBinOpStrictEq :: Prism' JSBinOp JSAnnot
_JSBinOpStrictEq = prism JSBinOpStrictEq $ \case
  JSBinOpStrictEq x -> Right x
  other             -> Left other

_JSBinOpStrictNeq :: Prism' JSBinOp JSAnnot
_JSBinOpStrictNeq = prism JSBinOpStrictNeq $ \case
  JSBinOpStrictNeq x -> Right x
  other              -> Left other

_JSBinOpTimes :: Prism' JSBinOp JSAnnot
_JSBinOpTimes = prism JSBinOpTimes $ \case
  JSBinOpTimes x -> Right x
  other          -> Left other

_JSBinOpUrsh :: Prism' JSBinOp JSAnnot
_JSBinOpUrsh = prism JSBinOpUrsh $ \case
  JSBinOpUrsh x -> Right x
  other         -> Left other

_JSUnaryOpDecr :: Prism' JSUnaryOp JSAnnot
_JSUnaryOpDecr = prism JSUnaryOpDecr $ \case
  JSUnaryOpDecr x -> Right x
  other           -> Left other

_JSUnaryOpDelete :: Prism' JSUnaryOp JSAnnot
_JSUnaryOpDelete = prism JSUnaryOpDelete $ \case
  JSUnaryOpDelete x -> Right x
  other             -> Left other

_JSUnaryOpIncr :: Prism' JSUnaryOp JSAnnot
_JSUnaryOpIncr = prism JSUnaryOpIncr $ \case
  JSUnaryOpIncr x -> Right x
  other           -> Left other

_JSUnaryOpMinus :: Prism' JSUnaryOp JSAnnot
_JSUnaryOpMinus = prism JSUnaryOpMinus $ \case
  JSUnaryOpMinus x -> Right x
  other            -> Left other

_JSUnaryOpNot :: Prism' JSUnaryOp JSAnnot
_JSUnaryOpNot = prism JSUnaryOpNot $ \case
  JSUnaryOpNot x -> Right x
  other          -> Left other

_JSUnaryOpPlus :: Prism' JSUnaryOp JSAnnot
_JSUnaryOpPlus = prism JSUnaryOpPlus $ \case
  JSUnaryOpPlus x -> Right x
  other           -> Left other

_JSUnaryOpTilde :: Prism' JSUnaryOp JSAnnot
_JSUnaryOpTilde = prism JSUnaryOpTilde $ \case
  JSUnaryOpTilde x -> Right x
  other            -> Left other

_JSUnaryOpTypeof :: Prism' JSUnaryOp JSAnnot
_JSUnaryOpTypeof = prism JSUnaryOpTypeof $ \case
  JSUnaryOpTypeof x -> Right x
  other             -> Left other

_JSUnaryOpVoid :: Prism' JSUnaryOp JSAnnot
_JSUnaryOpVoid = prism JSUnaryOpVoid $ \case
  JSUnaryOpVoid x -> Right x
  other           -> Left other

_JSAssign :: Prism' JSAssignOp JSAnnot
_JSAssign = prism JSAssign $ \case
  JSAssign x -> Right x
  other      -> Left other

_JSTimesAssign :: Prism' JSAssignOp JSAnnot
_JSTimesAssign = prism JSTimesAssign $ \case
  JSTimesAssign x -> Right x
  other           -> Left other

_JSDivideAssign :: Prism' JSAssignOp JSAnnot
_JSDivideAssign = prism JSDivideAssign $ \case
  JSDivideAssign x -> Right x
  other            -> Left other

_JSModAssign :: Prism' JSAssignOp JSAnnot
_JSModAssign = prism JSModAssign $ \case
  JSModAssign x -> Right x
  other         -> Left other

_JSPlusAssign :: Prism' JSAssignOp JSAnnot
_JSPlusAssign = prism JSPlusAssign $ \case
  JSPlusAssign x -> Right x
  other          -> Left other

_JSMinusAssign :: Prism' JSAssignOp JSAnnot
_JSMinusAssign = prism JSMinusAssign $ \case
  JSMinusAssign x -> Right x
  other           -> Left other

_JSLshAssign :: Prism' JSAssignOp JSAnnot
_JSLshAssign = prism JSLshAssign $ \case
  JSLshAssign x -> Right x
  other         -> Left other

_JSRshAssign :: Prism' JSAssignOp JSAnnot
_JSRshAssign = prism JSRshAssign $ \case
  JSRshAssign x -> Right x
  other         -> Left other

_JSUrshAssign :: Prism' JSAssignOp JSAnnot
_JSUrshAssign = prism JSUrshAssign $ \case
  JSUrshAssign x -> Right x
  other          -> Left other

_JSBwAndAssign :: Prism' JSAssignOp JSAnnot
_JSBwAndAssign = prism JSBwAndAssign $ \case
  JSBwAndAssign x -> Right x
  other           -> Left other

_JSBwXorAssign :: Prism' JSAssignOp JSAnnot
_JSBwXorAssign = prism JSBwXorAssign $ \case
  JSBwXorAssign x -> Right x
  other           -> Left other

_JSBwOrAssign :: Prism' JSAssignOp JSAnnot
_JSBwOrAssign = prism JSBwOrAssign $ \case
  JSBwOrAssign x -> Right x
  other          -> Left other

_JSIdentName :: Prism' JSIdent (JSAnnot, String)
_JSIdentName = prism g s
  where
    g (a, b) = JSIdentName a b
    s = \case
      JSIdentName a b -> Right (a, b)
      other           -> Left other

_JSIdentNone :: Prism' JSIdent ()
_JSIdentNone = prism (const JSIdentNone) $ \case
  JSIdentNone -> Right ()
  other       -> Left other
