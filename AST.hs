{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module AST where

import Prelude hiding (replicate)
import Common

data AbsExpr expr = Variable Name
                  | Number Double
                  | String Text
                  | Regex Text
                  | InString (InString expr)
                  | Assign expr expr
                  | Block [expr]
                  | Array [expr]
                  | ArrayRange expr expr
                  | Object [(Name, expr)]
                  | ObjectDeref expr expr
                  | Function [Name] expr
                  | FatArrowFunction [Name] expr
                  | Dotted expr Name
                  | Call expr [expr]
                  | Binary Name expr expr
                  | Prefix Name expr
                  | Postfix expr Name
                  | Return (Maybe expr)
                  | Throw expr
                  | New expr
                  | Switch expr [([expr], expr)] (Maybe expr)
                  | If expr expr (Maybe expr)
                  | Unless expr expr
                  | ForIn [Name] expr expr
                  | ForOf [Name] expr expr
                  | ForInComp expr [Name] expr
                  | ForOfComp expr [Name] expr
                  | While expr expr
                  | TryCatch expr (Maybe (Name, expr)) (Maybe expr)
                  | Do [Name] expr
                  | Comment Text
                  | Break
                  | Continue
                  | EmptyExpr
                  | Class (Maybe Name) (Maybe expr) [ClassDec expr]
                  deriving (Show, Eq, Functor)

class IsExpr a where
  -- | Pulls the abstract expression out of a wrapped AbsExpr.
  unExpr :: a -> AbsExpr a
  deepUnExpr :: a -> AbsExpr (AbsExpr a)
  deepUnExpr = unExpr ~> fmap unExpr

data InString e = Plain Text
                | Interpolated (InString e) (AbsExpr e) (InString e)
                deriving (Show, Eq, Functor)

data ClassDec expr = ClassDecExpr expr
                   | ClassDecDef Name expr
                   deriving (Show, Eq, Functor)

instance (IsExpr expr, Render expr) => Render (AbsExpr expr) where
  render = \case
    Variable n -> n
    Number n -> render n
    String s -> render s
    Regex r -> render r
    InString s -> error "interp string rendering"
    Assign pat expr -> render pat <> " = " <> render expr <> ";"
    Block exprs -> "{" <> imapr "; " exprs <> "}"
    Array exprs -> "[" <> imapr ", " exprs <> "]"
    ArrayRange e1 e2 -> "[" <> render e1 <> " .. " <> render e2 <> "]"
    ObjectDeref e ref -> render e <> "[" <> render ref <> "]"
    Object pairs -> "{" <> intercalate ", " (map renderP pairs) <> "}"
    Function names expr -> "(" <> intercalate ", " names <> ") -> "
                               <> render expr
    Call expr exprs -> do
      let func = render' expr
      case exprs of
        [] -> func <> "()"
        exprs -> func <> " " <> imapr "," exprs
    Binary op e1 e2 -> "(" <> render e1 <> ") " <> op <> " (" <> render e2 <> ")"
    Prefix op expr -> op <> render expr
    Postfix expr op -> render expr <> op
    Return Nothing -> "return"
    Return (Just expr) -> "return " <> render expr
    Throw expr -> "throw " <> render expr
    New expr -> "new " <> render expr
    Switch expr cases _else -> rswitch expr cases _else
    If c t Nothing -> "if " <> render c <> " then " <> render t
    If c t (Just f) -> "if " <> render c <> " then " <> render t <> " else " <> render f
    ForIn pat e1 e2 -> "for " <> intercalate ", " pat <> " in "
                              <> render e1 <> render e2
    ForOf pat e1 e2 -> "for " <> intercalate ", " pat <> " of "
                              <> render e1 <> render e2
    ForInComp e1 pat e2 -> render e1 <> " for " <> intercalate ", " pat
                                     <> " in " <> render e2
    ForOfComp e1 pat e2 -> render e1 <> " for " <> intercalate ", " pat
                                     <> " of " <> render e2
    While cond expr -> "while " <> render cond <> render expr
    TryCatch e c f -> do
      let catch = case c of Nothing -> ""
                            Just c -> "catch " <> render c
      let finally = case f of Nothing -> ""
                              Just f -> "finally " <> render f
      "try " <> render e <> catch <> finally
    Comment c -> "# " <> c
    Break -> "break"
    Continue -> "continue"
    EmptyExpr -> "### empty expression ###"
    Dotted expr n -> render expr <> "." <> n
    Class name extends decs -> "class" <>
      case name of {Nothing -> ""; Just n -> " " <> n} <>
      case extends of {Nothing -> ""; Just e -> " extends " <> render e} <>
      case decs of {[] -> ""; ds -> "{" <> intercalate "; " (map render ds) <> "}"}
    e -> error $ "can't render " <> show e
    where
      mapr = map render
      imapr t = intercalate t . mapr
      renderP (name, expr) = name <> ":" <> render (unExpr expr)
      rswitch expr cases _else = do
        let rcase (es, res) = "when " <> imapr "," es <> " then " <> render res
        let relse e = case e of {Nothing -> ""; Just e -> "else " <> render e}
        "switch " <> render expr <> "{" <> intercalate "," (map rcase cases)
                                 <> relse _else
      render' :: (Render e, IsExpr e) => e -> Text
      render' e = case unExpr e of
        Binary _ _ _ -> "(" <> render e <> ")"
        Call _ _ -> "(" <> render e <> ")"
        Prefix _ _ -> "(" <> render e <> ")"
        Postfix _ _ -> "(" <> render e <> ")"
        Function _ _ -> "(" <> render e <> ")"
        _ -> render e

  -- Special case at the top so that we don't indent top-level blocks.
  pretty (Block es) = intercalate "\n" $ map pretty es
  pretty e = removeConsecutive '\n' $ evalState (go e) 0 where
    go :: (IsExpr e, Render e) => AbsExpr e -> State Int Text
    go = \case
      Variable n -> return n
      Number n -> return $ render n
      String s -> return $ render s
      Regex r -> return $ render r
      EmptyExpr -> return "# empty line"
      InString s -> error "interp string pretty printing"
      Assign pat expr -> do
        expr' <- go' expr
        return $ render pat <> " = " <> expr'
      Block exprs -> indented exprs go'
      Array exprs -> do
        res <- mapM go' exprs
        return $ "[" <> intercalate ", " res <> "]"
      ArrayRange e1 e2 -> do
        res1 <- go' e1
        res2 <- go' e2
        return $ "[" <> res1 <> " .. " <> res2 <> "]"
      ObjectDeref e ref -> do
        e' <- go' e
        ref' <- go' ref
        return $ e' <> "[" <> ref' <> "]"
      Object pairs -> indented pairs $ \(name, expr) -> do
        expr' <- go' expr
        return $ name <> ": " <> expr'
      Function names expr -> do
        let start = "(" <> intercalate ", " names <> ") -> "
        mappend start <$> go' expr
      FatArrowFunction names expr -> do
        let start = "(" <> intercalate ", " names <> ") => "
        mappend start <$> go' expr
      Call expr exprs -> go'' expr >>= \e -> case exprs of
        [] -> return $ e <> "()"
        exprs -> do
          exprs' <- mapM go'' exprs
          return $ e <> " " <> intercalate ", " exprs'
      Binary op e1 e2 -> do
        e1' <- go'' e1
        e2' <- go'' e2
        return $ e1' <> " " <> op <> " " <> e2'
      Prefix op expr -> go' expr >>= \e -> return $ op <> e
      Postfix expr op -> go' expr >>= \e -> return $ e <> op
      Return Nothing -> return "return"
      Return (Just expr) -> ("return " <>) <$> go' expr
      Throw expr -> ("throw " <>) <$> go' expr
      New expr -> ("new " <>) <$> go' expr
      Switch expr cases _else -> do
        expr' <- go' expr
        cases' <- indented cases $ \(es, res) -> do
          es' <- mapM go' es
          res' <- go' res
          return $ "when " <> intercalate ", " es' <> res'
        _else' <- case _else of
          Nothing -> return ""
          Just expr -> indented [expr] $ \e -> do
            e' <- go' e
            return $ "else " <> e'
        return $ "switch " <> expr' <> cases' <> _else'
      If c t f -> do
        c' <- go' c
        t' <- go' t
        case f of
          Nothing -> return $ "if " <> c' <> t'
          Just f -> do
            f' <- go' f
            els <- line "else"
            return $ "if " <> c' <> t' <> els <> f'
      ForIn pat e1 e2 -> do
        e1' <- go' e1
        e2' <- go' e2
        return $ "for " <> intercalate ", " pat <> " in " <> e1' <> e2'
      ForOf pat e1 e2 -> do
        e1' <- go' e1
        e2' <- go' e2
        return $ "for " <> intercalate ", " pat <> " of " <> e1' <> e2'
      ForInComp e1 names e2 -> do
        e1' <- go' e1
        e2' <- go' e2
        return $ e1' <> " for " <> intercalate ", " names <> " in " <> e2'
      ForOfComp e1 names e2 -> do
        e1' <- go' e1
        e2' <- go' e2
        return $ e1' <> " for " <> intercalate ", " names <> " of " <> e2'
      While cond expr -> do
        cond' <- go' cond
        expr' <- go' expr
        return $ "while " <> cond' <> expr'
      TryCatch e c f -> do
        e' <- go' e
        catch <- case c of Nothing -> return ""
                           Just (name, c) -> do
                             c' <- go' c
                             pure $ "catch " <> name <> " " <> c' <> "\n"
        finally <- case f of Nothing -> return ""
                             Just f -> fmap ("finally " <>) $ go' f
        return $ "try " <> e' <> catch <> finally
      Comment c -> return $ "# " <> c
      Break -> return "break"
      Continue -> return "continue"
      Dotted expr n -> do
        expr' <- go' expr
        return $ expr' <> "." <> n
      Class name extends decs -> do
        let name' = case name of {Nothing -> ""; Just n -> " " <> n}
        extends' <- case extends of
          Nothing -> return ""
          Just e -> go' e >>= \e' -> return $ " extends " <> e'
        decs' <- case decs of
          [] -> return ""
          ds -> do
            modify (+1)
            strs <- forM decs $ \d -> do
              level <- get
              return $ replicate level " " <> render d <> "\n"
            modify (\i -> i - 1)
            return $ "\n" <> mconcat strs
        return $ "class" <> name' <> extends' <> decs'
      e -> error $ "can't pretty print " <> show e
    line txt = do
      level <- get
      when (level < 0) $ error "Illegal indent level"
      return $ replicate level "  " <> txt
    line' txt = fmap (<> "\n") $ line txt
    indented list f = do
      modify (+1)
      strs <- forM list $ f >=> line'
      modify (\i -> i - 1)
      return $ "\n" <> mconcat strs
    wrap e = go' e >>= \e' -> return $ "(" <> e' <> ")"
    go' :: (IsExpr e, Render e) => e -> State Int Text
    go' = go . unExpr
    go'' e = case unExpr e of
      Binary _ _ _ -> wrap e
      Call _ _ -> wrap e
      Prefix _ _ -> wrap e
      Postfix _ _ -> wrap e
      Function _ _ -> wrap e
      _ -> go' e

removeConsecutive :: Char -> Text -> Text
removeConsecutive c input = pack $ scanit $ input' where
  input' = unpack input
  scanit [] = []
  scanit [c] = [c]
  scanit (c1:c2:rest) | c1 == c && c2 == c = c2 : scanit rest
                      | otherwise = c1 : scanit (c2 : rest)

instance IsString (InString e) where
  fromString str = Plain $ pack str

instance Render e => Render (ClassDec e) where
  render (ClassDecExpr e) = render e
  render (ClassDecDef name e) = name <> ":" <> render e

instance Monoid (InString e) where
  mempty = Plain mempty
  is1 `mappend` is2 = case (is1, is2) of
    (Plain s, Plain s') -> Plain (s <> s')
    (s, Interpolated is e is') -> Interpolated (s <> is) e is'
    (Interpolated is e is', s) -> Interpolated is e (is' <> s)

validSymbols :: Set String
validSymbols = fromList
  [ "+", "*", "-", "/", ">", "<", ">=", "<=", "==", "===", "&", "|", "&&"
  , "||", "^", "**", "//", "+=", "-=", "*=", "/=", "->", "=>", "=", "?", "=->"]

symChars :: String
symChars = "+*-/|&><=@?"

instance Render a => Render (Maybe a) where
  render Nothing = "Nothing"
  render (Just a) = "Just " <> render a

instance Render Int
