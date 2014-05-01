{-# LANGUAGE LambdaCase #-}
module AST where

import Common

type Name = Text
data AbsExpr expr = Variable Name
                  | Number Double
                  | String Text
                  | Regex Text
                  | InString (InString expr)
                  | Assign Pattern expr
                  | Block [expr]
                  | Array [expr]
                  | ArrayRange expr expr
                  | Object [(Name, expr)]
                  | Function [Name] expr
                  | Dotted expr Name
                  | Call expr [expr]
                  | Binary Name expr expr
                  | Prefix Name expr
                  | Postfix expr Name
                  | Return (Maybe expr)
                  | Throw expr
                  | New expr
                  | Switch expr [SwitchCase expr]
                  | If expr expr (Maybe expr)
                  | ForIn Pattern expr expr
                  | ForOf Pattern expr expr
                  | While expr expr
                  | TryCatch expr Name expr (Maybe expr)
                  | Comment Text
                  | Break
                  | Continue
                  | Class (Maybe Name) (Maybe expr) [ClassDec expr]
                  deriving (Show, Eq)

data InString e = Plain Text
                | Interpolated (InString e) (AbsExpr e) (InString e)
                deriving (Show, Eq)

data ClassDec expr = ClassDecExpr expr
                   | ClassDecDef Name expr
                   deriving (Show, Eq)

data SwitchCase expr = SwitchCase expr deriving (Show, Eq)
data Pattern = Pattern deriving (Show, Eq)

instance Pretty expr => Pretty (AbsExpr expr) where
  render = \case
    Variable n -> n
    Number n -> render n
    String s -> render s
    Regex r -> render r
    InString s -> error "interp string rendering"
    Assign pat expr -> render pat <> " = " <> render expr <> ";"
    Block exprs -> "{" <> intercalate "; " (map render exprs) <> "}"
    Array exprs -> "[" <> intercalate ", " (map render exprs) <> "]"
    ArrayRange e1 e2 -> "[" <> render e1 <> " .. " <> render e2 <> "]"
    Function names expr -> "(" <> intercalate ", " names <> ") -> " <> render expr
    --Call (Variable n) exprs -> n <> " " <> intercalate ", " (map render exprs)
    Call expr exprs -> case exprs of
      [] -> "(" <> render expr <> ") " <> "()"
      exprs -> "(" <> render expr <> ") " <> intercalate ", " (map render exprs)
    Binary op e1 e2 -> "(" <> render e1 <> ") " <> op <> " (" <> render e2 <> ")"
    Prefix op expr -> op <> render expr
    Postfix expr op -> render expr <> op
    Return Nothing -> "return"
    Return (Just expr) -> "return " <> render expr
    Throw expr -> "throw " <> render expr
    New expr -> "new " <> render expr
    Switch expr cases -> error "switch rendering"
    If c t Nothing -> "if " <> render c <> " then " <> render t
    If c t (Just f) -> "if " <> render c <> " then " <> render t <> " else " <> render f
    ForIn pat e1 e2 -> error "for/in rendering"
    ForOf pat e1 e2 -> error "for/of rendering"
    While cond expr -> "while " <> render cond <> render expr
    TryCatch e1 name e2 finally -> error "try/catch rendering"
    Comment c -> "# " <> c
    Break -> "break"
    Continue -> "continue"
    Dotted expr n -> render expr <> "." <> n
    Class name extends decs -> "class" <>
      case name of {Nothing -> ""; Just n -> " " <> n} <>
      case extends of {Nothing -> ""; Just e -> " extends " <> render e} <>
      case decs of {[] -> ""; ds -> "{" <> intercalate "; " (map render ds) <> "}"}
    e -> error $ "can't render " <> show e
    where
      render' :: Pretty e => AbsExpr e -> Text
      render' e = case e of
        Binary _ _ _ -> "(" <> render e <> ")"
        Call _ _ -> "(" <> render e <> ")"
        Prefix _ _ -> "(" <> render e <> ")"
        Postfix _ _ -> "(" <> render e <> ")"
        Function _ _ -> "(" <> render e <> ")"
        _ -> render e

instance Pretty Pattern

instance IsString (InString e) where
  fromString str = Plain $ pack str

instance Pretty e => Pretty (ClassDec e) where
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
