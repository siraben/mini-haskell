------------------------------------------------------------------------
-- A mini Haskell compiler with typeclasses.
-- Originally written by Ben Lynn, modified by Ben Siraphob
------------------------------------------------------------------------
-- Delete code below and uncomment the block to compile in GHC
{-
{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Compiler where
import Prelude (Char, Int, String, succ)
import Data.Char (chr, ord)
import qualified Prelude
a <= b = if a Prelude.<= b then True else False
(*) = (Prelude.*)
(+) = (Prelude.+)
(-) = (Prelude.-)
(/) = Prelude.div
(%) = Prelude.mod
class Eq a where { (==) :: a -> a -> Bool };
class Show a where { show :: a -> String };
class Functor f where { fmap :: (a -> b) -> f a -> f b };
class Applicative f where { pure :: a -> f a; (<*>) :: f (a -> b) -> f a -> f b };
class Monad m where { return :: a -> m a ; (>>=) :: m a -> (a -> m b) -> m b};
instance Eq Char where { (==) x y = if x Prelude.== y then True else False };
instance Eq Int where { (==) x y = if x Prelude.== y then True else False };
infixr 5 ++;
infixr 9 .;
infixl 4 <*> , <$> , <* , *>;
infixl 3 <|>;
infixr 0 $;
infixl 7 *;
infixl 6 + , -;
(*) = (Prelude.*);
(+) = (Prelude.+);
(-) = (Prelude.-);
-}
infixr 5 :, ++;
infixr 9 .;
infixl 4 <*> , <$> , <* , *>;
infixl 3 <|>;
infixr 0 $;
infixl 7 *;
infixl 6 + , -;
(*) = (.*.);
(+) = (.+.);
(-) = (.-.);
(%) = (.%.);
(/) = (./.);
-- Delete code above and uncomment the block to compile in GHC
undefined = undefined;
($) f = f;
id x = x;
const x y = x;
flip f x y = f y x;
(&) x f = f x;
(<$>) = fmap;
liftA2 f x = (<*>) (fmap f x);
(*>) = liftA2 $ \x y -> y;
(<*) = liftA2 const;
data Bool = True | False;
data Maybe a = Nothing | Just a;
-- fpair = flip curry
fpair p f = case p of { (,) x y -> f x y };
fst p = case p of { (,) x y -> x };
snd p = case p of { (,) x y -> y };
first f p = fpair p $ \x y -> (f x, y);
second f p = fpair p $ \x y -> (x, f y);
ife a b c = case a of { True -> b ; False -> c };
not a = case a of { True -> False; False -> True };
(.) f g x = f (g x);
(||) f g = ife f True (ife g True False);
(&&) f g = ife f (ife g True False) False;
-- fold a list
-- flist :: [a] -> b -> (a -> [a] -> b) -> b
flst xs n c = case xs of { [] -> n; (:) h t -> c h t };
-- (==) on lists
lstEq xs ys = case xs of
  { []       -> flst ys True (\h t -> False)
  ; (:) x xt -> flst ys False (\y yt -> ife (x == y) (lstEq xt yt) False)
  };
instance Eq a => Eq [a] where { (==) = lstEq };
(/=) x y = not (x == y);
-- Append two lists
(++) xs ys = flst xs ys (\x xt -> x:xt ++ ys);
-- maybe :: b -> (a -> b) -> Maybe a -> b
maybe n j m = case m of { Nothing -> n; Just x -> j x };
instance Show a => Show (Maybe a) where
  { show = maybe "Nothing" (\x -> "Just " ++ show x) };
instance Functor Maybe where
  { fmap f = maybe Nothing (Just . f) };
instance Applicative Maybe where
  { pure = Just ; (<*>) f y = maybe Nothing (`fmap` y) f};
instance Monad Maybe where
  { return = Just ; (>>=) ma f = maybe Nothing f ma };
fromMaybe a m = case m of { Nothing -> a; Just x -> x };
unmaybe = fromMaybe undefined;
foldr c n l = flst l n (\h t -> c h (foldr c n t));
-- foldr1' :: (a -> a -> a) -> [a] -> Maybe a
foldr1' c l =
    flst
       l
       Nothing
       (\h t ->
          foldr
            (\x m ->
               Just
                 (case m of
                  { Nothing -> x
                  ; Just y -> c x y}))
            Nothing
            l);

foldl f a bs = foldr (\b g x -> g (f x b)) id bs a;
-- foldl1' :: (p -> p -> p) -> [p] -> Maybe p
foldl1' f l = flst l Nothing (\x xs -> Just (foldl f x xs));
elem k = foldr (\x t -> ife (x == k) True t) False;
find f = foldr (\x t -> ife (f x) (Just x) t) Nothing;
concat = foldr (++) [];
itemize c = [c];
map f = foldr (\x xs -> f x : xs) [];
concatMap f l = concat (map f l);
instance Functor [] where { fmap = map };
instance Monad [] where { return = itemize ; (>>=) = flip concatMap };
instance Applicative [] where
  { pure = itemize
  ; (<*>) fs xs = fs >>= \f -> xs >>= \x -> return $ f x};
prependToAll s l = case l of {
                     []       -> [];
                     (:) x xs -> s : x : prependToAll s xs
};

intersperse s l = case l of {
    []       -> [];
    (:) x xs -> x : prependToAll s xs
};


-- Show a non-empty list
intercalate d = concat . intersperse d;
showList' l = "[" ++ intercalate "," (map show l) ++ "]";
showList l = case l of {
               []       -> "[]";
               (:) x xs -> showList' l
};

mapconcat f l = concat (map f l);
escapeC c = ife (c == '\n') "\\n"
            (ife (c == '\\') "\\\\"
             [c]);
showString s = "\"" ++ mapconcat escapeC s ++ "\"";

-- instance Show String where { show = showString };
instance Show a => Show [a] where { show = showList };

any f = foldr (\x t -> ife (f x) True t) False;
-- fold a maybe
-- fmaybe :: Maybe a -> b -> (a -> b) -> b
fmaybe m n j = case m of { Nothing -> n; Just x -> j x };
lookupWith eq s =
  foldr (\h t -> fpair h (\k v -> ife (eq s k) (Just v) t)) Nothing;

lstLookup = lookupWith (==);


-- Representation of types
--          type ctor.  type var.  type app.
data Type = TC String | TV String | TAp Type Type;
-- Representation of AST
data Ast
  = R String     -- raw combinator assembly
  | V String     -- variable
  | A Ast Ast    -- application
  | L String Ast -- lambda abstraction
  | Proof Pred;  -- proof for typeclass instantiation?

data Either a b = Left a | Right b;

--  * instance environment
--  * definitions, including those of instances
--  * Typed ASTs, ready for compilation, including ADTs and methods,
--    e.g. (==), (Eq a => a -> a -> Bool, select-==)
data Neat =
  Neat
    [(String, [Qual])]
    [Either (String, Ast) (String, (Qual, [(String, Ast)]))]
    [(String, (Qual, Ast))];

-- Parser combinators (applicative style)
data Parser a = Parser (String -> Maybe (a, String));
showRes p = case p of { Nothing -> "Nothing"
                      ; Just p -> case p of
                                    { (,) x y -> "(" ++ show x ++ ", " ++
                                                 show y ++ ")"}};
parserpure a = Parser (\s -> Just (a, s));
parse p = case p of { Parser f -> f };
parserbind pa f = Parser $ \s ->
            case parse pa s of
            { Nothing -> Nothing
            ; Just a  -> case a of
                              { (,) x s -> parse (f x) s }};
instance Monad Parser where
  { return = parserpure
  ; (>>=) = parserbind };
parserap f x = f >>= (\b -> x >>= (\x -> parserpure (b x)));
parserfmap f x = Parser $ \s ->
                   case parse x s of
                   { Nothing -> Nothing
                   ; Just a  -> case a of
                                { (,) x s -> Just (f x, s) }};
instance Functor Parser where
  { fmap = parserfmap };

instance Applicative Parser where
  { pure = parserpure
  ; (<*>) = parserap };

(<|>) x y = Parser $ \s -> case parse x s of
  { Nothing -> parse y s
  ; Just x  -> Just x };
-- Using liftA2 f x y over f <$> x <*> y results in a heap usage
-- decrease of around 100
-- liftA2 f x = ap (parserfmap f x);

item = Parser $ \s -> case s of { [] -> Nothing; (:) c cs -> Just (c,cs) };
-- sat :: (Char -> Bool) -> Parser Char
sat p = Parser $ \s -> case s of { [] -> Nothing
                                 ; (:) c cs ->
                                   ife (p c) (Just (c, cs)) Nothing };

many p = liftA2 (:) p (many p) <|> pure [];
many1 p = liftA2 (:) p (many p);
sepBy1 p sep = liftA2 (:) p (many (sep *> p));
sepBy p sep = sepBy1 p sep <|> pure [];

char c = sat (== c);
string s =
  case s of
  { [] -> pure s
  ; (:) c cs -> char c *> string cs *> pure s};

between x y p = x *> (p <* y);
-- Parse line comments
com = char '-' *> between (char '-') (char '\n') (many (sat (/= '\n')));
-- Block comments
notComEnd = (sat (/= '-') <|> (char '-' *> sat (/= '}'))) *> parserpure [];
blockcom = let { content = many (blockcom <|> notComEnd) }
           in between (string "{-") (string "-}") content *> parserpure [];
sp =
  many
    ((pure <$> sat (\c -> (c == ' ') || (c == '\n'))) <|> com <|> blockcom);
spc f = f <* sp;
spch = spc . char;
-- wantWith :: (a -> Bool) -> Parser a -> Parser a
wantWith pred p = Parser $ \s ->
                  case parse p s of
                  { Nothing -> Nothing
                  ; Just x -> case x of
                       { (,) a s -> ife (pred a) (Just (a,s)) Nothing }};
-- want :: Eq a => Parser a -> a -> Parser a
want f s = wantWith (== s) f;

paren = between (spch '(') (spch ')');
upper = sat $ \x -> ((x <= 'z') && ('a' <= x)) || (x == '_');
lower = sat $ \x -> (x <= 'Z') && ('A' <= x);
digit = sat $ \x -> (x <= '9') && ('0' <= x);
varLex = liftA2 (:) upper (many (upper <|> lower <|> digit <|> char '\''));
-- Constructor identifier
conId = spc (liftA2 (:) lower (many (upper <|> lower <|> digit <|> char '\'')));
keyword s = spc (want varLex s);
varId = spc (wantWith (\s -> not ((s == "of") || (s == "where"))) varLex);
-- Operator characters
opLex = many1 (sat (`elem` ":!#$%&*+./<=>?@\\^|-~"));
-- Operators
op = spc opLex <|> between (spch '`') (spch '`') varId;
var = varId <|> paren (spc opLex);
anyOne = fmap pure (spc (sat (const True)));

-- Lambda
lam r =
  spch '\\' *>
  liftA2 (flip (foldr L)) (many1 varId) (char '-' *> (spch '>' *> r));

listify = fmap (foldr (\h t -> A (A (V ":") h) t) (V "[]"));
escChar = char '\\' *> (sat (`elem` "'\"\\") <|> (const '\n' <$> char 'n'));
litOne delim = fmap (\c -> R ('#' : pure c)) (escChar <|> sat (/= delim));
litInt = R . ('(' :) . (++ ")") <$> spc (many1 digit);
litStr = listify (between (char '"') (spch '"') (many (litOne '"')));
litChar = between (char '\'') (spch '\'') (litOne '\'');
lit = litStr <|> litChar <|> litInt;
sqLst r = listify (between (spch '[') (spch ']') (sepBy r (spch ',')));
alt r =
  (,) <$>
  (conId <|> (pure <$> paren (spch ':' <|> spch ',')) <|>
   liftA2 (:) (spch '[') (pure <$> spch ']')) <*>
  liftA2 (flip (foldr L)) (many varId) (want op "->" *> r);
braceSep f = between (spch '{') (spch '}') (sepBy f (spch ';'));
alts r = braceSep (alt r);
cas' x as = foldl A (V (concatMap (('|' :) . fst) as)) (x : map snd as);
-- Case expressions
cas r = liftA2 cas' (between (keyword "case") (keyword "of") r) (alts r);
thenComma r =
  spch ',' *> (((\x y -> A (A (V ",") y) x) <$> r) <|> parserpure (A (V ",")));

-- Sections
parenExpr r =
  liftA2
    (&)
    r
    (((\v a -> A (V v) a) <$> op) <|> thenComma r <|> parserpure id);

rightSect r =
  ((\v a -> A (A (V "\\C") (V v)) a) <$> (op <|> (pure <$> spch ','))) <*> r;
section r = paren (parenExpr r <|> rightSect r);

-- Does a string occur free in the AST?
-- isFree :: String -> Ast -> Bool
isFree v expr = case expr of
  { R s     -> False
  ; V s     -> s == v
  ; A x y   -> isFree v x || isFree v y
  ; L w t   -> (v /= w) && isFree v t
  ; Proof _ -> False
  };

maybeFix s x = ife (isFree s x) (A (V "\\Y") (L s x)) x;

def r =
  liftA2 (,) var (flip (foldr L) <$> many varId <*> (spch '=' *> r));

-- Convert a list of let bindings and the let body into a single AST.
addLets ls x =
  foldr (\p t -> fpair p (\name def -> A (L name t) $ maybeFix name def)) x ls;

-- Parse lets
letin r =
  liftA2
    addLets
    (between (keyword "let") (keyword "in") (braceSep (def r)))
    r;

atom r =
  letin r                                  <|>
  sqLst r                                  <|>
  section r                                <|>
  cas r                                    <|>
  lam r                                    <|>
  (paren (spch ',') *> parserpure (V ",")) <|>
  parserfmap V (conId <|> var)             <|>
  lit;

aexp r = parserfmap (unmaybe . foldl1' A) (many1 (atom r));
fix f = f (fix f);

-- Parse infix operators
--            infix   infixl   infixr
data Assoc = NAssoc | LAssoc | RAssoc;

instance Show Assoc where
  { show a =
     case a of
     { NAssoc -> "NAssoc"
     ; LAssoc -> "LAssoc"
     ; RAssoc -> "RAssoc" } };

eqAssoc x y = case x of
  { NAssoc -> case y of { NAssoc -> True  ; LAssoc -> False ; RAssoc -> False }
  ; LAssoc -> case y of { NAssoc -> False ; LAssoc -> True  ; RAssoc -> False }
  ; RAssoc -> case y of { NAssoc -> False ; LAssoc -> False ; RAssoc -> True }
  };
instance Eq Assoc where { (==) = eqAssoc };

precOf s precTab = fmaybe (lstLookup s precTab) 5 fst;
assocOf s precTab = fmaybe (lstLookup s precTab) LAssoc snd;
opWithPrec precTab n = wantWith (\s -> n == precOf s precTab) op;
-- opFold'
--   :: [(String, (a, Assoc))] -> Ast -> [(String, Ast)] -> Maybe Ast
opFold' precTab e xs =
  case xs of
  { [] -> Just e
  ; (:) x xt ->
      case find
             (\y ->
                not (assocOf (fst x) precTab == assocOf (fst y) precTab))
             xt of
      { Nothing ->
          case assocOf (fst x) precTab of
          { NAssoc ->
              case xt of
              {  [] -> Just $ fpair x (\op y -> A (A (V op) e) y)
              ;  (:) y yt -> Nothing }
          ; LAssoc -> Just $ foldl (\a b -> fpair b (\op y -> A (A (V op) a) y)) e xs
          ; RAssoc ->
              Just $ foldr (\a b -> fpair a (\op y e -> A (A (V op) e) (b y))) id xs e }
      ; Just y -> Nothing }};

expr precTab =
  fix $ \r n ->
    ife
      (n <= 9)
      ((unmaybe .) . opFold' precTab <$> r (succ n) <*>
       many (liftA2 (,) (opWithPrec precTab n) (r (succ n))))
      (aexp (r 0));

data Constr = Constr String [Type];
data Pred = Pred String Type;
data Qual = Qual [Pred] Type;

data Top = Adt Type [Constr]
         | Def (String, Ast)
         | Class String Type [(String, Type)]
         | Inst String Qual [(String, Ast)];

-- arrow type constructor
arr a = TAp (TAp (TC "->") a);

bType r = unmaybe . foldl1' TAp <$> many1 r;

_type r = unmaybe . foldr1' arr <$> sepBy (bType r) (spc (want opLex "->"));
typeConstant =
  (\s -> ife (s == "String") (TAp (TC "[]") (TC "Int")) (TC s)) <$> conId;

aType =
  paren
    (liftA2
       (&)
       (_type aType)
       ((spch ',' *> ((\a b -> TAp (TAp (TC ",") b) a) <$> _type aType)) <|>
        parserpure id)) <|>
  typeConstant <|>
  (TV <$> varId) <|>
  (spch '[' *>
   (spch ']' *> parserpure (TC "[]") <|>
    TAp (TC "[]") <$> (_type aType <* spch ']')));

simpleType c vs = foldl TAp (TC c) (map TV vs);

adt =
  liftA2
    Adt
    (between (keyword "data") (spch '=') (liftA2 simpleType conId (many varId)))
    (sepBy (liftA2 Constr conId (many aType)) (spch '|'));

prec = (\c -> ord c - ord '0') <$> spc digit;
fixityList a n = map (, (n, a));
fixityDecl kw a =
  between
    (keyword kw)
    (spch ';')
    (liftA2 (fixityList a) prec (sepBy op (spch ',')));
fixity =
  fixityDecl "infix" NAssoc  <|>
  fixityDecl "infixl" LAssoc <|>
  fixityDecl "infixr" RAssoc;

noQual = Qual [];
genDecl = liftA2 (,) var (char ':' *> spch ':' *> _type aType);

-- Class declarations
classDecl =
  keyword "class" *>
  (Class <$> conId <*> (TV <$> varId) <*> (keyword "where" *> braceSep genDecl));

inst = _type aType;

-- Instance declarations
instDecl r =
  keyword "instance" *>
  ((\ps cl ty defs -> Inst cl (Qual ps ty) defs) <$>
   (liftA2 ((pure .) . Pred) conId (inst <* want op "=>") <|>
    parserpure []) <*>
   conId <*>
   inst <*>
   (keyword "where" *> braceSep (def r)));

tops precTab =
  sepBy
    (adt <|> Def <$> def (expr precTab 0) <|> classDecl <|>
     instDecl (expr precTab 0))
    (spch ';');

program' = sp *> (concat <$> many fixity) >>= tops;

eqPre = fmaybe (parse program' $
  "class Eq a where { (==) :: a -> a -> Bool };\n" ++
  "class Show a where { show :: a -> String };\n" ++
  "class Functor f where { fmap :: (a -> b) -> f a -> f b };\n" ++
  "class Applicative f where { pure :: a -> f a; (<*>) :: f (a -> b) -> f a -> f b };\n" ++
  "class Monad m where { return :: a -> m a ; (>>=) :: m a -> (a -> m b) -> m b};\n" ++
  "instance Eq Int where { (==) = intEq };\n") undefined fst;

program =
  ((eqPre ++
      -- data [] a = [] | (:) a ([] a)
    [ Adt
        (TAp (TC "[]") (TV "a"))
        [Constr "[]" [], Constr ":" [TV "a", TAp (TC "[]") (TV "a")]]
    -- data (,) a b = (,) a b
    , Adt (TAp (TAp (TC ",") (TV "a")) (TV "b")) [Constr "," [TV "a", TV "b"]]
    ]) ++) <$>
  program';


-- Primitives
-- prims :: [(String, (Qual, Ast))]
prims =
  let { ii = arr (TC "Int") (TC "Int")
      ; iii = arr (TC "Int") ii
      ; bin s = R $ "``BT`T" ++ s }
   in map (second (first noQual)) $
      [ ("\\Y", (arr (arr (TV "a") (TV "a")) (TV "a"), R "Y"))
      , ( "\\C"
        , ( arr
              (arr (TV "a") (arr (TV "b") (TV "c")))
              (arr (TV "b") (arr (TV "a") (TV "c")))
          , R "C"))
      , ("intEq", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "="))
      , ("<=", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "L"))
      , ("chr", (ii, R "I"))
      , ("ord", (ii, R "I"))
      , ("succ", (ii, R "`T`(1)+"))
      ] ++
      map (\s -> ('.':s ++ ".", (iii, bin s))) ["+", "-", "*", "/", "%"];

ifz n = ife (0 == n);
showInt' n = ifz n id (showInt' (n/10) . (:) (chr (48+(n%10))));
showInt n = ifz n ('0':) (showInt' n);

-- N.B. using show on Ints will make GHC fail to compile to due GHC
-- having multiple numeric types.
instance Show Int where { show n = showInt n "" };

-- rank :: [(String, b)] -> String -> [Char]
rank ds v =
  foldr
    (\d t -> ife (v == fst d) (\n -> '[' : showInt n "]") (t . succ))
    undefined
    ds
    0;

-- Total variant
rank' ds v =
  let { loop l v c =
         case l of
         { [] -> Nothing
         ; (:) x xs ->
            ife (v == fst x) (Just ('[' : showInt c "]")) (loop xs v (succ c)) } }
   in loop ds v 0;

-- showC :: [(String, b)] -> Ast -> String
-- Total version of showC
showC ds t = case t of
  { R s     -> Just s
  ; V v     -> rank' ds v
  ; A x y   -> liftA2 (\a b -> '`':a ++ b) (showC ds x) (showC ds y)
  ; L w t   -> Nothing
  ; Proof _ -> Nothing
  };

-- De Bruijn encoding of lambda calculus terms
--        z    s       lift ast   abs.   app.
data LC = Ze | Su LC | Pass Ast | La LC | App LC LC;

-- Convert the AST into a nameless representation
-- debruijn :: [String] -> Ast ->  LC
debruijn n e = case e of
  { R s     -> Pass (R s)
  ; V v     -> foldr (\h m -> ife (h == v) Ze (Su m)) (Pass (V v)) n
  ; A x y   -> App (debruijn n x) (debruijn n y)
  ; L s t   -> La (debruijn (s:n) t)
  ; Proof _ -> undefined
  };

-- See Kiselyov's paper - "Lambda to SKI, semantically", pages 10 - 11
--         V       C            N          W
data Sem = Defer | Closed Ast | Need Sem | Weak Sem;
-- ($$) algorithm

-- ($$), case Defer
-- Parameters: r == self
ldef r y = case y of
  { -- (V, V)   -> N (C S.(S $! I $! I))
    Defer    -> Need (Closed (A (A (R "S") (R "I")) (R "I")))
    -- (V, C d) -> N (C S.(kC $! kI $! d))
  ; Closed d -> Need (Closed (A (R "T") d))
    -- (V, N e) -> N (C S.(kS $! kI) $$ e)
  ; Need e   -> Need (r (Closed (A (R "S") (R "I"))) e)
    -- (V, W e) -> N (C (S.(kS $! kI)) $$ e)
  ; Weak e   -> Need (r (Closed (R "T")) e)
  };

-- ($$), case Closed
-- d is the argument to Closed (i.e. lclo r (Closed d) y = ...)
lclo r d y = case y of
  { -- (C d, V)     -> N (C d)
    Defer     -> Need (Closed d)
    -- (C d1, C d2) -> C (S.(d1 $! d2))
  ; Closed dd -> Closed (A d dd)
    -- (C d, N e)   -> N (C S.(kB $! d) $$ e)
  ; Need e    -> Need (r (Closed (A (R "B") d)) e)
    -- (C d, W e)   -> W (C d $$ e)
  ; Weak e    -> Weak (r (Closed d) e)
  };

-- ($$), case Need
-- e is the argument to Need (i.e. lnee r (Need e) y = ...)
lnee r e y = case y of
  { -- (N e, V)     -> N (C S.kS $$ e $$ C S.kI)
    Defer    -> Need (r (r (Closed (R "S")) e) (Closed (R "I")))
    -- (N e, C d)   -> N (C S.(kC $! kC $! d) $$ e)
  ; Closed d -> Need (r (Closed (A (R "R") d)) e)
    -- (N e1, N e2) -> N ((C S.kS) $$ e1 $$ e2)
  ; Need ee  -> Need (r (r (Closed (R "S")) e) ee)
    -- (N e1, W e2) -> N ((C S.kC) $$ e1 $$ e2)
  ; Weak ee  -> Need (r (r (Closed (R "C")) e) ee)
  };

-- ($$), case Weak
-- e is the argument to Weak (i.e. lweak r (Weak e) y = ...)
lwea r e y = case y of
  { -- (W e, V)     -> N e
    Defer    -> Need e
    -- (W e, C d)   -> W (e $$ C d)
  ; Closed d -> Weak (r e (Closed d))
    -- (W e1, N e2) -> N ((C S.kB) $$ e1 $$ e2)
  ; Need ee  -> Need (r (r (Closed (R "B")) e) ee)
    -- (W e1, W e2) -> W (e1 $$ e2)
  ; Weak ee  -> Weak (r e ee)
  };

-- ($$), the full thing.
babsa x y = case x of
  { Defer    -> ldef babsa y
  ; Closed d -> lclo babsa d y
  ; Need e   -> lnee babsa e y
  ; Weak e   -> lwea babsa e y
  };

-- Full bracket abstraction algorithm, from De Bruijn to combinators
-- babs :: LC -> Sem
babs t = case t of
  { -- let z : (a*y, a) repr = V
    Ze -> Defer
    -- let s: (b*y, a) repr -> (_*(b*y), a) repr = fun e -> W e
    -- Looks like this version recurs on e.
  ; Su e -> Weak (babs e)
    -- A lifted AST is closed.
  ; Pass s -> Closed s
    -- See "lam" function on page 10 of Kiselyov
    -- Lambda abstraction
  ; La t -> case babs t of
    { -- V     -> C S.kI
      Defer    -> Closed (R "I")
      -- C d   -> C S.(kK $! d)
      -- Remark: d is a closed body of a lambda abstraction, so the
      -- variable being abstracted over is not used and thus we can
      -- use the K combinator
    ; Closed d -> Closed (A (R "K") d)
      -- N e   -> e
    ; Need e   -> e
      -- W e   -> (C S.kK) $$ e
    ; Weak e   -> babsa (Closed (R "K")) e
    }
    -- Application
  ; App x y -> babsa (babs x) (babs y)
  };

-- Convert an AST into debruijn form, then perform bracket abstraction,
-- return if and only if we have a closed form.
-- nolam :: Ast -> Maybe Ast
nolam x = case babs (debruijn [] x) of
  { Defer    -> Nothing
  ; Closed d -> Just d
  ; Need e   -> Nothing
  ; Weak e   -> Nothing
  };

dump tab ds =
  case ds of
  { [] -> return []
  ; (:) h t ->
    nolam (snd h) >>= \a ->
    showC tab a   >>= \b ->
    dump tab t    >>= \c ->
    return (b ++ (';' : c)) };

asm ds = dump ds ds;

-- Apply substitutions to a tree
apply sub t = case t of
  { TC v    -> t
    -- Lookup v in the substitutions, if found, replace it with t
  ; TV v    -> fromMaybe t (lstLookup v sub)
  ; TAp a b -> TAp (apply sub a) (apply sub b)
  };

-- Combine two substitution lists while applying the substitutions in
-- the first.
(@@) s1 s2 = map (second (apply s1)) s2 ++ s1;

-- Occurs check
-- occurs :: String -> Type -> Bool
occurs s t = case t of
  { TC v    -> False
  ; TV v    -> s == v
  ; TAp a b -> occurs s a || occurs s b
  };

-- Bind the type variable s to the type t
varBind s t = case t of
  { --         Just (pure (s, t)) is clearer
    TC v    -> pure (pure (s, t))
    -- Binding a variable with another variable
  ; TV v    -> ife (v == s) (pure []) (pure (pure (s, t)))
    -- Infinite types not allowed
  ; TAp a b -> ife (occurs s t) Nothing (pure (pure (s, t)))
  };

-- Most general unifier.  Given two type trees, possibly return the
-- assignments that make them equal.

-- We pass unify as an argument to achieve mutual recursion.
mgu unify t u = case t of
  { TC a -> case u of
    { TC b    -> ife (a == b) (pure []) Nothing
    ; TV b    -> varBind b t
    ; TAp a b -> Nothing
    }
  ; TV a -> varBind a u
  ; TAp a b -> case u of
    { TC b    -> Nothing
    ; TV b    -> varBind b t
    ; TAp c d -> unify b d (mgu unify a c)
    }
  };

unify a b =
  maybe Nothing (\s -> fmap (@@ s) (mgu unify (apply s a) (apply s b)));

-- instantiate' ::
--   Type -> Int -> [(String, Type)] -> ((Type, Int), [(String, Type)])
instantiate' t n tab = case t of
  { TC s -> ((t, n), tab)
  ; TV s -> case lstLookup s tab of
    { Nothing -> let { va = TV (s ++ '_':showInt n "") }
                 in ((va, n + 1), (s, va):tab)
    ; Just v -> ((v, n), tab)
    }
  ; TAp x y ->
    fpair (instantiate' x n tab) $ \tn1 tab1 ->
    fpair tn1 $ \t1 n1 ->
    fpair (instantiate' y n1 tab1) $ \tn2 tab2 ->
    fpair tn2 $ \t2 n2 -> ((TAp t1 t2, n2), tab2)
  };

instantiatePred pred xyz =
  case pred of
  { Pred s t ->
      fpair xyz $ \xy tab ->
        fpair xy $ \out n ->
          first (first ((: out) . Pred s)) (instantiate' t n tab) };

-- instantiate :: Qual -> Int -> (Qual, Int)
instantiate qt n =
  case qt of
  { Qual ps t ->
      fpair (foldr instantiatePred (([], n), []) ps) $ \xy tab ->
        fpair xy $ \ps1 n1 -> first (Qual ps1) (fst (instantiate' t n1 tab)) };


-- type SymTab = [(String, (Qual, Ast))];
-- type Subst = [(String, Type)];

-- infer' ::
--   [(String, (Qual, b))]
--   -> [(String, Type)]
--   -> Ast
--   -> (Maybe [(String, Type)], Int)
--   -> ((Type, Ast), (Maybe [(String, Type)], Int))
infer' typed loc ast csn =
  fpair csn $ \cs n ->
    let { va = TV ('_' : showInt n "") }
     in case ast of
        {  -- Raw code is treated as Int
           R s -> ((TC "Int", ast), csn)
        ;  V s ->
             fmaybe
               (lstLookup s loc)
               (fmaybe (lstLookup s typed) undefined $ \ta ->
                  fpair (instantiate (fst ta) n) $ \q n1 ->
                    case q of {
                      Qual preds ty ->
                        ((ty, foldl A ast (map Proof preds)), (cs, n1))
                    })
               (flip (,) csn . flip (,) ast)
        ;  A x y ->
             fpair (infer' typed loc x (cs, n + 1)) $ \tax csn1 ->
               fpair tax $ \tx ax ->
                 fpair (infer' typed loc y csn1) $ \tay csn2 ->
                   fpair tay $ \ty ay ->
                     ((va, A ax ay), first (unify tx (arr ty va)) csn2)
           -- Lambda abstraction.  Infer the body of the lambda with
           -- the substitution list extended with s := <newvar>
        ;  L s x ->
             first
               (\ta -> fpair ta $ \t a -> (arr va t, L s a))
               (infer' typed ((s, va) : loc) x (cs, n + 1))
        ;  Proof _ -> undefined };


onType f pred = case pred of { Pred s t -> Pred s (f t) };

-- typeEq :: Type -> Type -> Bool
typeEq t u = case t of
  { TC s -> case u of
    { TC t    -> t == s
    ; TV _    -> False
    ; TAp _ _ -> False
    }
  ; TV s ->  case u of
    { TC _    -> False
    ; TV t    -> t == s
    ; TAp _ _ -> False
    }
  ; TAp a b -> case u of
    { TC _    -> False
    ; TV _    -> False
    ; TAp c d -> typeEq a c && typeEq b d
    }
  };

instance Eq Type where { (==) = typeEq };
predEq p q = case p of { Pred s a -> case q of { Pred t b ->
  (s == t) && (a == b) }};

instance Eq Pred where { (==) = predEq };
predApply sub = onType (apply sub);

all f = foldr ((&&) . f) True;
filter f = foldr (\x xs -> ife (f x) (x:xs) xs) [];

intersect xs ys = filter (\x -> fmaybe (find (== x) ys) False (const True)) xs;

merge s1 s2 =
  ife
    (all (\v -> apply s1 (TV v) == apply s2 (TV v)) $
     map fst s1 `intersect` map fst s2)
    (Just $ s1 ++ s2)
    Nothing;

match h t = case h of
  { TC a -> case t of
    { TC b    -> ife (a == b) (Just []) Nothing
    ; TV b    -> Nothing
    ; TAp a b -> Nothing
    }
  ; TV a -> Just [(a, t)]
  ; TAp a b -> case t of
    { TC b -> Nothing
    ; TV b -> Nothing
    ; TAp c d -> case match a c of
      { Nothing -> Nothing
      ; Just ac -> case match b d of
        { Nothing -> Nothing
        ; Just bd -> merge ac bd
        }
      }
    }
  };

matchPred h p = case p of { Pred _ t -> match h t };

-- TODO: Add support for printing of infix type operators.
showType t = case t of
  { TC s    -> s
  ; TV s    -> s
  ; TAp a b -> concat ["(", showType a, " ", showType b, ")"]
  };

instance Show Type where { show = showType };
showPred p = case p of { Pred s t -> s ++ (' ':show t) ++ " => "};

findInst r qn p insts =
  case insts of
  {  [] ->
       fpair qn $ \q n ->
         let { v = '*' : showInt n "" }
          in (((p, v) : q, n + 1), V v)
  ;  (:) i is ->
       case i of {
         Qual ps h ->
           case matchPred h p of
             { Nothing -> findInst r qn p is
             ; Just u ->
               foldl
                 (\qnt p ->
                    fpair qnt $ \qn1 t -> second (A t) (r (predApply u p) qn1))
                 ( qn
                 , V (case p of
                       { Pred s _ -> showPred $ Pred s h }))
                 ps }}};


findProof is pred psn = fpair psn $ \ps n -> case lookupWith (==) pred ps of
  { Nothing -> case pred of { Pred s t -> case lstLookup s is of
    { Nothing    -> undefined  -- No instances!
    ; Just insts -> findInst (findProof is) psn pred insts
    }}
  ; Just s -> (psn, V s)
  };

prove' ienv sub psn a = case a of
  { R _ -> (psn, a)
  ; V _ -> (psn, a)
  ; A x y -> let { p1 = prove' ienv sub psn x } in fpair p1 $ \psn1 x1 ->
    second (A x1) (prove' ienv sub psn1 y)
  ; L s t -> second (L s) (prove' ienv sub psn t)
  ; Proof raw -> findProof ienv (predApply sub raw) psn
  };

-- prove :: [(String, [Qual])] -> (Type, Ast) -> Subst -> (Qual, Ast)
prove ienv ta sub =
  fpair ta $ \t a ->
    fpair (prove' ienv sub ([], 0) a) $ \psn x ->
      fpair psn $ \ps _ ->
        (Qual (map fst ps) (apply sub t), foldr (L . snd) x ps);

dictVars ps n =
  flst ps ([], n) $ \p pt ->
    first ((p, '*' : showInt n "") :) (dictVars pt $ n + 1);

-- qi = Qual of instance, e.g. Eq t => [t] -> [t] -> Bool
inferMethod ienv typed qi def = fpair def $ \s expr ->
  fpair (infer' typed [] expr (Just [], 0)) $ \ta msn ->
  case lstLookup s typed of
    { Nothing -> undefined -- No such method.
    -- e.g. qac = Eq a => a -> a -> Bool, some AST (product of single method)
    ; Just qac -> fpair msn $ \ms n -> case ms of
      { Nothing -> undefined  -- Type check fails.
      ; Just sub -> fpair (instantiate (fst qac) n) $ \q1 n1 -> case q1 of
        { Qual psc tc -> case psc of
        { [] -> undefined  -- Unreachable.
        ; (:) headPred shouldBeNull -> case qi of { Qual psi ti ->
          case headPred of { Pred _ headT -> case match headT ti of
          { Nothing -> undefined
          -- e.g. Eq t => [t] -> [t] -> Bool
          -- instantiate and match it against type of ta
          ; Just subc ->
            fpair (instantiate (Qual psi $ apply subc tc) n1) $ \q2 n2 ->
            case q2 of { Qual ps2 t2 -> fpair ta $ \tx ax ->
              case match (apply sub tx) t2 of
                { Nothing -> undefined  -- Class/instance type conflict.
                ; Just subx -> snd $ prove' ienv (subx @@ sub) (dictVars ps2 0) ax
              }}}}}}}}};

genProduct ds = foldr L (L "*" $ foldl A (V "*") $ map V ds) ds;

inferInst ienv typed inst = fpair inst $ \cl qds -> fpair qds $ \q ds ->
  case q of { Qual ps t -> let { s = showPred $ Pred cl t } in
  (s, (,) (noQual $ TC "DICT") $ maybeFix s $
  foldr (L . snd)
    (foldl A (genProduct $ map fst ds)
       (map (inferMethod ienv typed q) ds))
    (fst $ dictVars ps 0)
  )
  };

reverse = foldl (flip (:)) [];
inferDefs ienv defs typed =
  flst defs (Right $ reverse typed) $ \edef rest ->
    case edef of
    { Left def ->
        fpair def $ \s expr ->
          fpair (infer' typed [] (maybeFix s expr) (Just [], 0)) $ \ta msn ->
            fpair msn $ \ms _ ->
              case fmap (prove ienv ta) ms of
              { Nothing -> Left ("bad type: " ++ s)
              ; Just qa -> inferDefs ienv rest ((s, qa) : typed)}
    ; Right inst -> inferDefs ienv rest (inferInst ienv typed inst : typed)};

conOf con = case con of { Constr s _ -> s };
mkCase t cs =
  ( concatMap (('|' :) . conOf) cs
  , ( noQual $
      arr t $
      foldr
        (arr .
         (\c ->
            case c of
            {  Constr _ ts -> foldr arr (TV "case") ts }))
        (TV "case")
        cs
    , L "x" $ V "x"));

mkStrs = snd . foldl (\p u -> fpair p (\s l -> ('*':s, s : l))) ("*", []);

-- For example, creates `Just = \x a b -> b x`.
-- Scott encoding
scottEncode vs s ts = foldr L (foldl (\a b -> A a (V b)) (V s) ts) (ts ++ vs);
scottConstr t cs c = case c of { Constr s ts -> (s,
  ( noQual $ foldr arr t ts
  , scottEncode (map conOf cs) s $ mkStrs ts)) };
mkAdtDefs t cs = mkCase t cs : map (scottConstr t cs) cs;

fneat neat f = case neat of { Neat a b c -> f a b c };

select f xs acc =
  flst xs (Nothing, acc) $ \x xt ->
    ife (f x) (Just x, xt ++ acc) (select f xt (x : acc));

addInstance s q is = fpair (select ((== s) . fst) is []) $ \m xs -> case m of
  { Nothing  -> (s, [q]):xs
  ; Just sqs -> second (q:) sqs:xs
  };

mkSel ms s = L "*" $ A (V "*") $ foldr (L . ('*' :) . fst) (V $ '*' : s) ms;

untangle = foldr (\top acc -> fneat acc $ \ienv fs typed -> case top of
  { Adt t cs -> Neat ienv fs (mkAdtDefs t cs ++ typed)
  ; Def f -> Neat ienv (Left f : fs) typed
  ; Class classId v ms -> Neat ienv fs (
    map (\st -> fpair st $ \s t -> (s, (Qual [Pred classId v] t, mkSel ms s))) ms
    ++ typed)
  ; Inst cl q ds -> Neat (addInstance cl q ienv) (Right (cl, (q, ds)):fs) typed
  }) (Neat [] [] prims);

infer prog = fneat (untangle prog) inferDefs;

showQual q = case q of { Qual ps t -> concatMap showPred ps ++ show t };

instance Show Qual where { show = showQual };
dumpTypes s =
  fmaybe (parse program s) "parse error" $ \progRest ->
    fpair progRest $ \prog rest ->
      case infer prog of
      { Left err -> err
      ; Right typed ->
          concatMap
            (\p -> fpair p $ \s qa -> s ++ " :: " ++ show (fst qa) ++ "\n")
            typed};

compile s =
  fmaybe (parse program s) "parse error" $ \progRest ->
    fpair progRest $ \prog rest ->
      case infer prog of
      { Left err  -> err
      ; Right qas -> unmaybe (asm $ map (second snd) qas)};
