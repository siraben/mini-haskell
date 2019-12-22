infixr 5 :, ++;
infixr 9 .;
infixl 4 <*> , <$> , <* , *>;
infixl 3 <|>, <||>;
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
data Either a b = Left a | Right b;
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
(<) a b = not (a == b) && (a <= b);
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
-- fold a maybe
-- fmaybe :: Maybe a -> b -> (a -> b) -> b
fmaybe m n j = case m of { Nothing -> n; Just x -> j x };
instance Show a => Show (Maybe a) where
  { show = maybe "Nothing" (\x -> "Just " ++ show x) };
instance Functor Maybe where
  { fmap f = maybe Nothing (Just . f) };
instance Applicative Maybe where
  { pure = Just ; (<*>) f y = maybe Nothing (`fmap` y) f};
instance Monad Maybe where
  { return = Just ; (>>=) ma f = maybe Nothing f ma };
fromMaybe a m = fmaybe m a id;
unmaybe = fromMaybe undefined;
foldr c n l = flst l n (\h t -> c h (foldr c n t));
-- foldr1' :: (a -> a -> a) -> [a] -> Maybe a
foldr1' c l =
    flst
       l
       Nothing
       (\h t ->
          foldr
            (\x m -> Just (fmaybe m x (c x)))
            Nothing
            l);

foldl f a bs = foldr (\b g x -> g (f x b)) id bs a;
-- foldl1' :: (p -> p -> p) -> [p] -> Maybe p
foldl1' f l = flst l Nothing (\x xs -> Just (foldl f x xs));
scanl f q ls = q : (case ls of
                       { []       -> []
                       ; (:) x xs -> scanl f (f q x) xs});

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
prependToAll s l = flst l [] (\x xs -> s : x : prependToAll s xs);
intersperse s l = flst l [] (\x xs -> x : prependToAll s xs);

-- Show a non-empty list
intercalate d = concat . intersperse d;
unwords = intercalate " ";
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

ifz n = ife (0 == n);
showInt' n = ifz n id (showInt' (n/10) . (:) (chr (48+(n%10))));
showInt n = ifz n ('0':) (showInt' n);

-- N.B. using show on Ints will make GHC fail to compile to due GHC
-- having multiple numeric types.
instance Show Int where { show n = showInt n "" };

instance Show String where { show = showString };
instance Show a => Show [a] where { show = showList };

any f = foldr (\x t -> ife (f x) True t) False;
lookupWith eq s =
  foldr (\h t -> fpair h (\k v -> ife (eq s k) (Just v) t)) Nothing;

lstLookup = lookupWith (==);
(!!) l i = case l of {
             []       -> undefined;
             (:) x xs -> ifz i x (xs !! (i - 1))
};
tail l = case l of {
             []       -> undefined;
             (:) x xs -> xs
         };
reverse = foldl (flip (:)) [];
zipWith f xs ys = case xs of
  { [] -> []
  ; (:) x xt -> case ys of
    { []       -> []
    ; (:) y yt -> f x y : zipWith f xt yt
    }
  };
zip = zipWith (,);
data Nat = Nat [Int];
unwrap x = case x of { Nat l -> l };

dropWhile p l = flst l [] (\x xs -> ife (p x) (dropWhile p xs) l);
replicate x n = ifz n [] (x:(replicate x (n - 1)));
dropWhileEnd p = foldr (\x xs -> ife ((p x) && (xs == [])) [] (x : xs)) [];

-- Since cell sizes are 2^32, we can use C's multiplication to
-- multiply numbers up to 2^16 without overflow.  Since we want an
-- easy way to print out the number, the base we choose must be a
-- power of 10.
-- baseSize = floor(log(2^16))
baseSize = 4;
-- base = 10^floor(log(2^16))
base = 10000;

showNatDigit' i r = ifz i (replicate '0' r)
                          (chr ((i%10) + ord '0')
                           : (showNatDigit' (i/10) (r - 1)));
showNatDigit i = showNatDigit' i baseSize;
showNat l = case unwrap l of
              { [] -> "0"
              ; (:) _ _ ->
                 (dropWhile (== '0')
                  (concatMap (reverse . showNatDigit)
                    (reverse (unwrap l)))) };
instance Show Nat where { show = showNat };

-- Will the addition of numbers a and b lead to an overflow?
willOverflow a b = (base - 1) < (a + b);
-- Increment a Nat list.
inc' l = case l of { [] -> [1]
                   ; (:) x xs ->
                       ife (x == (base - 1))
                            (0 : inc' xs)
                            (x + 1 : xs)};
add' a b = case a of
             { [] -> b
             ; (:) x xs ->
               case b of
                 { [] -> a
                 ; (:) y ys ->
                   ((x + y)%base):((ife (willOverflow x y) inc' id) (add' xs ys))
                 }};
add a b = Nat (add' (unwrap a) (unwrap b));
-- Since for any n > 0, Nat (replicate n 0) represents 0, we must
-- check for this.
allZero l = case l of { [] -> True ; (:) x xs -> (x == 0) && allZero xs };
isZero n = case unwrap n of { [] -> True ; (:) x xs -> allZero (x:xs) };

pow' i n = ifz i n (pow' (i - 1) (0:n));
-- Shift a Nat by the base.
shift i n = Nat (ifz i [] (pow' i (unwrap n)));
fromInt' x = ifz x [] ((x%base):fromInt' (x/base));

fromInt = Nat . fromInt';

length l = flst l 0 (\_ xs -> 1 + length xs);
numDigits = length . unwrap;
splitAt' m l = case l of { [] -> ([], [])
                         ; (:) x xs ->
                           ife (m == 1)
                               ([x], xs)
                               (fpair (splitAt' (m - 1) xs)
                                   (\xs' xs'' -> (x:xs', xs''))) };
splitAt n ls = ifz n ([], ls) (splitAt' n ls);
splitDigits ab n = fpair (splitAt n (unwrap ab)) (\x y -> (Nat x, Nat y));

dec' l = case l of { [] -> []
                    ; (:) x xs ->
                        ifz x
                            (base - 1 : dec' xs)
                            (x - 1 : xs)};
dec n = Nat (dec' (unwrap n));
-- if a < b, then sub returns 0
-- Written in CPS so we can return 0 immediately when a is 0 but b is
-- not.
sub' a b k = case a of
               { [] ->
                   case b of
                     { []      -> k []
                     -- Even if b is represented as Nat [0,0,0], 0 - 0 == 0.
                     ; (:) _ _ -> [] }
               ; (:) x xs ->
                   case b of
                     { [] -> k a
                     ; (:) y ys ->
                         ife (x < y)
                         (sub' (dec' xs) ys (\r -> k (x + base - y : r)))
                         (sub' xs ys (\r -> k (x - y : r)))}};
sub x y = Nat $ sub' (unwrap x) (unwrap y) (dropWhileEnd (== 0));
max a b = ife (a < b) b a;
mul x y = case unwrap x of
            { [] -> Nat [] -- 0 * y = 0
            ; (:) a as ->
              case unwrap y of
                { [] -> Nat [] -- x * 0 = 0
                ; (:) b bs ->
                    ife ((as == []) && (bs == []))
                    (fromInt (a * b))
                    (let { digits = max (numDigits x) (numDigits y)
                         ; ba = splitDigits x (digits / 2)
                         ; dc = splitDigits y (digits / 2)
                         ; a = snd ba ; b = fst ba
                         ; c = snd dc ; d = fst dc
                         ; z0 = mul b d
                         ; z1 = mul (add a b) (add c d)
                         ; z2 = mul a c
                         }
                      in
                        add (shift ((digits / 2) * 2) z2)
                        (add z0 (shift (digits / 2) (sub (sub z1 z2) z0))))}};
fibs = Nat [] : Nat [1] : zipWith add fibs (tail fibs);
numsFrom n = n : numsFrom (add (Nat [1]) n);
intsFrom n = n : intsFrom (succ n);
facs = scanl mul (Nat [1]) (numsFrom (Nat [1]));
take n l = ifz n [] (case l of { [] -> [] ; (:) x xs -> x : take (n - 1) xs });
main s = concat ["The 3000th Fibonacci number is\n", show (fibs !! 3000),
                 "\n70000^2 = ", show (mul (fromInt 70000) (fromInt 70000)),
                 "\n300! is\n", show (facs !! 300)];
