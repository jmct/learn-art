-- A single generation of a 1D cellular automaton
data Gen x = Gen [x] x [x]

-- Shift the focus of the generation to the left or right, respectively
left  (Gen (l:ls) x r)      = Gen ls    l (x:r)
right (Gen l      x (r:rs)) = Gen (x:l) r rs

instance Functor Gen where
  fmap f (Gen l x r) = Gen (fmap f l) (f x) (fmap f r)

-- Some people call `Context` `Comonad`, those people are correct.
class Functor w => Context w where
  extract :: w a -> a
  (=>>) :: w a -> (w a -> b) -> w b

instance Context Gen where
  -- Extract something from the context
  extract (Gen _ x _) = x

  -- Take something `k` that works on one part of the context, and apply
  -- it everywhere in the context
  c =>> k = Gen l' x' r'
    where
      x' = k c
      l' = map k (tail (iterate left c))
      r' = map k (tail (iterate right c))

-- The singleton starting generation
initConfig = Gen (repeat False) True (repeat False)

-- rule (Gen (l:_) x (r:_)) = not (l && x && not r || (l==x))
rule110 :: Gen Bool -> Bool
rule110 (Gen (l:_) c (r:_)) = not ((l == c && c == r) || (not l && not c && r))


bound :: Int -> Gen a -> [a]
bound halfwidth (Gen l c r) = reverse (take halfwidth l) ++ c:take halfwidth r

makeImage :: Int -> Int -> [Gen Bool] -> String
makeImage w h gens = unlines ["P1", show (2 * w + 1) ++ " " ++ show h]
            ++ unlines (map (unwords . map bitIt . bound w) (take h gens))

bitIt False = "0"
bitIt True  = "1"

main = do
  let gens = iterate (=>> rule110) (iterate right initConfig !! 99)
  writeFile "rule110.pbm" (makeImage 100 300 gens)
