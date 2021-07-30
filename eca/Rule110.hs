-- A single generation of a 1D cellular automaton
data Gen x = Gen [x] x [x]

-- Shift the focus of the generation to the left or right, respectively
left  (Gen (l:ls) x r)      = Gen ls    l (x:r)
right (Gen l      x (r:rs)) = Gen (x:l) r rs

-- Take something `k` that works on one part of the context, and apply
-- it everywhere in the context (this is equivalent to Comonad's 'extend')
(=>>) :: Gen x -> (Gen x -> x) -> Gen x
c =>> k = Gen l' x' r'
  where
    x' = k c
    l' = map k (tail (iterate left c))
    r' = map k (tail (iterate right c))

-- The singleton starting generation
initConfig = Gen (repeat False) True (repeat False)

-- rule 110 of 1D Elementary Cellular Automata
rule110 :: Gen Bool -> Bool
rule110 (Gen (l:_) c (r:_)) = not ((l == c && c == r) || (not l && not c && r))

-- Each `Gen` is infinite in each direction, `bound` takes the same
-- amount from each side
bound :: Int -> Gen a -> [a]
bound halfwidth (Gen l c r) = reverse (take halfwidth l) ++ c:take halfwidth r

-- Generate the string representing the image in PPM format.
-- In this case, each pixel is described by three values between 0 and 255
makePPM :: Int -> Int -> [Gen Bool] -> String
makePPM w h gens = unlines ["P3", show (2 * w + 1) ++ " " ++ show h, show 255]
                ++ unlines (map (unwords . map bitIt . bound w) (take h gens))

bitIt False = "63   63  63" -- A blue that I like
bitIt True  = "127 159 127" -- A magenta that I like

main = do
  -- Shift the focus of the initial Configuration right so that we get
  -- more of the cool parts in our image. Then we iterate on rule110
  let gens = iterate (=>> rule110) (iterate right initConfig !! 99)

  -- Make an image of 300 iterations of Rule 110
  writeFile "rule110.pbm" (makePPM 100 300 gens)
