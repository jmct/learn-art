data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap f (Store g s) = Store (f . g) s

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  -- pronounced 'extend'
  (=>>) :: w a -> (w a -> b) -> w b

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s
  (Store f s) =>> k = Store f' s
    where f' = k . Store f

-- The initial configuration: True at 0 False everywhere else
initConfig = Store (\c -> c == 0) 0

rule110 :: Num s => Store s Bool -> Bool
rule110 (Store f s) = not (l == c && c == r) || (not l && not c && r)
  where
    (l, c, r) = (f (s - 1), f s, f (s + 1))

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment k (Store f s) = f `fmap` k s

bound :: (Num s, Enum s) => s -> Store s a -> [a]
bound halfwidth = experiment (\s -> [(s - halfwidth) .. (s + halfwidth)])

bitIt :: Bool -> String
bitIt v = if v then "1" else "0"

makeImage :: Int -> [[Bool]] -> String
makeImage n gens =
  unlines ["P1", show 21 ++ " " ++ show n]
  ++ unlines (map (unwords . map bitIt) (take n gens))

main = do
  let gens = map (bound 20) (iterate (=>> rule110) initConfig)
  putStr (makeImage 20 gens)
