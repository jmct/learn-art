height = 1080
width  = 900

type Pixel = Bool

-- | A location is the coordinate on our grid
type Loc = (Int, Int)

-- | An image is a mapping from location to Pixel Value
type Image = [[Pixel]]

-- | Complex numbers have their real and imaginary parts
data Complex = Double :+ Double
  deriving (Show)

-- We do not need all possible operations over complex numbers, only
-- addC (x + y), sqrC (x^2), and absC, |x|
addC :: Complex -> Complex -> Complex
addC (x :+ y) (u :+ v) = (x+u) :+ (y+v)

sqrC :: Complex -> Complex
sqrC (x :+ y) = (x*x - y*y) :+ (2*x*y)

absC :: Complex -> Double 
absC (x :+ y) = sqrt (x*x + y*y)

-- In order to determine whether a point (z) lies within the Julia set, we
-- repeatedly perform (z' = sqrt(z) + c) until we either run out of iterations,
-- in which case it's in the Julia set, or it's gotten too big (not in the set)
transform :: Int -> Complex -> Complex -> Pixel
transform i c z
  | absC z > 10000 = False                   -- Too big
  | i == 1         = True                    -- max iterations
  | otherwise      = transform (i - 1) c z'
 where
  z' = (sqrC z) `addC` c

-- Generate the list of lists representing each horizontal line of the final
-- image. We call `transform` for each generated point
pixels :: Double -> [[Pixel]]
pixels ci = map mkRow [0..height - 1]
  where
    isMember c = transform 50 ((-0.8) :+ ci) c
    mkRow y    = [ isMember ((x / width) :+ (y / width)) | x <- [0..width - 1]]

makePPM ci = unlines ["P3", show w ++ " " ++ show h, "255"]
          ++ unlines (map (unwords . map colorPixel) (pixels ci))
  where
    h = floor height
    w = floor width

colorPixel False = "251 246 227" -- The background color (off-white)
colorPixel True  = "211  54 130" -- The foreground color (magenta-like)

main = writeFile "julia.ppm" (makePPM 0.151)
