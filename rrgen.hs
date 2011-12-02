type Point      = (Float,Float)
type Color      = (Int,Int,Int)
type Polygon    = [Point]

writePoint :: Point -> String
writePoint (x,y) = show x ++ "," ++ show y ++ " "

writePolygon :: Polygon -> String
writePolygon p = "<polygon points=\"" ++ concatMap writePoint p
                ++ "\" style=\"fill:#ffffff;stroke:#000000;stroke-width:1\"/>"

writePolygons :: [Polygon] -> String
writePolygons p = "<svg xmlns=\"http://www.w3.org/2000/svg\">"
                    ++ concatMap writePolygon p
                    ++ "</svg>"

polygon :: Int -> Float -> Polygon
polygon n r = [(x i, y i) | i <- [0..(n-1)]]
        where a = (2*pi) / fromIntegral n
              x i = r * cos (a * fromIntegral i)
              y i = r * sin (a * fromIntegral i)

polygon' :: Int -> Float -> Point -> Polygon
polygon' n r p = translateTo p (polygon n r)

hexagon :: Float -> Polygon
hexagon = polygon 6

hexagon' :: Float -> Point -> Polygon
hexagon' r p = translateTo p (hexagon r)


pi2 = 2 * pi

hexGridPoints :: Float -> Int -> Int -> [Point]
hexGridPoints r n m = let
    yOffset = 2 * r * sin (pi2/6)
    xOffset = r + r * sin (pi2/12)
    y i j
     | odd j        = (yOffset * fromIntegral i) + (yOffset/2)
     | otherwise    = (yOffset * fromIntegral i) + yOffset
    x i j = (xOffset * fromIntegral j) + r
    in [ (x i j, y i j) | i <- [0..(n-1)], j <- [0..(m-1)]]


translateTo :: Point -> Polygon -> Polygon
translateTo (x,y) poly = map f poly where f (a,b) = (a + x, b + y)

offset :: Float -> Polygon -> Polygon
offset r polys = map (oh r) polys where
  oh r pt@(x,y) = (x+(1.5*r),y+(r*sin 1))


main = putStrLn $ writePolygons (map (hexagon' 10) (hexGridPoints 10 38 66))
