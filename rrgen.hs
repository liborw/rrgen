type Point      = (Float,Float)
type Color      = (Int,Int,Int)
type Polygon    = [Point]

writePoint :: Point -> String
writePoint (x,y) = show x ++ "," ++ show y ++ " "

writePolygon :: Polygon -> String
writePolygon p = "<polygon points=\"" ++ concatMap writePoint p
                ++ "\" style=\"fill:#ffffff;stroke:#000000;stroke-width:2\"/>"

writePolygons :: [Polygon] -> String
writePolygons p = "<svg xmlns=\"http://www.w3.org/2000/svg\">"
                    ++ concatMap writePolygon p
                    ++ "</svg>"

polygon :: Int -> Float -> Polygon
polygon n r = [(x i, y i)| i <- [0..(n-1)]]
        where a = (2*pi) / fromIntegral n
              x i = r * cos (a * fromIntegral i)
              y i = r * sin (a * fromIntegral i)

hexagon :: Float -> Polygon
hexagon = polygon 6

translateTo :: Point -> Polygon -> Polygon
translateTo (x,y) poly = map f poly where f (a,b) = (a + x, b + y)

-- hexField :: Float -> Int -> Int -> [Polygon]
-- hexField r n m = let
--     mkHex n = hexagon (1.5*(fromIntegral n)*(r*2),(r*2)) r
--     row n = map mkHex [1..n]
--     aRow = row n
--  in concat [map (offset (r*(fromIntegral x))) aRow |x<-[1..m]]

offset :: Float -> Polygon -> Polygon
offset r polys = map (oh r) polys where
  oh r pt@(x,y) = (x+(1.5*r),y+(r*sin 1))


main = putStrLn $ writePolygons [translateTo (100, 100) $ polygon 6 50]
