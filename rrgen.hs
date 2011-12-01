type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Polygon   = [Point]

writePoint :: Point -> String 
writePoint (x,y) = (show x)++","++(show y)++" "

writePolygon :: (Color,Polygon) -> String 
writePolygon ((r,g,b),p) = "<polygon points=\""++(concatMap writePoint p)++"\" style=\"fill:#cccccc;stroke:rgb("++(show r)++","++(show g)++","++(show b)++");stroke-width:2\"/>"

writePolygons :: [(Color,Polygon)] -> String 
writePolygons p = "<svg xmlns=\"http://www.w3.org/2000/svg\">"++(concatMap writePolygon p)++"</svg>"

colorize :: Color -> [Polygon] -> [(Color,Polygon)] 
colorize = zip.repeat

rainbow@[red,green,blue,yellow,purple,teal] = map colorize [(255,0,0),(0,255,0),(0,0,255),(255,255,0),(255,0,255),(0,255,255)]

t0 = writeFile "tut0.svg" $ writePolygons (blue [[(100,100),(200,100),(200,200),(100,200)],[(200,200),(300,200),(300,300),(200,300)]])

hexagon c r = translateTo c basicHexagon where
  basicHexagon =  top ++ (negate r, 0):bottom 
  top          =  [(r,0),(r * cos 1,(r * sin 1)),(negate (r * cos 1), r * (sin 1))]
  bottom       =  map (\(x,y)->(x,negate y)) (reverse top)

translateTo (x,y) poly = map f poly where f (a,b)= ((a+x),(b+y))

t1 = writeFile "t1.svg" $ writePolygons (blue [hexagon (100,100) 50] )

hexField r n m = let 
     mkHex n = hexagon (1.5*n*(r*2),(r*2)) r
     row n = map mkHex [1..n]
     aRow = row n
  in concat [map (offset (r*x)) aRow |x<-[1..m]]

offset r polys = map (oh r) polys where
  oh r pt@(x,y) = (x+(1.5*r),y+(r*sin 1))

t2 = writeFile "t2.svg" $ writePolygons (blue $ hexField 50 4 5 )

main = t2
