import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)


-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

pridePalette :: Int -> [(Int,Int,Int)]
pridePalette n = take n $ cycle [(255,0,0),(255,150,0),(255,255,0),(0,255,0),(0,0,255),(255,0,255)]


-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

genStripes :: Int -> [Rect]
genStripes n  = [((0.0, m*(83)), w, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (1500,83)


-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style


svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 


svgEnd :: String
svgEnd = "</svg>"


svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b


svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  writeFile "rects.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs = svgElements svgRect rects (map svgStyle palette)
        rects = genStripes nstripes
        palette = pridePalette nstripes
        nstripes = 6
        (w,h) = (1500,500)
