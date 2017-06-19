
module TLTree where

import Data.List
import System.Process
import Cp
import List 
import Nat  
import Exp
import BTree
import LTree
import Control.Parallel.Strategies
import System.Environment( getArgs )
import Probability
import X3d

            
type Tri = (Point,Side)
type Side = Int
type Point = (Int,Int)



data TLTree a = L a | N (TLTree a,(TLTree a,TLTree a)) deriving (Eq,Show)



--inTLTree :: Either a (TLTree a, (TLTree a, TLTree a)) -> TLTree a
inTLTree = either L N


--outTLTree :: TLTree a -> Either a (TLTree a, (TLTree a, TLTree a))
outTLTree (L a) = i1 a
outTLTree (N (tl1,(tl2,tl3))) = i2 (tl1,(tl2,tl3))



--baseTLTree :: (a -> b) -> (c -> d) -> Either a (c, (c, c)) -> Either b (d, (d, d))
baseTLTree g f = g -|- (f >< (f >< f))


--recTLTree :: (c -> d) -> Either b (c,(c,c)) -> Either b (d,(d,d))
recTLTree f = id -|- (f >< (f >< f))
-- Adaptação da função recLTree


--cataTLTree :: (Either b (d,(d,d)) -> d) -> TLTree b -> d
cataTLTree a = a . (recTLTree (cataTLTree a)) . outTLTree
-- Adaptação da função cataLTree


--anaTLTree :: (c -> Either a (c,(c,c))) -> c -> TLTree a
anaTLTree f = inTLTree . (recTLTree (anaTLTree f) ) . f
-- Adaptação da função anaLTree

--hyloTLTree :: (Either b (c,(c,c)) -> c) -> (a -> Either b (a,(a,a))) -> a -> c
hyloTLTree a c = cataTLTree a . anaTLTree c
-- Adaptação da função hyloLTree


--tipsTLTree :: TLTree b -> [b]
tipsTLTree = cataTLTree (either singl conc)
           where conc(l,(r1,r2)) = l ++ r1 ++ r2
-- Adaptação da função tips da biblioteca LTree          


--invTLTree :: TLTree (a,b) -> TLTree (b,a)
invTLTree = cataTLTree (inTLTree . (swap -|- id))
-- Adaptação da função invLTree



--depthTLTree :: TLTree b -> Integer
depthTLTree = cataTLTree (either one (succ.myMax))
			where myMax (a,(b,c)) = max a (max b c)
-- Adaptação da função depth definida na Parte A do prabalho



--countTLTree :: TLTree b -> Integer
countTLTree = fromInteger.cataTLTree (either one add')
			--where add' (x,(y,z))= x+y+z
              where  add' = add.(id >< add)
-- Adaptação da função countLTree 



--geraSierp :: Tri -> Int -> TLTree Tri
geraSierp t x = anaTLTree tlsplit (t,x)


--tlsplit :: (Eq t, Integral t1, Num t)    => (((t1, t1), t1), t) -> Either ((t1, t1), t1) ((((t1, t1), t1), t), ((((t1, t1), t1), t), (((t1, t1), t1), t)))
tlsplit (((x,y),s),n) | n==0      = i1 ((x,y),s)
                      | otherwise = i2 ((((x,y),div s 2),n-1),((((x,y+(div s 2)),div s 2),n-1),(((x+(div s 2),y),div s 2),n-1)))



----------------------------------------
-- Desenhar o triangulo de Sierpinski --
----------------------------------------

draw = render html where
       html = rep dados

dados = (((0,0), 32),2)

render html = do { writeFile "_.html" html ; system "open _.html" }

rep x =  finalize $ concatMap drawTriangle $ tipsTLTree $ muda geraSierp x
	 where muda f (((x,y),s),n) = f ((x,y),s) n

{-
muda geraSierp -> altera os tipos do argumentos, pois a função geraSierp recebe ((x,y),s) n e não (((x,y),s),n)
tipsTLTree $ -> recebe a arvore que resulta do passo anterior e poe todos os elementos numa lista ordenada
map drawTriangle $ -> aplica a função drawTriangle a todos os elemtos da lista de triangulos que resulta da tipsTLTree
concat $ -> junta todas as String que existem na lista anterior, numa só.
finalize $ -> escreve o codigo html para desenhar os triangulos num ficheiro ".html"
-}

----------------------------------------
















