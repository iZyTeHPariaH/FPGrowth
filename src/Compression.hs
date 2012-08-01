import System.Environment
import Control.Parallel.Eden
import Control.Parallel
import Control.Monad.Cont
import Control.Monad
import Control.DeepSeq
import Control.Parallel.Eden.ParPrim hiding (noPe)
import Control.Parallel.Eden.EdenSkel.MapRedSkels
import Control.Parallel.Eden.EdenSkel.MapSkels
import Control.Parallel.Eden.EdenSkel.Auxiliary
import qualified Data.ByteString.Lazy as B

import Data.Binary
import Data.List hiding (insert)
import Data.Map as M hiding (map, foldl, null, filter)
import Data.Tree
import FPTree

--TODO Réaliser un skeletton pour pouvoir transmettre les données sous forme de listes
--TODO etudier dynamic channels


instance (Trans a) => Trans (Tree a) 
                                                

fun2proc f x = process $ \ y -> f x y


wcMap :: [String] -> M.Map String Int
wcMap t = foldl f M.empty t
    where f acc elem = if elem `M.member` acc
                       then M.adjust (+1) elem acc
                       else M.insert elem 1 acc
                            
                            
wcCombine :: [M.Map String Int] -> M.Map String Int
wcCombine lMap = foldl (M.unionWith (+)) M.empty lMap


mergeL l1 l2 = foldl f l1 l2
    where f l1' (n,supp) = let (b,r) = foldl g (False,[]) l1'
                           in if b then r else (n,supp):l1'
              where g (b,l) (i,supp') = if b then (b,(i,supp'):l)
                                        else if i == n then (True, (i,supp+supp'):l)
                                        else (False,(i,supp'):l)



mParMap f input = shuffle $ spawn (repeat proc) (unshuffle noPe input)
    where proc =  fun2proc map f
    
          
mParMap' :: (Trans z, Trans a) => (y -> a -> b) -> ([x] -> y) -> ([b] -> z ) -> [x] -> [a] -> [z]
mParMap' f extractArgs contractArgs rawArgs input =  parMap g (unshuffle noPe input)
    where g = contractArgs . (map (f (extractArgs rawArgs)))

--fixme mettre un rfi
          
mParMapRed :: (Trans a, Trans c) => ([c] -> d) -> ([b] -> c) -> (a -> b) -> [a] -> d
mParMapRed reducer combiner mapper input = reducer $ parMap (combiner . (map mapper)) (unshuffle noPe input)
  




main = do
  [x] <- getArgs
  str <- readFile x
  
  let lignes = lines str
      topk = 50
      minsupp = 2
      suppdb =  lignes `seq` mParMapRed (wcCombine.(map fromList)) (assocs.wcCombine) wcMap (map words lignes)
      suppdb' = assocs suppdb
     -- ans = minsupp `seq` suppdb' `seq` mParMap (filterMap minsupp (fromList suppdb')) lignes
      ans' = minsupp `seq` suppdb' `seq` mParMapRed filterReduce filterCombine (filterMap minsupp (fromList suppdb')) lignes
    --  ans'' = ans' `seq` mParMap (map (\(t,s) -> if s >= minsupp then (take topk) $ runCont (fpgrowth [t] ans' (fromIntegral $ minsupp)) id else [])) (unshuffle noPe suppdb')
      --ans'' = ans' `seq` mParMapRed id (map (filter (not.null))) exploreChunk (unshuffle noPe (assocs suppdb))
      --exploreChunk ch = foldl f [] ch
      --f acc (elem,t) = let ret = if toInteger t < minsupp then [] else take topk (runCont (fpgrowth [elem] ans' minsupp) id)  in (ret `par` acc) `seq` ret:acc
  --    ans'' = ans' `seq` mParMapRed (concatMap concat) id (\(t,s)  -> if s >= 50 then take topk (runCont (fpgrowth [t] ans' 50) id) else []) (assocs suppdb)
--      suppdb = lignes `seq` parMapRedr (unionWith (+)) M.empty wcMap (map words lignes)
--      ans = suppdb `seq` (shuffle $ parMap (map (filterMap 50 empty)) (unshuffle noPe lignes))
  putStrLn $ show suppdb
  putStrLn "Compression..."
  x <- return $! ans'
  putStrLn $ "Terminé. Enregistrement de l'arbre"
  B.writeFile "output.bin" (encode x)
  
--  putStrLn $ show suppdb
--  putStrLn $ show ans
  

filterMap :: Int -> M.Map String Int -> String -> [String]
filterMap minsupp suppdb t = map fst (takeWhile (\tup -> snd tup >= minsupp) sortedT)
    where t' = map (\x -> (x, suppdb ! x)) (words t)
          sortedT = sortBy (\(_,s) (_,s') -> s  `compare` s') t'
          
filterCombine :: [[String]] -> [FPTree String]
filterCombine l = runCont (foldM (\a e -> build e >>= mergeF a) [] l) id

filterReduce :: [[FPTree String]] -> [FPTree String]
filterReduce l = runCont (foldM mergeF [] l) id