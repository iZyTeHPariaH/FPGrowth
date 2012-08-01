
module FPTree (start,fpgrowth,buildDB,FPForest,FPTree,build, mergeF) where

{-| Génération de motifs fréquents par l'algorithme FP-Growth.
      Optimisations mineures : -> Usage de continuations pour le parcours des arbres en profondeur. 
-}

import TreeBase
import Control.Monad.Cont
import Data.Tree
import Data.List hiding (insert)
import Data.Maybe
import Data.Map hiding (map,null,foldl)
import Debug.Trace


{-| Un objet dans l'arbre est associé à son support, c'est à dire le nombre de transactions contenant cet item -}
type Item a = (a,Integer)

{-| Un arbre de préfixes (respectivement, une forêt d'arbres de préfixes) est un arbre (respectivement une forêt) d'items. -}
type FPTree a = Tree (Item a)
type FPForest a = [FPTree a]

{-| Une transaction est une liste d'objets -}
type Transaction a = [a]

{-| Quelques accesseurs utiles -}
itemval = fst.rootLabel
suppval = snd.rootLabel
item = fst
support = snd

  
{-| Des fonctions utiles pour un affichage plus intelligible -}  
dumpF f = unlines $ map (\t -> dumpFPTree t []) f   
dumpFPTree (Node x l) ind = ind ++ show x  ++ "\n" ++ unlines (map (\t -> dumpFPTree t (ind ++ "  ")) l)


{-| Génération d'une forêt (ne contenant qu'un seul arbre) à partir d'une transaction -}
build :: Transaction a -> Cont r (FPForest a)
build [] = return []
build (x:xs) = do 
  ret <- build xs
  return $ [Node (x,1) ret]

{-| Ajoute un arbre dans une forêt -}
merge       :: (Eq a) => FPTree a -> FPForest a -> Cont r (FPForest a)               
merge tr [] =  return [tr]
merge tr (x:xs) 
 | itemval x == itemval tr = let suppx = suppval x
                                 supptr = suppval tr
                                 itemx = itemval x in
                             do ret <- mergeF (subForest x) (subForest tr)
                                return $ Node (itemx, suppx + supptr) ret : xs
 | otherwise               = do ret <- merge tr xs
                                return $ x:ret
                                
{-| Fusionne deux forêts -}
mergeF :: (Eq a) => FPForest a -> FPForest a -> Cont r (FPForest a)                                
mergeF f1 f2 
 | null f1   = return f2
 | null f2   = return f1
 | otherwise = foldM (\a e -> merge e a) f1 f2      



{-| Détermination d'un support dans un arbre -}
getSupp :: (Eq a) => a -> FPTree a -> Cont r Integer
getSupp x tr 
 | itemval tr == x = return $ suppval tr
 | otherwise       =  getSuppF x (subForest tr)

{-| Calcul du support dans une forêt -}
getSuppF           :: (Eq a) => a -> FPForest a -> Cont r Integer
getSuppF _ []      =   return 0
getSuppF x (tr:xs) =   do s1 <- getSupp x tr
                          s2 <- getSuppF x xs
                          return $ s1 + s2
                       
{-| Retourne le couple (mtr, nb) où
       nb représente le nombre total de transactions contenant x
       mtr = Nothing si aucune transaction ne contient x, ou que x se trouve à la racine de l'arbre
             Just t avec t l'arbre initial élagué -}

elaguer      :: (Eq a) => a -> FPTree a -> Cont r (Maybe (FPTree a), Integer)
elaguer x tr 
 | itemval tr == x = return (Nothing,suppval tr)
 | otherwise       = do (retf,nb) <- elaguerF x (subForest tr)
                        if 0 == nb 
                          then return (Nothing, 0)
                          else return $ (Just $ Node ((itemval tr), nb) retf,nb)
   
{-| Retourne le couple (newForest, nb) où newForest est la forêt élaguée à partir de x, et nb le nombre de transactions contenant x. -}
elaguerF :: (Eq a) => a -> FPForest a -> Cont r (FPForest a, Integer)
elaguerF _ [] = return ([],0)
elaguerF x (tr:xs) = do (mret,nb) <- elaguer x tr
                        (rets,nbs) <- elaguerF x xs
                        if isJust mret
                           then return $ ((fromJust mret):rets,nb+nbs)
                           else return $ (rets, nb + nbs)

{-| Génère tous les motifs fréquents de support minsupp dans une forêt -}
fpgrowth (x:xs) for minsupp = do (nF,supp) <- elaguerF x for
                                 if supp < minsupp
                                    then return []
                                    else if null nF then return [(x:xs,supp)]
                                    else do its <- selectAllF (\ _ -> True) nF
                                            ret <- foldM (\a e -> fpgrowth (e:x:xs) nF minsupp >>= \r -> return$ r ++ a ) [] (nub $ map item its)
                                            return $ ((x:xs),supp):ret

{-| Initialise une recherche dans une base de données, en construisant l'arbre et en l'exploitant. -}
start db minsupp = do f <- buildDB db 2
                      its <- selectAllF (\_ -> True) f
                      foldM (fun f) [] (nub $ map item its)
   where fun f acc elem = do ret <- fpgrowth [elem] f minsupp
                             trace ("Found " ++ show (length ret) ++ " patterns for feature " ++ show elem)
                                   (return $ ret ++ acc)
{-| Compte le support des items dans une base de données -}
count db = foldl count' empty db
    where count' acc elem = foldl count'' acc elem
          count'' acc e = if e `member` acc then adjust (+1) e acc
                                            else insert e 1 acc
{-| Compare deux éléments, à partir de leur support stocké dans une hashmap -}                                                
compareSupp db e1 e2 = let supp1 = db ! e1
                           supp2 = db ! e2 in
                       case supp1 `compare` supp2 of
                         LT -> GT
                         GT -> LT
                         EQ -> e1 `compare` e2

{-| Construit un arbre à partir d'une base de données -}
buildDB db minsupp = let suppDB = count db
                         filtredDB = map (Data.List.filter (\e ->suppDB ! e >= minsupp)) db
                     in foldM (\a e -> build e >>= (\e' -> mergeF e' a)) [] (map ( (sortBy (compareSupp suppDB))) filtredDB)

test = do t1 <- build  [1,2,4]
          t2 <- build [1,2,3]
          mergeF t1 t2
          
          


db = ["facdgimp",
      "abcflmo",
      "bfhjo",
      "bcksp",
      "afcelpmn"]