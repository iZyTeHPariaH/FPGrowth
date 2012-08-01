
module EdenMapReduce 
where
import System.Environment
import Control.Parallel.Eden
import Control.Parallel.Eden.EdenSkel.Auxiliary
import Control.Parallel.Eden.EdenSkel.MapSkels
import Data.List


-- Un enregistrement de base est un couple (Clef, Valeur)
data MRTuple a b = MRTuple {key :: a, value:: b} deriving (Eq,Show)

type Mapper a k v = a -> [MRTuple k v]
type Reducer k1 v1 k2 v2 = (k1,[v1]) -> [MRTuple k2 v2]
type Combiner k1 v1 k2 v2 = Reducer k1 v1 k2 v2

instance (Ord a,Eq b) => Ord (MRTuple a b) where
  (MRTuple x y) `compare` (MRTuple x' y') = x `compare` x'
  
instance (NFData a, NFData b) => NFData (MRTuple a b) where
  rnf (MRTuple x y) = rnf x `seq` rnf y
  
instance (Trans a, Trans b) => Trans (MRTuple a b)


compareKey (MRTuple k1 v1) (MRTuple k2 v2) = k1 == k2

-- Regroupe les items par clef dans un chunk
groupInChunk :: (Eq k) => [MRTuple k v] -> [(k,[v])]
groupInChunk ch = map (\l -> (key $ head l, map value l)) (groupBy compareKey ch)

mergeOutput [] l = l
mergeOutput l [] = l
mergeOutput (x:xs) (y:ys)
 | x < y = x : mergeOutput xs (y:ys)
 | x >= y = y : mergeOutput (x:xs) ys

sortInChunk ch = sort ch
sortAndGroup ch = groupInChunk.sortInChunk $ ch

{- Fonction appellée par le master sur les processus distants.
           -> Les enregistrements sont regroupes en n chunks, qui seront traités indépendamments par un processus
           -> Pour chaque chunk :
                                   + La fonction mf est appellée sur chaque enregistrement
                                   + Ses résultats sont collectés dans un spill de taille nspill
                                   + Quand un spill est rempli, il est trié et traité par le combiner
                                   + Les resultats du combiner sont fusionnés (LES SORTIES DU COMBINER NE DOIVENT PAS PERTURBER L'ORDRE)
-}
mapper :: (Eq v1, Ord k1, Eq v2, Ord k2) => Int -> -- Nombre de spill (avant d'appeler le combiner)
                                            Mapper a k1 v1 ->  -- Mappeur
                                            Combiner k1 v1 k2 v2 ->  -- Combiner
                                            [a] ->  -- Liste d'entrée
                                            [MRTuple k2 v2] -- Sortie
mapper nchunks mf mc input = combineChunks
    where -- On calcule les résultats du map sur le chunk
          tuples = concatMap mf input
          
          -- On groupe les sorties du mappeur en spill
          mrChunks = chunk nchunks tuples
          
          -- On trie chaque spill (l'évaluation paresseuse fait que le tri sera fait dès que le spill sera plein)
          sortChunks = map sortAndGroup mrChunks
          
          -- On applique le combiner sur chaque spill (conserve la relation d'ordre) et on
          -- fusionne les resultats.
          combineChunks = foldl mergeOutput [] (map (concatMap mc) sortChunks)
          
 
reducer ::(Eq k1) => Reducer k1 v1 k2 v2 -> [(k1 , [v1])] -> [MRTuple k2 v2]
reducer mr input = concatMap mr input


master :: (Ord k1, Eq v1, Ord k2, Eq v2, 
           Trans a, Trans k1, Trans k2 , 
           Trans v1, Trans v2, Trans k3, 
           Trans v3) =>   Mapper a k1 v1 ->    -- Mappeur
                          Combiner k1 v1 k2 v2 -> -- Combiner
                          Reducer k2 v2 k3 v3 -> -- Reducer
                          [a] -> -- Entrées
                          Int -> -- Taille d'un spill
                          Int ->
                          [MRTuple k3 v3]
                          
master mf mc mr input nspill ndiv = shuffle $ parMap (reducer mr) reduceInput
    where chunkedInput = unshuffle ndiv input
          mergedMapOutput = foldl mergeOutput [] (parMap (mapper nspill mf mc) chunkedInput)
          reduceInput = unshuffle ndiv (groupInChunk mergedMapOutput)



