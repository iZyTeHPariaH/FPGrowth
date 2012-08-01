import Control.Parallel.MPI.Simple
import Control.Monad.Cont
import Data.Serialize
import Data.Map hiding (map, foldl)
import Data.List hiding (insert)
import System.Environment (getArgs)
import System.Posix.Unistd
import FPTree
import qualified Data.ByteString.Lazy as B 
import Data.Binary as BIN
import TreeBase
import Debug.Trace

type Pattern  = ([String],Integer)

readDB db = map words (lines db)
         
slog rank log = putStrLn $ "[Slave] " ++ show rank ++ " : " ++ log
mlog log = putStrLn $ "[Master] " ++ log
strace rank log act = trace ("[Slave] " ++ show rank ++ " : " ++ log) act


repartir :: [String] -> Int -> Map Int [String]
repartir liste nbGroupes = let l' = zip liste (concat $ repeat [1..nbGroupes])         
                               f acc (elem,gid) = if gid `member` acc then adjust (elem:) gid acc
                                                                      else insert gid [elem] acc
                           in foldl f empty l'

pfpMaster ::  B.ByteString -> Rank -> Int -> Integer -> IO ()
pfpMaster db rank size minsupp = let --tree = (runCont (buildDB db minsupp) id)
                                     its tree = runCont (selectAllF (\_ -> True ) tree) id
                                     candidats tree= repartir (nub $ map fst (its tree)) ((fromIntegral size) - 1)
                                     waitForMsg ::   [[Pattern]] -> Int -> IO [[Pattern]] 
                                     waitForMsg acc src =  do (msg,stat) <- recv commWorld (fromIntegral src) unitTag
                                                              return $ (msg::[Pattern]):acc
                                                             
                                 in do mlog "Décompression de l'arbre"
                                       
                                       let tree = (BIN.decode db)::(FPForest String)
                                       mlog "Transmission de la forêt de transactions en broadcast..."
                                       bcastSend commWorld 0 (tree,(candidats tree))
                                       mlog "Attente de réponse..."
                                       x <- foldM waitForMsg [] [1..(size - 1)]
                                       let ret = concat x
                                       mlog $ "Fin de la recherche."
                                       putStrLn $ show ret
                                       mlog $ show (length ret) ++ " motifs ont été découverts au total."
                                    

pfpSlave rank size minsupp topk = do 
  slog rank "En attente de la forêt de transaction..."
  (tree,candidats) <- bcastRecv commWorld 0 
  slog rank "Début de l'exploration."
  let ourCandidats = (candidats::Map Int [String]) ! ((fromIntegral rank)::Int)
      miningTree = foldM mining [] ourCandidats
      mining a e = do x <- fpgrowth [e] (tree::FPForest String) minsupp
                      let ret = take topk x
                      strace rank 
                            ("Découverte de " ++ show (length ret) ++ " motifs pour le suffixe " ++ show e) 
                            return (ret ++ a)
  ssend commWorld 0 unitTag (runCont miningTree id)
  slog rank "Terminé"
                                       
                                      

  
main = do
  x <- getArgs
  case x of
   [filename,minsupp,topk] -> do 
     str <- B.readFile filename
     mpiWorld $ \size rank ->
       if size < 2 
        then putStrLn "Au moins 2 processus sont nécessaires."
        else case rank of
          0 -> pfpMaster str rank size (read minsupp)
          _ -> pfpSlave rank size ((read minsupp)::Integer) ((read topk)::Int)
   _ -> print_usage
    
    
print_usage = putStrLn "usage : mpirun -np <nb process> <this exec> <input filename> <minsupp> <topk>"


