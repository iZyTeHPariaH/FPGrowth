module TreeBase where

import Control.Monad.Cont
import Data.Tree
import Data.Maybe

data Tree2 a = Tree2 {val :: a, left :: Tree2 a, right :: Tree2 a} | TNode


freetree = Node 2 [Node 1 [Node 4 [], Node 5 []],
                   Node 3 []]
testTr2 = Tree2 2 (Tree2 3 TNode TNode)
                  (Tree2 1 TNode 
                           (Tree2 4 TNode TNode))


{- Recherche d'un élément dans un arbre, optimisée à l'aide de continuations. -}
search         :: (Eq a) => a -> Tree a -> Cont r Bool
search item tr =  callCC $ \k -> searchInTree item tr k k

searchF             :: (Eq a) => a -> Forest a -> Cont r Bool
searchF item forest =  callCC $ \k -> searchInForest item forest k k


{- Recherche d'un élément dans un arbre -}
searchInTree                           :: (Eq a) => a -> Tree a -> (Bool -> Cont b Bool) -> (Bool -> Cont b Bool) -> Cont b Bool 
searchInTree item tree success failure =  if rootLabel tree == item
                                         then success True
                                         else searchInForest item (subForest tree) success (\_ -> failure False)
{- Recherche d'un élément dans une forêt -}
searchInForest                             :: (Eq a) => a -> Forest a -> (Bool -> Cont b Bool) -> (Bool -> Cont b Bool) -> Cont b Bool 
searchInForest item forest success failure =  if null forest
                                             then failure False
                                             else let x = head forest
                                                      xs = tail forest in
                                                  searchInTree item x success (\_ -> searchInForest item xs success failure)
                                              

{---------------------------------------------
 |    Sélection d'items                      |
 ---------------------------------------------}
selectAllItems                          :: (t -> Bool) -> Tree t -> ([t] -> Cont a [t]) -> Cont a [t]
selectAllItems pred tree@(Node i for) k =  if pred i 
                                          then selectAllItemsF pred for (\ret -> k $ i:ret)  
                                          else selectAllItemsF pred for k          
selectAllItemsF pred for k = if null for then k []
                                         else let x = head for
                                                  xs = tail for in
                                              selectAllItems pred x (\ret -> selectAllItemsF pred xs (\t -> k $ t ++ ret))
selectAll pred tr = callCC $ \k -> selectAllItems pred tr k
selectAllF pred tr = callCC $ \k -> selectAllItemsF pred tr k


{- selection des branches satisfaisant le prédicat sur l'item
     Renvoie Nothing si ni l'arbre, ni les sous arbres ne satisfont le prédicat.
     Renvoie (Just tr') avec tr' étant l'arbre tr privé des branches ne satisfaisant pas le prédicat
                        (sauf si l'item de tr lui même satisfait le prédicat) -}
selectTr :: (a -> Bool) -> Tree a -> Cont r (Maybe (Tree a))
selectTr pred tr 
 | pred (rootLabel tr) = return $ Just tr
 | otherwise           = do ret <- selectF pred (subForest tr)                       
                            if null ret then return Nothing
                                        else return $ Just $ tr{subForest=ret}

selectF :: (a -> Bool) -> [Tree a] -> Cont r [Tree a]
selectF _ [] = return []
selectF pred (x:xs) = do mx <- selectTr pred x                            
                         ret <- selectF pred xs
                         if isJust mx then return $ (fromJust mx):ret
                                      else return ret