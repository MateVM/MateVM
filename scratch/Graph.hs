module Graph where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Control.Monad.State

data G a = Node  a (G a) (G a)
         | Leaf  a
         | Nil deriving (Show)

type LoopCheck a = S.Set a


{- Actually i am not sure about the cata and algebra definition.
 -  
 -  ** check: http://dl.acm.org/citation.cfm?id=128035
 -}

-- Represents an acumulator
type TreeAlgebra a r = (a -> r, r -> r -> r)

foldG :: TreeAlgebra a r -> r -> G a -> r
foldG (f,g) s (Leaf val)     = g (f val) s
foldG (f,g) s (Node val l r) = g (f val) $ g (foldG (f,g) s l) (foldG (f,g) s r)
                        
printG = foldG ((: []), (++)) []

loopCheck :: (Ord k) => G a -> (G a -> k) -> State (LoopCheck k) Bool
loopCheck g f = do state <- get
                   return $ S.member (f g) state

addNode :: (Ord k) => k -> State (LoopCheck k) ()
addNode k = do s <- get 
               put $ S.insert k s
               return ()

foldGM :: (Ord k) => TreeAlgebra a r -> (G a -> k) -> r -> G a -> State (LoopCheck k) r
foldGM _     _ s    Nil           = return s
foldGM (f,g) c s k@(Leaf val)     = loopCheck k c >>= \p -> if p then return s else return $ g (f val) s
foldGM (f,g) c s k@(Node val l r) = loopCheck k c >>= \p -> if p then return s else continue
                                    where t        = foldGM (f,g) c s
                                          self     = g (f val)
                                          continue = do addNode $ c k
                                                        left  <- t l
                                                        right <- t r
                                                        return $ self $ g left right

-- Generates a node from a list of children.
packNodes :: [G a] -> a -> G a
packNodes (x:y:[]) e = Node e x y    -- full binary
packNodes (x:[]  ) e = Node e x Nil  -- unary
packNodes []       e = Leaf e        -- leaf

-- Generates a cyclic datastructure given edges and nodes
-- TODO: remove lists with maps in inputs
toG :: (Ord k,Show k) => ([r] -> a -> r) -> [(k,[k])] -> k -> [(k, a)] -> Maybe r
toG g xs start pls = let nodeTable = M.fromList pls
                         payload s = case M.lookup s nodeTable of
                                       Just x  -> x
                                       Nothing -> error "edge with no corresponding node"

                         conn = M.fromList $ map (
                                  \(f,ts) -> let pl t = node f (payload t) t
                                                 succ = map pl ts
                                            in (f, g succ (payload f))
                                 ) xs

                         node f p t = case M.lookup t conn of
                                            (Just x) -> x
                                            Nothing  
                                              -> error $ "illformed edge/node list in toG" ++ (show t)
                   in M.lookup start conn



testG = [(0,[1,2]),(1,[3]),(2,[0]),(3,[])]
nodeLoads :: [(Int,String)]
nodeLoads = [(0,"a"),(1,"b"),(2,"c"),(3,"d")]



