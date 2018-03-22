{-# LANGUAGE RankNTypes, TypeFamilies #-}

import Expr

import Control.DeepSeq
import Weigh


doTrees :: NFData r
        => String
        -> (forall a. (NFData a, IsExpr     a) => a -> r)
        -> (forall a. (NFData a, IsMemoExpr a) => a -> r)
        -> Weigh ()
doTrees name fU fM = mapM_ step
    [("5",  tu5,  te5),  ("10", tu10, te10),
     ("15", tu15, te15), ("20", tu20, te20)]
  where
    step (n, tu, te) = do
      func (name ++ "U tu" ++ n) fU tu
      func (name ++ "U te" ++ n) fU te
      func (name ++ "M te" ++ n) fM te

main :: IO ()
main =
  deepseq (tu5, tu10, tu15, tu20, te5, te10, te15, te20) $
  mainWith $ do
    setColumns [Case, Allocated, Live, Max, GCs]
    doTrees "substXIn subst"
      (rnf . substXIn substU) (rnf . substXIn substM)
        -- the rnfs are to make the return types (), because otherwise
        -- the rank 2 vars in doTrees's type escape their scopes
    doTrees "fv" fvU fvM

tu5, tu10, tu15, tu20 :: ExprU
tu5  = force $ treeTerm 5
tu10 = force $ treeTerm 10
tu15 = force $ treeTerm 15
tu20 = force $ treeTerm 20

te5, te10, te15, te20 :: Expr
te5  = force $ treeTerm 5
te10 = force $ treeTerm 10
te15 = force $ treeTerm 15
te20 = force $ treeTerm 20
