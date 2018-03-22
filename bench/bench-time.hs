import Expr

import Control.DeepSeq
import Gauge


config :: Config
config = defaultConfig {displayMode = Condensed}

main :: IO ()
main = defaultMainWith config
  [bgroup "substXIn"
    [bench "substXIn substU tu2"  $ nf (substXIn substU) tu2,
     bench "substXIn substU te2"  $ nf (substXIn substU) te2,
     bench "substXIn substM te2"  $ nf (substXIn substM) te2,

     bench "substXIn substU tu5"  $ nf (substXIn substU) tu5,
     bench "substXIn substU te5"  $ nf (substXIn substU) te5,
     bench "substXIn substM te5"  $ nf (substXIn substM) te5,

     bench "substXIn substU tu10" $ nf (substXIn substU) tu10,
     bench "substXIn substU te10" $ nf (substXIn substU) te10,
     bench "substXIn substM te10" $ nf (substXIn substM) te10],
   bgroup "fv"
    [bench "fvU tu2" $ nf fvU tu2,
     bench "fvU te2" $ nf fvU te2,
     bench "fvM te2" $ nf fvM te2,

     bench "fvU tu5" $ nf fvU tu5,
     bench "fvU te5" $ nf fvU te5,
     bench "fvM te5" $ nf fvM te5,

     bench "fvU tu10" $ nf fvU tu10,
     bench "fvU te10" $ nf fvU te10,
     bench "fvM te10" $ nf fvM te10]]
  where
    tu2, tu5, tu10 :: ExprU
    !tu2  = force $ treeTerm 2
    !tu5  = force $ treeTerm 5
    !tu10 = force $ treeTerm 10

    te2, te5, te10 :: Expr
    !te2  = force $ treeTerm 2
    !te5  = force $ treeTerm 5
    !te10 = force $ treeTerm 10
