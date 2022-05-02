###### combn 2, wt, env
for c in $(seq 1 55)
do
  for r in $(seq 1 10)
  do
    echo "=============== 2 traits, wt, env, combn $c, rep $r ================"
    Rscript NN_run_MT_once.R T T 21 $c $r
  done
done

###### combn 2, wt, no env
for c in $(seq 1 55)
do
  for r in $(seq 1 10)
  do
    echo "=============== 2 traits, wt, no env, combn $c, rep $r ================"
    Rscript NN_run_MT_once.R T F 22 $c $r
  done
done

###### combn 2, no wt, no env
for c in $(seq 1 55)
do
  for r in $(seq 1 10)
  do
    echo "=============== 2 traits, no wt, no env, combn $c, rep $r ================"
    Rscript NN_run_MT_once.R F F 23 $c $r
  done
done

###### combn 2, no wt, env
for c in $(seq 1 55)
do
  for r in $(seq 1 10)
  do
    echo "=============== 2 traits, no wt, env, combn $c, rep $r ================"
    Rscript NN_run_MT_once.R F T 24 $c $r
  done
done
