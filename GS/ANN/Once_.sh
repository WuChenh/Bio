###### single-trait
for c in $(seq 1 11)
do
  for r in $(seq 1 10)
  do
    echo "=============== Single trait, $c th trait, rep $r ================"
    Rscript NN_run_MT_once.R F F 3 $c $r 1
    Rscript NN_run_MT_once.R F T 4 $c $r 1
    Rscript NN_run_MT_once.R T F 5 $c $r 1
    Rscript NN_run_MT_once.R T T 6 $c $r 1
  done
done

###### 11-trait combn

for r in $(seq 1 10)
do
  echo "=============== 11 traits, rep $r ================"
  Rscript NN_run_MT_once.R F F 7 1 $r 11
  Rscript NN_run_MT_once.R F T 8 1 $r 11
  Rscript NN_run_MT_once.R T F 9 1 $r 11
  Rscript NN_run_MT_once.R T T 10 1 $r 11
done

###### 2-trait combn
for c in $(seq 1 55)
do
  for r in $(seq 1 10)
  do
    echo "=============== 2 traits, un wt, no env, combn $c, rep $r ================"
    Rscript NN_run_MT_once.R F F 11 $c $r 2
  done
done

for c in $(seq 1 55)
do
  for r in $(seq 1 10)
  do
    echo "=============== 2 traits, un wt, env, combn $c, rep $r ================"
    Rscript NN_run_MT_once.R F T 12 $c $r 2
  done
done

for c in $(seq 1 55)
do
  for r in $(seq 1 10)
  do
    echo "=============== 2 traits, wt, no env, combn $c, rep $r ================"
    Rscript NN_run_MT_once.R T F 13 $c $r 2
  done
done

for c in $(seq 1 55)
do
  for r in $(seq 1 10)
  do
    echo "=============== 2 traits, wt, env, combn $c, rep $r ================"
    Rscript NN_run_MT_once.R T T 14 $c $r 2
  done
done

###### 3-trait combn
for c in $(seq 1 4)
do
  for r in $(seq 1 10)
  do
    echo "=============== 3 traits, combn $c, rep $r ================"
    Rscript NN_run_MT_once.R F F 15 $c $r 3
    Rscript NN_run_MT_once.R F T 16 $c $r 3
    Rscript NN_run_MT_once.R T F 17 $c $r 3
    Rscript NN_run_MT_once.R T T 18 $c $r 3
  done
done