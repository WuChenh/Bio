@echo off
for /L %%R in (1,1,10) do (
echo %%R
Rscript.exe NN_run_MT_once.R F F 7 1 %%R 11
Rscript.exe NN_run_MT_once.R F T 8 1 %%R 11
Rscript.exe NN_run_MT_once.R T F 9 1 %%R 11
Rscript.exe NN_run_MT_once.R T T 10 1 %%R 11
)
pause