@echo off
for /L %%C in (1,1,11) do (
for /L %%R in (1,1,10) do (
echo %%C %%R
Rscript.exe NN_run_MT_once.R F F 3 %%C %%R 1
Rscript.exe NN_run_MT_once.R F T 4 %%C %%R 1
Rscript.exe NN_run_MT_once.R T F 5 %%C %%R 1
Rscript.exe NN_run_MT_once.R T T 6 %%C %%R 1
)
)
pause