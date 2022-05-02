@echo off
for /L %%C in (30,1,55) do (
for /L %%R in (1,1,10) do (
echo %%C %%R
Rscript.exe NN_run_MT_once.R F F 23 %%C %%R
)
)
pause