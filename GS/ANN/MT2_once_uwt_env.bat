@echo off
for /L %%C in (1,1,55) do (
for /L %%R in (1,1,10) do (
echo %%C %%R
Rscript.exe NN_run_MT_once.R F T 24 %%C %%R
)
)
pause