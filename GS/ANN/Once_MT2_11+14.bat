@echo off
for /L %%C in (1,1,55) do (
for /L %%R in (1,1,10) do (
echo %%C %%R
Rscript.exe NN_run_MT_once.R F F 11 %%C %%R 2
)
)
for /L %%C in (1,1,55) do (
for /L %%R in (1,1,10) do (
echo %%C %%R
Rscript.exe NN_run_MT_once.R F T 12 %%C %%R 2
)
)
for /L %%C in (1,1,55) do (
for /L %%R in (1,1,10) do (
echo %%C %%R
Rscript.exe NN_run_MT_once.R T F 13 %%C %%R 2
)
)
for /L %%C in (1,1,55) do (
for /L %%R in (1,1,10) do (
echo %%C %%R
Rscript.exe NN_run_MT_once.R T T 14 %%C %%R 2
)
)
pause