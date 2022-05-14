@echo off
for /L %%C in (1,1,4) do (
for /L %%R in (1,1,10) do (
echo %%C %%R
Rscript.exe NN_run_MT_once.R F F 15 %%C %%R 3
)
)

for /L %%C in (1,1,4) do (
for /L %%R in (1,1,10) do (
echo %%C %%R
Rscript.exe NN_run_MT_once.R F T 16 %%C %%R 3
)
)

for /L %%C in (1,1,4) do (
for /L %%R in (1,1,10) do (
echo %%C %%R
Rscript.exe NN_run_MT_once.R T F 17 %%C %%R 3
)
)

for /L %%C in (1,1,4) do (
for /L %%R in (1,1,10) do (
echo %%C %%R
Rscript.exe NN_run_MT_once.R T T 18 %%C %%R 3
)
)
pause