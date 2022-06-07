@echo off

set RS_PATH=%~dp0R-Portable\App\R-Portable\bin\Rscript.exe

if exist %RS_PATH% (
   goto allgood
)
set RS_PATH=
echo Searching for R installation location
where /q Rscript
IF ERRORLEVEL 1 (
    goto notonpath
) ELSE (
    set RS_PATH="Rscript"
    goto allgood
)

:notonpath
rem not on path, see if we can find it in the registry
set R_REGKEY=HKLM\Software\R-core\R
set R_REGVAL=InstallPath

REM Check for presence of registry key
reg query %R_REGKEY% /v %R_REGVAL% 2>nul || (echo Edit your path to include the R bin directory & exit /b 1)
set R_PATH=
for /f "tokens=2,*" %%a in ('reg query %R_REGKEY% /v %R_REGVAL% ^| findstr %R_REGVAL%') do (
    set R_PATH=%%b
)

rem catch failures
if not defined R_PATH (echo Edit your path to include the R-portable bin directory & exit /b 1)

set RS_PATH=%R_PATH%\bin\Rscript.exe

:allgood

set GITHUB_PAT=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
"%RS_PATH%" ov_scouter.R \"%~dp0" %1 %2 %3 %4 %5 %6 %7 %8 %9
pause
