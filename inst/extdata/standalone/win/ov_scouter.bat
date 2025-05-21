@echo off

echo Searching for R installation

set RS_PATH=
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
set R_PATH=
for /f "tokens=2,*" %%a in ('reg query %R_REGKEY% /v %R_REGVAL% 2^>nul') do set R_PATH=%%b
if defined R_PATH (
    set RS_PATH=%R_PATH%\bin\Rscript.exe
    goto allgood
)

rem if R was installed without admin privileges then the registry setting will be under HKCU not HKLM
set R_REGKEY=HKCU\Software\R-core\R
set R_PATH=
for /f "tokens=2,*" %%a in ('reg query %R_REGKEY% /v %R_REGVAL% 2^>nul') do set R_PATH=%%b

rem catch failures
if not defined R_PATH (echo Edit your path to include the R-portable bin directory & goto eof)

set RS_PATH=%R_PATH%\bin\Rscript.exe

:allgood
echo OK
"%RS_PATH%" ov_scouter.R \"%~dp0" %1 %2 %3 %4 %5 %6 %7 %8 %9

:eof
pause
