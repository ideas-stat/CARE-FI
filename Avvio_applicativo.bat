@echo off
setlocal

echo Leggo il percorso di installazione di R dal registro...

REM Estrae il percorso di R
for /f "tokens=2,*" %%a in ('reg query "HKLM\Software\R-core\R" /v "InstallPath" 2^>nul ^| find "InstallPath"') do set RPATH=%%b

if not defined RPATH (
    echo ERRORE: impossibile leggere InstallPath dal registro.
    echo Verifica che R sia installato correttamente.
    pause
    exit /b
)

set "REXEC=%RPATH%\bin\Rscript.exe"

if not exist "%REXEC%" (
    echo ERRORE: Rscript non trovato in "%REXEC%"
    pause
    exit /b
)

REM Determina la directory in cui si trova questo file .bat
set "BATDIR=%~dp0"

REM Path completo allo script R dell’app: qui metti il nome corretto del tuo script
set "APPFILE=%BATDIR%App_new_layout.R"

if not exist "%APPFILE%" (
    echo ERRORE: impossibile trovare lo script dell'app: "%APPFILE%"
    pause
    exit /b
)

echo Avvio dell'applicativo in corso...
"%REXEC%" "%APPFILE%"

echo Operazione completata. Premi un tasto per chiudere...
pause >nul
endlocal