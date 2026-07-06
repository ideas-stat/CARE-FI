@echo off
setlocal

echo Leggo il percorso di installazione di R dal registro...

REM Estrae solo la parte del path dal registro di sistema
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

echo Installazione pacchetti in corso...
"%REXEC%" -e "user_lib <- file.path(Sys.getenv('USERPROFILE'), 'Rlibs'); if (!dir.exists(user_lib)) dir.create(user_lib, recursive=TRUE); .libPaths(c(user_lib, .libPaths())); cat('Installazione nella libreria utente:', user_lib, '\n'); packages <- c('shiny','shinythemes','shinyWidgets','shinydashboard','shinyalert','shinyFeedback','plotly','ggplot2','dplyr','readr','readxl','DT','parsec','tidyverse','shinycssloaders','htmltools','shinybusy','shinyjs','highcharter','rmarkdown','gridExtra','png'); install_if_missing <- function(pkg) { if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies = TRUE, repos='https://cran.rstudio.com') }; invisible(lapply(packages, install_if_missing))"

echo Operazione completata. Premi un tasto per chiudere...
pause >nul
endlocal