@echo off
setlocal
for /f "delims=" %%i in ('git sh -c "cygpath -wa ""$(which bash)"""') do set "bash=%%i"
if not defined bash exit /b 127
"%bash%" "%~dp0dlt" %*
exit /b %errorlevel%
