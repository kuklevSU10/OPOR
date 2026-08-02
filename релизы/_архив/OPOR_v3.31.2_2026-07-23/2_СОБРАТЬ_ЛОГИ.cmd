@echo off
setlocal
title OPOR Diagnostic Logs

set "OPOR_PAYLOAD=%~dp0_OPOR_FILES"
if not exist "%OPOR_PAYLOAD%\collect_opor_logs.ps1" (
  echo OPOR service files were not found.
  echo Extract the complete ZIP archive and run this file again.
  echo.
  pause
  exit /b 2
)

powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%OPOR_PAYLOAD%\collect_opor_logs.ps1"
set "OPOR_LOG_RC=%ERRORLEVEL%"

echo.
if "%OPOR_LOG_RC%"=="0" (
  echo The ZIP archive is ready on the Desktop.
  echo Send that ZIP file for diagnostics.
) else (
  echo OPOR logs could not be collected. Error code: %OPOR_LOG_RC%
)
echo.
pause
exit /b %OPOR_LOG_RC%
