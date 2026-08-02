@echo off
setlocal
title OPOR Installer

echo OPOR v3.31.2 automatic installer
echo AutoCAD must be closed before installation.
echo.

set "OPOR_PAYLOAD=%~dp0_OPOR_FILES"
if not exist "%OPOR_PAYLOAD%\install_opor.ps1" (
  echo OPOR service files were not found.
  echo Extract the complete ZIP archive and run this file again.
  echo.
  pause
  exit /b 2
)

powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%OPOR_PAYLOAD%\install_opor.ps1" -SourceRoot "%OPOR_PAYLOAD%"
set "OPOR_INSTALL_RC=%ERRORLEVEL%"

echo.
if "%OPOR_INSTALL_RC%"=="0" (
  echo OPOR installation completed successfully.
  echo Start AutoCAD normally, then use command OPOR or XX.
) else (
  echo OPOR installation failed. Error code: %OPOR_INSTALL_RC%
)
echo.
pause
exit /b %OPOR_INSTALL_RC%
