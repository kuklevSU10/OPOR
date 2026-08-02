@echo off
rem ============================================================
rem  OPOR environment probe. Copy this single file to the other
rem  PC, double-click it. A report OPOR_DIAG_<time>.txt appears
rem  on the Desktop. Send that file back. Does NOT change anything.
rem ============================================================
powershell -NoProfile -ExecutionPolicy Bypass -Command "$s=[IO.File]::ReadAllText('%~f0');$m=$s.IndexOf(('#PS'+'CODE#'));iex $s.Substring($m)"
echo.
pause
exit /b
#PSCODE#
$ErrorActionPreference = 'Continue'
$ts = Get-Date -Format 'yyyyMMdd-HHmmss'
$desktop = [Environment]::GetFolderPath('Desktop')
$report = Join-Path $desktop ("OPOR_DIAG_$ts.txt")
$lines = New-Object System.Collections.Generic.List[string]
function Add-Line($s){ $lines.Add([string]$s) | Out-Null; Write-Host $s }

Add-Line "OPOR environment probe  $ts"
Add-Line "============================================================"
Add-Line ("PowerShell 64-bit process : " + [Environment]::Is64BitProcess)
Add-Line ("OS 64-bit                 : " + [Environment]::Is64BitOperatingSystem)
Add-Line ("User / Computer           : $env:USERNAME / $env:COMPUTERNAME")
Add-Line ""
Add-Line "-- Environment paths --"
Add-Line ("ProgramFiles       = $env:ProgramFiles")
Add-Line ("ProgramW6432       = $env:ProgramW6432")
Add-Line ("ProgramFiles(x86)  = " + ${env:ProgramFiles(x86)})
Add-Line ("ProgramData        = $env:ProgramData")
Add-Line ("APPDATA            = $env:APPDATA")
Add-Line ("LOCALAPPDATA       = $env:LOCALAPPDATA")
Add-Line ""

$roots = [ordered]@{}
$roots['ProgramData']       = (Join-Path $env:ProgramData 'Autodesk\ApplicationPlugins')
if ($env:ProgramW6432)      { $roots['ProgramFiles(x64)'] = (Join-Path $env:ProgramW6432 'Autodesk\ApplicationPlugins') }
if (${env:ProgramFiles(x86)}) { $roots['ProgramFiles(x86)'] = (Join-Path ${env:ProgramFiles(x86)} 'Autodesk\ApplicationPlugins') }
$roots['APPDATA']           = (Join-Path $env:APPDATA 'Autodesk\ApplicationPlugins')

Add-Line "-- ApplicationPlugins roots (where AutoCAD looks) --"
foreach ($k in $roots.Keys) {
  $r = $roots[$k]
  Add-Line ""
  Add-Line ("[$k] $r")
  if (Test-Path -LiteralPath $r) {
    $bundles = Get-ChildItem -LiteralPath $r -Directory -Filter '*.bundle' -ErrorAction SilentlyContinue
    if ($bundles) { foreach ($b in $bundles) { Add-Line ("   bundle: " + $b.Name) } } else { Add-Line "   (no bundles)" }
    $oporDir = Join-Path $r 'OPOR.bundle'
    $oporLoader = Join-Path $oporDir 'Contents\OPOR\opor-loader.lsp'
    if (Test-Path -LiteralPath $oporLoader) { Add-Line "   >>> OPOR.bundle FOUND, loader present: $oporLoader" }
    elseif (Test-Path -LiteralPath $oporDir) { Add-Line "   !!! OPOR.bundle exists but opor-loader.lsp is MISSING" }
  } else { Add-Line "   (folder does not exist)" }
}

Add-Line ""
Add-Line "-- Installed AutoCAD (registry) --"
foreach ($hive in 'HKLM:\SOFTWARE\Autodesk\AutoCAD','HKLM:\SOFTWARE\WOW6432Node\Autodesk\AutoCAD') {
  if (Test-Path $hive) {
    Get-ChildItem $hive -ErrorAction SilentlyContinue | ForEach-Object { Add-Line ("   " + $hive + "\" + $_.PSChildName) }
  }
}

Add-Line ""
Add-Line "-- OPOR install logs (%LOCALAPPDATA%\OPOR\logs) --"
$logDir = Join-Path $env:LOCALAPPDATA 'OPOR\logs'
if (Test-Path -LiteralPath $logDir) {
  $logs = Get-ChildItem -LiteralPath $logDir -File -Filter '*.log' -ErrorAction SilentlyContinue
  if ($logs) {
    foreach ($lg in $logs) { Add-Line ("   file: " + $lg.Name + "  (" + $lg.Length + " bytes)") }
    Add-Line ""
    Add-Line "-- Content of install_*.log --"
    foreach ($lg in ($logs | Where-Object { $_.Name -like 'install_*' })) {
      Add-Line ("---- " + $lg.Name + " ----")
      Get-Content -LiteralPath $lg.FullName -ErrorAction SilentlyContinue | ForEach-Object { Add-Line ("   " + $_) }
    }
  } else { Add-Line "   (no *.log files)" }
} else { Add-Line "   (log folder does not exist - installer never ran under this user)" }

try { Set-Content -LiteralPath $report -Value $lines -Encoding UTF8 } catch { Write-Host ("Could not write report: " + $_.Exception.Message) }
Write-Host ""
Write-Host ("Report saved: " + $report)
try { Start-Process explorer.exe ('/select,"{0}"' -f $report) } catch {}
