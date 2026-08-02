[CmdletBinding()]
param(
    [string]$LogDirectory = "",
    [string]$OutputDirectory = "",
    [switch]$NoOpen
)

$ErrorActionPreference = "Stop"

function Add-OporEnvReport([string]$path) {
    $lines = New-Object System.Collections.Generic.List[string]
    function L($s) { $lines.Add([string]$s) | Out-Null }
    L ("OPOR diagnostics  " + (Get-Date -Format "yyyy-MM-dd HH:mm:ss"))
    L "============================================================"
    L ("PowerShell 64-bit : " + [Environment]::Is64BitProcess)
    L ("OS 64-bit         : " + [Environment]::Is64BitOperatingSystem)
    L ("User / Computer   : $env:USERNAME / $env:COMPUTERNAME")
    L ("ProgramFiles      = $env:ProgramFiles")
    L ("ProgramW6432      = $env:ProgramW6432")
    L ("ProgramFiles(x86) = " + ${env:ProgramFiles(x86)})
    L ("ProgramData       = $env:ProgramData")
    L ("APPDATA           = $env:APPDATA")
    L ("LOCALAPPDATA      = $env:LOCALAPPDATA")
    L ""
    $roots = [ordered]@{}
    $roots['ProgramData'] = (Join-Path $env:ProgramData 'Autodesk\ApplicationPlugins')
    if ($env:ProgramW6432) { $roots['ProgramFiles(x64)'] = (Join-Path $env:ProgramW6432 'Autodesk\ApplicationPlugins') }
    if (${env:ProgramFiles(x86)}) { $roots['ProgramFiles(x86)'] = (Join-Path ${env:ProgramFiles(x86)} 'Autodesk\ApplicationPlugins') }
    $roots['APPDATA'] = (Join-Path $env:APPDATA 'Autodesk\ApplicationPlugins')
    L "-- ApplicationPlugins roots (where AutoCAD looks) --"
    foreach ($k in $roots.Keys) {
        $r = $roots[$k]
        L ("[$k] $r")
        if (Test-Path -LiteralPath $r) {
            $bs = Get-ChildItem -LiteralPath $r -Directory -Filter '*.bundle' -ErrorAction SilentlyContinue
            if ($bs) { foreach ($b in $bs) { L ("   " + $b.Name) } } else { L "   (no bundles)" }
            $ldr = Join-Path $r 'OPOR.bundle\Contents\OPOR\opor-loader.lsp'
            if (Test-Path -LiteralPath $ldr) { L "   >>> OPOR.bundle present, loader OK" }
            elseif (Test-Path -LiteralPath (Join-Path $r 'OPOR.bundle')) { L "   !!! OPOR.bundle present but loader MISSING" }
        }
        else { L "   (folder missing)" }
        L ""
    }
    L "-- Installed AutoCAD (registry) --"
    foreach ($hive in 'HKLM:\SOFTWARE\Autodesk\AutoCAD', 'HKLM:\SOFTWARE\WOW6432Node\Autodesk\AutoCAD') {
        if (Test-Path $hive) {
            Get-ChildItem $hive -ErrorAction SilentlyContinue | ForEach-Object { L ("   " + $_.PSChildName) }
        }
    }
    Set-Content -LiteralPath $path -Value $lines -Encoding UTF8
}

try {
    if ([string]::IsNullOrWhiteSpace($LogDirectory)) {
        $LogDirectory = Join-Path $env:LOCALAPPDATA "OPOR\logs"
    }
    if ([string]::IsNullOrWhiteSpace($OutputDirectory)) {
        $OutputDirectory = [Environment]::GetFolderPath("Desktop")
    }
    New-Item -ItemType Directory -Path $OutputDirectory -Force | Out-Null

    $stamp = Get-Date -Format "yyyyMMdd-HHmmss"
    $staging = Join-Path $env:TEMP ("OPOR_LOGS_stage_" + $PID + "_" + $stamp)
    if (Test-Path -LiteralPath $staging) { Remove-Item -LiteralPath $staging -Recurse -Force }
    New-Item -ItemType Directory -Path $staging -Force | Out-Null

    # Always capture an environment snapshot, so the archive is useful even when
    # no runtime logs exist yet (e.g. the command never loaded).
    Add-OporEnvReport (Join-Path $staging "environment.txt")

    $logCount = 0
    if (Test-Path -LiteralPath $LogDirectory -PathType Container) {
        $logFiles = @(Get-ChildItem -LiteralPath $LogDirectory -File -Filter "*.log" -ErrorAction SilentlyContinue)
        $logCount = $logFiles.Count
        foreach ($lf in $logFiles) {
            Copy-Item -LiteralPath $lf.FullName -Destination (Join-Path $staging $lf.Name) -Force -ErrorAction SilentlyContinue
        }
    }

    $archive = Join-Path $OutputDirectory ("OPOR_LOGS_{0}.zip" -f $stamp)
    if (Test-Path -LiteralPath $archive) { Remove-Item -LiteralPath $archive -Force }
    Compress-Archive -Path (Join-Path $staging "*") -DestinationPath $archive -Force
    Remove-Item -LiteralPath $staging -Recurse -Force -ErrorAction SilentlyContinue

    Write-Host "OPOR diagnostic archive created:"
    Write-Host $archive
    Write-Host ("Log files included: {0}" -f $logCount)
    if ($logCount -eq 0) {
        Write-Host "Note: no OPOR runtime logs were found yet; environment.txt is still included."
    }
    if (-not $NoOpen) {
        Start-Process -FilePath "explorer.exe" -ArgumentList ('/select,"{0}"' -f $archive)
    }
    exit 0
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}
