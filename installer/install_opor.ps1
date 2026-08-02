[CmdletBinding()]
param(
    [string]$SourceRoot = (Split-Path -Parent $PSCommandPath),
    [string]$DestinationRoot = "",
    [string]$LogFile = "",
    [switch]$NoElevation,
    [switch]$AllowAutoCADRunning
)

$ErrorActionPreference = "Stop"
$script:OporInstallLog = $null

function Initialize-OporInstallLog {
    try {
        if (-not [string]::IsNullOrWhiteSpace($LogFile)) {
            $script:OporInstallLog = [IO.Path]::GetFullPath($LogFile)
            $logDirectory = Split-Path -Parent $script:OporInstallLog
        }
        else {
            $logDirectory = if (-not [string]::IsNullOrWhiteSpace($env:OPOR_LOG_ROOT)) {
                [IO.Path]::GetFullPath($env:OPOR_LOG_ROOT)
            }
            else {
                Join-Path $env:LOCALAPPDATA "OPOR\logs"
            }
            $logName = "install_{0}_{1}.log" -f (Get-Date -Format "yyyyMMdd-HHmmss"), $PID
            $script:OporInstallLog = Join-Path $logDirectory $logName
        }
        New-Item -ItemType Directory -Path $logDirectory -Force | Out-Null
    }
    catch {
        $script:OporInstallLog = $null
    }
}

function Write-OporInstallLog([string]$Message) {
    if ([string]::IsNullOrWhiteSpace($script:OporInstallLog)) {
        return
    }
    try {
        $line = "{0} | {1}" -f (Get-Date -Format "yyyy-MM-dd HH:mm:ss.fff"), $Message
        Add-Content -LiteralPath $script:OporInstallLog -Value $line -Encoding UTF8
    }
    catch {
        # Logging must never prevent installation.
    }
}

Initialize-OporInstallLog
Write-OporInstallLog "START | PID=$PID | SourceRoot=$SourceRoot | DestinationRoot=$DestinationRoot"

function Test-OporAdministrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($identity)
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

function Get-OporCleanupRoots([bool]$adminScope) {
    # Autodesk\ApplicationPlugins roots that may still hold an OPOR.bundle.
    # A per-user run can only touch APPDATA; an elevated run can also clean the
    # machine-wide roots (this is what removes the old mis-placed x86 copy).
    $result = New-Object System.Collections.Generic.List[string]
    if ($env:APPDATA) { $result.Add((Join-Path $env:APPDATA "Autodesk\ApplicationPlugins")) }
    if ($adminScope) {
        if ($env:ProgramData) { $result.Add((Join-Path $env:ProgramData "Autodesk\ApplicationPlugins")) }
        if ($env:ProgramW6432) { $result.Add((Join-Path $env:ProgramW6432 "Autodesk\ApplicationPlugins")) }
        if ($env:ProgramFiles) { $result.Add((Join-Path $env:ProgramFiles "Autodesk\ApplicationPlugins")) }
        $pfx86 = ${env:ProgramFiles(x86)}
        if ($pfx86) { $result.Add((Join-Path $pfx86 "Autodesk\ApplicationPlugins")) }
    }
    return $result
}

try {
    $SourceRoot = [IO.Path]::GetFullPath($SourceRoot)
    $autoScope = [string]::IsNullOrWhiteSpace($DestinationRoot)
    $isAdmin = Test-OporAdministrator
    Write-OporInstallLog "CONTEXT | AutoScope=$autoScope | Administrator=$isAdmin"

    if (-not $AllowAutoCADRunning -and (Get-Process -Name acad -ErrorAction SilentlyContinue)) {
        Write-OporInstallLog "BLOCKED | AutoCAD process is running"
        throw "Close all AutoCAD windows and run the OPOR installer again."
    }

    # Choose a destination AutoCAD actually scans. Both are architecture-neutral,
    # so there is no Program Files vs Program Files (x86) ambiguity.
    #   - not elevated -> per-user %APPDATA% (no admin rights required)
    #   - elevated     -> all-users %PROGRAMDATA% (covers every Windows user)
    if ($autoScope) {
        if ($isAdmin) {
            if ([string]::IsNullOrWhiteSpace($env:ProgramData)) {
                throw "ProgramData environment variable is empty; cannot perform all-users install."
            }
            $DestinationRoot = Join-Path $env:ProgramData "Autodesk\ApplicationPlugins"
            Write-OporInstallLog "SCOPE | all-users -> $DestinationRoot"
        }
        else {
            if ([string]::IsNullOrWhiteSpace($env:APPDATA)) {
                throw "APPDATA environment variable is empty; cannot perform per-user install."
            }
            $DestinationRoot = Join-Path $env:APPDATA "Autodesk\ApplicationPlugins"
            Write-OporInstallLog "SCOPE | per-user -> $DestinationRoot"
        }
    }

    $DestinationRoot = [IO.Path]::GetFullPath($DestinationRoot)
    Write-OporInstallLog "DESTINATION | $DestinationRoot"
    $sourceOpor = Join-Path $SourceRoot "OPOR"
    $sourceBootstrap = Join-Path $SourceRoot "autoload\OPOR_bootstrap.lsp"
    $sourceManifest = Join-Path $SourceRoot "autoload\PackageContents.xml"

    foreach ($requiredFile in @(
        (Join-Path $sourceOpor "opor-loader.lsp"),
        (Join-Path $sourceOpor "opor.dcl"),
        (Join-Path $sourceOpor "opor-table-blocks.dwg"),
        $sourceBootstrap,
        $sourceManifest
    )) {
        if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
            throw "Required installation file not found: $requiredFile"
        }
    }

    [xml]$manifest = Get-Content -LiteralPath $sourceManifest -Raw
    if ($manifest.ApplicationPackage.Name -ne "OPOR") {
        throw "Invalid OPOR PackageContents.xml."
    }
    Write-OporInstallLog "SOURCE VALIDATED | AppVersion=$($manifest.ApplicationPackage.AppVersion)"

    New-Item -ItemType Directory -Path $DestinationRoot -Force | Out-Null

    $destinationBundle = Join-Path $DestinationRoot "OPOR.bundle"
    $stamp = Get-Date -Format "yyyyMMdd-HHmmss"
    $stagingBundle = Join-Path $DestinationRoot ("OPOR.bundle.installing-" + $PID)

    # Every root we should migrate an existing OPOR.bundle out of, so exactly one
    # live copy remains. The destination is always included; the rest depend on
    # whether we are elevated.
    $cleanupRoots = New-Object System.Collections.Generic.List[string]
    $cleanupRoots.Add($DestinationRoot)
    if ($autoScope) {
        foreach ($r in (Get-OporCleanupRoots $isAdmin)) { $cleanupRoots.Add($r) }
    }
    $cleanupRoots = @(
        $cleanupRoots |
        Where-Object { -not [string]::IsNullOrWhiteSpace($_) } |
        ForEach-Object { [IO.Path]::GetFullPath($_) } |
        Sort-Object -Unique
    )
    Write-OporInstallLog ("CLEANUP ROOTS | " + ($cleanupRoots -join " ; "))

    if (Test-Path -LiteralPath $stagingBundle) {
        Remove-Item -LiteralPath $stagingBundle -Recurse -Force
    }

    $stagingContents = Join-Path $stagingBundle "Contents"
    New-Item -ItemType Directory -Path $stagingContents -Force | Out-Null
    Copy-Item -LiteralPath $sourceOpor -Destination $stagingContents -Recurse -Force
    Copy-Item -LiteralPath $sourceBootstrap -Destination $stagingContents -Force
    Copy-Item -LiteralPath $sourceManifest -Destination $stagingBundle -Force
    Write-OporInstallLog "STAGED | $stagingBundle"

    $installedLoader = Join-Path $stagingBundle "Contents\OPOR\opor-loader.lsp"
    if (-not (Test-Path -LiteralPath $installedLoader -PathType Leaf)) {
        throw "Staged OPOR bundle validation failed."
    }

    $movedBackups = @()
    try {
        foreach ($bundleRoot in $cleanupRoots) {
            $existingBundle = Join-Path $bundleRoot "OPOR.bundle"
            if (Test-Path -LiteralPath $existingBundle) {
                $backupBundle = Join-Path $bundleRoot ("OPOR.bundle.backup-" + $stamp)
                Move-Item -LiteralPath $existingBundle -Destination $backupBundle
                Write-OporInstallLog "BACKUP | $existingBundle -> $backupBundle"
                $movedBackups += [pscustomobject]@{
                    Original = $existingBundle
                    Backup = $backupBundle
                }
            }
        }
        Move-Item -LiteralPath $stagingBundle -Destination $destinationBundle
        Write-OporInstallLog "INSTALLED | $destinationBundle"
    }
    catch {
        if (Test-Path -LiteralPath $stagingBundle) {
            Remove-Item -LiteralPath $stagingBundle -Recurse -Force
        }
        foreach ($moved in $movedBackups) {
            if ((Test-Path -LiteralPath $moved.Backup) -and
                -not (Test-Path -LiteralPath $moved.Original)) {
                Move-Item -LiteralPath $moved.Backup -Destination $moved.Original
            }
        }
        throw
    }

    # Version comes from the manifest we just validated, never from a literal:
    # a hardcoded number silently drifts away from the payload on every release.
    $installedVersion = $manifest.ApplicationPackage.AppVersion
    if ($isAdmin) {
        Write-Host "OPOR v$installedVersion installed for ALL users to:"
    }
    else {
        Write-Host "OPOR v$installedVersion installed for the current user to:"
    }
    Write-Host $destinationBundle
    foreach ($moved in $movedBackups) {
        Write-Host "Previous/other copy backed up:"
        Write-Host $moved.Backup
    }
    Write-Host "Start AutoCAD normally and run OPOR or XX."
    Write-Host "Diagnostic log:"
    Write-Host $script:OporInstallLog
    Write-OporInstallLog "SUCCESS"
    exit 0
}
catch {
    Write-OporInstallLog ("ERROR | " + $_.Exception.ToString())
    if (-not [string]::IsNullOrWhiteSpace($script:OporInstallLog)) {
        Write-Host "Diagnostic log: $script:OporInstallLog"
    }
    Write-Error $_.Exception.Message
    exit 1
}
