[CmdletBinding()]
param(
    [string]$SourceRoot = (Split-Path -Parent $PSCommandPath),
    [string]$DestinationRoot = "",
    [switch]$NoElevation,
    [switch]$AllowAutoCADRunning
)

$ErrorActionPreference = "Stop"

function Test-OporAdministrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($identity)
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

try {
    $SourceRoot = [IO.Path]::GetFullPath($SourceRoot)

    if (-not $AllowAutoCADRunning -and (Get-Process -Name acad -ErrorAction SilentlyContinue)) {
        throw "Close all AutoCAD windows and run INSTALL_OPOR.cmd again."
    }

    if ([string]::IsNullOrWhiteSpace($DestinationRoot)) {
        if (-not $NoElevation -and -not (Test-OporAdministrator)) {
            $elevatedArguments = @(
                "-NoProfile",
                "-ExecutionPolicy", "Bypass",
                "-File", ('"{0}"' -f $PSCommandPath),
                "-SourceRoot", ('"{0}"' -f $SourceRoot)
            )
            $elevated = Start-Process -FilePath "powershell.exe" `
                -Verb RunAs `
                -ArgumentList $elevatedArguments `
                -Wait `
                -PassThru
            exit $elevated.ExitCode
        }
        $DestinationRoot = Join-Path $env:ProgramFiles "Autodesk\ApplicationPlugins"
    }

    $DestinationRoot = [IO.Path]::GetFullPath($DestinationRoot)
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

    New-Item -ItemType Directory -Path $DestinationRoot -Force | Out-Null

    $destinationBundle = Join-Path $DestinationRoot "OPOR.bundle"
    $stamp = Get-Date -Format "yyyyMMdd-HHmmss"
    $stagingBundle = Join-Path $DestinationRoot ("OPOR.bundle.installing-" + $PID)
    $backupBundle = Join-Path $DestinationRoot ("OPOR.bundle.backup-" + $stamp)

    if (Test-Path -LiteralPath $stagingBundle) {
        Remove-Item -LiteralPath $stagingBundle -Recurse -Force
    }

    $stagingContents = Join-Path $stagingBundle "Contents"
    New-Item -ItemType Directory -Path $stagingContents -Force | Out-Null
    Copy-Item -LiteralPath $sourceOpor -Destination $stagingContents -Recurse -Force
    Copy-Item -LiteralPath $sourceBootstrap -Destination $stagingContents -Force
    Copy-Item -LiteralPath $sourceManifest -Destination $stagingBundle -Force

    $installedLoader = Join-Path $stagingBundle "Contents\OPOR\opor-loader.lsp"
    if (-not (Test-Path -LiteralPath $installedLoader -PathType Leaf)) {
        throw "Staged OPOR bundle validation failed."
    }

    $previousMoved = $false
    try {
        if (Test-Path -LiteralPath $destinationBundle) {
            Move-Item -LiteralPath $destinationBundle -Destination $backupBundle
            $previousMoved = $true
        }
        Move-Item -LiteralPath $stagingBundle -Destination $destinationBundle
    }
    catch {
        if (Test-Path -LiteralPath $stagingBundle) {
            Remove-Item -LiteralPath $stagingBundle -Recurse -Force
        }
        if ($previousMoved -and -not (Test-Path -LiteralPath $destinationBundle)) {
            Move-Item -LiteralPath $backupBundle -Destination $destinationBundle
        }
        throw
    }

    Write-Host "OPOR v3.31 installed to:"
    Write-Host $destinationBundle
    if ($previousMoved) {
        Write-Host "Previous installation backup:"
        Write-Host $backupBundle
    }
    Write-Host "Start AutoCAD normally and run OPOR or XX."
    exit 0
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}
