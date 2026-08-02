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
    $systemInstall = [string]::IsNullOrWhiteSpace($DestinationRoot)

    if (-not $AllowAutoCADRunning -and (Get-Process -Name acad -ErrorAction SilentlyContinue)) {
        throw "Close all AutoCAD windows and run the OPOR installer again."
    }

    if ($systemInstall) {
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
        $programFilesX86 = [Environment]::GetEnvironmentVariable("ProgramFiles(x86)")
        $programFilesPlugins = Join-Path $env:ProgramFiles "Autodesk\ApplicationPlugins"
        $programFilesX86Plugins = if ($programFilesX86) {
            Join-Path $programFilesX86 "Autodesk\ApplicationPlugins"
        }
        $x86HasBundles = $programFilesX86Plugins -and
            (Test-Path -LiteralPath $programFilesX86Plugins) -and
            (Get-ChildItem -LiteralPath $programFilesX86Plugins -Directory -Filter "*.bundle" -ErrorAction SilentlyContinue)
        $programFilesHasBundles = (Test-Path -LiteralPath $programFilesPlugins) -and
            (Get-ChildItem -LiteralPath $programFilesPlugins -Directory -Filter "*.bundle" -ErrorAction SilentlyContinue)
        if ($x86HasBundles) {
            $DestinationRoot = $programFilesX86Plugins
        }
        elseif ($programFilesHasBundles) {
            $DestinationRoot = $programFilesPlugins
        }
        else {
            $DestinationRoot = $programFilesPlugins
        }
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
    $bundleRoots = @($DestinationRoot)
    if ($systemInstall) {
        $bundleRoots += @(
            (Join-Path $env:ProgramFiles "Autodesk\ApplicationPlugins"),
            (if ($programFilesX86) { Join-Path $programFilesX86 "Autodesk\ApplicationPlugins" }),
            (Join-Path $env:ProgramData "Autodesk\ApplicationPlugins"),
            (Join-Path $env:APPDATA "Autodesk\ApplicationPlugins")
        )
    }
    $bundleRoots = @(
        $bundleRoots |
        Where-Object { -not [string]::IsNullOrWhiteSpace($_) } |
        ForEach-Object { [IO.Path]::GetFullPath($_) } |
        Sort-Object -Unique
    )

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

    $movedBackups = @()
    try {
        foreach ($bundleRoot in $bundleRoots) {
            $existingBundle = Join-Path $bundleRoot "OPOR.bundle"
            if (Test-Path -LiteralPath $existingBundle) {
                $backupBundle = Join-Path $bundleRoot ("OPOR.bundle.backup-" + $stamp)
                Move-Item -LiteralPath $existingBundle -Destination $backupBundle
                $movedBackups += [pscustomobject]@{
                    Original = $existingBundle
                    Backup = $backupBundle
                }
            }
        }
        Move-Item -LiteralPath $stagingBundle -Destination $destinationBundle
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

    Write-Host "OPOR v3.31.1 installed to:"
    Write-Host $destinationBundle
    foreach ($moved in $movedBackups) {
        Write-Host "Previous installation backup:"
        Write-Host $moved.Backup
    }
    Write-Host "Start AutoCAD normally and run OPOR or XX."
    exit 0
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}
