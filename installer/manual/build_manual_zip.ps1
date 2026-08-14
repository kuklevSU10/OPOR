[CmdletBinding()]
param(
    [string]$OutputPath
)

$ErrorActionPreference = 'Stop'

$scriptRoot = [System.IO.Path]::GetFullPath($PSScriptRoot)
$repoRoot = [System.IO.Path]::GetFullPath((Join-Path $scriptRoot '..\..'))
$sourceOpor = Join-Path $repoRoot 'OPOR'
$loaderPath = Join-Path $sourceOpor 'opor-loader.lsp'
$configPath = Join-Path $sourceOpor 'opor-config.lsp'
$dclPath = Join-Path $sourceOpor 'opor.dcl'
$libraryPath = Join-Path $sourceOpor 'opor-table-blocks.dwg'
$launcherPath = Join-Path $scriptRoot '00_ЗАГРУЗИТЬ_OPOR.lsp'
$instructionsPath = Join-Path $scriptRoot 'КАК_ПОДКЛЮЧИТЬ.txt'

foreach ($required in @(
    $loaderPath,
    $configPath,
    $dclPath,
    $libraryPath,
    $launcherPath,
    $instructionsPath
)) {
    if (-not (Test-Path -LiteralPath $required -PathType Leaf)) {
        throw "Required manual-package input is missing: $required"
    }
}

$loaderText = Get-Content -LiteralPath $loaderPath -Raw -Encoding UTF8
$moduleBlock = [regex]::Match(
    $loaderText,
    '\(foreach\s+module\s+\(list(?<body>.*?)\)\s+\(opor-load-module\s+module\)\)',
    [System.Text.RegularExpressions.RegexOptions]::Singleline
)
if (-not $moduleBlock.Success) {
    throw "Cannot read the runtime module list from $loaderPath"
}

$declaredLspNames = @('opor-loader.lsp') + @(
    [regex]::Matches($moduleBlock.Groups['body'].Value, '"(?<name>opor-[^"]+\.lsp)"') |
        ForEach-Object { $_.Groups['name'].Value }
)
$sourceLspFiles = @(Get-ChildItem -LiteralPath $sourceOpor -Filter '*.lsp' -File)
$sourceLspNames = @($sourceLspFiles | ForEach-Object { $_.Name })
$missingModules = @($declaredLspNames | Where-Object { $_ -notin $sourceLspNames })
$unexpectedModules = @($sourceLspNames | Where-Object { $_ -notin $declaredLspNames })
if ($missingModules.Count -gt 0 -or $unexpectedModules.Count -gt 0) {
    throw (
        'Runtime LSP set does not match opor-loader.lsp. Missing: [{0}]; unexpected: [{1}]' -f
            ($missingModules -join ', '),
            ($unexpectedModules -join ', ')
    )
}

$configText = Get-Content -LiteralPath $configPath -Raw -Encoding UTF8
$versionMatch = [regex]::Match(
    $configText,
    '\*opor-version\*\s+"(?<runtime>(?<major>\d+)\.(?<minor>\d+)[^"]*)"'
)
if (-not $versionMatch.Success) {
    throw "Cannot derive manual-package version from $configPath"
}

$runtimeVersion = $versionMatch.Groups['runtime'].Value
$shortVersion = '{0}.{1}' -f `
    $versionMatch.Groups['major'].Value,
    $versionMatch.Groups['minor'].Value
$packageFolder = "OPOR_${shortVersion}_MANUAL"

if (-not $OutputPath) {
    $OutputPath = Join-Path $scriptRoot "output\OPOR_${shortVersion}_MANUAL.zip"
}
$OutputPath = [System.IO.Path]::GetFullPath($OutputPath)
$outputDir = Split-Path -Parent $OutputPath
New-Item -ItemType Directory -Path $outputDir -Force | Out-Null

$payloadFiles = @($sourceLspFiles | Sort-Object Name) + @(
    Get-Item -LiteralPath $dclPath
    Get-Item -LiteralPath $libraryPath
)

Add-Type -AssemblyName System.IO.Compression
Add-Type -AssemblyName System.IO.Compression.FileSystem

$stream = [System.IO.File]::Open(
    $OutputPath,
    [System.IO.FileMode]::Create,
    [System.IO.FileAccess]::ReadWrite,
    [System.IO.FileShare]::None
)
try {
    $zip = [System.IO.Compression.ZipArchive]::new(
        $stream,
        [System.IO.Compression.ZipArchiveMode]::Create,
        $false,
        [System.Text.Encoding]::UTF8
    )
    try {
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile(
            $zip,
            $launcherPath,
            "$packageFolder/00_ЗАГРУЗИТЬ_OPOR.lsp",
            [System.IO.Compression.CompressionLevel]::Optimal
        ) | Out-Null
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile(
            $zip,
            $instructionsPath,
            "$packageFolder/КАК_ПОДКЛЮЧИТЬ.txt",
            [System.IO.Compression.CompressionLevel]::Optimal
        ) | Out-Null
        foreach ($file in $payloadFiles) {
            [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile(
                $zip,
                $file.FullName,
                "$packageFolder/OPOR/$($file.Name)",
                [System.IO.Compression.CompressionLevel]::Optimal
            ) | Out-Null
        }
    }
    finally {
        $zip.Dispose()
    }
}
finally {
    $stream.Dispose()
}

$archive = [System.IO.Compression.ZipFile]::OpenRead($OutputPath)
try {
    $entryCount = $archive.Entries.Count
}
finally {
    $archive.Dispose()
}

Write-Host (
    'OPOR manual ZIP built: {0}; runtime: {1}; payload files: {2}; ZIP entries: {3}; size: {4}' -f
        $OutputPath,
        $runtimeVersion,
        $payloadFiles.Count,
        $entryCount,
        (Get-Item -LiteralPath $OutputPath).Length
)
