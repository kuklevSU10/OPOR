[CmdletBinding()]
param(
    [switch]$StageOnly,
    [switch]$KeepPayload,
    [switch]$Clean
)

$ErrorActionPreference = 'Stop'

$scriptRoot = [System.IO.Path]::GetFullPath($PSScriptRoot)
$repoRoot = [System.IO.Path]::GetFullPath((Join-Path $scriptRoot '..\..'))
$sourceOpor = Join-Path $repoRoot 'OPOR'
$sourceBootstrap = Join-Path $repoRoot 'installer\autoload\OPOR_bootstrap.lsp'
$configPath = Join-Path $sourceOpor 'opor-config.lsp'
$loaderPath = Join-Path $sourceOpor 'opor-loader.lsp'
$libraryPath = Join-Path $sourceOpor 'opor-table-blocks.dwg'
$payloadRoot = [System.IO.Path]::GetFullPath((Join-Path $scriptRoot 'payload'))
$expectedPayloadParent = $scriptRoot + [System.IO.Path]::DirectorySeparatorChar
$versionInclude = Join-Path $scriptRoot 'version.generated.iss'

if (-not $payloadRoot.StartsWith($expectedPayloadParent, [System.StringComparison]::OrdinalIgnoreCase)) {
    throw "Unsafe payload path: $payloadRoot"
}

if ($Clean) {
    if (Test-Path -LiteralPath $payloadRoot) {
        Remove-Item -LiteralPath $payloadRoot -Recurse -Force
    }
    if (Test-Path -LiteralPath $versionInclude) {
        Remove-Item -LiteralPath $versionInclude -Force
    }
    Write-Host 'OPOR installer staging files removed.'
    return
}

foreach ($required in @($configPath, $loaderPath, $libraryPath, $sourceBootstrap)) {
    if (-not (Test-Path -LiteralPath $required -PathType Leaf)) {
        throw "Required installer input is missing: $required"
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
    '\*opor-version\*\s+"(?<major>\d+)\.(?<minor>\d+)[^"]*"'
)
if (-not $versionMatch.Success) {
    throw "Cannot derive installer version from $configPath"
}

$installerVersion = '{0}.{1}.0' -f `
    $versionMatch.Groups['major'].Value,
    $versionMatch.Groups['minor'].Value
$versionLine = '#define MyAppVersion "{0}"' -f $installerVersion
[System.IO.File]::WriteAllText(
    $versionInclude,
    $versionLine + [Environment]::NewLine,
    [System.Text.Encoding]::ASCII
)

if (Test-Path -LiteralPath $payloadRoot) {
    Remove-Item -LiteralPath $payloadRoot -Recurse -Force
}

$payloadContents = Join-Path $payloadRoot 'Contents'
$payloadOpor = Join-Path $payloadContents 'OPOR'
New-Item -ItemType Directory -Path $payloadOpor -Force | Out-Null
Copy-Item -LiteralPath $sourceBootstrap -Destination $payloadContents

$runtimeFiles = @(
    $sourceLspFiles
    Get-Item -LiteralPath (Join-Path $sourceOpor 'opor.dcl')
    Get-Item -LiteralPath $libraryPath
)
foreach ($file in $runtimeFiles) {
    Copy-Item -LiteralPath $file.FullName -Destination $payloadOpor
}

$stagedCount = @(Get-ChildItem -LiteralPath $payloadOpor -File).Count
$expectedCount = $runtimeFiles.Count
if ($stagedCount -ne $expectedCount) {
    throw "Installer payload is incomplete: staged $stagedCount of $expectedCount files"
}

Write-Host "OPOR installer payload staged: $stagedCount files, version $installerVersion"

if (-not $StageOnly) {
    $compilerCandidates = @(
        (Get-Command ISCC.exe -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Source -First 1),
        (Join-Path $env:LOCALAPPDATA 'Programs\Inno Setup 6\ISCC.exe'),
        'C:\Program Files (x86)\Inno Setup 6\ISCC.exe',
        'C:\Program Files\Inno Setup 6\ISCC.exe'
    ) | Where-Object { $_ -and (Test-Path -LiteralPath $_ -PathType Leaf) }

    $compiler = $compilerCandidates | Select-Object -First 1
    if (-not $compiler) {
        throw 'Inno Setup 6 compiler (ISCC.exe) was not found. Use -StageOnly or install Inno Setup 6.'
    }

    & $compiler (Join-Path $scriptRoot 'OPOR_Setup.iss')
    if ($LASTEXITCODE -ne 0) {
        throw "Inno Setup compiler failed with exit code $LASTEXITCODE"
    }
}

if (-not $KeepPayload -and -not $StageOnly) {
    Remove-Item -LiteralPath $payloadRoot -Recurse -Force
    Remove-Item -LiteralPath $versionInclude -Force
}
