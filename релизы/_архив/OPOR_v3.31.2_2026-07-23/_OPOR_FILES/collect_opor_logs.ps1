[CmdletBinding()]
param(
    [string]$LogDirectory = "",
    [string]$OutputDirectory = "",
    [switch]$NoOpen
)

$ErrorActionPreference = "Stop"

try {
    if ([string]::IsNullOrWhiteSpace($LogDirectory)) {
        $LogDirectory = Join-Path $env:LOCALAPPDATA "OPOR\logs"
    }
    if (-not (Test-Path -LiteralPath $LogDirectory -PathType Container)) {
        throw "OPOR log folder does not exist yet. Install or run OPOR first."
    }

    $logFiles = @(Get-ChildItem -LiteralPath $LogDirectory -File -Filter "*.log" -ErrorAction Stop)
    if ($logFiles.Count -eq 0) {
        throw "No OPOR log files were found. Install or run OPOR first."
    }

    if ([string]::IsNullOrWhiteSpace($OutputDirectory)) {
        $OutputDirectory = [Environment]::GetFolderPath("Desktop")
    }
    New-Item -ItemType Directory -Path $OutputDirectory -Force | Out-Null
    $archive = Join-Path $OutputDirectory ("OPOR_LOGS_{0}.zip" -f (Get-Date -Format "yyyyMMdd-HHmmss"))
    Compress-Archive -Path (Join-Path $LogDirectory "*") -DestinationPath $archive -Force

    Write-Host "OPOR diagnostic archive created:"
    Write-Host $archive
    if (-not $NoOpen) {
        Start-Process -FilePath "explorer.exe" -ArgumentList ('/select,"{0}"' -f $archive)
    }
    exit 0
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}
