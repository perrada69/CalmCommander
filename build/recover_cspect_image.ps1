param(
    [Parameter(Mandatory = $true)][string]$HdfMonkey,
    [Parameter(Mandatory = $true)][string]$SourceImage,
    [Parameter(Mandatory = $true)][string]$TargetImage,
    [Parameter(Mandatory = $true)][string]$ReplacementCc,
    [switch]$Resume
)

$ErrorActionPreference = 'Stop'

function Invoke-Hdf {
    param([string[]]$Arguments)

    $output = & $HdfMonkey @Arguments 2>&1
    $text = ($output | Out-String)
    $reportedFailure = $text -match '(?im)^(?:Error\b|Formatting failed\b|read\(\) error\b|write\(\) error\b|open\(\).*error\b|ftruncate\(\) error\b)'
    if ($LASTEXITCODE -ne 0 -or $reportedFailure) {
        throw "hdfmonkey $($Arguments[0]) failed:`n$text"
    }
    return @($output)
}

$targetFull = [System.IO.Path]::GetFullPath($TargetImage)
$buildRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '.'))
if (-not $targetFull.StartsWith($buildRoot, [System.StringComparison]::OrdinalIgnoreCase)) {
    throw "Target image must stay inside the build directory: $targetFull"
}

if (-not $Resume -and (Test-Path -LiteralPath $targetFull)) {
    Remove-Item -LiteralPath $targetFull -Force
}

if (-not $Resume) {
    Invoke-Hdf @('create', '--fat16', $targetFull, '1900M', 'CALMREC') | Out-Null
} elseif (-not (Test-Path -LiteralPath $targetFull)) {
    throw "Resume target does not exist: $targetFull"
}

$tempRoot = Join-Path $PSScriptRoot 'image-recovery-temp'
if (Test-Path -LiteralPath $tempRoot) {
    Remove-Item -LiteralPath $tempRoot -Recurse -Force
}
New-Item -ItemType Directory -Path $tempRoot | Out-Null

$script:directoryCount = 0
$script:fileCount = 0

function Copy-ImageDirectory {
    param([string]$RelativePath)

    $listArgs = @('ls', $SourceImage)
    if ($RelativePath) {
        $listArgs += $RelativePath
    }
    $lines = Invoke-Hdf $listArgs

    $targetListArgs = @('ls', $TargetImage)
    if ($RelativePath) {
        $targetListArgs += $RelativePath
    }
    $targetLines = Invoke-Hdf $targetListArgs
    $targetDirectories = @{}
    $targetFiles = @{}
    foreach ($targetLineObject in $targetLines) {
        $targetLine = [string]$targetLineObject
        if ($targetLine -match '^\[DIR\]\t(.+)$') {
            $targetDirectories[$Matches[1].ToLowerInvariant()] = $true
        } elseif ($targetLine -match '^(\d+)\t(.+)$') {
            $targetFiles[$Matches[2].ToLowerInvariant()] = [int64]$Matches[1]
        }
    }

    foreach ($lineObject in $lines) {
        $line = [string]$lineObject
        if ($line -match '^\[DIR\]\t(.+)$') {
            $name = $Matches[1]
            if ($name -eq '.' -or $name -eq '..') { continue }
            $child = if ($RelativePath) { "$RelativePath/$name" } else { $name }
            if (-not $targetDirectories.ContainsKey($name.ToLowerInvariant())) {
                Invoke-Hdf @('mkdir', $TargetImage, $child) | Out-Null
            }
            $script:directoryCount++
            Copy-ImageDirectory $child
            continue
        }

        if ($line -notmatch '^(\d+)\t(.+)$') { continue }
        $size = [int64]$Matches[1]
        $name = $Matches[2]
        $imagePath = if ($RelativePath) { "$RelativePath/$name" } else { $name }

        if ($imagePath -ieq 'CalmCommander/cc.bin') {
            continue
        }

        $nameKey = $name.ToLowerInvariant()
        if ($targetFiles.ContainsKey($nameKey) -and $targetFiles[$nameKey] -eq $size) {
            $script:fileCount++
            continue
        }

        if ($targetFiles.ContainsKey($nameKey)) {
            $removeOutput = & $HdfMonkey rm $TargetImage $imagePath 2>&1
            if ($LASTEXITCODE -ne 0) {
                throw "Unable to remove incomplete target file $imagePath`n$($removeOutput | Out-String)"
            }
        }

        $localDirectory = if ($RelativePath) {
            Join-Path $tempRoot ($RelativePath -replace '/', '\')
        } else {
            $tempRoot
        }
        New-Item -ItemType Directory -Path $localDirectory -Force | Out-Null
        $localFile = Join-Path $localDirectory $name

        Invoke-Hdf @('get', $SourceImage, $imagePath, $localFile) | Out-Null
        if ((Get-Item -LiteralPath $localFile).Length -ne $size) {
            throw "Size mismatch while extracting $imagePath"
        }

        $targetDirectory = if ($RelativePath) { "$RelativePath/" } else { '/' }
        Invoke-Hdf @('put', $TargetImage, $localFile, $targetDirectory) | Out-Null
        Remove-Item -LiteralPath $localFile -Force
        $script:fileCount++
    }
}

Copy-ImageDirectory ''

$ccListing = Invoke-Hdf @('ls', $TargetImage, 'CalmCommander')
$ccPresent = $false
foreach ($ccLineObject in $ccListing) {
    if ([string]$ccLineObject -match '^(\d+)\tcc\.bin$' -and [int64]$Matches[1] -eq (Get-Item -LiteralPath $ReplacementCc).Length) {
        $ccPresent = $true
        break
    }
}
if (-not $ccPresent) {
    $removeOutput = & $HdfMonkey rm $TargetImage 'CalmCommander/cc.bin' 2>&1
    Invoke-Hdf @('put', $TargetImage, $ReplacementCc, 'CalmCommander/') | Out-Null
}

Remove-Item -LiteralPath $tempRoot -Recurse -Force

Write-Output "Recovered directories: $script:directoryCount"
Write-Output "Recovered files: $script:fileCount"
