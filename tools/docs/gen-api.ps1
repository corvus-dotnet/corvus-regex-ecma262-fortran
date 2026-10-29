<#
.SYNOPSIS
    Writes docs/api.md, the API reference the documentation site publishes, from the public part of
    src/corvus_regex_ecma262.f90 and its comments.

.DESCRIPTION
    The module's own comment (its conventions), then its public constants, its public types (a type with procedures
    bound to it has each binding, a generic one with each of its specific procedures) and its public procedures, each
    with the comment written above it and the declarations of its arguments.

.PARAMETER Check
    Compares what would be written with docs/api.md and writes nothing. The script fails when they differ.
#>
param([switch] $Check)

$ErrorActionPreference = 'Stop'
$root = Split-Path (Split-Path $PSScriptRoot -Parent) -Parent
$sourcePath = Join-Path $root 'src' 'corvus_regex_ecma262.f90'
$out = Join-Path $root 'docs' 'api.md'
$lines = [System.IO.File]::ReadAllText($sourcePath) -replace "`r", '' -split "`n"

# A comment as Markdown: its prose escaped, so that $` and $<name> are not read as code or HTML.
function Convert-Comment([string[]]$text) {
    return (($text | ForEach-Object { $_ -replace '([\\`*<\[\]])', '\$1' }) -join "`n").Trim()
}

$moduleLine = [Array]::FindIndex($lines, [Predicate[string]] { param($l) $l -match '^module corvus_regex_ecma262\s*$' })
$containsLine = [Array]::FindIndex($lines, [Predicate[string]] { param($l) $l -match '^contains\s*$' })
$conventions = Convert-Comment @($lines[0..($moduleLine - 1)] | Where-Object { $_ -match '^!>' } | ForEach-Object { $_ -replace '^!> ?', '' })

# The names the module makes public.
$public = [System.Collections.Generic.List[string]]::new()
foreach ($line in $lines[$moduleLine..$containsLine]) {
    if ($line -match '^\s*public\s*::\s*(.*)$') { $Matches[1] -split ',\s*' | ForEach-Object { $public.Add($_.Trim()) } }
}

# The procedures of the module: each with its comment, and its first line with the declarations of its arguments.
$procedures = @{}
for ($i = $containsLine; $i -lt $lines.Count; $i++) {
    if ($lines[$i] -notmatch '^  (?:[\w() ,]*\s)?(function|subroutine) (\w+)\(') { continue }
    $kind, $name = $Matches[1], $Matches[2]
    $doc = @()
    for ($j = $i - 1; $j -ge 0 -and $lines[$j] -match '^\s*!>'; $j--) { $doc = @($lines[$j].Trim() -replace '^!> ?', '') + $doc }
    $head = @($lines[$i])
    while ($head[-1].TrimEnd().EndsWith('&')) { $i++; $head += $lines[$i] }
    $joined = ($head | ForEach-Object { $_.Trim().TrimEnd('&').Trim() }) -join ' '
    $names = @()
    if ($joined -match '\(([^)]*)\)') { $names += $Matches[1] -split ',\s*' | ForEach-Object { $_.Trim() } }
    if ($joined -match 'result\((\w+)\)') { $names += $Matches[1] } elseif ($kind -eq 'function') { $names += $name }
    $decls = @()
    for ($i++; $i -lt $lines.Count; $i++) {
        $l = $lines[$i]
        if ($l -notmatch '::') { break }
        $declared = ($l -split '::', 2)[1] -split ',' | ForEach-Object { ($_ -replace '[=(].*$', '').Trim() }
        if (@($declared | Where-Object { $names -contains $_ }).Count) { $decls += $l }
    }
    $code = ((@($head) + @($decls)) | ForEach-Object { $_ -replace '^  ', '' }) -join "`n"
    $procedures[$name] = [pscustomobject]@{ Name = $name; Kind = $kind; Code = "$code`nend $kind $name"; Doc = $doc }
}

# The constants: each comment with the public parameters under it.
$constants = [System.Collections.Generic.List[object]]::new()
$types = [ordered]@{}
$comment = @()
for ($i = $moduleLine; $i -lt $containsLine; $i++) {
    $line = $lines[$i]
    if ($line.Trim() -eq '') { $comment = @(); continue }
    if ($line -match '^\s*!') { $comment += ($line.Trim() -replace '^!>? ?', ''); continue }
    if ($line -match 'parameter, public\s*::\s*(\w+)') {
        $last = if ($constants.Count) { $constants[$constants.Count - 1] } else { $null }
        if ($comment.Count -eq 0 -and $last -and $last.Open) { $last.Code += "`n$($line.Trim())"; $last.Names += $Matches[1] }
        else { $constants.Add([pscustomobject]@{ Names = @($Matches[1]); Code = $line.Trim(); Doc = $comment; Open = $true }) }
        $comment = @()
        continue
    }
    if ($line -match '^\s*type\s*::\s*(\w+)') {
        $name = $Matches[1]
        $body = @()
        for ($i++; $lines[$i] -notmatch "^\s*end type"; $i++) { $body += $lines[$i].Trim() }
        if ($public -contains $name) { $types[$name] = [pscustomobject]@{ Name = $name; Body = $body; Doc = $comment } }
        foreach ($c in $constants) { $c.Open = $false }
        $comment = @()
        continue
    }
    foreach ($c in $constants) { $c.Open = $false }
    $comment = @()
}

$sb = [System.Text.StringBuilder]::new()
function Add([string]$line = '') { [void]$sb.Append($line).Append("`n") }
function Add-Code([string]$code) { Add; Add '```fortran'; Add $code; Add '```' }

Add '# API reference'
Add
Add 'The module `corvus_regex_ecma262`, generated from its public declarations and their comments by'
Add '`tools/docs/gen-api.ps1`. [Getting started](getting-started.md) shows it at work.'
Add
Add '## Conventions'
Add
Add $conventions
Add
Add '## Constants'
foreach ($c in $constants) {
    Add
    Add "### $($c.Names -join ', ')"
    Add-Code $c.Code
    if ($c.Doc.Count) { Add; Add (Convert-Comment $c.Doc) }
}
Add
Add '## Types'
foreach ($t in $types.Values) {
    # A type's public part: its components (none when it is private) and the procedures bound to it.
    $components = @()
    $bindings = [ordered]@{}
    $private = $false
    $inContains = $false
    foreach ($b in $t.Body) {
        if ($b -eq 'private') { $private = $true; continue }
        if ($b -eq 'contains') { $inContains = $true; continue }
        if ($b -match '^!' -or $b -eq '') { continue }
        if (-not $inContains) { if (-not $private) { $components += "  $b" }; continue }
        if ($b -match '^procedure\s*::\s*(\w+)\s*=>\s*(\w+)') { $bindings[$Matches[1]] = @($Matches[2]) }
        elseif ($b -match '^generic\s*::\s*(\w+)\s*=>\s*(.*)$') { $bindings[$Matches[1]] = @($Matches[2] -split ',\s*' | ForEach-Object { $_.Trim() }) }
    }
    $code = @("type :: $($t.Name)") + $components
    if ($bindings.Count) { $code += '  ! (procedures below)' }
    $code += "end type $($t.Name)"
    Add
    Add "### $($t.Name)"
    Add-Code ($code -join "`n")
    if ($t.Doc.Count) { Add; Add (Convert-Comment $t.Doc) }
    foreach ($binding in $bindings.Keys) {
        $specifics = @($bindings[$binding] | ForEach-Object { $procedures[$_] } | Where-Object { $_ })
        Add
        Add "#### $($t.Name)%$binding"
        Add-Code (($specifics | ForEach-Object Code) -join "`n`n")
        $docs = @($specifics | Where-Object { $_.Doc.Count } | ForEach-Object { Convert-Comment $_.Doc } | Select-Object -Unique)
        if ($docs.Count) { Add; Add ($docs -join "`n`n") }
    }
}
Add
Add '## Procedures'
foreach ($name in $public) {
    if (-not $procedures.ContainsKey($name)) { continue }
    $p = $procedures[$name]
    Add
    Add "### $name"
    Add-Code $p.Code
    if ($p.Doc.Count) { Add; Add (Convert-Comment $p.Doc) }
}
$text = $sb.ToString().TrimEnd("`n") + "`n"

if ($Check) {
    $current = if (Test-Path $out) { [System.IO.File]::ReadAllText($out) } else { '' }
    if ($current -ne $text) { throw 'docs/api.md is not what the module makes: run tools/docs/gen-api.ps1.' }
    Write-Host 'docs/api.md is current.'
    return
}
[System.IO.File]::WriteAllText($out, $text, [System.Text.UTF8Encoding]::new($false))
Write-Host 'Wrote docs/api.md.'
