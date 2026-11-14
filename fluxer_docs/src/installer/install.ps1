# SPDX-License-Identifier: AGPL-3.0-or-later

[CmdletBinding(PositionalBinding = $false)]
param(
	[string]$Domain = '',
	[string]$Email = '',
	[string]$Dir = '',
	[string]$Engine = '',
	[string]$Ref = '',
	[string]$ImageTag = '',
	[string]$Tls = '',
	[string]$EdgeBind = '',
	[string]$BackupDir = '',
	[string]$KeepBackups = '',
	[switch]$NonInteractive,
	[switch]$DryRun,
	[switch]$NoStart,
	[switch]$Update,
	[switch]$Rollback,
	[switch]$NoVolumeBackup,
	[switch]$NoVolumeCompression,
	[switch]$SkipBackupAcceptDataLoss,
	[switch]$Check,
	[switch]$RegenerateVapid,
	[switch]$Help,
	[Parameter(ValueFromRemainingArguments = $true)]
	[string[]]$Rest = @()
)

& {
	param($Bound, $Rest, $ScriptRoot)

	Set-StrictMode -Version Latest
	$ErrorActionPreference = 'Stop'
	$ProgressPreference = 'SilentlyContinue'

	function Get-Bound([string]$Name) {
		if ($Bound.ContainsKey($Name)) {
			return $Bound[$Name]
		}
		return $null
	}

	function Test-Switch([string]$Name) {
		$value = Get-Bound $Name
		return $null -ne $value -and [bool]$value
	}

	function Get-Text([string]$Name) {
		$value = Get-Bound $Name
		if ($null -eq $value) {
			return ''
		}
		return [string]$value
	}

	$Resolver = 'https://fluxer.dev/rexctl/release.json'
	if ($env:REXCTL_RELEASE_URL) {
		$Resolver = $env:REXCTL_RELEASE_URL
	}

	$OldEncoding = $null
	$Plain = $env:NO_COLOR -or $env:TERM -eq 'dumb' -or [Console]::IsErrorRedirected
	if (-not $Plain -and [Console]::OutputEncoding.CodePage -ne 65001) {
		try {
			$OldEncoding = [Console]::OutputEncoding
			[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
		} catch {
			$OldEncoding = $null
			$Plain = $true
		}
	}
	$GlyphOk = 'ok'
	$GlyphFail = 'x'
	if (-not $Plain) {
		$GlyphOk = [string][char]0x2713
		$GlyphFail = [string][char]0x2717
	}

	function Stop-Bootstrap([string]$Message, [string]$Fix = '', [int]$Code = 1) {
		[Console]::Error.WriteLine("$GlyphFail $Message")
		if ($Fix) {
			[Console]::Error.WriteLine("  $Fix")
		}
		$global:LASTEXITCODE = $Code
		throw 'rexctl-bootstrap-stop'
	}

	function Test-Instance([string]$Path) {
		if (-not $Path) { return $false }
		return (Test-Path -LiteralPath (Join-Path $Path '.rexctl/state.json') -PathType Leaf) -or (Test-Path -LiteralPath (Join-Path $Path '.env') -PathType Leaf)
	}

	function Get-RexctlArguments {
		$out = New-Object System.Collections.Generic.List[string]
		if ($Rest.Count -gt 0 -and -not $Rest[0].StartsWith('-')) {
			$out.AddRange([string[]]$Rest)
			foreach ($name in @('Dir', 'Domain', 'Engine')) {
				$value = Get-Text $name
				if ($value) { $out.Add('--' + $name.ToLowerInvariant()); $out.Add($value) }
			}
			if (Test-Switch 'NonInteractive') { $out.Add('--yes') }
			if (Test-Switch 'DryRun') { $out.Add('--dry-run') }
			if (Test-Switch 'NoStart') { $out.Add('--no-start') }
			if (Test-Switch 'Check') { $out.Add('--check') }
			if (Test-Switch 'SkipBackupAcceptDataLoss') { $out.Add('--no-backup') }
			if (Test-Switch 'Help') { $out.Add('--help') }
			return , $out.ToArray()
		}
		$target = Get-Text 'Dir'
		if (-not $target -and ((Test-Switch 'Update') -or (Test-Switch 'Rollback') -or (Test-Switch 'Check'))) {
			foreach ($candidate in @($ScriptRoot, (Get-Location).Path)) {
				if (Test-Instance $candidate) {
					$target = $candidate
					break
				}
			}
		}
		if (Test-Switch 'Update') {
			$out.Add('upgrade')
			if (Test-Switch 'SkipBackupAcceptDataLoss') { $out.Add('--no-backup') }
			if (Test-Switch 'DryRun') { $out.Add('--dry-run') }
		} elseif (Test-Switch 'Check') {
			$out.Add('upgrade')
			$out.Add('--check')
		} elseif (Test-Switch 'Rollback') {
			$out.Add('rollback')
			if (Test-Switch 'DryRun') { $out.Add('--dry-run') }
		} else {
			$out.Add('install')
			$domain = Get-Text 'Domain'
			$engine = Get-Text 'Engine'
			if ($domain) { $out.Add('--domain'); $out.Add($domain) }
			if ($engine) { $out.Add('--engine'); $out.Add($engine) }
			if ((Get-Text 'Tls') -eq 'proxy') {
				$out.Add('--behind-proxy')
				$bind = Get-Text 'EdgeBind'
				if ($bind) { $out.Add('--bind'); $out.Add($bind) }
			}
			if (Test-Switch 'NoStart') { $out.Add('--no-start') }
			if (Test-Switch 'DryRun') { $out.Add('--dry-run') }
		}
		if ($target) { $out.Add('--dir'); $out.Add($target) }
		if (Test-Switch 'NonInteractive') { $out.Add('--yes') }
		if (Test-Switch 'Help') { $out.Add('--help') }
		return , $out.ToArray()
	}

	function Install-Rexctl([string]$New, [string]$Exe) {
		$old = "$Exe.old"
		Remove-Item -LiteralPath $old -Force -ErrorAction SilentlyContinue
		if (Test-Path -LiteralPath $Exe -PathType Leaf) {
			try {
				Move-Item -LiteralPath $Exe -Destination $old -Force
			} catch {
				Stop-Bootstrap "Couldn't replace rexctl, which is open in another window." 'Close rexctl in your other windows, then run this again.'
			}
		}
		try {
			Move-Item -LiteralPath $New -Destination $Exe -Force
		} catch {
			Stop-Bootstrap "Couldn't replace rexctl, which is open in another window." 'Close rexctl in your other windows, then run this again.'
		}
		Remove-Item -LiteralPath $old -Force -ErrorAction SilentlyContinue
	}

	try {
		$RexctlArguments = Get-RexctlArguments
		$Trial = (Test-Switch 'DryRun') -or (Test-Switch 'Check') -or ($RexctlArguments -contains '--dry-run') -or ($RexctlArguments -contains '--check')

		if ($env:OS -ne 'Windows_NT') {
			Stop-Bootstrap 'This script runs on Windows.' 'On Linux and macOS, run install.sh instead.'
		}

		$Arch = $env:PROCESSOR_ARCHITEW6432
		if (-not $Arch) {
			$Arch = $env:PROCESSOR_ARCHITECTURE
		}
		switch ($Arch) {
			'AMD64' { $Arch = 'amd64' }
			'ARM64' { $Arch = 'arm64' }
			default { Stop-Bootstrap "Fluxer runs on 64-bit Intel, AMD and Arm machines, and this one is $Arch." }
		}

		if ($PSVersionTable.PSVersion.Major -lt 6) {
			[Net.ServicePointManager]::SecurityProtocol = [Net.ServicePointManager]::SecurityProtocol -bor [Net.SecurityProtocolType]::Tls12
		}

		try {
			$Release = Invoke-RestMethod -Uri $Resolver -UseBasicParsing
		} catch {
			Stop-Bootstrap "Couldn't find the latest rexctl release." 'Check that this machine can reach fluxer.dev, then run this again.'
		}
		$Asset = $null
		$Version = ''
		if ($null -ne $Release -and $Release.PSObject.Properties['assets'] -and $Release.PSObject.Properties['version']) {
			$Version = [string]$Release.version
			$Asset = @($Release.assets | Where-Object {
					$null -ne $_ -and $_.PSObject.Properties['os'] -and $_.PSObject.Properties['arch'] -and $_.os -eq 'windows' -and $_.arch -eq $Arch
				}) | Select-Object -First 1
		}
		if (-not $Asset -or -not $Asset.PSObject.Properties['sha256'] -or -not $Asset.PSObject.Properties['url'] -or
			[string]$Asset.sha256 -notmatch '^[0-9a-f]{64}$' -or -not ([string]$Asset.url).StartsWith('https://')) {
			Stop-Bootstrap "The release information from fluxer.dev doesn't look right." 'Run this again in a few minutes.'
		}

		$Scratch = Join-Path ([System.IO.Path]::GetTempPath()) ('rexctl-' + [guid]::NewGuid().ToString('N'))
		New-Item -ItemType Directory -Path $Scratch | Out-Null
		try {
			$Zip = Join-Path $Scratch 'rexctl.zip'
			try {
				Invoke-WebRequest -Uri $Asset.url -OutFile $Zip -UseBasicParsing
			} catch {
				Stop-Bootstrap "Couldn't download rexctl $Version." 'Check that this machine can reach github.com, then run this again.'
			}
			if ((Get-FileHash -LiteralPath $Zip -Algorithm SHA256).Hash.ToLowerInvariant() -ne $Asset.sha256) {
				Stop-Bootstrap "The rexctl download doesn't match its checksum." 'Run this again. If it keeps happening, something between you and GitHub is changing the file.'
			}
			$Out = Join-Path $Scratch 'out'
			Expand-Archive -LiteralPath $Zip -DestinationPath $Out
			$New = Join-Path $Out 'rexctl.exe'
			if (-not (Test-Path -LiteralPath $New -PathType Leaf)) {
				Stop-Bootstrap "The rexctl download doesn't contain rexctl." 'Run this again in a few minutes.'
			}
			if ($Trial) {
				& $New @RexctlArguments
			} else {
				$BinDir = Join-Path $env:LOCALAPPDATA 'Programs\rexctl'
				New-Item -ItemType Directory -Path $BinDir -Force | Out-Null
				$Exe = Join-Path $BinDir 'rexctl.exe'
				Install-Rexctl $New $Exe
			}
		} finally {
			Remove-Item -LiteralPath $Scratch -Recurse -Force -ErrorAction SilentlyContinue
		}
		if ($Trial) {
			return
		}
		[Console]::Error.WriteLine("$GlyphOk Installed rexctl $Version")

		$UserPath = [Environment]::GetEnvironmentVariable('Path', 'User')
		if (-not $UserPath) {
			$UserPath = ''
		}
		if (-not (($UserPath -split ';') -contains $BinDir)) {
			[Environment]::SetEnvironmentVariable('Path', (($UserPath.TrimEnd(';'), $BinDir) -join ';').TrimStart(';'), 'User')
		}
		if (-not (($env:Path -split ';') -contains $BinDir)) {
			$env:Path = "$env:Path;$BinDir"
		}

		& $Exe @RexctlArguments
	} catch {
		if ([string]$_.Exception.Message -ne 'rexctl-bootstrap-stop') {
			[Console]::Error.WriteLine("$GlyphFail The installer stopped: $($_.Exception.Message)")
			[Console]::Error.WriteLine('  Run this again in a few minutes.')
			$global:LASTEXITCODE = 1
		}
	} finally {
		if ($OldEncoding) {
			try { [Console]::OutputEncoding = $OldEncoding } catch { }
		}
	}
} (@{} + $PSBoundParameters) $Rest $PSScriptRoot

if ($PSCommandPath) {
	exit $LASTEXITCODE
}
