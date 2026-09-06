# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Fluxer self-hosting installer and upgrader for Windows.
#
# Three modes, one file:
#
#   default    Check the host, download the stack files, write .env with every value the stack
#              requires, start the containers.
#   -Update    Upgrade an instance that already exists. Record the running images, back up,
#              refresh the stack files, pull, recreate, verify.
#   -Rollback  Put the images and the stack files of the last recorded upgrade back.
#
# Why one script and not a separate upgrader: an upgrade needs the host checks, the stack
# download, the readiness poll and the health probe that the install already carries. A second
# script either copies them or drifts from them, and the operator has two downloads and two
# checksums to verify instead of one.
#
# This file is also the procedure. Every step of the upgrade carries the command an operator
# types to do that step by hand, and the reason the step exists.
#
# Read this file before running it. The default mode writes .env, which holds every secret the
# instance has. The upgrade generates no secret and rewrites no secret. The only line either
# upgrade mode ever changes in .env is FLUXER_IMAGE_TAG, and only during a rollback that moves
# off a pinned tag.
#
# Rewriting a secret in .env against volumes that already exist is the one thing this script
# could do that an operator cannot undo. A fresh POSTGRES_PASSWORD does not open the existing
# postgres-data volume, and the instance never starts again.
#
# Source:   https://fluxer.dev/install.ps1
# Checksum: https://fluxer.dev/install.ps1.sha256
#
# Every file this script writes uses LF line endings. Docker Compose keeps a trailing carriage
# return as part of a value, so a CRLF .env produces secrets that do not match the ones the
# containers were given.

[CmdletBinding()]
param(
	[string]$Domain = '',
	[string]$Email = '',
	[string]$Dir = '',
	[string]$Ref = '',
	[string]$ImageTag = 'v1',
	[string]$Tls = 'bundled',
	[string]$EdgeBind = '127.0.0.1:8080',
	[string]$BackupDir = '',
	[switch]$NonInteractive,
	[switch]$DryRun,
	[switch]$NoStart,
	[switch]$Update,
	[switch]$Rollback,
	[switch]$NoVolumeBackup,
	[switch]$SkipBackupAcceptDataLoss,
	[switch]$Help,
	[Parameter(ValueFromRemainingArguments = $true)]
	[string[]]$Rest = @()
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

$FluxerRawBase = 'https://raw.githubusercontent.com/fluxerapp/fluxer'
$FluxerStackPath = 'deploy/self-hosting'
$FluxerHealthPath = '/_health'
$FluxerInitService = 'seaweedfs-init'
$FluxerMinimumEngineVersion = '24.0.0'
$FluxerMinimumComposeVersion = '2.20.2'
$FluxerReadyTimeoutSeconds = 600
$FluxerReadyIntervalSeconds = 5
$FluxerReadyReportSeconds = 30
$FluxerVapidAttempts = 8

# The image that copies volumes and measures them. Pinned, because an upgrade that reaches for a
# moving tag to take its backup has one more thing that can change under it.
$FluxerHelperImage = 'alpine:3.22'

# Names inside a record directory. A record is one upgrade: what was running, what the stack files
# said, and the backup taken before the images moved. install.sh writes the same names, so either
# script reads a record the other one wrote.
$FluxerRecordPrefix = 'record-'
$FluxerImagesFile = 'images'
$FluxerTagFile = 'image-tag'
$FluxerDumpFile = 'fluxer.dump'

# Free space demanded before a volume copy, as a percentage of the measured volume size. The
# tarball compresses, so this is generous on purpose. A backup that fills the disk it writes to
# takes the instance down with it.
$FluxerVolumeHeadroomPercent = 110

$FluxerExitUsage = 1
$FluxerExitPrerequisite = 2
$FluxerExitRefused = 3
$FluxerExitDownload = 4
$FluxerExitSecret = 5
$FluxerExitUnhealthy = 6
$FluxerExitBackup = 7
$FluxerExitInterrupted = 130

$FluxerStackFiles = @(
	'docker-compose.yml'
	'docker-compose.proxy.yml'
	'tunnel.compose.yml'
	'Caddyfile'
	'.env.example'
)

# The file Compose bind-mounts from the working directory, with the service that mounts it.
# Compose decides whether to recreate a container by comparing its configuration, and the
# contents of a bind-mounted file are not part of that comparison, so a changed Caddyfile survives
# docker compose up -d with the old bytes still loaded in the running container.
#
# By hand, after a refresh that changed the file:
#   docker compose restart edge
$FluxerMountedFiles = @(
	@{Name = 'Caddyfile'; Service = 'edge'}
)

# The volumes an upgrade copies, and the reason the rest are absent.
#
# postgres-data is not here because the dump supersedes it. A custom-format dump restores into the
# major version that wrote it or a newer one, a tarball of the data directory restores only into
# the major that wrote it, and taking both doubles the downtime and the disk for a strictly weaker
# artifact.
#
# valkey-data and nats-data hold queued work, not records. Losing them drops scheduled bulk
# deletions and background jobs, which is degradation rather than loss, and the upgrade never
# removes a volume.
#
# meilisearch-data rebuilds on the next API start, edge-data is one certificate request,
# edge-config is rewritten by Caddy on every start.
#
# seaweedfs-data is the one that matters. It holds every upload, avatar, report and harvest, and
# nothing else in the stack can recreate any of it.
$FluxerBackupVolumes = @(
	'seaweedfs-data'
)

$FluxerSecretKeys = @(
	@{Name = 'POSTGRES_PASSWORD'; Kind = 'hex'}
	@{Name = 'MEILI_MASTER_KEY'; Kind = 'hex'}
	@{Name = 'FLUXER_S3_SECRET_KEY'; Kind = 'hex'}
	@{Name = 'FLUXER_SUDO_MODE_SECRET'; Kind = 'hex'}
	@{Name = 'FLUXER_CONNECTION_INITIATION_SECRET'; Kind = 'hex'}
	@{Name = 'FLUXER_GATEWAY_RPC_AUTH_TOKEN'; Kind = 'hex'}
	@{Name = 'FLUXER_ERLANG_COOKIE'; Kind = 'hex'}
	@{Name = 'FLUXER_MEDIA_PROXY_SECRET_KEY'; Kind = 'hex'}
	@{Name = 'FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64'; Kind = 'base64'}
	@{Name = 'FLUXER_ADMIN_SECRET_KEY_BASE'; Kind = 'hex'}
	@{Name = 'FLUXER_ADMIN_OAUTH_CLIENT_SECRET'; Kind = 'hex'}
	@{Name = 'LIVEKIT_API_SECRET'; Kind = 'hex'}
	@{Name = 'FLUXER_VAPID_PUBLIC_KEY'; Kind = 'vapid_public'}
	@{Name = 'FLUXER_VAPID_PRIVATE_KEY'; Kind = 'vapid_private'}
)

$FluxerNonSecretKeys = @(
	@{Name = 'FLUXER_DOMAIN'; Kind = 'domain'; Value = ''}
	@{Name = 'FLUXER_PUBLIC_SCHEME'; Kind = 'literal'; Value = 'https'}
	@{Name = 'FLUXER_PUBLIC_PORT'; Kind = 'literal'; Value = '443'}
	@{Name = 'FLUXER_VAPID_EMAIL'; Kind = 'email'; Value = ''}
	@{Name = 'FLUXER_IMAGE_TAG'; Kind = 'image_tag'; Value = ''}
	@{Name = 'FLUXER_S3_ACCESS_KEY'; Kind = 'literal'; Value = 'fluxer'}
	@{Name = 'LIVEKIT_API_KEY'; Kind = 'literal'; Value = 'fluxer'}
)

function Write-FluxerLine([string]$Message) {
	Write-Host $Message
}

function Write-FluxerProblem([string]$Message) {
	[Console]::Error.WriteLine($Message)
}

function Stop-Fluxer([string]$Message, [int]$Code) {
	Write-FluxerProblem $Message
	exit $Code
}

function Show-FluxerUsage {
	Write-FluxerLine 'Usage: install.ps1 -Domain <host> -Email <address> [options]'
	Write-FluxerLine '       install.ps1 -Update [options]'
	Write-FluxerLine '       install.ps1 -Rollback [options]'
	Write-FluxerLine ''
	Write-FluxerLine 'Options:'
	Write-FluxerLine '  -Domain <host>          Hostname the instance answers on. Prompted when absent.'
	Write-FluxerLine '  -Email <address>        Address written as FLUXER_VAPID_EMAIL. Prompted when absent.'
	Write-FluxerLine '  -Dir <path>             Working directory. Default: the fluxer folder in the home directory.'
	Write-FluxerLine '  -Ref <git ref>          Ref the stack files come from. Default: the image tag, and main when that tag is v1 or latest.'
	Write-FluxerLine '  -ImageTag <tag>         Value written as FLUXER_IMAGE_TAG. Default: v1.'
	Write-FluxerLine '  -Tls <bundled|proxy>    Certificate mode. Default: bundled.'
	Write-FluxerLine '  -EdgeBind <addr:port>   Plain HTTP bind under -Tls proxy. Default: 127.0.0.1:8080.'
	Write-FluxerLine '  -NonInteractive         Never prompt. A missing required value exits 1.'
	Write-FluxerLine '  -DryRun                 Print the plan. Change nothing.'
	Write-FluxerLine '  -NoStart                Write everything. Skip docker compose up -d.'
	Write-FluxerLine '  -Update                 Upgrade: record, back up, refresh, pull, recreate, verify.'
	Write-FluxerLine '  -Rollback               Restore the images and stack files of the last record.'
	Write-FluxerLine '  -BackupDir <path>       Where records go. Default: the backups folder under -Dir.'
	Write-FluxerLine '  -NoVolumeBackup         Take the database dump and skip the uploads copy.'
	Write-FluxerLine '  -SkipBackupAcceptDataLoss'
	Write-FluxerLine '                          Upgrade with no backup at all. Losable data is lost.'
	Write-FluxerLine '  -Help                   Print this text.'
	Write-FluxerLine ''
	Write-FluxerLine 'Exit codes: 0 success, 1 usage, 2 prerequisite, 3 refused, 4 download, 5 secret,'
	Write-FluxerLine '6 unhealthy, 7 backup.'
}

function Test-FluxerWindowsHost {
	if ($PSVersionTable.PSVersion.Major -lt 6) {
		return $true
	}
	return $IsWindows
}

function ConvertTo-FluxerVersion([string]$Text) {
	$match = [regex]::Match($Text, '(\d+)\.(\d+)(?:\.(\d+))?')
	if (-not $match.Success) {
		return $null
	}
	$patch = 0
	if ($match.Groups[3].Success) {
		$patch = [int]$match.Groups[3].Value
	}
	return @([int]$match.Groups[1].Value, [int]$match.Groups[2].Value, $patch)
}

function Test-FluxerVersionAtLeast([string]$Found, [string]$Minimum) {
	$left = ConvertTo-FluxerVersion $Found
	$right = ConvertTo-FluxerVersion $Minimum
	if ($null -eq $left) {
		return $false
	}
	for ($index = 0; $index -lt 3; $index++) {
		if ($left[$index] -gt $right[$index]) {
			return $true
		}
		if ($left[$index] -lt $right[$index]) {
			return $false
		}
	}
	return $true
}

function Invoke-FluxerCapture([string[]]$CommandArgs) {
	$previous = $ErrorActionPreference
	$ErrorActionPreference = 'Continue'
	$text = ''
	$code = 0
	try {
		$output = & docker @CommandArgs 2>$null
		$code = $LASTEXITCODE
		if ($null -ne $output) {
			$text = (@($output) | ForEach-Object {[string]$_}) -join "`n"
		}
	} finally {
		$ErrorActionPreference = $previous
	}
	return @{Code = $code; Text = $text.Trim()}
}

function Invoke-FluxerDocker([string[]]$CommandArgs) {
	$previous = $ErrorActionPreference
	$ErrorActionPreference = 'Continue'
	$code = 0
	try {
		& docker @CommandArgs
		$code = $LASTEXITCODE
	} finally {
		$ErrorActionPreference = $previous
	}
	return $code
}

# pg_dump writes a binary stream. PowerShell redirection decodes that stream as text and corrupts
# it, so the bytes go to the file through the process itself.
function Invoke-FluxerDockerToFile([string[]]$CommandArgs, [string]$OutFile, [string]$WorkingDir) {
	$errorFile = "$OutFile.stderr"
	$process = Start-Process -FilePath 'docker' -ArgumentList $CommandArgs -RedirectStandardOutput $OutFile -RedirectStandardError $errorFile -WorkingDirectory $WorkingDir -NoNewWindow -Wait -PassThru
	$code = $process.ExitCode
	if (Test-Path -LiteralPath $errorFile) {
		Remove-Item -LiteralPath $errorFile -Force
	}
	return $code
}

function Test-FluxerWsl2 {
	$command = Get-Command wsl.exe -ErrorAction SilentlyContinue
	if ($null -eq $command) {
		return $false
	}
	$previous = $ErrorActionPreference
	$ErrorActionPreference = 'Continue'
	$code = 1
	try {
		& wsl.exe --status 2>&1 | Out-Null
		$code = $LASTEXITCODE
	} finally {
		$ErrorActionPreference = $previous
	}
	return ($code -eq 0)
}

function Invoke-FluxerPreflight {
	if ($PSVersionTable.PSVersion.Major -lt 5) {
		Stop-Fluxer 'This installer needs Windows PowerShell 5.1 or PowerShell 7 or newer.' $FluxerExitPrerequisite
	}
	if (-not (Test-FluxerWindowsHost)) {
		Stop-Fluxer 'This installer runs on Windows. On Linux and macOS run install.sh instead.' $FluxerExitPrerequisite
	}
	if ($PSVersionTable.PSVersion.Major -lt 6) {
		[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]::Tls12
	}
	if ($null -eq (Get-Command docker -ErrorAction SilentlyContinue)) {
		Stop-Fluxer 'docker is not on PATH. Install Docker Desktop with the WSL2 backend.' $FluxerExitPrerequisite
	}
	$engine = Invoke-FluxerCapture @('version', '--format', '{{.Server.Version}}')
	if ($engine.Code -ne 0) {
		Stop-Fluxer 'The Docker daemon does not answer. Start Docker Desktop and run this script again.' $FluxerExitPrerequisite
	}
	$compose = Invoke-FluxerCapture @('compose', 'version', '--short')
	if ($compose.Code -ne 0) {
		Stop-Fluxer 'The Docker Compose v2 plugin is missing. Docker Desktop ships it.' $FluxerExitPrerequisite
	}
	if (-not (Test-FluxerVersionAtLeast $engine.Text $FluxerMinimumEngineVersion)) {
		Stop-Fluxer "Docker Engine $($engine.Text) is older than $FluxerMinimumEngineVersion. Compose is $($compose.Text)." $FluxerExitPrerequisite
	}
	if (-not (Test-FluxerVersionAtLeast $compose.Text $FluxerMinimumComposeVersion)) {
		Stop-Fluxer "Docker Compose $($compose.Text) is older than $FluxerMinimumComposeVersion. Engine is $($engine.Text)." $FluxerExitPrerequisite
	}
	$osType = Invoke-FluxerCapture @('info', '--format', '{{.OSType}}')
	if ($osType.Code -ne 0) {
		Stop-Fluxer 'The Docker daemon does not report its container platform.' $FluxerExitPrerequisite
	}
	if ($osType.Text -ne 'linux') {
		Stop-Fluxer "Docker runs $($osType.Text) containers. Switch Docker Desktop to Linux containers." $FluxerExitPrerequisite
	}
	Write-FluxerLine "Docker Engine $($engine.Text) and Compose $($compose.Text) are ready."
	if (-not (Test-FluxerWsl2)) {
		Write-FluxerLine 'WSL 2 is not present. Docker Desktop with the WSL2 backend is the supported Windows setup.'
	}
}

function Assert-FluxerDomain([string]$Value) {
	if ($Value -cmatch '^[a-z0-9]([a-z0-9-]*[a-z0-9])?(\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+$') {
		return
	}
	Stop-Fluxer "-Domain must be a lowercase hostname such as chat.example.com. Got: $Value" $FluxerExitUsage
}

function Assert-FluxerEmail([string]$Value) {
	if ($Value -match '^[^@\s]+@[^@\s]+$') {
		return
	}
	Stop-Fluxer "-Email must be one address such as you@example.com. Got: $Value" $FluxerExitUsage
}

function Get-FluxerRefForTag([string]$Tag) {
	if ($Tag -eq 'v1' -or $Tag -eq 'latest') {
		return 'main'
	}
	return $Tag
}

# The images come from FLUXER_IMAGE_TAG and the stack files come from a git ref. A release tags its
# images and its commit with the same CalVer string, so a pinned tag names the commit that carries
# its compose files. The moving tags v1 and latest track main.
function Assert-FluxerDerivedRef([string]$Value, [string]$EnvPath) {
	if ($Value.Length -eq 0) {
		Stop-Fluxer "$EnvPath declares no FLUXER_IMAGE_TAG, so no ref can be derived. Pass -Ref." $FluxerExitRefused
	}
	if ($Value -match '\s' -or $Value -match '(^|/)\.\.(/|$)') {
		Stop-Fluxer "FLUXER_IMAGE_TAG is $Value, which is not a git ref. Pass -Ref." $FluxerExitRefused
	}
}

function Assert-FluxerRef([string]$Value) {
	if ($Value.Length -eq 0) {
		return
	}
	if ($Value -match '\s') {
		Stop-Fluxer '-Ref must not contain whitespace.' $FluxerExitUsage
	}
	if ($Value -match '(^|/)\.\.(/|$)') {
		Stop-Fluxer '-Ref must not contain a parent segment.' $FluxerExitUsage
	}
}

function Assert-FluxerEdgeBind([string]$Value) {
	if ($Value -match '^\S+:\d{1,5}$') {
		return
	}
	Stop-Fluxer "-EdgeBind must be an address and port such as 127.0.0.1:8080. Got: $Value" $FluxerExitUsage
}

function Read-FluxerValue([string]$Prompt) {
	for ($attempt = 0; $attempt -lt 3; $attempt++) {
		$answer = Read-Host -Prompt $Prompt
		if ($null -ne $answer) {
			$answer = $answer.Trim()
			if ($answer.Length -gt 0) {
				return $answer
			}
		}
	}
	Stop-Fluxer 'Three empty answers. Nothing was written.' $FluxerExitUsage
}

function Resolve-FluxerValue([string]$Value, [string]$Prompt, [string]$Flag, [bool]$AllowPrompt) {
	if ($Value.Length -gt 0) {
		return $Value
	}
	if (-not $AllowPrompt) {
		Stop-Fluxer "$Flag is required." $FluxerExitUsage
	}
	return Read-FluxerValue $Prompt
}

function New-FluxerRandomBytes([int]$Count) {
	$bytes = New-Object byte[] $Count
	$generator = [System.Security.Cryptography.RandomNumberGenerator]::Create()
	try {
		$generator.GetBytes($bytes)
	} finally {
		$generator.Dispose()
	}
	return ,$bytes
}

function ConvertTo-FluxerHex([byte[]]$Bytes) {
	return [System.BitConverter]::ToString($Bytes).Replace('-', '').ToLowerInvariant()
}

function ConvertTo-FluxerBase64Url([byte[]]$Bytes) {
	$text = [System.Convert]::ToBase64String($Bytes)
	return $text.TrimEnd('=').Replace('+', '-').Replace('/', '_')
}

# The VAPID pair is the one secret in .env that no random draw produces.
# FLUXER_VAPID_PUBLIC_KEY is base64url of the 65-byte uncompressed P-256 point and
# FLUXER_VAPID_PRIVATE_KEY is base64url of its 32-byte scalar. The stack requires both even when
# nobody enables browser notifications. The private key is 43 characters and the public key is 87.
#
# By hand on a host with openssl:
#   openssl ecparam -name prime256v1 -genkey -noout -out vapid.pem
#   openssl ec -in vapid.pem -outform DER | od -An -tx1 -N7 | tr -d ' \n'
#   The line above must print 30770201010420. Any other value means a short
#   scalar, so delete vapid.pem and draw again before going on.
#   openssl ec -in vapid.pem -outform DER | tail -c +8 | head -c 32 | openssl base64 -A | tr '+/' '-_' | tr -d '='
#   openssl ec -in vapid.pem -pubout -outform DER | tail -c 65 | openssl base64 -A | tr '+/' '-_' | tr -d '='
function New-FluxerVapidPair {
	for ($attempt = 0; $attempt -lt $FluxerVapidAttempts; $attempt++) {
		$key = $null
		$parameters = $null
		try {
			$curve = [System.Security.Cryptography.ECCurve]::CreateFromFriendlyName('nistP256')
			$key = [System.Security.Cryptography.ECDsa]::Create($curve)
			$parameters = $key.ExportParameters($true)
		} catch {
			Stop-Fluxer 'The .NET P-256 provider is unavailable. Install .NET Framework 4.7.2 or newer, or run this script under PowerShell 7.' $FluxerExitSecret
		} finally {
			if ($null -ne $key) {
				$key.Dispose()
			}
		}
		if ($parameters.D.Length -ne 32) {
			continue
		}
		if ($parameters.Q.X.Length -ne 32) {
			continue
		}
		if ($parameters.Q.Y.Length -ne 32) {
			continue
		}
		$point = New-Object byte[] 65
		$point[0] = 4
		[System.Array]::Copy($parameters.Q.X, 0, $point, 1, 32)
		[System.Array]::Copy($parameters.Q.Y, 0, $point, 33, 32)
		$public = ConvertTo-FluxerBase64Url $point
		$private = ConvertTo-FluxerBase64Url $parameters.D
		if ($public.Length -ne 87) {
			continue
		}
		if ($private.Length -ne 43) {
			continue
		}
		return @{Public = $public; Private = $private}
	}
	Stop-Fluxer "The P-256 provider returned $FluxerVapidAttempts keys of the wrong size. Nothing was written." $FluxerExitSecret
}

function ConvertTo-FluxerLfFile([string]$Path) {
	$bytes = [System.IO.File]::ReadAllBytes($Path)
	$output = New-Object System.Collections.Generic.List[byte]
	for ($index = 0; $index -lt $bytes.Length; $index++) {
		if ($bytes[$index] -eq 13) {
			if (($index + 1) -lt $bytes.Length) {
				if ($bytes[$index + 1] -eq 10) {
					continue
				}
			}
		}
		$output.Add($bytes[$index])
	}
	if ($output.Count -eq $bytes.Length) {
		return
	}
	[System.IO.File]::WriteAllBytes($Path, $output.ToArray())
}

function Set-FluxerPrivateFile([string]$Path) {
	$identity = [System.Security.Principal.WindowsIdentity]::GetCurrent()
	$acl = Get-Acl -Path $Path
	$acl.SetAccessRuleProtection($true, $false)
	foreach ($rule in @($acl.Access)) {
		[void]$acl.RemoveAccessRule($rule)
	}
	$owner = New-Object System.Security.AccessControl.FileSystemAccessRule($identity.User, 'FullControl', 'Allow')
	$acl.AddAccessRule($owner)
	Set-Acl -Path $Path -AclObject $acl
}

# A record holds a copy of .env, so the directory itself is closed to everyone but the account
# that took it.
function Set-FluxerPrivateDirectory([string]$Path) {
	$identity = [System.Security.Principal.WindowsIdentity]::GetCurrent()
	$acl = Get-Acl -Path $Path
	$acl.SetAccessRuleProtection($true, $false)
	foreach ($rule in @($acl.Access)) {
		[void]$acl.RemoveAccessRule($rule)
	}
	$owner = New-Object System.Security.AccessControl.FileSystemAccessRule($identity.User, 'FullControl', 'ContainerInherit, ObjectInherit', 'None', 'Allow')
	$acl.AddAccessRule($owner)
	Set-Acl -Path $Path -AclObject $acl
}

function Remove-FluxerTemporary([string]$Path) {
	if (Test-Path -LiteralPath $Path) {
		Remove-Item -LiteralPath $Path -Force
	}
}

function New-FluxerStagingDirectory([string]$Parent) {
	$staging = Join-Path $Parent ".fluxer-install.$PID"
	if (Test-Path -LiteralPath $staging) {
		Remove-Item -LiteralPath $staging -Recurse -Force
	}
	New-Item -ItemType Directory -Path $staging -Force | Out-Null
	return $staging
}

function Remove-FluxerStagingDirectory([string]$Path) {
	if ($Path.Length -gt 0 -and (Test-Path -LiteralPath $Path)) {
		Remove-Item -LiteralPath $Path -Recurse -Force
	}
}

# By hand, for one file:
#   Invoke-WebRequest -Uri https://raw.githubusercontent.com/fluxerapp/fluxer/main/deploy/self-hosting/docker-compose.yml -OutFile docker-compose.yml
#
# The files come from a git ref and the images come from FLUXER_IMAGE_TAG. The ref is derived from
# the tag unless -Ref names one, which is the pairing rule that stops a compose file from asking for
# a variable the running images do not read.
#
# Everything lands in a staging directory first, so a failed download leaves the working directory
# on the set it already had, and so the upgrade can compare old against new before replacing.
function Get-FluxerStackFiles([string]$StagingDir, [string]$RefValue) {
	$base = "$FluxerRawBase/$RefValue/$FluxerStackPath"
	foreach ($name in $FluxerStackFiles) {
		$destination = Join-Path $StagingDir $name
		try {
			Invoke-WebRequest -Uri "$base/$name" -OutFile $destination -UseBasicParsing -MaximumRedirection 5 -TimeoutSec 120
		} catch {
			Stop-Fluxer "Download failed: $base/$name. Pass -Ref to name the ref the stack files come from." $FluxerExitDownload
		}
		if ((Get-Item -LiteralPath $destination).Length -eq 0) {
			Stop-Fluxer "Download returned an empty file: $name" $FluxerExitDownload
		}
		ConvertTo-FluxerLfFile $destination
		if ($name -eq 'docker-compose.yml') {
			$content = [System.IO.File]::ReadAllText($destination)
			if ($content -notmatch '(?m)^services:') {
				Stop-Fluxer "docker-compose.yml from $RefValue holds no services block." $FluxerExitDownload
			}
		}
		Write-FluxerLine "Downloaded $name"
	}
}

# None of these files is part of an image, and all four are read from the working directory, so
# docker compose pull never updates any of them. That is why an upgrade refreshes them itself.
#
# A refreshed docker-compose.yml can declare a variable the running .env does not carry. Compose
# writes ${NAME:?message} for a variable the stack requires and stops with that message until .env
# sets it, and ${NAME:-default} for one that needs nothing from the operator. Every optional
# override ships commented out in .env.example, so a new required key is the only kind that asks
# for an edit.
function Move-FluxerStackFiles([string]$StagingDir, [string]$TargetDir) {
	foreach ($name in $FluxerStackFiles) {
		Move-Item -LiteralPath (Join-Path $StagingDir $name) -Destination (Join-Path $TargetDir $name) -Force
	}
}

# The same file by hand, which is what the caller of this function does in one pass:
#
#   Copy-Item .env.example .env
#
# Close .env to every account but your own, then set FLUXER_DOMAIN and FLUXER_VAPID_EMAIL, the two
# values only the operator knows. The five other non-secret keys in the list above ship correct in
# .env.example and need no edit.
#
# Every secret in .env.example carries the literal CHANGE_ME. A key whose name ends in _BASE64
# takes 32 random bytes as base64, every other key takes 32 random bytes as hex, and the VAPID pair
# comes from the generator above.
#
# Under -Tls proxy, COMPOSE_FILE and FLUXER_EDGE_BIND already sit in .env.example as commented
# lines, so by hand they are uncommented rather than added.
function Write-FluxerEnvFile([string]$Path, [string[]]$Lines) {
	$temporary = "$Path.new"
	Remove-FluxerTemporary $temporary
	$encoding = New-Object System.Text.UTF8Encoding($false)
	[System.IO.File]::WriteAllText($temporary, '', $encoding)
	Set-FluxerPrivateFile $temporary
	$text = ($Lines -join "`n") + "`n"
	[System.IO.File]::WriteAllText($temporary, $text, $encoding)
	Move-Item -LiteralPath $temporary -Destination $Path -Force
}

function Get-FluxerEnvLines([string]$EnvPath) {
	$text = [System.IO.File]::ReadAllText($EnvPath)
	return $text.Replace("`r`n", "`n").TrimEnd("`n").Split("`n")
}

function Get-FluxerEnvValue([string]$EnvPath, [string]$Name) {
	foreach ($line in Get-FluxerEnvLines $EnvPath) {
		if ($line.StartsWith("$Name=")) {
			return $line.Substring($Name.Length + 1)
		}
	}
	return ''
}

function Get-FluxerProperty($Row, [string]$Name) {
	$property = $Row.PSObject.Properties[$Name]
	if ($null -eq $property) {
		return ''
	}
	if ($null -eq $property.Value) {
		return ''
	}
	return [string]$property.Value
}

function Get-FluxerComposeRows {
	$result = Invoke-FluxerCapture @('compose', 'ps', '--all', '--format', 'json')
	if ($result.Code -ne 0) {
		return @()
	}
	$text = $result.Text
	if ($text.Length -eq 0) {
		return @()
	}
	$rows = @()
	if ($text.StartsWith('[')) {
		$rows = @($text | ConvertFrom-Json)
		return $rows
	}
	foreach ($line in $text.Split("`n")) {
		$candidate = $line.Trim()
		if ($candidate.Length -eq 0) {
			continue
		}
		$rows += ($candidate | ConvertFrom-Json)
	}
	return $rows
}

function Measure-FluxerReadyRows($Rows) {
	$ready = 0
	foreach ($row in $Rows) {
		$service = Get-FluxerProperty $row 'Service'
		$state = Get-FluxerProperty $row 'State'
		$health = Get-FluxerProperty $row 'Health'
		if ($service -eq $FluxerInitService) {
			if ($state -eq 'exited') {
				if ((Get-FluxerProperty $row 'ExitCode') -eq '0') {
					$ready++
				}
			}
			continue
		}
		if ($state -ne 'running') {
			continue
		}
		if ($health.Length -gt 0) {
			if ($health -ne 'healthy') {
				continue
			}
		}
		$ready++
	}
	return $ready
}

# Readiness comes from Compose state, which is local and authoritative.
#
# By hand:
#   docker compose ps
#
# Every service reads running or healthy except seaweedfs-init, which reads exited (0) because it
# is a one-shot bucket initialiser.
function Wait-FluxerStack([string]$Lead) {
	Write-FluxerLine $Lead
	$deadline = (Get-Date).AddSeconds($FluxerReadyTimeoutSeconds)
	$reportAt = (Get-Date).AddSeconds($FluxerReadyReportSeconds)
	while ((Get-Date) -lt $deadline) {
		$rows = @(Get-FluxerComposeRows)
		if ($rows.Count -gt 0) {
			$ready = Measure-FluxerReadyRows $rows
			if ($ready -eq $rows.Count) {
				Write-FluxerLine "All $($rows.Count) services report healthy."
				return
			}
			if ((Get-Date) -ge $reportAt) {
				Write-FluxerLine "$ready of $($rows.Count) services are ready."
				$reportAt = (Get-Date).AddSeconds($FluxerReadyReportSeconds)
			}
		}
		Start-Sleep -Seconds $FluxerReadyIntervalSeconds
	}
	Stop-Fluxer "The stack did not report healthy within $FluxerReadyTimeoutSeconds seconds. Read docker compose ps and docker compose logs." $FluxerExitUnhealthy
}

# The public probe is informational. A host behind hairpin NAT cannot always reach its own
# hostname, and a false failure there would be worse than no probe.
function Test-FluxerPublicHealth([string]$DomainValue) {
	$url = "https://$DomainValue$FluxerHealthPath"
	try {
		$response = Invoke-WebRequest -Uri $url -UseBasicParsing -TimeoutSec 20
		Write-FluxerLine "$url returned $([int]$response.StatusCode)."
	} catch {
		Write-FluxerLine "$url did not answer with 200. Confirm the DNS record for $DomainValue and that ports 80 and 443 reach this host."
	}
}

function Get-FluxerComposeProject([string]$TargetDir) {
	if ($null -ne $env:COMPOSE_PROJECT_NAME -and $env:COMPOSE_PROJECT_NAME.Length -gt 0) {
		return $env:COMPOSE_PROJECT_NAME
	}
	foreach ($line in [System.IO.File]::ReadAllText((Join-Path $TargetDir 'docker-compose.yml')).Split("`n")) {
		if ($line -match '^name:\s*(\S+)') {
			return $Matches[1]
		}
	}
	return ''
}

# The image references the stack resolves to, deduplicated. Compose expands them from
# docker-compose.yml and .env, so this is the same resolution docker compose pull performs. It
# names the images, not the versions of them.
#
# By hand:
#   docker compose config --images
function Get-FluxerComposeImages {
	$result = Invoke-FluxerCapture @('compose', 'config', '--images')
	if ($result.Code -ne 0 -or $result.Text.Length -eq 0) {
		return @()
	}
	$refs = @()
	foreach ($line in $result.Text.Split("`n")) {
		$candidate = $line.Trim()
		if ($candidate.Length -gt 0) {
			$refs += $candidate
		}
	}
	return @($refs | Sort-Object -Unique)
}

function Get-FluxerImageId([string]$Reference) {
	$result = Invoke-FluxerCapture @('image', 'inspect', '--format', '{{.Id}}', $Reference)
	if ($result.Code -ne 0) {
		return ''
	}
	return $result.Text
}

# The image ID each container was actually created from, against the reference Compose created it
# under.
#
# docker image inspect <reference> answers a different question: what that tag points at on this
# host now. The two disagree whenever a newer image already sits under a moving tag, which is the
# state an upgrade that pulled and then failed leaves behind, and that is the state the operator
# re-runs -Update from. Reading the tag there records the image the stack is about to move to as
# the image it is moving from, and the rollback that follows puts back what is already running
# while reporting success.
#
# By hand:
#   docker compose ps -aq | xargs docker inspect --format '{{.Config.Image}} {{.Image}}'
function Get-FluxerRunningImageIds {
	$ids = Invoke-FluxerCapture @('compose', 'ps', '-aq')
	$map = @{}
	if ($ids.Code -ne 0 -or $ids.Text.Length -eq 0) {
		return $map
	}
	foreach ($container in $ids.Text.Split("`n")) {
		$candidate = $container.Trim()
		if ($candidate.Length -eq 0) {
			continue
		}
		$row = Invoke-FluxerCapture @('inspect', '--format', '{{.Config.Image}} {{.Image}}', $candidate)
		if ($row.Code -ne 0) {
			continue
		}
		$parts = $row.Text.Trim().Split(' ')
		if ($parts.Count -lt 2) {
			continue
		}
		if (-not $map.ContainsKey($parts[0])) {
			$map[$parts[0]] = $parts[1]
		}
	}
	return $map
}

function Get-FluxerRunningImageId($Running, [string]$Reference) {
	if ($null -ne $Running -and $Running.ContainsKey($Reference)) {
		return [string]$Running[$Reference]
	}
	return ''
}

function Get-FluxerPostgresMajor([string]$Path) {
	if (-not (Test-Path -LiteralPath $Path)) {
		return ''
	}
	foreach ($line in [System.IO.File]::ReadAllText($Path).Split("`n")) {
		if ($line -match '^\s*image:\s*postgres:(\d+)') {
			return $Matches[1]
		}
	}
	return ''
}

function Test-FluxerSameFile([string]$Left, [string]$Right) {
	if (-not (Test-Path -LiteralPath $Left)) {
		return $false
	}
	if (-not (Test-Path -LiteralPath $Right)) {
		return $false
	}
	return ((Get-FileHash -LiteralPath $Left -Algorithm SHA256).Hash -eq (Get-FileHash -LiteralPath $Right -Algorithm SHA256).Hash)
}

# Which mounted files the refresh changes, and therefore which services need a restart rather than
# a recreate. The comparison happens while the new copies are still staged, because once they are
# in place the old bytes are gone.
function Get-FluxerChangedMounts([string]$TargetDir, [string]$StagingDir) {
	$changed = @()
	foreach ($entry in $FluxerMountedFiles) {
		$current = Join-Path $TargetDir $entry.Name
		$fresh = Join-Path $StagingDir $entry.Name
		if (-not (Test-Path -LiteralPath $current)) {
			continue
		}
		if (-not (Test-FluxerSameFile $current $fresh)) {
			$changed += $entry
		}
	}
	return @($changed)
}

# The service names the docker-compose.yml in place defines.
#
# A rollback puts back the stack files the record holds, and a record taken before a service was
# renamed names the old service. Restarting a service the file in place does not define fails, so
# the name is checked first.
#
# By hand:
#   docker compose config --services
function Get-FluxerComposeServices {
	$result = Invoke-FluxerCapture @('compose', 'config', '--services')
	if ($result.Code -ne 0 -or $result.Text.Length -eq 0) {
		return @()
	}
	$services = @()
	foreach ($line in $result.Text.Split("`n")) {
		$candidate = $line.Trim()
		if ($candidate.Length -gt 0) {
			$services += $candidate
		}
	}
	return @($services)
}

function Restart-FluxerMounts($Entries) {
	if (@($Entries).Count -eq 0) {
		return
	}
	$services = Get-FluxerComposeServices
	foreach ($entry in $Entries) {
		if ($services -notcontains $entry.Service) {
			Write-FluxerLine "Skipping the restart of $($entry.Service), because the docker-compose.yml in place defines no service by that name."
			continue
		}
		Write-FluxerLine "Restarting $($entry.Service), because $($entry.Name) is mounted into it and up -d does not reload a mounted file."
		$code = Invoke-FluxerDocker @('compose', 'restart', $entry.Service)
		if ($code -ne 0) {
			Stop-Fluxer "docker compose restart $($entry.Service) failed." $FluxerExitUnhealthy
		}
	}
}

# A record is created before the upgrade touches anything, so a run that refuses or fails after
# this point still leaves one behind. Such a record describes the state the instance is already
# in, which makes a rollback to it a no-op rather than a mistake. Only the newest record is ever
# used.
function New-FluxerRecord([string]$BackupRoot) {
	if (-not (Test-Path -LiteralPath $BackupRoot)) {
		New-Item -ItemType Directory -Path $BackupRoot -Force | Out-Null
	}
	Set-FluxerPrivateDirectory $BackupRoot
	$stamp = (Get-Date).ToUniversalTime().ToString('yyyyMMddTHHmmssZ')
	$record = Join-Path $BackupRoot "$FluxerRecordPrefix$stamp"
	if (Test-Path -LiteralPath $record) {
		Stop-Fluxer "$record exists already." $FluxerExitRefused
	}
	New-Item -ItemType Directory -Path $record -Force | Out-Null
	Set-FluxerPrivateDirectory $record
	return $record
}

function Get-FluxerRecordTag([string]$Record) {
	$path = Join-Path $Record $FluxerTagFile
	if (-not (Test-Path -LiteralPath $path)) {
		return ''
	}
	$lines = @(Get-Content -LiteralPath $path -TotalCount 1)
	if ($lines.Count -eq 0 -or $null -eq $lines[0]) {
		return ''
	}
	return ([string]$lines[0]).Trim()
}

function Get-FluxerNewestRecord([string]$BackupRoot) {
	if (-not (Test-Path -LiteralPath $BackupRoot)) {
		return ''
	}
	$records = @(Get-ChildItem -LiteralPath $BackupRoot -Directory -ErrorAction SilentlyContinue | Where-Object {$_.Name.StartsWith($FluxerRecordPrefix)} | Sort-Object -Property Name)
	if ($records.Count -eq 0) {
		return ''
	}
	return $records[$records.Count - 1].FullName
}

function Write-FluxerTextFile([string]$Path, [string[]]$Lines) {
	$encoding = New-Object System.Text.UTF8Encoding($false)
	$text = ($Lines -join "`n") + "`n"
	[System.IO.File]::WriteAllText($Path, $text, $encoding)
}

# Step 1 of an upgrade: record what is running before anything moves.
#
# FLUXER_IMAGE_TAG defaults to v1, which tracks the latest compatible release. The tag therefore
# reads v1 before and after the upgrade, and the image ID is the only thing that tells two
# releases apart. That is what makes this record the version history the stack does not otherwise
# keep, and what makes a rollback possible on a moving tag.
#
# The reference list comes from Compose and the ID under each reference comes from the container
# running it, for the reason in Get-FluxerRunningImageIds. A reference no container carries is
# recorded as `-`, which a rollback skips, because a version that was not running is not a version
# to go back to.
#
# By hand:
#   docker compose images
function Save-FluxerVersionRecord([string]$Record, [string]$EnvPath) {
	Write-FluxerTextFile (Join-Path $Record $FluxerTagFile) @((Get-FluxerEnvValue $EnvPath 'FLUXER_IMAGE_TAG'))
	$refs = @(Get-FluxerComposeImages)
	if ($refs.Count -eq 0) {
		Stop-Fluxer 'docker compose config --images returned nothing, so the running version cannot be recorded.' $FluxerExitPrerequisite
	}
	$running = Get-FluxerRunningImageIds
	$lines = @()
	foreach ($reference in $refs) {
		$id = Get-FluxerRunningImageId $running $reference
		if ($id.Length -eq 0) {
			$id = '-'
		}
		$lines += "$reference $id"
	}
	Write-FluxerTextFile (Join-Path $Record $FluxerImagesFile) $lines
	Write-FluxerLine "Recorded $($lines.Count) image references in $Record."
}

# .env and the stack files go into the record because nothing else regenerates them. .env holds
# every secret the instance was built with, so the record directory is closed to the current
# account alone, and the record belongs wherever the operator already keeps secrets.
function Save-FluxerCurrentFiles([string]$Record, [string]$TargetDir, [string]$EnvPath) {
	$envCopy = Join-Path $Record '.env'
	Copy-Item -LiteralPath $EnvPath -Destination $envCopy -Force
	Set-FluxerPrivateFile $envCopy
	foreach ($name in $FluxerStackFiles) {
		$source = Join-Path $TargetDir $name
		if (Test-Path -LiteralPath $source) {
			Copy-Item -LiteralPath $source -Destination (Join-Path $Record $name) -Force
		}
	}
}

function Test-FluxerPostgresRunning {
	$result = Invoke-FluxerCapture @('compose', 'ps', '-q', 'postgres')
	if ($result.Code -ne 0 -or $result.Text.Length -eq 0) {
		return $false
	}
	$id = $result.Text.Split("`n")[0].Trim()
	if ($id.Length -eq 0) {
		return $false
	}
	$state = Invoke-FluxerCapture @('inspect', '--format', '{{.State.Status}}', $id)
	return ($state.Code -eq 0 -and $state.Text -eq 'running')
}

function Test-FluxerDumpHeader([string]$Path) {
	$stream = [System.IO.File]::OpenRead($Path)
	try {
		$buffer = New-Object byte[] 5
		if ($stream.Read($buffer, 0, 5) -ne 5) {
			return $false
		}
		return ([System.Text.Encoding]::ASCII.GetString($buffer) -eq 'PGDMP')
	} finally {
		$stream.Dispose()
	}
}

# Step 2 of an upgrade, and the part that gates the rest.
#
# api, worker, users-shard and messages-shard apply schema changes while they start, and starting
# an older image does not undo them. The dump taken before the pull is the only way back across a
# schema change.
#
# By hand:
#   docker compose exec -T postgres pg_dump -U fluxer -d fluxer --format=custom > backups\fluxer.dump
#
# The database and the role are both named fluxer and are fixed in docker-compose.yml. Keep -T.
# Without it Docker attaches a terminal to the command and the dump arrives corrupted, which is
# why the first five bytes are checked against the custom-format magic rather than only the size.
#
# The dump runs against the live stack. pg_dump reads inside one transaction, so it sees a
# consistent database without stopping anything. The volume copy below has no equivalent and is
# why the stack stops there and not here.
#
# The volume copy measures its volume and refuses when the disk is short. This step does not,
# because the size of a custom-format dump is not knowable before pg_dump writes it. Point
# -BackupDir at a drive with room for the database.
function Backup-FluxerDatabase([string]$Record, [string]$TargetDir) {
	if (-not (Test-FluxerPostgresRunning)) {
		Write-FluxerLine 'Postgres is not running. Starting it for the dump.'
		if ((Invoke-FluxerDocker @('compose', 'up', '-d', '--wait', 'postgres')) -ne 0) {
			Stop-Fluxer 'Postgres does not start, so no dump can be taken.' $FluxerExitBackup
		}
	}
	$dump = Join-Path $Record $FluxerDumpFile
	Write-FluxerLine 'Dumping the database.'
	$code = Invoke-FluxerDockerToFile @('compose', 'exec', '-T', 'postgres', 'pg_dump', '-U', 'fluxer', '-d', 'fluxer', '--format=custom') $dump $TargetDir
	if ($code -ne 0) {
		Remove-FluxerTemporary $dump
		Stop-Fluxer 'pg_dump failed. The instance is untouched.' $FluxerExitBackup
	}
	if (-not (Test-FluxerDumpHeader $dump)) {
		Remove-FluxerTemporary $dump
		Stop-Fluxer 'The dump does not carry the custom-format header. The instance is untouched.' $FluxerExitBackup
	}
	Write-FluxerLine "Dumped the database to $dump."
}

function Get-FluxerVolumeSizeKb([string]$Volume) {
	$result = Invoke-FluxerCapture @('run', '--rm', '-v', "${Volume}:/data:ro", $FluxerHelperImage, 'du', '-sk', '/data')
	if ($result.Code -ne 0) {
		return -1
	}
	$first = $result.Text.Split("`n")[0].Trim()
	if ($first -match '^(\d+)') {
		return [long]$Matches[1]
	}
	return -1
}

function Get-FluxerFreeKb([string]$Path) {
	$root = [System.IO.Path]::GetPathRoot([System.IO.Path]::GetFullPath($Path))
	$drive = New-Object System.IO.DriveInfo($root)
	return [long]($drive.AvailableFreeSpace / 1024)
}

# Step 2 continued: the volume copy.
#
# A live copy can catch a file mid-write, so the stack stops for it. The stack comes back up on
# the images it was already running before anything else happens, so a failure here leaves a
# working instance rather than a stopped one.
#
# By hand:
#   docker compose stop
#   docker run --rm -v fluxer_seaweedfs-data:/data -v "${PWD}\backups:/backup" alpine tar czf /backup/seaweedfs-data.tgz -C /data .
#   docker compose up -d
function Copy-FluxerVolumes([string]$Record, [string]$Project) {
	foreach ($volume in $FluxerBackupVolumes) {
		$full = "${Project}_$volume"
		$inspect = Invoke-FluxerCapture @('volume', 'inspect', $full)
		if ($inspect.Code -ne 0) {
			Stop-Fluxer "The volume $full does not exist, so the uploads cannot be copied." $FluxerExitBackup
		}
		$size = Get-FluxerVolumeSizeKb $full
		if ($size -lt 0) {
			Stop-Fluxer "Cannot measure the volume $full." $FluxerExitBackup
		}
		$free = Get-FluxerFreeKb $Record
		$need = [long]($size * $FluxerVolumeHeadroomPercent / 100)
		if ($free -lt $need) {
			Stop-Fluxer "$full holds $([long]($size / 1024)) MB and $Record has $([long]($free / 1024)) MB free. Point -BackupDir at a drive with room, or pass -NoVolumeBackup to take the database dump alone." $FluxerExitBackup
		}
	}
	Write-FluxerLine 'Stopping the stack for a consistent copy of the uploads.'
	if ((Invoke-FluxerDocker @('compose', 'stop')) -ne 0) {
		Stop-Fluxer 'docker compose stop failed.' $FluxerExitBackup
	}
	foreach ($volume in $FluxerBackupVolumes) {
		$full = "${Project}_$volume"
		Write-FluxerLine "Copying $full."
		$code = Invoke-FluxerDocker @('run', '--rm', '-v', "${full}:/data:ro", '-v', "${Record}:/backup", $FluxerHelperImage, 'tar', 'czf', "/backup/$volume.tgz", '-C', '/data', '.')
		if ($code -ne 0) {
			[void](Invoke-FluxerDocker @('compose', 'up', '-d', '--remove-orphans'))
			Stop-Fluxer "Copying $full failed. The stack is started again on the images it was running." $FluxerExitBackup
		}
	}
	Write-FluxerLine 'Starting the stack again before the upgrade continues.'
	if ((Invoke-FluxerDocker @('compose', 'up', '-d', '--remove-orphans')) -ne 0) {
		Stop-Fluxer 'docker compose up -d failed after the copy. Read docker compose logs.' $FluxerExitBackup
	}
}

function Backup-FluxerInstance([string]$Record, [string]$TargetDir, [string]$Project) {
	if ($SkipBackupAcceptDataLoss) {
		Write-FluxerLine 'Skipping the backup. -SkipBackupAcceptDataLoss was given, so a schema change has no way back.'
		return
	}
	Backup-FluxerDatabase $Record $TargetDir
	if ($NoVolumeBackup) {
		Write-FluxerLine 'Skipping the uploads copy. -NoVolumeBackup was given.'
		return
	}
	Copy-FluxerVolumes $Record $Project
}

# A newer Postgres major does not read the data directory an older major wrote, so moving between
# majors means dumping, removing the volume that holds the database, and restoring into an empty
# directory. Removing that volume is the one destructive act in the whole procedure, and it is not
# something a script should do on an operator's behalf while they read scrolling output.
#
# The refreshed file is still staged when this runs, so a refusal here leaves the instance exactly
# as it was.
function Assert-FluxerPostgresMajor([string]$TargetDir, [string]$StagingDir) {
	$old = Get-FluxerPostgresMajor (Join-Path $TargetDir 'docker-compose.yml')
	$new = Get-FluxerPostgresMajor (Join-Path $StagingDir 'docker-compose.yml')
	if ($old.Length -eq 0 -or $new.Length -eq 0 -or $old -eq $new) {
		return
	}
	Stop-Fluxer "The refreshed docker-compose.yml pins postgres:$new and this instance runs postgres:$old. A major version change goes through a dump, an empty data directory and a restore, which this script does not do because it destroys the volume holding the database. Nothing was changed. The procedure is at https://fluxer.dev/operator/upgrading/" $FluxerExitRefused
}

# Puts one line of .env back, and proves it touched nothing else.
#
# FLUXER_IMAGE_TAG is the only key either upgrade mode writes. Every other line, which is every
# secret, is compared before the new file replaces the old one, so a rewrite that lost or changed
# a secret cannot land.
function Set-FluxerImageTag([string]$EnvPath, [string]$Tag) {
	$before = Get-FluxerEnvLines $EnvPath
	$found = $false
	$after = @()
	foreach ($line in $before) {
		if ($line.StartsWith('FLUXER_IMAGE_TAG=')) {
			$after += "FLUXER_IMAGE_TAG=$Tag"
			$found = $true
		} else {
			$after += $line
		}
	}
	if (-not $found) {
		Stop-Fluxer "$EnvPath declares no FLUXER_IMAGE_TAG, so the tag cannot be put back. Set it by hand." $FluxerExitRefused
	}
	$keptBefore = @($before | Where-Object {-not $_.StartsWith('FLUXER_IMAGE_TAG=')})
	$keptAfter = @($after | Where-Object {-not $_.StartsWith('FLUXER_IMAGE_TAG=')})
	if ($keptBefore.Count -eq 0 -or ($keptBefore -join "`n") -ne ($keptAfter -join "`n")) {
		Stop-Fluxer 'Rewriting FLUXER_IMAGE_TAG would have changed another line in .env. Nothing was written.' $FluxerExitRefused
	}
	Write-FluxerEnvFile $EnvPath $after
	Write-FluxerLine "FLUXER_IMAGE_TAG in .env is $Tag."
}

function Show-FluxerUpdatePlan([string]$TargetDir, [string]$EnvPath, [string]$BackupRoot) {
	Write-FluxerLine 'Plan: upgrade'
	Write-FluxerLine "  Directory:  $TargetDir"
	Write-FluxerLine "  Ref:        $Ref"
	Write-FluxerLine "  Image tag:  $(Get-FluxerEnvValue $EnvPath 'FLUXER_IMAGE_TAG') from .env"
	Write-FluxerLine "  Backup dir: $BackupRoot"
	Write-FluxerLine '  Running now:'
	$running = Get-FluxerRunningImageIds
	foreach ($reference in Get-FluxerComposeImages) {
		$id = Get-FluxerRunningImageId $running $reference
		if ($id.Length -eq 0) {
			$id = 'no container carries this image'
		}
		Write-FluxerLine "    $reference $id"
	}
	if ($SkipBackupAcceptDataLoss) {
		Write-FluxerLine '  Backup:     none, and a schema change would have no way back'
	} elseif ($NoVolumeBackup) {
		Write-FluxerLine '  Backup:     the database dump, .env, and the stack files'
	} else {
		Write-FluxerLine '  Backup:     the database dump, the uploads volume, .env, and the stack files'
		Write-FluxerLine '  Downtime:   the stack stops for the uploads copy, then again for the recreate'
	}
	# The dry run downloads into a temporary directory so it can name the files that actually
	# change, the services that actually restart, and a Postgres major that would stop the run. It
	# removes that directory before it returns and touches nothing in the working directory.
	$staging = New-FluxerStagingDirectory ([System.IO.Path]::GetTempPath())
	try {
		Get-FluxerStackFiles $staging $Ref
		Write-FluxerLine '  File changes:'
		$changed = 0
		foreach ($name in $FluxerStackFiles) {
			$current = Join-Path $TargetDir $name
			if (-not (Test-Path -LiteralPath $current)) {
				Write-FluxerLine "    $name is new"
				$changed++
			} elseif (Test-FluxerSameFile $current (Join-Path $staging $name)) {
				Write-FluxerLine "    $name is unchanged"
			} else {
				Write-FluxerLine "    $name changes"
				$changed++
			}
		}
		if ($changed -eq 0) {
			Write-FluxerLine "  Note:       ref $Ref moves no stack file"
		}
		$old = Get-FluxerPostgresMajor (Join-Path $TargetDir 'docker-compose.yml')
		$new = Get-FluxerPostgresMajor (Join-Path $staging 'docker-compose.yml')
		if ($old.Length -gt 0 -and $new.Length -gt 0 -and $old -ne $new) {
			Write-FluxerLine "  Refusal:    postgres moves from $old to $new, which this script does not do"
			Write-FluxerLine '  Outcome:    the run stops at that refusal and changes nothing'
			Write-FluxerLine 'Nothing outside a temporary directory was written.'
			return
		}
		foreach ($entry in Get-FluxerChangedMounts $TargetDir $staging) {
			Write-FluxerLine "  Restart:    $($entry.Service), because $($entry.Name) changes and a mounted file survives up -d"
		}
	} finally {
		Remove-FluxerStagingDirectory $staging
	}
	Write-FluxerLine '  Commands:   docker compose pull, docker compose up -d'
	Write-FluxerLine 'Nothing outside a temporary directory was written. Drop -DryRun to run this.'
}

function Show-FluxerRollbackPlan([string]$TargetDir, [string]$EnvPath, [string]$BackupRoot) {
	$record = Get-FluxerNewestRecord $BackupRoot
	if ($record.Length -eq 0) {
		Stop-Fluxer "No record in $BackupRoot. A rollback needs an upgrade that recorded what was running." $FluxerExitPrerequisite
	}
	$recordedTag = Get-FluxerRecordTag $record
	$currentTag = Get-FluxerEnvValue $EnvPath 'FLUXER_IMAGE_TAG'
	Write-FluxerLine 'Plan: rollback'
	Write-FluxerLine "  Directory:  $TargetDir"
	Write-FluxerLine "  Record:     $record"
	if ($recordedTag -eq $currentTag) {
		Write-FluxerLine "  Image tag:  stays $currentTag, so the recorded image IDs move back onto it"
	} else {
		Write-FluxerLine "  Image tag:  $currentTag becomes $recordedTag in .env"
	}
	Write-FluxerLine '  Images:'
	foreach ($line in Get-Content -LiteralPath (Join-Path $record $FluxerImagesFile)) {
		$parts = $line.Trim().Split(' ')
		if ($parts.Count -lt 2) {
			continue
		}
		if ($parts[1] -eq '-') {
			Write-FluxerLine "    $($parts[0]) was not recorded with an ID"
		} elseif ((Get-FluxerImageId $parts[1]).Length -gt 0) {
			Write-FluxerLine "    $($parts[0]) back to $($parts[1])"
		} else {
			Write-FluxerLine "    $($parts[0]) is gone from this host, so $($parts[1]) cannot come back"
		}
	}
	Write-FluxerLine "  Files:      restored from $record"
	Write-FluxerLine '  Database:   stays where the new release left it'
	Write-FluxerLine 'Nothing is written. Drop -DryRun to run this.'
}

# The full upgrade, in the order that leaves a working instance behind at every point it can fail.
function Invoke-FluxerUpgrade([string]$TargetDir, [string]$EnvPath, [string]$BackupRoot, [string]$Project) {
	$record = New-FluxerRecord $BackupRoot
	Save-FluxerVersionRecord $record $EnvPath
	Save-FluxerCurrentFiles $record $TargetDir $EnvPath
	Backup-FluxerInstance $record $TargetDir $Project

	$staging = New-FluxerStagingDirectory $TargetDir
	$changedMounts = @()
	try {
		Get-FluxerStackFiles $staging $Ref
		Assert-FluxerPostgresMajor $TargetDir $staging
		$changedMounts = Get-FluxerChangedMounts $TargetDir $staging
		Move-FluxerStackFiles $staging $TargetDir
	} finally {
		Remove-FluxerStagingDirectory $staging
	}
	Write-FluxerLine "Stack files in $TargetDir are at ref $Ref."

	# The pull runs while the old containers still serve, so the long part of an upgrade costs no
	# downtime.
	#
	# By hand:
	#   docker compose pull
	Write-FluxerLine 'Pulling images.'
	if ((Invoke-FluxerDocker @('compose', 'pull')) -ne 0) {
		Stop-Fluxer 'docker compose pull failed. The stack files are refreshed and the instance still runs the old images.' $FluxerExitDownload
	}
	# Recreates the containers whose image or configuration changed and leaves the rest running.
	#
	# By hand:
	#   docker compose up -d --remove-orphans
	#
	# Behind your own reverse proxy every command names the overlay, which COMPOSE_FILE in .env
	# does once. An invocation without it recreates the edge from the base file, which binds 80 and
	# 443 and requests its own certificate.
	#
	# api, worker, users-shard and messages-shard each apply the database schema while they start,
	# so the upgrade is not finished until all four are back up. All four take the same Postgres
	# advisory lock around that work, so several of them starting at once is safe.
	#
	# app-proxy waits for api to report healthy and the edge waits for the Gateway, so the hostname
	# returns errors for a minute or two after this call. The api healthcheck allows 90 seconds
	# before it counts a failure.
	Write-FluxerLine 'Recreating the stack.'
	if ((Invoke-FluxerDocker @('compose', 'up', '-d', '--remove-orphans')) -ne 0) {
		Stop-Fluxer 'docker compose up -d failed. Read docker compose logs.' $FluxerExitUnhealthy
	}
	Restart-FluxerMounts $changedMounts
	Wait-FluxerStack 'Waiting for every service to report ready.'
	$domainValue = Get-FluxerEnvValue $EnvPath 'FLUXER_DOMAIN'
	if ($domainValue.Length -gt 0) {
		Test-FluxerPublicHealth $domainValue
	}
	Write-FluxerLine "Instance upgraded in $TargetDir."
	Write-FluxerLine "The record of what it ran before is in $record."
	Write-FluxerLine "Go back with install.ps1 -Rollback -Dir $TargetDir."
	exit 0
}

# A rollback moves the images and the stack files back. The database stays where the new release
# left it, because api, worker, users-shard and messages-shard apply schema work in place while
# they start and an older image does not undo it. Across a release that changed the schema, the
# dump in the record is the only way back, and putting it back is a separate decision an operator
# makes.
#
# Two shapes, depending on what the upgrade moved:
#
#   A pinned tag moved, so the old images still carry their own tag. The tag goes back into .env
#   and Compose finds them.
#
#     By hand: set FLUXER_IMAGE_TAG back, then docker compose up -d
#
#   A moving tag such as v1 stayed put and the images under it changed. The recorded image IDs are
#   still on the host until a prune removes them, so the old ID goes back onto the tag it had.
#
#     By hand: docker image tag <recorded id> ghcr.io/fluxerapp/fluxer-api:v1
#
# Neither shape pulls. A pull is what moved the instance forward in the first place, and running
# one here would undo the rollback in the same breath.
function Invoke-FluxerRollback([string]$TargetDir, [string]$EnvPath, [string]$BackupRoot) {
	$record = Get-FluxerNewestRecord $BackupRoot
	if ($record.Length -eq 0) {
		Stop-Fluxer "No record in $BackupRoot. A rollback needs an upgrade that recorded what was running." $FluxerExitPrerequisite
	}
	$imagesPath = Join-Path $record $FluxerImagesFile
	if (-not (Test-Path -LiteralPath $imagesPath)) {
		Stop-Fluxer "$record records no images." $FluxerExitPrerequisite
	}
	Write-FluxerLine "Rolling back to $record."

	$recordedTag = Get-FluxerRecordTag $record
	$currentTag = Get-FluxerEnvValue $EnvPath 'FLUXER_IMAGE_TAG'
	$moved = $false
	if ($recordedTag.Length -gt 0 -and $recordedTag -ne $currentTag) {
		if ($recordedTag -match '[\s/]') {
			Stop-Fluxer "The recorded image tag $recordedTag is not an image tag." $FluxerExitRefused
		}
		Set-FluxerImageTag $EnvPath $recordedTag
		$moved = $true
	} else {
		foreach ($line in Get-Content -LiteralPath $imagesPath) {
			$parts = $line.Trim().Split(' ')
			if ($parts.Count -lt 2 -or $parts[1] -eq '-') {
				continue
			}
			if ((Get-FluxerImageId $parts[1]).Length -eq 0) {
				Write-FluxerLine "$($parts[0]) is gone from this host, so it keeps the image it has now."
				continue
			}
			if ((Invoke-FluxerDocker @('image', 'tag', $parts[1], $parts[0])) -ne 0) {
				Stop-Fluxer "Cannot put $($parts[1]) back on $($parts[0])." $FluxerExitRefused
			}
			$moved = $true
		}
	}
	if (-not $moved) {
		Stop-Fluxer "Nothing in $record can be put back. The recorded tag is the one in .env and every recorded image has been removed from this host, which a docker image prune does." $FluxerExitPrerequisite
	}

	foreach ($name in $FluxerStackFiles) {
		$source = Join-Path $record $name
		if (Test-Path -LiteralPath $source) {
			Copy-Item -LiteralPath $source -Destination (Join-Path $TargetDir $name) -Force
		}
	}
	Write-FluxerLine "Stack files in $TargetDir are the ones the record holds."

	Write-FluxerLine 'Recreating the stack.'
	if ((Invoke-FluxerDocker @('compose', 'up', '-d', '--remove-orphans')) -ne 0) {
		Stop-Fluxer 'docker compose up -d failed. Read docker compose logs.' $FluxerExitUnhealthy
	}
	# The mounted file came back from the record, so its service restarts. Comparing it first
	# would save one restart and cost the reader a reason.
	Restart-FluxerMounts $FluxerMountedFiles
	Wait-FluxerStack 'Waiting for every service to report ready.'
	$domainValue = Get-FluxerEnvValue $EnvPath 'FLUXER_DOMAIN'
	if ($domainValue.Length -gt 0) {
		Test-FluxerPublicHealth $domainValue
	}
	Write-FluxerLine "Instance rolled back in $TargetDir."
	$dumpPath = Join-Path $record $FluxerDumpFile
	if ((Test-Path -LiteralPath $dumpPath) -and ((Get-Item -LiteralPath $dumpPath).Length -gt 0)) {
		Write-FluxerLine "The database did not move. Restore it from $dumpPath only when the release you left changed the schema."
	} else {
		Write-FluxerLine 'The database did not move. That record holds no dump, because the upgrade ran with -SkipBackupAcceptDataLoss, so a release that changed the schema has no way back.'
	}
	exit 0
}

function Invoke-FluxerInstall {
	if ($Help) {
		Show-FluxerUsage
		exit 0
	}
	if ($Rest.Count -gt 0) {
		Write-FluxerProblem "Unknown argument: $($Rest[0])"
		Show-FluxerUsage
		exit $FluxerExitUsage
	}
	if ($Tls -ne 'bundled' -and $Tls -ne 'proxy') {
		Stop-Fluxer "-Tls takes bundled or proxy. Got: $Tls" $FluxerExitUsage
	}
	Assert-FluxerRef $Ref
	Assert-FluxerEdgeBind $EdgeBind
	if ($ImageTag.Length -eq 0) {
		Stop-Fluxer '-ImageTag must not be empty.' $FluxerExitUsage
	}
	if ($Update -and $Rollback) {
		Stop-Fluxer '-Update and -Rollback do not combine.' $FluxerExitUsage
	}
	if ($NoStart -and ($Update -or $Rollback)) {
		Stop-Fluxer '-NoStart belongs to an install. An upgrade that does not recreate is not an upgrade.' $FluxerExitUsage
	}
	if ($SkipBackupAcceptDataLoss -and -not $Update) {
		Stop-Fluxer '-SkipBackupAcceptDataLoss belongs to -Update.' $FluxerExitUsage
	}
	if ($NoVolumeBackup -and -not $Update) {
		Stop-Fluxer '-NoVolumeBackup belongs to -Update.' $FluxerExitUsage
	}
	if ($SkipBackupAcceptDataLoss -and $NoVolumeBackup) {
		Stop-Fluxer '-SkipBackupAcceptDataLoss already skips the volume copy.' $FluxerExitUsage
	}

	Invoke-FluxerPreflight

	$targetPath = $Dir
	if ($targetPath.Length -eq 0) {
		$targetPath = Join-Path $HOME 'fluxer'
	}
	$targetDir = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($targetPath)
	$envPath = Join-Path $targetDir '.env'
	$backupPath = $BackupDir
	if ($backupPath.Length -eq 0) {
		$backupPath = Join-Path $targetDir 'backups'
	}
	$backupRoot = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($backupPath)

	if ($Update -or $Rollback) {
		if (-not (Test-Path -LiteralPath $envPath)) {
			Stop-Fluxer "No .env in $targetDir. That directory holds no instance. Run install.ps1 with neither -Update nor -Rollback to set one up." $FluxerExitPrerequisite
		}
		if (-not (Test-Path -LiteralPath (Join-Path $targetDir 'docker-compose.yml'))) {
			Stop-Fluxer "No docker-compose.yml in $targetDir. That directory does not hold an instance." $FluxerExitPrerequisite
		}
		if ($Ref.Length -eq 0) {
			$script:Ref = Get-FluxerRefForTag (Get-FluxerEnvValue $envPath 'FLUXER_IMAGE_TAG')
			Assert-FluxerDerivedRef $Ref $envPath
		}
		$project = Get-FluxerComposeProject $targetDir
		if ($project.Length -eq 0) {
			Stop-Fluxer "docker-compose.yml in $targetDir declares no project name, so the volume names cannot be derived." $FluxerExitPrerequisite
		}
		Push-Location -LiteralPath $targetDir
		try {
			if ($DryRun) {
				if ($Rollback) {
					Show-FluxerRollbackPlan $targetDir $envPath $backupRoot
				} else {
					Show-FluxerUpdatePlan $targetDir $envPath $backupRoot
				}
				exit 0
			}
			if ($Rollback) {
				Invoke-FluxerRollback $targetDir $envPath $backupRoot
			}
			Invoke-FluxerUpgrade $targetDir $envPath $backupRoot $project
		} finally {
			Pop-Location
		}
		return
	}

	$allowPrompt = $true
	if ($NonInteractive) {
		$allowPrompt = $false
	}
	if ($DryRun) {
		$allowPrompt = $false
	}
	if ([Console]::IsInputRedirected) {
		$allowPrompt = $false
	}

	$domainValue = Resolve-FluxerValue $Domain 'Hostname the instance answers on' '-Domain' $allowPrompt
	$emailValue = Resolve-FluxerValue $Email 'Address to write as FLUXER_VAPID_EMAIL' '-Email' $allowPrompt
	Assert-FluxerDomain $domainValue
	Assert-FluxerEmail $emailValue

	if ($Ref.Length -eq 0) {
		$script:Ref = Get-FluxerRefForTag $ImageTag
		Assert-FluxerDerivedRef $Ref $envPath
	}

	if ($DryRun) {
		Write-FluxerLine 'Plan:'
		Write-FluxerLine "  Directory:  $targetDir"
		Write-FluxerLine "  Ref:        $Ref"
		Write-FluxerLine "  Image tag:  $ImageTag"
		Write-FluxerLine "  TLS mode:   $Tls"
		if ($Tls -eq 'proxy') {
			Write-FluxerLine "  Edge bind:  $EdgeBind"
		}
		Write-FluxerLine "  Domain:     $domainValue"
		Write-FluxerLine "  Email:      $emailValue"
		Write-FluxerLine "  Files:      $($FluxerStackFiles -join ', ')"
		Write-FluxerLine "  Secrets:    $($FluxerSecretKeys.Count) generated into .env"
		Write-FluxerLine 'Nothing was written.'
		exit 0
	}

	if (-not (Test-Path -LiteralPath $targetDir)) {
		New-Item -ItemType Directory -Path $targetDir -Force | Out-Null
	}
	if (Test-Path -LiteralPath $envPath) {
		Stop-Fluxer "$envPath already exists. Run this script with -Update to upgrade the instance and keep every secret." $FluxerExitRefused
	}

	$staging = New-FluxerStagingDirectory $targetDir
	try {
		Get-FluxerStackFiles $staging $Ref
		Move-FluxerStackFiles $staging $targetDir
	} finally {
		Remove-FluxerStagingDirectory $staging
	}

	Push-Location -LiteralPath $targetDir
	try {
		$vapid = New-FluxerVapidPair
		$lines = @()
		foreach ($entry in $FluxerNonSecretKeys) {
			$value = ''
			if ($entry.Kind -eq 'literal') {
				$value = $entry.Value
			} elseif ($entry.Kind -eq 'domain') {
				$value = $domainValue
			} elseif ($entry.Kind -eq 'email') {
				$value = $emailValue
			} elseif ($entry.Kind -eq 'image_tag') {
				$value = $ImageTag
			} else {
				Stop-Fluxer "Unknown source for $($entry.Name)." $FluxerExitUsage
			}
			$lines += "$($entry.Name)=$value"
		}
		if ($Tls -eq 'proxy') {
			$lines += 'COMPOSE_FILE=docker-compose.yml:docker-compose.proxy.yml'
			$lines += "FLUXER_EDGE_BIND=$EdgeBind"
		}
		foreach ($entry in $FluxerSecretKeys) {
			$value = ''
			if ($entry.Kind -eq 'hex') {
				$value = ConvertTo-FluxerHex (New-FluxerRandomBytes 32)
			} elseif ($entry.Kind -eq 'base64') {
				$value = [System.Convert]::ToBase64String((New-FluxerRandomBytes 32))
			} elseif ($entry.Kind -eq 'vapid_public') {
				$value = $vapid.Public
			} elseif ($entry.Kind -eq 'vapid_private') {
				$value = $vapid.Private
			} else {
				Stop-Fluxer "Unknown generator for $($entry.Name)." $FluxerExitSecret
			}
			$lines += "$($entry.Name)=$value"
		}
		Write-FluxerEnvFile $envPath $lines
		Write-FluxerLine "Wrote $envPath with $($lines.Count) values, readable by the current account only."

		if ($NoStart) {
			Write-FluxerLine "Run docker compose up -d in $targetDir to start the instance."
			Write-FluxerLine "Secrets live in $envPath. Back that file up."
			exit 0
		}

		if ((Invoke-FluxerDocker @('compose', 'up', '-d')) -ne 0) {
			Stop-Fluxer 'docker compose up -d failed.' $FluxerExitUnhealthy
		}
		Wait-FluxerStack 'Waiting for the stack to report healthy. The first start pulls images and takes several minutes.'
		Test-FluxerPublicHealth $domainValue
		Write-FluxerLine "Instance ready at https://$domainValue"
		Write-FluxerLine 'Open it and create the first admin account. Finish the setup wizard in the same sitting.'
		Write-FluxerLine "Secrets live in $envPath. Back that file up."
	} finally {
		Pop-Location
	}
}

try {
	Invoke-FluxerInstall
} catch [System.Management.Automation.PipelineStoppedException] {
	Write-FluxerProblem 'Interrupted.'
	exit $FluxerExitInterrupted
}
