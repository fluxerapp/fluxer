#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';
import {existsSync, mkdirSync, readFileSync} from 'node:fs';
import {homedir} from 'node:os';
import path from 'node:path';
import process from 'node:process';

const repoRoot = path.resolve(new URL('..', import.meta.url).pathname);
const defaultConfigPath = path.join(repoRoot, '.fluxer', 'remote-hosts.json');
const defaultControlDir = path.join(repoRoot, '.fluxer', 'remote-ssh');

function usage(exitCode = 0) {
	console.log(`Usage:
  pnpm remote list
  pnpm remote doctor <host>
  pnpm remote bootstrap <host> [--branch <branch>]
  pnpm remote pull <host> [--branch <branch>]
  pnpm remote run <host> -- <command>
  pnpm remote apply-diff <host> [-- <path>...]
  pnpm remote tunnel start|status|stop <host>
  pnpm remote macos-setup [--pubkey ~/.ssh/id_ed25519.pub]

Global options:
  --config <path>   Host config path. Defaults to .fluxer/remote-hosts.json.
  --verbose         Print spawned ssh commands.
`);
	process.exit(exitCode);
}

function fail(message, exitCode = 1) {
	console.error(message);
	process.exit(exitCode);
}

function parseGlobalArgs(argv) {
	const options = {configPath: defaultConfigPath, verbose: false};
	const rest = [];
	for (let i = 0; i < argv.length; i++) {
		const arg = argv[i];
		if (arg === '--config') {
			options.configPath = path.resolve(argv[++i] ?? fail('--config needs a path'));
		} else if (arg === '--verbose') {
			options.verbose = true;
		} else if (arg === '--help' || arg === '-h') {
			usage(0);
		} else {
			rest.push(arg);
		}
	}
	return {options, args: rest};
}

function readJson(filePath) {
	try {
		return JSON.parse(readFileSync(filePath, 'utf8'));
	} catch (error) {
		fail(`Failed to read ${filePath}: ${error instanceof Error ? error.message : String(error)}`);
	}
}

function loadConfig(configPath) {
	if (!existsSync(configPath)) {
		fail(
			`Remote host config not found: ${configPath}\n` +
				`Copy scripts/remote/hosts.example.json to .fluxer/remote-hosts.json and edit it.`,
		);
	}
	const config = readJson(configPath);
	if (!config || typeof config !== 'object' || !config.hosts || typeof config.hosts !== 'object') {
		fail(`Invalid remote host config: ${configPath}`);
	}
	return config;
}

function getHost(config, name) {
	const host = config.hosts[name];
	if (!host || typeof host !== 'object') {
		const known = Object.keys(config.hosts).sort().join(', ') || '(none)';
		fail(`Unknown remote host "${name}". Known hosts: ${known}`);
	}
	if (typeof host.host !== 'string' || host.host.length === 0) {
		fail(`Host "${name}" is missing "host"`);
	}
	if (host.platform !== 'windows' && host.platform !== 'macos' && host.platform !== 'linux') {
		fail(`Host "${name}" needs platform "windows", "macos", or "linux"`);
	}
	return host;
}

function expandHome(value) {
	if (typeof value !== 'string') return value;
	if (value === '~') return homedir();
	if (value.startsWith('~/')) return path.join(homedir(), value.slice(2));
	return value;
}

function targetFor(host) {
	return host.user ? `${host.user}@${host.host}` : host.host;
}

function controlPathFor(name, host) {
	if (host.controlPath) return path.resolve(expandHome(host.controlPath));
	mkdirSync(defaultControlDir, {recursive: true, mode: 0o700});
	return path.join(defaultControlDir, `${name.replace(/[^A-Za-z0-9_.-]/g, '_')}.ctl`);
}

function sshBaseArgs(name, host, {control = true} = {}) {
	const args = [];
	if (host.port) args.push('-p', String(host.port));
	if (host.identityFile) args.push('-i', path.resolve(expandHome(host.identityFile)));
	args.push('-o', 'ServerAliveInterval=30', '-o', 'ServerAliveCountMax=3', '-o', 'StrictHostKeyChecking=accept-new');
	if (control) {
		args.push('-S', controlPathFor(name, host), '-o', 'ControlMaster=auto', '-o', 'ControlPersist=30m');
	}
	return args;
}

function forwardArgs(host) {
	const forwards = Array.isArray(host.forwards) ? host.forwards : [];
	const args = [];
	for (const forward of forwards) {
		if (typeof forward === 'string') {
			args.push('-L', forward);
		} else if (forward && typeof forward === 'object' && forward.local && forward.remote) {
			args.push('-L', `${forward.local}:${forward.remote}`);
		} else {
			fail(`Invalid forward entry for ${host.host}`);
		}
	}
	return args;
}

function shQuote(value) {
	return `'${String(value).replace(/'/g, `'\\''`)}'`;
}

function psQuote(value) {
	return `'${String(value).replace(/'/g, "''")}'`;
}

function remoteCommand(host, command) {
	const repo = host.repo;
	if (host.platform === 'windows') {
		const body = `${repo ? `Set-Location -LiteralPath ${psQuote(repo)}; ` : ''}${command}`;
		return `powershell.exe -NoProfile -ExecutionPolicy Bypass -EncodedCommand ${Buffer.from(body, 'utf16le').toString('base64')}`;
	}
	const body = `${repo ? `cd ${shQuote(repo)} && ` : ''}${command}`;
	return `bash -lc ${shQuote(body)}`;
}

function spawnChecked(command, args, {verbose = false, input} = {}) {
	if (verbose) console.error(`$ ${command} ${args.join(' ')}`);
	const result = spawnSync(command, args, {
		cwd: repoRoot,
		input,
		stdio: input === undefined ? 'inherit' : ['pipe', 'inherit', 'inherit'],
	});
	if (result.error) fail(`${command} failed to start: ${result.error.message}`);
	if (result.status !== 0) process.exit(result.status ?? 1);
}

function spawnCapture(command, args) {
	const result = spawnSync(command, args, {cwd: repoRoot, encoding: 'utf8'});
	if (result.error || result.status !== 0) return null;
	return result.stdout.trim();
}

function spawnCaptureRaw(command, args) {
	const result = spawnSync(command, args, {cwd: repoRoot, encoding: 'utf8'});
	if (result.error || result.status !== 0) return null;
	return result.stdout;
}

function ssh(name, host, command, options = {}) {
	spawnChecked('ssh', [...sshBaseArgs(name, host), targetFor(host), remoteCommand(host, command)], options);
}

function sshWithInput(name, host, command, input, options = {}) {
	spawnChecked('ssh', [...sshBaseArgs(name, host), targetFor(host), remoteCommand(host, command)], {
		...options,
		input,
	});
}

function currentBranch() {
	return spawnCapture('git', ['rev-parse', '--abbrev-ref', 'HEAD']) || 'main';
}

function originUrl() {
	return spawnCapture('git', ['config', '--get', 'remote.origin.url']) || '';
}

function splitAfterDoubleDash(args) {
	const index = args.indexOf('--');
	if (index === -1) return {head: args, tail: []};
	return {head: args.slice(0, index), tail: args.slice(index + 1)};
}

function optionValue(args, name, fallback) {
	const index = args.indexOf(name);
	if (index === -1) return fallback;
	const value = args[index + 1];
	if (!value) fail(`${name} needs a value`);
	return value;
}

function cloneOrUpdateCommand(host, branch) {
	const repo = host.repo;
	const repoUrl = host.repoUrl || originUrl();
	if (!repo) fail('bootstrap/pull requires host.repo in the config');
	if (!repoUrl) fail('bootstrap requires host.repoUrl or a local git remote.origin.url');
	if (host.platform === 'windows') {
		return [
			`$repo = ${psQuote(repo)}`,
			`$repoUrl = ${psQuote(repoUrl)}`,
			`$branch = ${psQuote(branch)}`,
			`if (!(Test-Path -LiteralPath (Join-Path $repo '.git'))) {`,
			`  New-Item -ItemType Directory -Force -Path (Split-Path -Parent $repo) | Out-Null`,
			`  git clone $repoUrl $repo`,
			`}`,
			`Set-Location -LiteralPath $repo`,
			`git fetch --all --prune`,
			`git checkout $branch`,
			`git pull --ff-only`,
			`corepack enable`,
			`pnpm install`,
		].join('; ');
	}
	return [
		`repo=${shQuote(repo)}`,
		`repo_url=${shQuote(repoUrl)}`,
		`branch=${shQuote(branch)}`,
		`if [ ! -d "$repo/.git" ]; then mkdir -p "$(dirname "$repo")"; git clone "$repo_url" "$repo"; fi`,
		`cd "$repo"`,
		`git fetch --all --prune`,
		`git checkout "$branch"`,
		`git pull --ff-only`,
		`corepack enable`,
		`pnpm install`,
	].join(' && ');
}

function pullCommand(host, branch) {
	const repo = host.repo;
	if (!repo) fail('pull requires host.repo in the config');
	if (host.platform === 'windows') {
		return [
			`Set-Location -LiteralPath ${psQuote(repo)}`,
			`git fetch --all --prune`,
			`git checkout ${psQuote(branch)}`,
			`git pull --ff-only`,
		].join('; ');
	}
	return [
		`cd ${shQuote(repo)}`,
		`git fetch --all --prune`,
		`git checkout ${shQuote(branch)}`,
		`git pull --ff-only`,
	].join(' && ');
}

function macosSetup(args) {
	const pubkeyPath = path.resolve(expandHome(optionValue(args, '--pubkey', '~/.ssh/id_ed25519.pub')));
	const pubkey = existsSync(pubkeyPath) ? readFileSync(pubkeyPath, 'utf8').trim() : '<paste-your-public-ssh-key-here>';
	console.log(`# Run this on the macOS machine while Tailscale is connected.
sudo systemsetup -setremotelogin on
mkdir -p ~/.ssh
chmod 700 ~/.ssh
printf '%s\\n' ${shQuote(pubkey)} >> ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys
tailscale status
tailscale ip -4
sudo lsof -nP -iTCP:22 -sTCP:LISTEN

# Optional, only if you intentionally use Tailscale SSH ACLs instead of OpenSSH keys:
# sudo tailscale up --ssh
`);
}

const {options, args} = parseGlobalArgs(process.argv.slice(2));
const command = args[0];
if (!command) usage(1);

if (command === 'macos-setup') {
	macosSetup(args.slice(1));
	process.exit(0);
}

const config = loadConfig(options.configPath);

switch (command) {
	case 'list': {
		for (const [name, host] of Object.entries(config.hosts)) {
			console.log(`${name}\t${host.platform}\t${targetFor(host)}\t${host.repo ?? ''}`);
		}
		break;
	}
	case 'doctor': {
		const name = args[1] ?? fail('doctor needs a host');
		const host = getHost(config, name);
		const remote =
			host.platform === 'windows'
				? '$PSVersionTable.PSVersion.ToString(); git --version; node --version; pnpm --version; rustc --version; cargo --version; Get-ComputerInfo | Select-Object -ExpandProperty OsName'
				: 'uname -a; git --version; node --version; pnpm --version; rustc --version; cargo --version';
		ssh(name, host, remote, options);
		break;
	}
	case 'bootstrap': {
		const name = args[1] ?? fail('bootstrap needs a host');
		const host = getHost(config, name);
		ssh(name, host, cloneOrUpdateCommand(host, optionValue(args, '--branch', currentBranch())), options);
		break;
	}
	case 'pull': {
		const name = args[1] ?? fail('pull needs a host');
		const host = getHost(config, name);
		ssh(name, host, pullCommand(host, optionValue(args, '--branch', currentBranch())), options);
		break;
	}
	case 'run': {
		const {head, tail} = splitAfterDoubleDash(args.slice(1));
		const name = head[0] ?? fail('run needs a host');
		if (tail.length === 0) fail('run needs -- <command>');
		ssh(name, getHost(config, name), tail.join(' '), options);
		break;
	}
	case 'apply-diff': {
		const {head, tail} = splitAfterDoubleDash(args.slice(1));
		const name = head[0] ?? fail('apply-diff needs a host');
		const host = getHost(config, name);
		const diff = spawnCaptureRaw('git', ['diff', '--binary', '--', ...tail]);
		if (!diff) {
			console.log('No local diff to apply.');
			break;
		}
		sshWithInput(name, host, 'git apply --whitespace=nowarn -', diff, options);
		break;
	}
	case 'tunnel': {
		const action = args[1] ?? fail('tunnel needs start|status|stop');
		const name = args[2] ?? fail('tunnel needs a host');
		const host = getHost(config, name);
		if (action === 'start') {
			spawnChecked(
				'ssh',
				[
					...sshBaseArgs(name, host),
					...forwardArgs(host),
					'-fN',
					'-M',
					'-o',
					'ExitOnForwardFailure=yes',
					targetFor(host),
				],
				options,
			);
		} else if (action === 'status') {
			spawnChecked('ssh', [...sshBaseArgs(name, host), '-O', 'check', targetFor(host)], options);
		} else if (action === 'stop') {
			spawnChecked('ssh', [...sshBaseArgs(name, host), '-O', 'exit', targetFor(host)], options);
		} else {
			fail('tunnel action must be start, status, or stop');
		}
		break;
	}
	default:
		usage(1);
}
