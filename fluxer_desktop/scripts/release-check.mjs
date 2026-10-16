// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFileSync, spawn} from 'node:child_process';
import * as fs from 'node:fs';
import * as net from 'node:net';
import * as os from 'node:os';
import * as path from 'node:path';
import {parseArgs} from 'node:util';

const CHANNEL_DATA_DIRECTORIES = Object.freeze({
	stable: 'fluxer',
	canary: 'fluxercanary',
	development: 'fluxerdevelopment',
});
const SERVED_RENDERER_PATTERN = /Serving the renderer: source=(\S+) version=(\S+) bundled=(\S+)/gu;
const REPORTED_BUILD_PATTERN = /The renderer reported its build: (.+)$/gmu;
const REPORTED_WEB_VERSION_PATTERN = /\bWeb (\d+\.\d+\.\d+)\b/u;
const POLL_INTERVAL_MS = 500;
const QUIT_GRACE_MS = 8000;
const USAGE = `Usage: node scripts/release-check.mjs --app <packaged app> --channel <stable|canary|development> --expect-renderer <version>

Runs a packaged desktop shell in a throwaway portable profile and fails unless the renderer
it serves, and the renderer that reports back from the window, are both the expected version.

  --app <path>               A macOS .app bundle, an unpacked app directory or its executable
  --channel <name>           The channel the shell was built for
  --expect-renderer <ver>    The renderer build version the user must end up on
  --expect-source <name>     bundled or module, when the source also has to match
  --seed-profile <dir>       A portable profile to start from, for example the previous release's
  --package-origin <url>     The package feed to use instead of the one the shell was built for
  --update                   Ask the running shell to update through its own command line path
  --update-from-renderer     Start the update the way the in app update button does instead
  --expect-initial <ver>     With --update, the renderer version before the update
  --before-update <command>  With --update, a shell command to run once the app is up, for example a publish
  --app-arg <arg>            Extra argument for the shell, repeatable
  --timeout-seconds <n>      How long each stage may take, default 180
  --workdir <dir>            Where to create the sandbox, default the system temp directory
  --keep                     Leave the sandbox in place for inspection
`;

class ReleaseCheckError extends Error {
	constructor(message) {
		super(message);
		this.name = 'ReleaseCheckError';
	}
}

function fail(message) {
	throw new ReleaseCheckError(message);
}

function compareVersions(left, right) {
	const a = left.split('.').map((part) => Number.parseInt(part, 10));
	const b = right.split('.').map((part) => Number.parseInt(part, 10));
	for (let index = 0; index < Math.max(a.length, b.length); index += 1) {
		const difference = (a[index] ?? 0) - (b[index] ?? 0);
		if (difference !== 0) return difference;
	}
	return 0;
}

function readOptions() {
	const {values} = parseArgs({
		options: {
			app: {type: 'string'},
			channel: {type: 'string'},
			'expect-renderer': {type: 'string'},
			'expect-source': {type: 'string'},
			'expect-initial': {type: 'string'},
			'before-update': {type: 'string'},
			'seed-profile': {type: 'string'},
			'package-origin': {type: 'string'},
			update: {type: 'boolean', default: false},
			'update-from-renderer': {type: 'boolean', default: false},
			'app-arg': {type: 'string', multiple: true, default: []},
			'timeout-seconds': {type: 'string', default: '180'},
			workdir: {type: 'string'},
			keep: {type: 'boolean', default: false},
			help: {type: 'boolean', default: false},
		},
	});
	if (values.help) {
		process.stdout.write(USAGE);
		process.exit(0);
	}
	for (const required of ['app', 'channel', 'expect-renderer']) {
		if (!values[required]) fail(`--${required} is required\n\n${USAGE}`);
	}
	if (!Object.hasOwn(CHANNEL_DATA_DIRECTORIES, values.channel)) {
		fail(`--channel must be one of ${Object.keys(CHANNEL_DATA_DIRECTORIES).join(', ')}`);
	}
	if (values['expect-source'] != null && !['bundled', 'module'].includes(values['expect-source'])) {
		fail('--expect-source must be bundled or module');
	}
	const timeoutSeconds = Number.parseInt(values['timeout-seconds'], 10);
	if (!Number.isSafeInteger(timeoutSeconds) || timeoutSeconds <= 0) {
		fail('--timeout-seconds must be a positive integer');
	}
	return {
		app: path.resolve(values.app),
		channel: values.channel,
		expectRenderer: values['expect-renderer'],
		expectSource: values['expect-source'] ?? null,
		expectInitial: values['expect-initial'] ?? null,
		beforeUpdate: values['before-update'] ?? null,
		seedProfile: values['seed-profile'] == null ? null : path.resolve(values['seed-profile']),
		packageOrigin: values['package-origin'] ?? null,
		update: values.update || values['update-from-renderer'],
		updateFromRenderer: values['update-from-renderer'],
		appArgs: values['app-arg'],
		timeoutMs: timeoutSeconds * 1000,
		workdir: values.workdir == null ? os.tmpdir() : path.resolve(values.workdir),
		keep: values.keep,
	};
}

function copyTree(source, target) {
	if (process.platform === 'darwin') {
		execFileSync('cp', ['-cR', source, target]);
		return;
	}
	fs.cpSync(source, target, {recursive: true, verbatimSymlinks: true});
}

function resolveMacExecutable(appBundle) {
	const macOSDirectory = path.join(appBundle, 'Contents', 'MacOS');
	const candidates = fs.readdirSync(macOSDirectory).filter((name) => !name.startsWith('.'));
	const named = candidates.find((name) => name === path.basename(appBundle, '.app'));
	const chosen = named ?? candidates[0];
	if (chosen == null) fail(`${macOSDirectory} holds no executable`);
	return path.join(macOSDirectory, chosen);
}

function stageApp(options, sandbox) {
	const source = fs.statSync(options.app);
	if (options.app.endsWith('.app') && source.isDirectory()) {
		const staged = path.join(sandbox, path.basename(options.app));
		copyTree(options.app, staged);
		return {executable: resolveMacExecutable(staged), portableRoot: path.join(sandbox, 'data')};
	}
	if (source.isDirectory()) {
		const staged = path.join(sandbox, 'app');
		copyTree(options.app, staged);
		const executable = fs
			.readdirSync(staged)
			.map((name) => path.join(staged, name))
			.find((candidate) => {
				const stats = fs.statSync(candidate);
				const name = path.basename(candidate);
				if (!stats.isFile() || !/^fluxer/iu.test(name)) return false;
				if (process.platform === 'win32') return /\.exe$/iu.test(name) && !/_ExecutionStub\.exe$/iu.test(name);
				return (stats.mode & 0o111) !== 0;
			});
		if (executable == null) fail(`${options.app} holds no fluxer executable`);
		return {executable, portableRoot: path.join(staged, 'data')};
	}
	const staged = path.join(sandbox, path.basename(options.app));
	fs.copyFileSync(options.app, staged);
	fs.chmodSync(staged, 0o755);
	return {executable: staged, portableRoot: path.join(sandbox, 'data')};
}

function freePort() {
	return new Promise((resolve, reject) => {
		const server = net.createServer();
		server.once('error', reject);
		server.listen(0, '127.0.0.1', () => {
			const {port} = server.address();
			server.close(() => resolve(port));
		});
	});
}

function sleep(ms) {
	return new Promise((resolve) => {
		setTimeout(resolve, ms);
	});
}

function readLog(logPath) {
	try {
		return fs.readFileSync(logPath, 'utf8');
	} catch {
		return '';
	}
}

function lastMatch(text, pattern) {
	let last = null;
	for (const match of text.matchAll(pattern)) {
		last = match;
	}
	return last;
}

function observeLog(logPath) {
	const text = readLog(logPath);
	const served = lastMatch(text, SERVED_RENDERER_PATTERN);
	const reported = lastMatch(text, REPORTED_BUILD_PATTERN);
	const reportedVersion = reported == null ? null : (REPORTED_WEB_VERSION_PATTERN.exec(reported[1])?.[1] ?? null);
	return {
		served: served == null ? null : {source: served[1], version: served[2], bundled: served[3]},
		reportedBuild: reported?.[1] ?? null,
		reportedVersion,
	};
}

async function evaluateInPage(debugPort, expression) {
	let targets;
	try {
		targets = await (await fetch(`http://127.0.0.1:${debugPort}/json/list`)).json();
	} catch {
		return null;
	}
	const page = targets.find((target) => target.type === 'page' && String(target.url).startsWith('fluxer-app://app'));
	if (page == null) return null;
	return await new Promise((resolve) => {
		const socket = new WebSocket(page.webSocketDebuggerUrl);
		const timer = setTimeout(() => {
			socket.close();
			resolve(null);
		}, 5000);
		socket.addEventListener('open', () => {
			socket.send(
				JSON.stringify({
					id: 1,
					method: 'Runtime.evaluate',
					params: {expression, awaitPromise: true, returnByValue: true},
				}),
			);
		});
		socket.addEventListener('message', (event) => {
			const message = JSON.parse(String(event.data));
			if (message.id !== 1) return;
			clearTimeout(timer);
			socket.close();
			resolve(message.result?.result?.value ?? null);
		});
		socket.addEventListener('error', () => {
			clearTimeout(timer);
			resolve(null);
		});
	});
}

async function servedVersionInPage(debugPort) {
	const value = await evaluateInPage(
		debugPort,
		"fetch('/version.json', {cache: 'no-store'}).then((response) => response.json()).then((body) => body.version).catch(() => null)",
	);
	return typeof value === 'string' ? value : null;
}

async function startUpdateFromRenderer(debugPort, timeoutMs) {
	const deadline = Date.now() + timeoutMs;
	await evaluateInPage(debugPort, "window.electron.updaterCheck?.('user').then(() => true, () => false)");
	while (Date.now() < deadline) {
		const available = await evaluateInPage(
			debugPort,
			'window.electron.desktopUpdate.state().then((state) => state.available === true, () => false)',
		);
		if (available === true) {
			await evaluateInPage(debugPort, 'window.electron.desktopUpdate.start().then(() => true, () => false)');
			return;
		}
		await sleep(POLL_INTERVAL_MS);
	}
	fail('update: the shell never told the renderer that an update is available');
}

function startupFailure(logPath) {
	try {
		const lines = fs.readFileSync(logPath, 'utf8').split(/\r?\n/u);
		const index = lines.findIndex((line) => /\[NativeModulePreflight\] Fatal/u.test(line));
		return index < 0 ? null : lines.slice(index, index + 3).join(' ');
	} catch {
		return null;
	}
}

async function waitForRenderer({logPath, debugPort, version, timeoutMs, label, child = null}) {
	const deadline = Date.now() + timeoutMs;
	let observed = {log: observeLog(logPath), page: null};
	while (Date.now() < deadline) {
		if (child != null && (child.exitCode != null || child.signalCode != null)) {
			const failure = startupFailure(logPath);
			fail(
				`${label}: the shell exited with ${child.exitCode ?? child.signalCode} before the renderer reported${
					failure == null ? '' : `, ${failure.trim()}`
				}`,
			);
		}
		observed = {log: observeLog(logPath), page: await servedVersionInPage(debugPort)};
		if (
			observed.page === version &&
			observed.log.reportedVersion === version &&
			observed.log.served?.version === version
		) {
			return observed;
		}
		await sleep(POLL_INTERVAL_MS);
	}
	fail(
		`${label}: expected renderer ${version}, the shell serves ${observed.log.served?.version ?? 'nothing'} from ${
			observed.log.served?.source ?? 'nowhere'
		}, the window loads ${observed.page ?? 'nothing'} and reports ${observed.log.reportedBuild ?? 'nothing'}${
			startupFailure(logPath) == null ? '' : `, ${startupFailure(logPath)}`
		}`,
	);
}

function killWindowsTree(child, executable) {
	for (const args of [
		['/PID', String(child.pid), '/T', '/F'],
		['/IM', path.basename(executable), '/T', '/F'],
	]) {
		try {
			execFileSync('taskkill', args, {stdio: 'ignore'});
		} catch {}
	}
}

async function removeSandbox(sandbox) {
	for (let attempt = 0; attempt < 20; attempt++) {
		try {
			fs.rmSync(sandbox, {recursive: true, force: true});
			return;
		} catch (error) {
			if (attempt === 19) {
				process.stderr.write(`could not remove ${sandbox}: ${error.message}\n`);
				return;
			}
			await sleep(500);
		}
	}
}

async function stop(child, executable) {
	if (process.platform === 'win32') {
		killWindowsTree(child, executable);
		return;
	}
	if (child.exitCode != null || child.signalCode != null) return;
	const exited = new Promise((resolve) => {
		child.once('exit', resolve);
	});
	child.kill('SIGTERM');
	const graceful = await Promise.race([exited.then(() => true), sleep(QUIT_GRACE_MS).then(() => false)]);
	if (!graceful) {
		child.kill('SIGKILL');
		await exited;
	}
}

async function run() {
	const options = readOptions();
	fs.mkdirSync(options.workdir, {recursive: true});
	const sandbox = fs.mkdtempSync(path.join(options.workdir, 'fluxer-release-check-'));
	const {executable, portableRoot} = stageApp(options, sandbox);
	const profile = path.join(portableRoot, CHANNEL_DATA_DIRECTORIES[options.channel]);
	if (options.seedProfile != null) {
		fs.mkdirSync(portableRoot, {recursive: true});
		copyTree(options.seedProfile, profile);
		for (const stale of ['SingletonLock', 'SingletonCookie', 'SingletonSocket', 'DevToolsActivePort']) {
			fs.rmSync(path.join(profile, stale), {force: true});
		}
		fs.rmSync(path.join(profile, 'logs'), {force: true, recursive: true});
	}
	fs.mkdirSync(profile, {recursive: true});
	const logPath = path.join(profile, 'logs', 'main.log');
	const debugPort = await freePort();
	const launchArguments = ['--fluxer-portable', `--remote-debugging-port=${debugPort}`, ...options.appArgs];
	const environment = {...process.env};
	if (options.packageOrigin != null) {
		environment.FLUXER_DESKTOP_PACKAGE_ORIGIN = options.packageOrigin;
	}
	const child = spawn(executable, launchArguments, {env: environment, stdio: 'ignore'});
	const result = {
		sandbox,
		executable,
		profile,
		expectRenderer: options.expectRenderer,
		stages: [],
	};
	try {
		const firstVersion =
			options.update && options.expectInitial != null ? options.expectInitial : options.expectRenderer;
		const first = await waitForRenderer({
			logPath,
			debugPort,
			version: firstVersion,
			timeoutMs: options.timeoutMs,
			label: 'launch',
			child: options.update ? null : child,
		});
		result.stages.push({stage: 'launch', ...first});
		let final = first;
		if (options.update) {
			if (options.beforeUpdate != null) {
				execFileSync(options.beforeUpdate, {shell: true, stdio: 'inherit'});
			}
			if (options.updateFromRenderer) {
				await startUpdateFromRenderer(debugPort, options.timeoutMs);
			} else {
				execFileSync(executable, ['--fluxer-portable', '--fluxer-update'], {env: environment, stdio: 'ignore'});
			}
			final = await waitForRenderer({
				logPath,
				debugPort,
				version: options.expectRenderer,
				timeoutMs: options.timeoutMs,
				label: 'update',
			});
			result.stages.push({stage: 'update', ...final});
		}
		const served = final.log.served;
		if (options.expectSource != null && served.source !== options.expectSource) {
			fail(`the shell serves the ${served.source} renderer, expected the ${options.expectSource} one`);
		}
		if (served.bundled !== 'none' && compareVersions(served.version, served.bundled) < 0) {
			fail(`the shell serves renderer ${served.version}, older than the ${served.bundled} it bundles`);
		}
		result.shellPid = child.pid;
		result.shellStillRunning = child.exitCode == null && child.signalCode == null;
		result.ok = true;
	} finally {
		await stop(child, executable);
		if (!options.keep) {
			await removeSandbox(sandbox);
		}
	}
	process.stdout.write(`${JSON.stringify(result, null, '\t')}\n`);
}

run().catch((error) => {
	process.stderr.write(`${error instanceof ReleaseCheckError ? error.message : (error?.stack ?? String(error))}\n`);
	process.exit(1);
});
