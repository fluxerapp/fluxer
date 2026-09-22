// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {spawn, spawnSync} from 'node:child_process';
import {chmodSync, existsSync, mkdtempSync, readFileSync, writeFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./LinuxLaunchPath.ts', import.meta.url));
const transformedSource = esbuild.transformSync(readFileSync(sourcePath, 'utf8'), {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

const APPIMAGE_PATH = '/home/hampus/Applications/My Fluxer.AppImage';
const EXEC_PATH = '/tmp/.mount_My FluhKjNGD/fluxer-canary';

function loadLaunchPath({
	platform = 'linux',
	appImagePath = APPIMAGE_PATH,
	argv = [],
	spawnThrows = false,
	shells = ['/bin/bash'],
} = {}) {
	const spawned = [];
	const relaunched = [];
	const module = {exports: {}};
	const stubs = {
		'@electron/common/Logger': {createChildLogger: () => ({debug() {}, info() {}, warn() {}, error() {}})},
		electron: {
			app: {
				relaunch(options) {
					relaunched.push(options);
				},
			},
		},
		'node:child_process': {
			spawn(command, args, options) {
				if (spawnThrows) {
					throw new Error('spawn EPERM');
				}
				let unrefCalls = 0;
				const child = {
					pid: 4242,
					unref() {
						unrefCalls += 1;
					},
					get unrefCalls() {
						return unrefCalls;
					},
				};
				spawned.push({command, args, options, child});
				return child;
			},
		},
		'node:fs': {existsSync: (path) => shells.includes(path)},
		'node:os': {homedir: () => '/home/hampus'},
	};
	const context = vm.createContext({
		module,
		exports: module.exports,
		console,
		process: {
			platform,
			pid: 13274,
			execPath: EXEC_PATH,
			argv: [EXEC_PATH, ...argv],
			env: appImagePath ? {APPIMAGE: appImagePath} : {},
		},
		require: (specifier) => stubs[specifier] ?? require(specifier),
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return {...module.exports, spawned, relaunched};
}

describe('Linux launch path', () => {
	test('reports the AppImage as the stable launch path', () => {
		assert.equal(loadLaunchPath({}).getStableLinuxLaunchPath(), APPIMAGE_PATH);
		assert.equal(loadLaunchPath({appImagePath: null}).getStableLinuxLaunchPath(), EXEC_PATH);
		assert.equal(loadLaunchPath({platform: 'darwin'}).getStableLinuxLaunchPath(), EXEC_PATH);
	});
});

function relaunchScript() {
	const probe = loadLaunchPath({});
	probe.relaunchStableLaunchPath();
	return probe.spawned[0].args[1];
}

describe('AppImage relaunch script', () => {
	for (const shell of ['/bin/bash', '/bin/sh']) {
		test(`replaces the process once the parent exits under ${shell}`, {skip: !existsSync(shell)}, async () => {
			const root = mkdtempSync(join(tmpdir(), 'fluxer-relaunch-test-'));
			const marker = join(root, 'replacement-argv');
			const replacement = join(root, 'My Fluxer.AppImage');
			writeFileSync(replacement, `#!${shell}\nprintf '%s\\n' "$@" > '${marker}'\n`, {mode: 0o755});
			const parent = spawn(shell, ['-c', 'sleep 30'], {stdio: 'ignore', detached: true});
			parent.unref();

			const relaunch = spawn(
				shell,
				[
					'-c',
					relaunchScript(),
					'fluxer-relaunch',
					String(parent.pid),
					replacement,
					root,
					'--fluxer-portable',
					'fluxer://invite/a b',
				],
				{cwd: root, detached: true, stdio: 'ignore'},
			);
			relaunch.unref();

			spawnSync('kill', [String(parent.pid)]);
			for (let attempt = 0; attempt < 100 && !existsSync(marker); attempt += 1) {
				await new Promise((resolve) => setTimeout(resolve, 100));
			}

			assert.equal(existsSync(marker), true, 'the replacement never started');
			assert.deepEqual(readFileSync(marker, 'utf8').split('\n').slice(0, 2), [
				'--fluxer-portable',
				'fluxer://invite/a b',
			]);
			chmodSync(replacement, 0o755);
		});
	}
});

describe('AppImage relaunch', () => {
	test('never asks Electron to relaunch an AppImage', () => {
		const updater = loadLaunchPath({argv: ['--fluxer-portable']});

		updater.relaunchStableLaunchPath();

		assert.equal(
			updater.relaunched.length,
			0,
			"Electron's relauncher runs under PR_SET_NO_NEW_PRIVS, which makes the setuid fusermount the AppImage runtime needs fail with EPERM, so the app quits and never comes back",
		);
		assert.equal(updater.spawned.length, 1);
		const [relaunch] = updater.spawned;
		assert.equal(relaunch.command, '/bin/bash');
		assert.equal(relaunch.options.detached, true);
		assert.equal(relaunch.options.stdio, 'ignore');
		assert.equal(relaunch.options.cwd, '/home/hampus');
		assert.equal(relaunch.child.unrefCalls, 1);
	});

	test('waits for this process to exit before starting the replacement, and keeps its arguments', () => {
		const updater = loadLaunchPath({argv: ['--fluxer-portable', 'fluxer://invite/a b']});

		updater.relaunchStableLaunchPath();

		const [flag, script, name, pid, target, home, ...forwarded] = updater.spawned[0].args;
		assert.equal(flag, '-c');
		assert.match(script, /kill -0 "\$pid"/);
		assert.match(script, /exec "\$app" "\$@"/);
		assert.equal(name, 'fluxer-relaunch');
		assert.equal(pid, '13274');
		assert.equal(target, APPIMAGE_PATH);
		assert.equal(home, '/home/hampus');
		assert.deepEqual(forwarded, ['--fluxer-portable', 'fluxer://invite/a b']);
	});

	test('hands the replacement a clean descriptor table', () => {
		const updater = loadLaunchPath({});

		updater.relaunchStableLaunchPath();

		const script = updater.spawned[0].args[1];
		assert.match(
			script,
			/cd \/proc\/self\/fd/,
			"Chromium leaves its icudtl.dat, v8 snapshot, .pak and app.asar descriptors open across a spawn, so the replacement would pin the outgoing build's fuse mount and its unlinked AppImage for as long as it runs",
		);
		assert.match(
			script,
			/\[ -n "\$BASH_VERSION" \]/,
			'dash only parses single digit descriptor numbers, and a failed exec kills a non-interactive shell, so the two-digit closes have to be guarded',
		);
	});

	test('falls back to the system shell when bash is missing', () => {
		const updater = loadLaunchPath({shells: ['/bin/sh']});

		updater.relaunchStableLaunchPath();

		assert.equal(updater.spawned[0].command, '/bin/sh');
		assert.equal(updater.relaunched.length, 0);
	});

	test('leaves every other install to Electron', () => {
		const deb = loadLaunchPath({appImagePath: null});
		deb.relaunchStableLaunchPath();
		assert.equal(deb.spawned.length, 0);
		assert.deepEqual(deb.relaunched, [undefined]);

		const mac = loadLaunchPath({platform: 'darwin'});
		mac.relaunchStableLaunchPath();
		assert.equal(mac.spawned.length, 0);
		assert.deepEqual(mac.relaunched, [undefined]);
	});

	test('falls back to Electron when the replacement process cannot be started', () => {
		const updater = loadLaunchPath({spawnThrows: true});

		updater.relaunchStableLaunchPath();

		assert.equal(updater.relaunched.length, 1);
		assert.equal(updater.relaunched[0]?.execPath, APPIMAGE_PATH);
	});
});
