// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import crypto from 'node:crypto';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import nodePath from 'node:path';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./NativeModulePreflight.ts', import.meta.url));
const transformedSource = esbuild.transformSync(readFileSync(sourcePath, 'utf8'), {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

const MARKER_PATH = nodePath.join('/userData', 'native-module-preflight-v2.json');

function loadPreflight({
	files = new Map(),
	unresolvable = new Set(),
	probeFailures = new Set(),
	isPackaged = true,
} = {}) {
	const spawns = [];
	const warnings = [];

	function spawnSyncStub(_execPath, args) {
		spawns.push(args.slice(2));
		const lines = [];
		for (const [index, modulePath] of args.slice(2).entries()) {
			lines.push(JSON.stringify({i: index, modulePath, phase: 'start'}));
			const failing = [...probeFailures].some((name) => modulePath.includes(name));
			lines.push(
				JSON.stringify(
					failing
						? {i: index, modulePath, phase: 'load-error', message: 'dlopen failed'}
						: {i: index, modulePath, phase: 'done'},
				),
			);
		}
		return {stdout: `${lines.join('\n')}\n`, stderr: '', status: 0, signal: null, error: undefined};
	}

	function requireStub(specifier) {
		if (specifier === 'node:child_process') return {spawnSync: spawnSyncStub};
		if (specifier === 'node:crypto') return crypto;
		if (specifier === 'node:path') return nodePath;
		if (specifier === 'node:fs') {
			return {
				existsSync: (path) => files.has(path),
				mkdirSync: () => {},
				readFileSync: (path) => {
					if (!files.has(path)) throw new Error(`ENOENT: ${path}`);
					return files.get(path);
				},
				writeFileSync: (path, contents) => files.set(path, contents),
			};
		}
		if (specifier === 'node:module') {
			return {
				createRequire: () => ({
					resolve: (name) => {
						if (unresolvable.has(name)) throw new Error(`Cannot find module '${name}'`);
						return `/app/node_modules/${name}/index.js`;
					},
				}),
			};
		}
		if (specifier === '@electron/main/NativeModulePreflightModules.json') {
			return JSON.parse(readFileSync(new URL('./NativeModulePreflightModules.json', import.meta.url), 'utf8'));
		}
		if (specifier === '@electron/main/AppStoreNativeBoundary') return {APP_STORE_ADDON_PACKAGE: '@fluxer/app-store'};
		if (specifier === '@electron/main/GatewaySocketNativeBoundary') {
			return {GATEWAY_SOCKET_ADDON_PACKAGE: '@fluxer/gateway-socket'};
		}
		if (specifier === 'electron') {
			return {
				app: {
					isPackaged,
					getVersion: () => '1.2.3',
					getPath: () => '/userData',
				},
			};
		}
		if (specifier === 'electron-log') {
			return {
				info: () => {},
				warn: (...args) => warnings.push(args),
			};
		}
		throw new Error(`Unexpected import: ${specifier}`);
	}

	const module = {exports: {}};
	const context = vm.createContext({
		exports: module.exports,
		module,
		process: {env: {}, platform: 'linux', arch: 'x64', execPath: '/app/fluxer'},
		require: requireStub,
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});

	return {files, module: module.exports, spawns, warnings};
}

function degradedModules(preflight) {
	return [...preflight.module.runNativeModulePreflight().degraded];
}

describe('NativeModulePreflight', () => {
	test('does nothing and reports no degraded modules when the app is not packaged', () => {
		const preflight = loadPreflight({isPackaged: false});
		assert.deepEqual(degradedModules(preflight), []);
		assert.equal(preflight.spawns.length, 0);
	});

	test('a missing optional addon degrades instead of aborting startup', () => {
		const preflight = loadPreflight({unresolvable: new Set(['@fluxer/app-store'])});
		assert.deepEqual(degradedModules(preflight), ['@fluxer/app-store']);
		const marker = JSON.parse(preflight.files.get(MARKER_PATH));
		assert.equal(marker.version, 2);
		assert.deepEqual(marker.degraded, ['@fluxer/app-store']);
	});

	test('an optional addon that fails to dlopen degrades instead of aborting startup', () => {
		const preflight = loadPreflight({probeFailures: new Set(['@fluxer/gateway-socket'])});
		assert.deepEqual(degradedModules(preflight), ['@fluxer/gateway-socket']);
		assert.equal(preflight.warnings.length, 1);
	});

	test('a required addon still aborts startup and clears the marker', () => {
		const files = new Map([[MARKER_PATH, JSON.stringify({version: 2, fingerprint: 'stale', degraded: []})]]);
		const {module} = loadPreflight({files, probeFailures: new Set(['@fluxer/linux-portals'])});
		assert.throws(() => module.runNativeModulePreflight(), /native module preflight failed/);
		assert.equal(files.get(MARKER_PATH), '');
	});

	test('a matching marker replays the recorded degraded set without probing again', () => {
		const first = loadPreflight({unresolvable: new Set(['@fluxer/gateway-socket'])});
		assert.deepEqual(degradedModules(first), ['@fluxer/gateway-socket']);
		const second = loadPreflight({files: first.files, unresolvable: new Set(['@fluxer/gateway-socket'])});
		assert.deepEqual(degradedModules(second), ['@fluxer/gateway-socket']);
		assert.equal(second.spawns.length, 0);
	});

	test('a version 1 marker is read as no degraded modules', () => {
		const probe = loadPreflight();
		assert.deepEqual(degradedModules(probe), []);
		const {fingerprint} = JSON.parse(probe.files.get(MARKER_PATH));
		const files = new Map([
			[MARKER_PATH, JSON.stringify({version: 1, fingerprint, completedAt: '2026-01-01T00:00:00.000Z'})],
		]);
		const replay = loadPreflight({files});
		assert.deepEqual(degradedModules(replay), []);
		assert.equal(replay.spawns.length, 0);
	});

	test('an unreadable marker is ignored rather than treated as a pass', () => {
		const files = new Map([[MARKER_PATH, 'not json']]);
		const preflight = loadPreflight({files});
		assert.deepEqual(degradedModules(preflight), []);
		assert.equal(preflight.spawns.length, 1);
	});
});

test('every module the preflight requires is a dependency of the desktop app', () => {
	const modules = JSON.parse(readFileSync(new URL('./NativeModulePreflightModules.json', import.meta.url), 'utf8'));
	const manifest = JSON.parse(readFileSync(new URL('../../package.json', import.meta.url), 'utf8'));
	const dependencies = {...manifest.dependencies, ...manifest.optionalDependencies};
	assert.deepEqual(
		modules.map((spec) => spec.name).filter((name) => !Object.hasOwn(dependencies, name)),
		[],
	);
});
