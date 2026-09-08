// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import path from 'node:path';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./WindowsShortcuts.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

const APPDATA = 'C:\\Users\\csh\\AppData\\Roaming';
const USERPROFILE = 'C:\\Users\\csh';
const PROGRAMS_DIR = path.win32.join(APPDATA, 'Microsoft', 'Windows', 'Start Menu', 'Programs');
const AUTHOR_DIR = path.win32.join(PROGRAMS_DIR, 'Fluxer Platform AB');
const ROOT_SHORTCUT = path.win32.join(PROGRAMS_DIR, 'Fluxer Canary.lnk');
const AUTHOR_SHORTCUT = path.win32.join(AUTHOR_DIR, 'Fluxer Canary.lnk');
const ROOT_APP_DIR = 'C:\\Users\\csh\\AppData\\Local\\fluxer_desktop_canary';
const CURRENT_DIR = path.win32.join(ROOT_APP_DIR, 'current');
const CURRENT_EXE = path.win32.join(CURRENT_DIR, 'Fluxer Canary.exe');
const STALE_EXE = path.win32.join(ROOT_APP_DIR, 'app-1.0.0', 'Fluxer Canary.exe');
const LEGACY_APP_USER_MODEL_ID = 'velopack.fluxer_desktop_canary';

function lnkBuffer(...values) {
	return Buffer.concat([Buffer.from('L\0\0\0', 'utf8'), ...values.map((value) => Buffer.from(value, 'utf16le'))]);
}

function loadWindowsShortcuts(initialFiles) {
	const files = new Map(initialFiles);
	const dirs = new Set([PROGRAMS_DIR, ROOT_APP_DIR, CURRENT_DIR]);
	for (const filePath of files.keys()) {
		dirs.add(path.win32.dirname(filePath));
	}
	files.set(path.win32.join(ROOT_APP_DIR, 'Update.exe'), Buffer.from('exe'));

	const fsOperations = [];
	const fakeFs = {
		existsSync: (target) => files.has(target) || dirs.has(target),
		readFileSync: (target) => {
			const contents = files.get(target);
			if (!contents) throw new Error(`ENOENT ${target}`);
			return contents;
		},
		mkdirSync: (target) => {
			fsOperations.push(['mkdir', target]);
			dirs.add(target);
		},
		renameSync: (from, to) => {
			fsOperations.push(['rename', from, to]);
			files.set(to, files.get(from));
			files.delete(from);
		},
		rmSync: (target) => {
			fsOperations.push(['rm', target]);
			files.delete(target);
		},
	};

	const createdShortcuts = [];
	const winShell = {
		loadError: null,
		createShortcut: async (options) => {
			createdShortcuts.push(options);
			files.set(options.lnkPath, lnkBuffer(options.target, options.appUserModelId));
		},
	};

	const module = {exports: {}};
	const context = vm.createContext({
		module,
		exports: module.exports,
		console,
		Buffer,
		process: {platform: 'win32', execPath: CURRENT_EXE, env: {APPDATA, USERPROFILE}},
		require: (specifier) => {
			if (specifier === 'node:fs') {
				return {default: fakeFs, ...fakeFs};
			}
			if (specifier === 'node:path') {
				return {default: path.win32, ...path.win32};
			}
			if (specifier === 'node:module') {
				return {
					createRequire: () => (moduleName) => {
						if (moduleName === '@fluxer/win-shell') return winShell;
						throw new Error(`unexpected require ${moduleName}`);
					},
				};
			}
			if (specifier === '@electron/common/DesktopIdentity') {
				return {
					DESKTOP_APP_NAME: 'Fluxer Canary',
					WINDOWS_APP_USER_MODEL_ID: 'Fluxer.Fluxer.Canary',
					WINDOWS_LEGACY_APP_USER_MODEL_IDS: [LEGACY_APP_USER_MODEL_ID],
					WINDOWS_SHORTCUT_AUTHOR: 'Fluxer Platform AB',
					WINDOWS_TOAST_ACTIVATOR_CLSID: '{9CEDB5C0-3552-43B0-A279-2232E0CDF74C}',
				};
			}
			return require(specifier);
		},
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return {...module.exports, createdShortcuts, dirs, files, fsOperations};
}

async function runRepair(harness) {
	harness.repairWindowsShortcuts();
	await new Promise((resolve) => setImmediate(resolve));
}

describe('Windows Start Menu shortcut repair', () => {
	test('leaves a user-placed root shortcut alone and does not recreate the author folder', async () => {
		const harness = loadWindowsShortcuts([[ROOT_SHORTCUT, lnkBuffer(CURRENT_EXE)]]);

		await runRepair(harness);

		assert.deepEqual(harness.fsOperations, []);
		assert.equal(harness.files.has(ROOT_SHORTCUT), true);
		assert.equal(harness.files.has(AUTHOR_SHORTCUT), false);
		assert.equal(harness.dirs.has(AUTHOR_DIR), false);
	});

	test('does not delete a root shortcut when the author shortcut also exists', async () => {
		const harness = loadWindowsShortcuts([
			[ROOT_SHORTCUT, lnkBuffer(CURRENT_EXE)],
			[AUTHOR_SHORTCUT, lnkBuffer(CURRENT_EXE)],
		]);

		await runRepair(harness);

		assert.deepEqual(harness.fsOperations, []);
		assert.equal(harness.files.has(ROOT_SHORTCUT), true);
		assert.equal(harness.files.has(AUTHOR_SHORTCUT), true);
	});

	test('rewrites a stale root shortcut in place instead of relocating it', async () => {
		const harness = loadWindowsShortcuts([[ROOT_SHORTCUT, lnkBuffer(STALE_EXE)]]);

		await runRepair(harness);

		assert.deepEqual(harness.fsOperations, []);
		const rewritten = harness.createdShortcuts.filter((options) => options.lnkPath === ROOT_SHORTCUT);
		assert.equal(rewritten.length, 1);
		assert.equal(rewritten[0].target, CURRENT_EXE);
		assert.equal(rewritten[0].appUserModelId, 'Fluxer.Fluxer.Canary');
		assert.equal(harness.files.has(AUTHOR_SHORTCUT), false);
	});

	test('still rewrites the author shortcut carrying the legacy AppUserModelID', async () => {
		const harness = loadWindowsShortcuts([[AUTHOR_SHORTCUT, lnkBuffer(CURRENT_EXE, LEGACY_APP_USER_MODEL_ID)]]);

		await runRepair(harness);

		const rewritten = harness.createdShortcuts.filter((options) => options.lnkPath === AUTHOR_SHORTCUT);
		assert.equal(rewritten.length, 1);
		assert.equal(rewritten[0].target, CURRENT_EXE);
		assert.equal(rewritten[0].appUserModelId, 'Fluxer.Fluxer.Canary');
		assert.equal(rewritten[0].toastActivatorClsid, '{9CEDB5C0-3552-43B0-A279-2232E0CDF74C}');
	});
});
