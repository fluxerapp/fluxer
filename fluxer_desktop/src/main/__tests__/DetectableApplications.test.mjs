// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('../DetectableApplications.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

function loadDetectableApplicationsModule({userDataPath, appPath}) {
	const module = {exports: {}};
	const electronApp = {
		isPackaged: false,
		getAppPath: () => appPath,
		getPath: (name) => {
			if (name !== 'userData') throw new Error(`Unexpected path lookup: ${name}`);
			return userDataPath;
		},
	};
	const electronLog = {
		info() {},
		warn() {},
		error() {},
	};
	const localRequire = (specifier) => {
		if (specifier === 'electron') {
			return {app: electronApp};
		}
		if (specifier === 'electron-log') {
			return electronLog;
		}
		if (specifier === '@electron/main/rpc/RpcConstants') {
			return {
				DISCORD_CDN_HOST: 'cdn.discordapp.com',
				EXECUTABLE_EXACT_MATCH_PREFIX: '>',
			};
		}
		return require(specifier);
	};
	const context = vm.createContext({
		module,
		exports: module.exports,
		require: localRequire,
		Buffer,
		console,
		process,
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return module.exports;
}

describe('DetectableApplications', () => {
	test('loads detectables from the synced user-data cache', () => {
		const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-detectables-test-'));
		const appPath = path.dirname(path.dirname(sourcePath));
		const rpcDir = path.join(tempDir, 'rpc');
		fs.mkdirSync(rpcDir, {recursive: true});
		fs.writeFileSync(
			path.join(rpcDir, 'detectables.json'),
			JSON.stringify([
				{
					name: 'Minecraft',
					url: 'https://www.minecraft.net/favicon.ico',
					executables: [{name: '>java', os: 'linux', arguments: 'net.minecraft.client.main.Main'}],
				},
			]),
		);
		const detectablesModule = loadDetectableApplicationsModule({userDataPath: tempDir, appPath});

		detectablesModule.loadDetectableApplications();
		const detectables = detectablesModule.getDetectableDb();

		assert.equal(detectables.length, 1);
		assert.equal(detectables[0]?.name, 'Minecraft');
		assert.ok(detectables[0]?.url?.startsWith('https://'));
		assert.ok(detectables[0]?.executables?.some((exe) => exe.os === 'linux' && exe.name === '>java'));
	});

	test('falls back to an empty database when the cache is missing', () => {
		const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-detectables-empty-'));
		const appPath = path.dirname(path.dirname(sourcePath));
		const detectablesModule = loadDetectableApplicationsModule({userDataPath: tempDir, appPath});

		detectablesModule.loadDetectableApplications();

		assert.deepEqual(detectablesModule.getDetectableDb(), []);
	});
});
