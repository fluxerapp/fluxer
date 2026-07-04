// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import vm from 'node:vm';
import {fileURLToPath} from 'node:url';
import {createRequire} from 'node:module';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('../DetectableApplications.ts', import.meta.url));
const source = fs.readFileSync(sourcePath, 'utf8');
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

const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-detectables-test-'));
const appPath = path.dirname(path.dirname(sourcePath));
const rpcDir = path.join(tempDir, 'rpc');
fs.mkdirSync(path.join(rpcDir, 'data'), {recursive: true});
fs.writeFileSync(
	path.join(rpcDir, 'data', 'detectables.json'),
	JSON.stringify([
		{
			name: 'Minecraft',
			executables: [{name: '>java', os: 'linux', arguments: 'net.minecraft.client.main.Main'}],
			presence_assets: {mode_0: 'osu/mode_0.png'},
		},
	]),
);

const detectablesModule = loadDetectableApplicationsModule({userDataPath: tempDir, appPath});

assert.equal(detectablesModule.resolveMappedRpcImage('minecraft', {oops: true}), undefined);
assert.equal(detectablesModule.resolveMappedRpcImage('minecraft', ['bad']), undefined);
assert.equal(
	detectablesModule.resolveMappedRpcImage('minecraft', 'https://example.com/cover.png'),
	'https://example.com/cover.png',
);

console.log('DetectableApplications test passed');
