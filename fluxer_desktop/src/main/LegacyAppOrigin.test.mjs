// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import {createRequire} from 'node:module';
import os from 'node:os';
import path from 'node:path';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

function transform(relativePath) {
	const sourcePath = fileURLToPath(new URL(relativePath, import.meta.url));
	const code = esbuild.transformSync(fs.readFileSync(sourcePath, 'utf8'), {
		loader: 'ts',
		format: 'cjs',
		platform: 'node',
		target: 'node20',
	}).code;
	return {sourcePath, code};
}

const constantsSource = transform('../common/Constants.ts');
const localAppRouteContractSource = transform('../../../packages/desktop_ipc/src/LocalAppRouteContract.ts');
const desktopConfigSource = transform('../common/DesktopConfig.ts');
const globalShortcutActionsSource = transform('../common/GlobalShortcutActions.ts');

const silentLog = {debug() {}, info() {}, warn() {}, error() {}};

function runModule({sourcePath, code}, requireStub) {
	const module = {exports: {}};
	const context = vm.createContext({
		require: requireStub,
		module,
		exports: module.exports,
		process,
		console,
		URL,
		JSON,
	});
	vm.runInContext(code, context, {filename: sourcePath});
	return module.exports;
}

function loadDesktop({channel = 'stable', settings} = {}) {
	const userDataPath = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-legacy-app-origin-test-'));
	const settingsPath = path.join(userDataPath, 'settings.json');
	if (settings !== undefined) {
		fs.writeFileSync(settingsPath, JSON.stringify(settings), 'utf-8');
	}
	const localAppRouteContract = runModule(localAppRouteContractSource, (specifier) => {
		throw new Error(`Unexpected import: ${specifier}`);
	});
	const constants = runModule(constantsSource, (specifier) => {
		if (specifier === '@electron/common/BuildChannel') return {BUILD_CHANNEL: channel};
		if (specifier === '@fluxer/desktop_ipc/src/LocalAppRouteContract') return localAppRouteContract;
		throw new Error(`Unexpected import: ${specifier}`);
	});
	const globalShortcutActions = runModule(globalShortcutActionsSource, (specifier) => {
		throw new Error(`Unexpected import: ${specifier}`);
	});
	const desktopConfig = runModule(desktopConfigSource, (specifier) => {
		if (specifier === 'node:fs') return fs;
		if (specifier === '@electron/common/GlobalShortcutActions') return globalShortcutActions;
		if (specifier === 'node:path') return path;
		if (specifier === '@electron/common/BuildChannel') return {BUILD_CHANNEL: channel};
		if (specifier === '@electron/common/Constants') return constants;
		if (specifier === '@fluxer/instance_bootstrap/src/EndpointNormalization') {
			return {InstanceEndpointKind: {API: 'api'}, normalizeInstanceEndpoint: () => null};
		}
		if (specifier === 'electron-log') return silentLog;
		throw new Error(`Unexpected import: ${specifier}`);
	});
	desktopConfig.loadDesktopConfig(userDataPath);
	return desktopConfig;
}

describe('DesktopConfig legacy app origin', () => {
	test('harvests the legacy web app origin when no app origin is stored', () => {
		assert.equal(loadDesktop().getLegacyAppOrigin(), 'https://web.fluxer.app');
		assert.equal(loadDesktop({channel: 'canary'}).getLegacyAppOrigin(), 'https://web.canary.fluxer.app');
	});

	test('harvests the migrated origin an older build stored', () => {
		const stable = loadDesktop({settings: {app_origin: 'https://fluxer.com'}});
		const canary = loadDesktop({channel: 'canary', settings: {app_origin: 'https://canary.fluxer.com'}});

		assert.equal(stable.getLegacyAppOrigin(), 'https://fluxer.com');
		assert.equal(canary.getLegacyAppOrigin(), 'https://canary.fluxer.com');
	});

	test('harvests the legacy web app origin for a stored legacy origin', () => {
		const desktopConfig = loadDesktop({settings: {app_origin: 'https://web.fluxer.app'}});

		assert.equal(desktopConfig.getLegacyAppOrigin(), 'https://web.fluxer.app');
	});

	test('drops stored origins outside the channel allowlist', () => {
		for (const appOrigin of [
			'https://canary.fluxer.com',
			'https://fluxer.com/',
			'https://evil.example',
			'http://fluxer.com',
			42,
		]) {
			const desktopConfig = loadDesktop({settings: {app_origin: appOrigin}});
			assert.equal(desktopConfig.getLegacyAppOrigin(), 'https://web.fluxer.app');
		}
	});
});
