// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

function transform(fileName) {
	const sourcePath = fileURLToPath(new URL(fileName, import.meta.url));
	return esbuild.transformSync(readFileSync(sourcePath, 'utf8'), {
		loader: 'ts',
		format: 'cjs',
		platform: 'node',
		target: 'node20',
		logLevel: 'silent',
	}).code;
}

const reuseSource = transform('./WindowsWindowCaptureItemReuse.ts');
const nativeAddonSource = transform('./NativeAddon.ts');

function loadReuse({platform = 'win32', windowsBuild = 19045, addon}) {
	const logged = [];
	const calls = {install: 0, loads: 0};
	const counts = {forwarded: 0, reused: 0};
	const fakeAddon = addon ?? {
		loadError: null,
		reuseWindowCaptureItems: () => {
			calls.install += 1;
			return true;
		},
		windowCaptureItemCalls: () => ({...counts}),
	};
	const context = vm.createContext({console, process: {platform, argv: []}});
	const load = (source) => {
		const module = {exports: {}};
		vm.runInContext(`(function (module, exports, require) {${source}\n})`, context)(
			module,
			module.exports,
			fakeRequire,
		);
		return module.exports;
	};
	const fakeRequire = (specifier) => {
		if (specifier === 'node:module') {
			return {
				createRequire: () => (name) => {
					assert.equal(name, '@fluxer/win-game-capture');
					calls.loads += 1;
					return fakeAddon;
				},
			};
		}
		if (specifier === '@electron/main/NativeAddon') return load(nativeAddonSource);
		if (specifier === '@electron/main/LaunchOptions') return {isSafeModeLaunch: () => false};
		if (specifier === '@electron/main/WindowsScreenCaptureGuard') return {getWindowsBuildNumber: () => windowsBuild};
		if (specifier === 'electron-log') {
			const record =
				(level) =>
				(...args) =>
					logged.push([level, ...args]);
			return {info: record('info'), warn: record('warn')};
		}
		throw new Error(`unexpected require ${specifier}`);
	};
	return {...load(reuseSource), calls, counts, logged};
}

const windowSource = {id: 'window:1234:0', name: 'Notepad'};

describe('Windows window capture item reuse', () => {
	test('installs once, only for window shares on Windows 10 2004 to 22H2', () => {
		const cases = [
			[17763, 0],
			[18362, 0],
			[19040, 0],
			[19041, 1],
			[19044, 1],
			[19045, 1],
			[19046, 0],
			[20348, 0],
			[22000, 0],
			[26100, 0],
			[null, 0],
		];
		for (const [windowsBuild, installs] of cases) {
			const reuse = loadReuse({windowsBuild});
			reuse.reuseWindowsCaptureItemsForSource({id: 'screen:0:0', name: 'Screen 1'});
			assert.equal(reuse.calls.loads, 0, `build ${windowsBuild} screen share`);
			reuse.reuseWindowsCaptureItemsForSource(windowSource);
			reuse.reuseWindowsCaptureItemsForSource({id: 'window:5678:0', name: 'Explorer'});
			assert.equal(reuse.calls.install, installs, `build ${windowsBuild}`);
			assert.equal(reuse.calls.loads, installs, `build ${windowsBuild}`);
		}
		for (const platform of ['darwin', 'linux']) {
			const reuse = loadReuse({platform});
			reuse.reuseWindowsCaptureItemsForSource(windowSource);
			assert.equal(reuse.calls.loads, 0, platform);
		}
	});

	test('reports the capture item requests made since the previous report', () => {
		const reuse = loadReuse({});
		reuse.reuseWindowsCaptureItemsForSource(windowSource);
		Object.assign(reuse.counts, {forwarded: 3, reused: 53999});
		reuse.reportWindowsCaptureItemReuse('streaming-priority-release');
		reuse.reportWindowsCaptureItemReuse('streaming-priority-reset');
		Object.assign(reuse.counts, {forwarded: 6, reused: 54009});
		reuse.reportWindowsCaptureItemReuse('streaming-priority-release');
		const reports = reuse.logged
			.filter(([, message]) => message.includes('since the last report'))
			.map(([, , fields]) => ({...fields}));
		assert.deepEqual(reports, [
			{reason: 'streaming-priority-release', forwardedToCaptureService: 3, reusedFromLiveShare: 53999},
			{reason: 'streaming-priority-release', forwardedToCaptureService: 3, reusedFromLiveShare: 10},
		]);
	});

	test('a failing addon never throws into the share request', () => {
		const reuse = loadReuse({
			addon: {
				loadError: null,
				reuseWindowCaptureItems: () => {
					throw new Error('binding exploded');
				},
				windowCaptureItemCalls: () => null,
			},
		});
		assert.doesNotThrow(() => reuse.reuseWindowsCaptureItemsForSource(windowSource));
		assert.doesNotThrow(() => reuse.reuseWindowsCaptureItemsForSource(windowSource));
		assert.doesNotThrow(() => reuse.reportWindowsCaptureItemReuse('streaming-priority-release'));
		assert.equal(reuse.logged.filter(([level]) => level === 'warn').length, 1);
	});
});
