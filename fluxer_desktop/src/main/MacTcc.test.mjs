// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./MacTcc.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

function loadMacTcc({
	platform = 'darwin',
	addon = null,
	addonError = null,
	mediaAccessStatus = 'granted',
	probe = null,
	screenRecordingRequested = false,
} = {}) {
	const handlers = new Map();
	const mediaAccessCalls = [];
	const config = {screenRecordingRequested};

	function requireStub(specifier) {
		if (specifier === 'node:module') {
			return {
				createRequire: () => (moduleSpecifier) => {
					if (moduleSpecifier === '@fluxer/mac-tcc') {
						if (addonError) throw addonError;
						if (!addon) throw new Error('No fake addon configured for @fluxer/mac-tcc');
						return addon;
					}
					if (moduleSpecifier === '@fluxer/mac-screen-capture') {
						if (!probe) throw new Error('No fake addon configured for @fluxer/mac-screen-capture');
						return probe;
					}
					throw new Error(`Unexpected createRequire import: ${moduleSpecifier}`);
				},
			};
		}
		if (specifier === '@electron/common/DesktopConfig') {
			return {
				hasRequestedMacScreenRecording: () => config.screenRecordingRequested,
				markMacScreenRecordingRequested: () => {
					config.screenRecordingRequested = true;
				},
			};
		}
		if (specifier === '@electron/common/Logger') {
			return {
				createChildLogger: () => ({
					info: () => {},
					warn: () => {},
				}),
			};
		}
		if (specifier === 'electron') {
			return {
				ipcMain: {
					handle(channel, handler) {
						handlers.set(channel, handler);
					},
					removeHandler(channel) {
						handlers.delete(channel);
					},
				},
				systemPreferences: {
					getMediaAccessStatus(type) {
						mediaAccessCalls.push(type);
						return mediaAccessStatus;
					},
				},
			};
		}
		throw new Error(`Unexpected import: ${specifier}`);
	}

	const module = {exports: {}};
	const context = vm.createContext({
		exports: module.exports,
		module,
		process: {env: {}, platform},
		require: requireStub,
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});

	return {config, handlers, mediaAccessCalls, module: module.exports};
}

function makeProbe(results) {
	const probe = {
		calls: 0,
		loadError: null,
		probeScreenRecordingAccess: async () => {
			probe.calls += 1;
			return results.length > 1 ? results.shift() : results[0];
		},
	};
	return probe;
}

function makeAddon(overrides = {}) {
	return {
		screenRecordingStatus: () => 'denied',
		requestScreenRecording: () => 'denied',
		inputMonitoringStatus: () => 'denied',
		requestInputMonitoring: () => 'denied',
		probeInputMonitoringAccess: () => 'denied',
		loadError: null,
		...overrides,
	};
}

describe('MacTcc', () => {
	test('reads statuses from the addon when it loads', () => {
		const {module, mediaAccessCalls} = loadMacTcc({
			addon: makeAddon({
				screenRecordingStatus: () => 'granted',
				inputMonitoringStatus: () => 'denied',
			}),
		});
		assert.equal(module.getTccStatus('screen-recording'), 'granted');
		assert.equal(module.getTccStatus('input-monitoring'), 'denied');
		assert.deepEqual(mediaAccessCalls, []);
	});

	test('falls back to systemPreferences for screen recording when the addon is missing', () => {
		const {module, mediaAccessCalls} = loadMacTcc({
			addonError: new Error('addon not built'),
			mediaAccessStatus: 'denied',
		});
		assert.equal(module.getTccStatus('screen-recording'), 'denied');
		assert.deepEqual(mediaAccessCalls, ['screen']);
	});

	test('maps restricted media access to denied in the screen fallback', () => {
		const {module} = loadMacTcc({
			addonError: new Error('addon not built'),
			mediaAccessStatus: 'restricted',
		});
		assert.equal(module.getTccStatus('screen-recording'), 'denied');
	});

	test('reports not-determined for input monitoring when the addon is missing', () => {
		const {module, mediaAccessCalls} = loadMacTcc({
			addonError: new Error('addon not built'),
		});
		assert.equal(module.getTccStatus('input-monitoring'), 'not-determined');
		assert.deepEqual(mediaAccessCalls, []);
	});

	test('treats an addon load error like a missing addon', () => {
		const {module, mediaAccessCalls} = loadMacTcc({
			addon: makeAddon({loadError: new Error('dlopen failed')}),
			mediaAccessStatus: 'granted',
		});
		assert.equal(module.getTccStatus('screen-recording'), 'granted');
		assert.deepEqual(mediaAccessCalls, ['screen']);
	});

	test('never touches systemPreferences off macOS', () => {
		const {module, mediaAccessCalls} = loadMacTcc({platform: 'linux'});
		assert.equal(module.getTccStatus('screen-recording'), 'not-determined');
		assert.equal(module.getTccStatus('input-monitoring'), 'not-determined');
		assert.deepEqual(mediaAccessCalls, []);
	});

	test('screen recording is not determined until the app has asked for it', async () => {
		const probe = makeProbe(['granted']);
		const {handlers, module} = loadMacTcc({addon: makeAddon(), probe});
		module.registerMacTccIpcHandlers();
		assert.equal(await handlers.get('mac-tcc:status')(null, 'screen-recording'), 'not-determined');
		assert.equal(probe.calls, 0);
	});

	test('asking for screen recording is remembered and reads as denied until it is granted', async () => {
		const requests = [];
		const {config, handlers, module} = loadMacTcc({
			addon: makeAddon({
				requestScreenRecording: () => {
					requests.push('screen');
					return 'denied';
				},
			}),
			probe: makeProbe(['denied']),
		});
		module.registerMacTccIpcHandlers();
		assert.equal(handlers.get('mac-tcc:request')(null, 'screen-recording'), 'denied');
		assert.deepEqual(requests, ['screen']);
		assert.equal(config.screenRecordingRequested, true);
		assert.equal(await handlers.get('mac-tcc:status')(null, 'screen-recording'), 'denied');
	});

	test('a grant made while the app runs is picked up by the live probe', async () => {
		const probe = makeProbe(['denied', 'granted']);
		const {handlers, module} = loadMacTcc({addon: makeAddon(), probe, screenRecordingRequested: true});
		module.registerMacTccIpcHandlers();
		const status = handlers.get('mac-tcc:status');
		assert.equal(await status(null, 'screen-recording'), 'denied');
		assert.equal(module.getTccStatus('screen-recording'), 'denied');
		assert.equal(await status(null, 'screen-recording'), 'granted');
		assert.equal(module.getTccStatus('screen-recording'), 'granted');
		assert.equal(probe.calls, 2);
	});

	test('a live grant that is taken away again reads as denied', async () => {
		const probe = makeProbe(['granted', 'denied']);
		const {module} = loadMacTcc({addon: makeAddon(), probe, screenRecordingRequested: true});
		assert.equal(await module.refreshTccStatus('screen-recording'), 'granted');
		assert.equal(await module.refreshTccStatus('screen-recording'), 'denied');
	});

	test('a probe that times out or throws keeps the last known answer', async () => {
		const probe = makeProbe(['granted', 'timeout']);
		const {module} = loadMacTcc({addon: makeAddon(), probe, screenRecordingRequested: true});
		assert.equal(await module.refreshTccStatus('screen-recording'), 'granted');
		assert.equal(await module.refreshTccStatus('screen-recording'), 'granted');
		probe.probeScreenRecordingAccess = async () => {
			throw new Error('probe crashed');
		};
		assert.equal(await module.refreshTccStatus('screen-recording'), 'granted');
		const stuck = loadMacTcc({addon: makeAddon(), probe: makeProbe(['timeout']), screenRecordingRequested: true});
		assert.equal(await stuck.module.refreshTccStatus('screen-recording'), 'denied');
	});

	test('the probe is skipped when the preflight already reports a grant', async () => {
		const probe = makeProbe(['denied']);
		const {module} = loadMacTcc({
			addon: makeAddon({screenRecordingStatus: () => 'granted'}),
			probe,
			screenRecordingRequested: true,
		});
		assert.equal(await module.refreshTccStatus('screen-recording'), 'granted');
		assert.equal(probe.calls, 0);
	});

	test('overlapping status reads share one probe', async () => {
		let release;
		const probe = {
			calls: 0,
			loadError: null,
			probeScreenRecordingAccess: () => {
				probe.calls += 1;
				return new Promise((resolve) => {
					release = () => resolve('granted');
				});
			},
		};
		const {module} = loadMacTcc({addon: makeAddon(), probe, screenRecordingRequested: true});
		const first = module.refreshTccStatus('screen-recording');
		const second = module.refreshTccStatus('screen-recording');
		release();
		assert.deepEqual(await Promise.all([first, second]), ['granted', 'granted']);
		assert.equal(probe.calls, 1);
	});

	test('a capture addon without the probe leaves the preflight answer in place', async () => {
		const {module} = loadMacTcc({addon: makeAddon(), probe: {loadError: null}, screenRecordingRequested: true});
		assert.equal(await module.refreshTccStatus('screen-recording'), 'denied');
		const missing = loadMacTcc({addon: makeAddon(), screenRecordingRequested: true});
		assert.equal(await missing.module.refreshTccStatus('screen-recording'), 'denied');
	});

	test('status and request reads are reported to the listener', async () => {
		const seen = [];
		const {handlers, module} = loadMacTcc({
			addon: makeAddon({inputMonitoringStatus: () => 'granted', probeInputMonitoringAccess: () => 'granted'}),
		});
		module.registerMacTccIpcHandlers({onStatus: (surface, status) => seen.push(`${surface}:${status}`)});
		assert.equal(await handlers.get('mac-tcc:status')(null, 'input-monitoring'), 'granted');
		assert.equal(handlers.get('mac-tcc:request')(null, 'screen-recording'), 'denied');
		assert.deepEqual(seen, ['input-monitoring:granted', 'screen-recording:denied']);
	});

	test('input monitoring is never probed before macOS has asked', () => {
		let probes = 0;
		const {module} = loadMacTcc({
			addon: makeAddon({
				inputMonitoringStatus: () => 'not-determined',
				probeInputMonitoringAccess: () => {
					probes += 1;
					return 'granted';
				},
			}),
		});
		assert.equal(module.getTccStatus('input-monitoring'), 'not-determined');
		assert.equal(probes, 0);
	});

	test('a grant made while the app runs is read from the live tap probe', () => {
		const access = {probe: 'denied'};
		const {module} = loadMacTcc({
			addon: makeAddon({
				inputMonitoringStatus: () => 'denied',
				probeInputMonitoringAccess: () => access.probe,
			}),
		});
		assert.equal(module.getTccStatus('input-monitoring'), 'denied');
		access.probe = 'granted';
		assert.equal(module.getTccStatus('input-monitoring'), 'granted');
	});

	test('a grant taken away while the app runs reads as denied', () => {
		const {module} = loadMacTcc({
			addon: makeAddon({
				inputMonitoringStatus: () => 'granted',
				probeInputMonitoringAccess: () => 'denied',
			}),
		});
		assert.equal(module.getTccStatus('input-monitoring'), 'denied');
	});

	test('asking for input monitoring lets later reads use the live probe', async () => {
		const access = {probe: 'denied'};
		const {handlers, module} = loadMacTcc({
			addon: makeAddon({
				inputMonitoringStatus: () => 'not-determined',
				requestInputMonitoring: () => 'denied',
				probeInputMonitoringAccess: () => access.probe,
			}),
		});
		module.registerMacTccIpcHandlers();
		assert.equal(handlers.get('mac-tcc:request')(null, 'input-monitoring'), 'denied');
		access.probe = 'granted';
		assert.equal(await handlers.get('mac-tcc:status')(null, 'input-monitoring'), 'granted');
	});

	test('request handlers fall back to the status path when the addon is missing', () => {
		const {handlers, module, mediaAccessCalls} = loadMacTcc({
			addonError: new Error('addon not built'),
			mediaAccessStatus: 'denied',
		});
		module.registerMacTccIpcHandlers();
		const request = handlers.get('mac-tcc:request');
		assert.equal(request(null, 'screen-recording'), 'denied');
		assert.equal(request(null, 'input-monitoring'), 'not-determined');
		assert.deepEqual(mediaAccessCalls, ['screen']);
	});
});
