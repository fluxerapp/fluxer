// SPDX-License-Identifier: AGPL-3.0-or-later

const {EventEmitter} = require('node:events');
const {existsSync} = require('node:fs');
const {join, sep} = require('node:path');
const {performance} = require('node:perf_hooks');
const {createNativeLoadError, loadNativeBinding} = require('./loader-diagnostics.cjs');
const MODULE_NAME = '@fluxer/mac-app-audio';

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

function nativeFileName() {
	if (process.platform !== 'darwin') {
		throw new Error(`@fluxer/mac-app-audio is only supported on macOS, got ${process.platform}`);
	}
	switch (process.arch) {
		case 'x64':
			return 'mac-app-audio.darwin-x64.node';
		case 'arm64':
			return 'mac-app-audio.darwin-arm64.node';
		default:
			throw new Error(`Unsupported macOS architecture: ${process.arch}`);
	}
}

let binding = null;
let loadError = null;

if (process.platform === 'darwin') {
	try {
		const nativeRoot = resolveNativeRoot();
		const nativePath = join(nativeRoot, nativeFileName());
		const loaded = loadNativeBinding({
			moduleName: MODULE_NAME,
			nativePath,
			nativeRoot,
			packageDir: __dirname,
			probe: false,
		});
		binding = loaded.binding;
		loadError = loaded.loadError;
		if (loadError) throw loadError;
	} catch (error) {
		loadError = createNativeLoadError({
			moduleName: MODULE_NAME,
			nativeRoot: resolveNativeRoot(),
			packageDir: __dirname,
			reason: 'native loader threw before binding load completed',
			cause: error,
		});
		throw loadError;
	}
}

function pidFromWindowId(windowId) {
	if (!binding) return 0;
	return binding.pidFromWindowId(windowId);
}

function listAudibleApplications() {
	if (!binding) return Promise.resolve([]);
	return binding.listAudibleApplications();
}

function getBackendInfo() {
	if (!binding) {
		return {
			backend: 'mac-app-audio',
			supported: false,
			reason:
				process.platform === 'darwin'
					? `@fluxer/mac-app-audio native binary unavailable: ${loadError?.message ?? 'unknown reason'}`
					: `@fluxer/mac-app-audio is only supported on macOS, got ${process.platform}`,
			minMacosVersion: '12.3',
			minMacosVersionCoreaudio: '14.2',
			detectedMacosVersion: undefined,
			sckAvailable: false,
			coreaudioAvailable: false,
		};
	}
	return binding.getBackendInfo();
}

function getBackendAvailability() {
	if (!binding) {
		return Promise.resolve({
			sck: {supported: false},
			coreaudio: {supported: false},
			screenPermission: 'not-determined',
			audioPermission: 'not-determined',
		});
	}
	return binding.getBackendAvailability();
}

class ProcessLoopback extends EventEmitter {
	constructor(pid, options = {}) {
		super();
		if (!binding) {
			throw loadError || new Error('@fluxer/mac-app-audio binding unavailable');
		}
		this.pid = pid;
		void options.excludeSelf;
		this.excludeSelf = true;
		this.includeProcessTree = options.includeProcessTree ?? true;
		this.backend = options.backend ?? options.macBackend ?? 'auto';
		this.captureScope = options.captureScope ?? options.macCaptureScope ?? options.scope ?? 'process';
		this.started = false;
		this.stopped = false;
		this.closedEmitted = false;
		this.nextTimestampUs = Math.round(performance.now() * 1000);
		this.native = new binding.ProcessLoopback();
		this.native.setFrameCallback((samples) => {
			if (this.stopped) return;
			const copied = new Float32Array(samples);
			const numFrames = copied.length / 2;
			const frame = {
				samples: copied,
				sampleRate: 48000,
				channels: 2,
				timestampUs: this.nextTimestampUs,
			};
			this.nextTimestampUs += Math.round((numFrames / frame.sampleRate) * 1_000_000);
			this.emit('frame', frame);
		});
		this.native.setLifecycleCallback((type, message) => {
			if (type === 'error') {
				this.emit('error', new Error(message || 'macOS app audio stream stopped'));
				return;
			}
			if (type === 'closed') {
				if (this.stopped) {
					this.emitClosedOnce();
					return;
				}
				this.stopped = true;
				Promise.resolve()
					.then(() => this.native.stop())
					.catch(() => {});
				this.emitClosedOnce();
			}
		});
	}

	emitClosedOnce() {
		if (this.closedEmitted) return;
		this.closedEmitted = true;
		this.emit('closed');
	}

	async start() {
		if (this.started || this.stopped) return;
		this.started = true;
		try {
			await this.native.start(this.pid, this.excludeSelf, this.includeProcessTree, this.backend, this.captureScope);
		} catch (error) {
			this.stopped = true;
			this.emit('error', error instanceof Error ? error : new Error(String(error)));
			throw error;
		}
	}

	setScreenAudioSink(handle) {
		if (typeof this.native.setScreenAudioSink !== 'function') return false;
		try {
			return this.native.setScreenAudioSink(handle) !== false;
		} catch {
			return false;
		}
	}

	clearScreenAudioSink() {
		if (typeof this.native.clearScreenAudioSink === 'function') {
			this.native.clearScreenAudioSink();
		}
	}

	async stop() {
		if (this.stopped) return;
		this.stopped = true;
		this.clearScreenAudioSink();
		try {
			await this.native.stop();
		} finally {
			this.emitClosedOnce();
		}
	}
}

module.exports = {
	ProcessLoopback,
	listAudibleApplications,
	getBackendAvailability,
	getBackendInfo,
	pidFromWindowId,
	loadError,
};
