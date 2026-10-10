// SPDX-License-Identifier: AGPL-3.0-or-later

const {EventEmitter} = require('node:events');
const {existsSync} = require('node:fs');
const {join, sep} = require('node:path');
const {createNativeLoadError, loadNativeBinding} = require('./loader-diagnostics.cjs');
const MODULE_NAME = '@fluxer/win-process-loopback';

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

function nativeFileName(arch) {
	switch (arch) {
		case 'x64':
			return 'win-process-loopback.win32-x64-msvc.node';
		case 'arm64':
			return 'win-process-loopback.win32-arm64-msvc.node';
		default:
			return null;
	}
}

let binding = null;
let loadError = null;

if (process.platform === 'win32') {
	const nativeRoot = resolveNativeRoot();
	const fileName = nativeFileName(process.arch);
	if (!fileName) {
		loadError = createNativeLoadError({
			moduleName: MODULE_NAME,
			nativeRoot,
			packageDir: __dirname,
			reason: `unsupported Windows architecture: ${process.arch}`,
		});
		throw loadError;
	} else {
		const nativePath = join(nativeRoot, fileName);
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
	}
} else {
	loadError = createNativeLoadError({
		moduleName: MODULE_NAME,
		nativeRoot: resolveNativeRoot(),
		packageDir: __dirname,
		reason: `not supported on platform ${process.platform}`,
	});
}

function isSupported() {
	if (!binding) return false;
	try {
		return Boolean(binding.isSupported());
	} catch {
		return false;
	}
}

function getBackendInfo() {
	if (!binding) {
		const reason =
			process.platform !== 'win32'
				? `@fluxer/win-process-loopback only supports Windows, got ${process.platform}. Microsoft's documented process-loopback minimum is Windows build 20348.`
				: `@fluxer/win-process-loopback native binary unavailable: ${loadError?.message ?? 'unknown reason'}`;
		return {
			backend: 'win-process-loopback',
			supported: false,
			reason,
			processSupported: false,
			systemSupported: false,
			systemExcludesSelf: false,
			processIncludeSupported: false,
			processExcludeSupported: false,
			sessionMixerSupported: false,
			systemLoopbackMode: 'unavailable',
			minWindowsBuild: 20348,
			minWindowsVersionLabel: 'Windows build 20348 (documented)',
			detectedWindowsBuild: undefined,
		};
	}
	try {
		return binding.getBackendInfo();
	} catch (err) {
		return {
			backend: 'win-process-loopback',
			supported: false,
			reason: `@fluxer/win-process-loopback getBackendInfo failed: ${err?.message ?? String(err)}`,
			processSupported: false,
			systemSupported: false,
			systemExcludesSelf: false,
			processIncludeSupported: false,
			processExcludeSupported: false,
			sessionMixerSupported: false,
			systemLoopbackMode: 'unavailable',
			minWindowsBuild: 20348,
			minWindowsVersionLabel: 'Windows build 20348 (documented)',
			detectedWindowsBuild: undefined,
		};
	}
}

function pidFromHwnd(hwnd) {
	if (!binding) return 0;
	try {
		return binding.pidFromHwnd(hwnd);
	} catch {
		return 0;
	}
}

function resolveAudioRootPid(pid) {
	if (!binding) return pid;
	try {
		return binding.resolveAudioRootPid(pid);
	} catch {
		return pid;
	}
}

class ProcessLoopback extends EventEmitter {
	constructor(pid, opts = {}) {
		super();
		if (!binding) {
			throw loadError ?? new Error('@fluxer/win-process-loopback binding unavailable');
		}
		const captureScope = opts.captureScope ?? opts.winCaptureScope ?? opts.scope ?? 'process';
		const includeProcessTree =
			captureScope === 'system' || captureScope === 'session-mixer' ? false : opts.includeProcessTree !== false;
		const sampleRate = opts.sampleRate;
		const channels = opts.channels;
		this._stopped = false;
		this._started = false;
		this._startPromise = null;
		this._resolveStart = null;
		this._rejectStart = null;
		this._native = new binding.ProcessLoopback(
			pid,
			{includeProcessTree, sampleRate, channels, captureScope},
			(frame) => this.emit('frame', frame),
			(err) => {
				if (!this._started && this._rejectStart) {
					this._rejectStart(err);
					this._clearStartWaiters();
				}
				this.emit('error', err);
			},
			() => {
				if (this._stopped) return;
				this._stopped = true;
				if (!this._started && this._rejectStart) {
					this._rejectStart(new Error('ProcessLoopback closed before audio capture started'));
					this._clearStartWaiters();
				}
				queueMicrotask(() => {
					try {
						this._native.dispose();
					} catch {}
				});
				this.emit('closed');
			},
			() => {
				this._started = true;
				if (this._resolveStart) {
					this._resolveStart();
					this._clearStartWaiters();
				}
				this.emit('started');
			},
		);
	}

	_clearStartWaiters() {
		this._resolveStart = null;
		this._rejectStart = null;
	}

	setScreenAudioSink(handle) {
		if (typeof this._native.setScreenAudioSink !== 'function') return false;
		try {
			return this._native.setScreenAudioSink(handle) !== false;
		} catch {
			return false;
		}
	}

	clearScreenAudioSink() {
		if (typeof this._native.clearScreenAudioSink === 'function') {
			this._native.clearScreenAudioSink();
		}
	}

	start() {
		if (this._stopped) return Promise.resolve();
		if (this._started) return Promise.resolve();
		if (this._startPromise) return this._startPromise;
		this._startPromise = new Promise((resolve, reject) => {
			this._resolveStart = resolve;
			this._rejectStart = reject;
			try {
				this._native.start();
			} catch (error) {
				this._clearStartWaiters();
				reject(error);
			}
		});
		return this._startPromise;
	}

	stop() {
		if (this._stopped) return;
		this._stopped = true;
		this.clearScreenAudioSink();
		if (!this._started && this._rejectStart) {
			this._rejectStart(new Error('ProcessLoopback stopped before audio capture started'));
			this._clearStartWaiters();
		}
		this._native.dispose();
	}
}

module.exports = {
	isSupported,
	getBackendInfo,
	pidFromHwnd,
	resolveAudioRootPid,
	ProcessLoopback,
	get loadError() {
		return loadError;
	},
};
