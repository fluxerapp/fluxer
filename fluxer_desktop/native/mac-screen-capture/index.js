// SPDX-License-Identifier: AGPL-3.0-or-later

const {EventEmitter} = require('node:events');
const {existsSync} = require('node:fs');
const {join, sep} = require('node:path');
const {createNativeLoadError, loadNativeBinding} = require('./loader-diagnostics.cjs');
const MODULE_NAME = '@fluxer/mac-screen-capture';

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

function nativeFileName() {
	if (process.platform !== 'darwin') {
		throw new Error(`@fluxer/mac-screen-capture is only supported on macOS, got ${process.platform}`);
	}
	switch (process.arch) {
		case 'x64':
			return 'mac-screen-capture.darwin-x64.node';
		case 'arm64':
			return 'mac-screen-capture.darwin-arm64.node';
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

function getBackendInfo() {
	if (!binding) {
		return {
			backend: 'mac-screen-capture',
			supported: false,
			reason:
				process.platform === 'darwin'
					? `@fluxer/mac-screen-capture native binary unavailable: ${loadError?.message ?? 'unknown reason'}`
					: `@fluxer/mac-screen-capture is only supported on macOS, got ${process.platform}`,
			minMacosVersion: '12.3',
			detectedMacosVersion: undefined,
			sckAvailable: false,
		};
	}
	return binding.getBackendInfo();
}

function getBackendAvailability() {
	if (!binding) {
		return Promise.resolve({
			sck: {supported: false},
			screenPermission: 'not-determined',
		});
	}
	return binding.getBackendAvailability();
}

function listSources() {
	if (!binding) return Promise.resolve([]);
	return binding.listSources();
}

function probeScreenRecordingAccess() {
	if (!binding || typeof binding.probeScreenRecordingAccess !== 'function') return Promise.resolve('unsupported');
	return Promise.resolve(binding.probeScreenRecordingAccess());
}

class ScreenCapture extends EventEmitter {
	constructor(options = {}) {
		super();
		if (!binding) {
			throw loadError || new Error('@fluxer/mac-screen-capture binding unavailable');
		}
		this.sourceId = options.sourceId;
		this.sourceKind = options.sourceKind ?? 'screen';
		this.width = options.width ?? 0;
		this.height = options.height ?? 0;
		this.frameRate = options.frameRate ?? 30;
		this.captureId = typeof options.captureId === 'string' ? options.captureId : undefined;
		this.colorRange = options.colorRange;
		this.colorSpace = options.colorSpace;
		this.showCursorClicks = options.showCursorClicks === true;
		this.captureRect = options.captureRect;
		this.frameSinkHandle = options.frameSinkHandle;
		this.nativeFrameSinkRequired = options.nativeFrameSinkRequired === true;
		this.started = false;
		this.stopped = false;
		this.closedEmitted = false;
		this.native = new binding.ScreenCapture();
		this.native.setLifecycleCallback((type, message) => {
			if (type === 'diagnostic') {
				if (this.stopped) return;
				this.emit('diagnostic', message);
				return;
			}
			if (type === 'error') {
				this.emit('error', new Error(message || 'macOS screen capture stream stopped'));
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
			if (this.frameSinkHandle != null) {
				if (typeof this.native.setFrameSinkHandle !== 'function') {
					throw new Error('@fluxer/mac-screen-capture native binding does not support native frame sink handles');
				}
				this.native.setFrameSinkHandle(this.frameSinkHandle);
			} else if (this.nativeFrameSinkRequired) {
				throw new Error('Native frame sink handle is required for macOS screen capture');
			}
			const result = await this.native.start(
				this.sourceId,
				this.sourceKind,
				this.width,
				this.height,
				this.frameRate,
				this.captureId,
				{
					colorRange: this.colorRange,
					colorSpace: this.colorSpace,
					showCursorClicks: this.showCursorClicks,
					captureRect: this.captureRect,
				},
			);
			if (result) {
				this.width = result.width ?? this.width;
				this.height = result.height ?? this.height;
				this.frameRate = result.frameRate ?? this.frameRate;
				this.pixelFormat = result.pixelFormat ?? 'nv12';
			}
			return {
				width: this.width,
				height: this.height,
				frameRate: this.frameRate,
				pixelFormat: this.pixelFormat ?? 'nv12',
			};
		} catch (error) {
			this.stopped = true;
			this.emit('error', error instanceof Error ? error : new Error(String(error)));
			throw error;
		}
	}

	async stop() {
		if (this.stopped) return;
		this.stopped = true;
		try {
			await this.native.stop();
		} finally {
			this.emitClosedOnce();
		}
	}

	getFrameSinkDiagnostics() {
		if (!this.native || typeof this.native.getFrameSinkDiagnostics !== 'function') {
			return {
				accepted: 0,
				coalesced: 0,
				rejected: 0,
				mediaFramesDroppedWithoutSink: 0,
			};
		}
		try {
			return this.native.getFrameSinkDiagnostics();
		} catch (error) {
			console.warn('[mac-screen-capture] getFrameSinkDiagnostics failed:', error?.message || error);
			return {
				accepted: 0,
				coalesced: 0,
				rejected: 0,
				mediaFramesDroppedWithoutSink: 0,
			};
		}
	}
}

module.exports = {
	ScreenCapture,
	getBackendAvailability,
	getBackendInfo,
	listSources,
	loadError,
	probeScreenRecordingAccess,
};
