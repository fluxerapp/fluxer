// SPDX-License-Identifier: AGPL-3.0-or-later

const {EventEmitter} = require('node:events');
const {existsSync} = require('node:fs');
const {join, sep} = require('node:path');
const {createNativeLoadError, loadNativeBinding} = require('./loader-diagnostics.cjs');
const MODULE_NAME = '@fluxer/linux-screen-capture';
const SKIP_NATIVE_PROBE_ENV = 'FLUXER_LINUX_SCREEN_CAPTURE_SKIP_NATIVE_PROBE';

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

function nativeFileName() {
	if (process.platform !== 'linux') {
		throw new Error(`@fluxer/linux-screen-capture is only supported on Linux, got ${process.platform}`);
	}
	switch (process.arch) {
		case 'x64':
			return 'linux-screen-capture.linux-x64-gnu.node';
		case 'arm64':
			return 'linux-screen-capture.linux-arm64-gnu.node';
		default:
			throw new Error(`Unsupported Linux architecture: ${process.arch}`);
	}
}

let binding = null;
let loadError = null;

if (process.platform === 'linux') {
	try {
		const nativeRoot = resolveNativeRoot();
		const nativePath = join(nativeRoot, nativeFileName());
		const loaded = loadNativeBinding({
			moduleName: MODULE_NAME,
			nativePath,
			nativeRoot,
			packageDir: __dirname,
			skipNativeProbeEnv: SKIP_NATIVE_PROBE_ENV,
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
			skipNativeProbeEnv: SKIP_NATIVE_PROBE_ENV,
		});
		throw loadError;
	}
}

function getBackendInfo() {
	if (!binding) {
		return {
			backend: 'linux-pipewire-portal',
			supported: false,
			reason:
				process.platform === 'linux'
					? `@fluxer/linux-screen-capture native binary unavailable: ${loadError?.message ?? 'unknown reason'}`
					: `@fluxer/linux-screen-capture is only supported on Linux, got ${process.platform}`,
			portalVersion: undefined,
			pipewireReachable: false,
		};
	}
	return binding.getBackendInfo();
}

function getAvailability() {
	if (!binding) {
		return Promise.resolve({
			available: false,
			backend: 'linux-pipewire-portal',
			reason: 'unsupported-platform',
			capabilities: {process: false, system: false},
		});
	}
	return binding.getAvailability();
}

function listSources() {
	if (!binding) return Promise.resolve([]);
	return binding.listSources();
}

function __setBindingForTests(nextBinding) {
	binding = nextBinding;
	loadError = null;
}

class ScreenCapture extends EventEmitter {
	constructor(options = {}) {
		super();
		if (!binding) {
			throw loadError || new Error('@fluxer/linux-screen-capture binding unavailable');
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
			if (type === 'error') {
				this.emit('error', new Error(message || 'Linux PipeWire screen capture stream stopped'));
				return;
			}
			if (type === 'closed' || type === 'closed-clean') {
				if (this.stopped) {
					this.emitClosedOnce();
					return;
				}
				this.stopped = true;
				Promise.resolve()
					.then(() => this.native.stop())
					.catch(() => {});
				this.emitClosedOnce();
				return;
			}
			if (type === 'stalled' || type === 'diagnostic') {
				this.emit(type, message);
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
					throw new Error('@fluxer/linux-screen-capture native binding does not support frame sink handles');
				}
				this.native.setFrameSinkHandle(this.frameSinkHandle);
			} else if (this.nativeFrameSinkRequired) {
				throw new Error('@fluxer/linux-screen-capture native frame sink handle is required');
			}
			const result = await this.native.start(
				String(this.sourceId ?? ''),
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

	getDiagnostics() {
		const addonDiagnostics = typeof this.native.getDiagnostics === 'function' ? this.native.getDiagnostics() : null;
		if (!addonDiagnostics) return null;
		return {
			...addonDiagnostics,
			sourceId: String(this.sourceId ?? ''),
			sourceKind: this.sourceKind,
			width: addonDiagnostics.width ?? this.width,
			height: addonDiagnostics.height ?? this.height,
		};
	}
}

module.exports = {
	ScreenCapture,
	getAvailability,
	getBackendInfo,
	listSources,
	loadError,
	__setBindingForTests,
};
