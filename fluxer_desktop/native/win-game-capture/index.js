// SPDX-License-Identifier: AGPL-3.0-or-later

const {EventEmitter} = require('node:events');
const {existsSync} = require('node:fs');
const {join, sep} = require('node:path');
const {createNativeLoadError, loadNativeBinding} = require('./loader-diagnostics.cjs');

const MODULE_NAME = '@fluxer/win-game-capture';

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

function nativeFileName(arch) {
	switch (arch) {
		case 'x64':
			return 'win-game-capture.win32-x64-msvc.node';
		case 'arm64':
			return 'win-game-capture.win32-arm64-msvc.node';
		default:
			return null;
	}
}

let binding = null;
let loadError = null;
const nativeRoot = resolveNativeRoot();

if (process.platform === 'win32') {
	const fileName = nativeFileName(process.arch);
	if (!fileName) {
		loadError = createNativeLoadError({
			moduleName: MODULE_NAME,
			nativeRoot,
			packageDir: __dirname,
			reason: `unsupported Windows architecture: ${process.arch}`,
		});
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
	}
} else {
	loadError = createNativeLoadError({
		moduleName: MODULE_NAME,
		nativeRoot,
		packageDir: __dirname,
		reason: `not supported on platform ${process.platform}`,
	});
}

function gameHookFileName(arch) {
	switch (arch) {
		case 'x64':
			return 'fluxer-game-hook.win32-x64-msvc.dll';
		case 'ia32':
			return 'fluxer-game-hook.win32-ia32-msvc.dll';
		case 'arm64':
			return 'fluxer-game-hook.win32-arm64-msvc.dll';
		default:
			return null;
	}
}

function resolveGameHookPathForArch(arch, root = nativeRoot) {
	if (process.platform !== 'win32') return null;
	const fileName = gameHookFileName(arch);
	if (!fileName) return null;
	const hookPath = join(root, fileName);
	return existsSync(hookPath) ? hookPath : null;
}

function resolveGameHookPath(root = nativeRoot) {
	return resolveGameHookPathForArch(process.arch, root);
}

function resolveGameHookPathX86(root = nativeRoot) {
	return resolveGameHookPathForArch('ia32', root);
}

function vulkanLayerManifestFileName(arch) {
	switch (arch) {
		case 'x64':
			return 'fluxer-vulkan-layer.win32-x64-msvc.json';
		case 'ia32':
			return 'fluxer-vulkan-layer.win32-ia32-msvc.json';
		case 'arm64':
			return 'fluxer-vulkan-layer.win32-arm64-msvc.json';
		default:
			return null;
	}
}

function resolveVulkanLayerManifestPath(root = nativeRoot) {
	if (process.platform !== 'win32') return null;
	const fileName = vulkanLayerManifestFileName(process.arch);
	if (!fileName) return null;
	const manifestPath = join(root, fileName);
	return existsSync(manifestPath) ? manifestPath : null;
}

function isGameCaptureHookAvailable(root = nativeRoot) {
	if (typeof binding?.isGameCaptureHookAvailable !== 'function') return false;
	if (binding.isGameCaptureHookAvailable() !== true) return false;
	return resolveGameHookPath(root) !== null;
}

function registerVulkanLayerManifest(root = nativeRoot) {
	if (!binding?.registerVulkanLayerManifest) return false;
	if (!isGameCaptureHookAvailable(root)) return false;
	const manifestPath = resolveVulkanLayerManifestPath(root);
	if (!manifestPath) return false;
	binding.registerVulkanLayerManifest(manifestPath);
	return true;
}

function unregisterVulkanLayerManifest(root = nativeRoot) {
	if (!binding?.unregisterVulkanLayerManifest) return false;
	const manifestPath = resolveVulkanLayerManifestPath(root);
	if (!manifestPath) return false;
	try {
		binding.unregisterVulkanLayerManifest(manifestPath);
		return true;
	} catch (error) {
		console.warn('[win-game-capture] unregisterVulkanLayerManifest failed:', error?.message || error);
		return false;
	}
}

function getVulkanLayerRegistrationState(root = nativeRoot) {
	const manifestPath = resolveVulkanLayerManifestPath(root);
	if (!binding?.getVulkanLayerRegistrationState) {
		return {registered: false, manifestExists: Boolean(manifestPath), dllExists: false, manifestPath};
	}
	try {
		return binding.getVulkanLayerRegistrationState(manifestPath ?? '');
	} catch (error) {
		console.warn('[win-game-capture] getVulkanLayerRegistrationState failed:', error?.message || error);
		return {registered: false, manifestExists: Boolean(manifestPath), dllExists: false, manifestPath};
	}
}

class ScreenCapture extends EventEmitter {
	constructor(options = {}) {
		super();
		if (!binding) {
			throw loadError || new Error(`${MODULE_NAME} binding unavailable`);
		}
		this.sourceId = options.sourceId;
		this.sourceKind = options.sourceKind ?? 'window';
		this.width = options.width ?? 0;
		this.height = options.height ?? 0;
		this.frameRate = options.frameRate ?? 30;
		this.hookDllPath = options.hookDllPath ?? resolveGameHookPath();
		this.hookDllPathX86 = options.hookDllPathX86 ?? resolveGameHookPathX86();
		this.injectionMethod = options.injectionMethod ?? undefined;
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

		this.native.setLifecycleCallback((...lifecycleArgs) => {
			const [type, message] =
				lifecycleArgs.length === 1 && Array.isArray(lifecycleArgs[0]) ? lifecycleArgs[0] : lifecycleArgs;
			if (type === 'stalled') {
				if (this.stopped) return;
				this.emit('stalled', message);
				return;
			}
			if (type === 'diagnostic') {
				if (this.stopped) return;
				this.emit('diagnostic', message);
				return;
			}
			if (type === 'error') {
				this.emit('error', new Error(message || 'DXGI capture error'));
				return;
			}
			if (type === 'closed') {
				if (this.stopped) {
					this._emitClosedOnce();
					return;
				}
				this.stopped = true;
				try {
					this.native.stop();
				} catch {}
				this._emitClosedOnce();
			}
		});
	}

	async start() {
		if (this.started || this.stopped) return undefined;
		this.started = true;
		try {
			if (this.frameSinkHandle != null) {
				if (typeof this.native.setFrameSinkHandle !== 'function') {
					throw new Error(`${MODULE_NAME} native binding does not support native frame sink handles`);
				}
				this.native.setFrameSinkHandle(this.frameSinkHandle);
			} else if (this.nativeFrameSinkRequired) {
				throw new Error('Native frame sink handle is required for Windows screen capture');
			}
			const result = this.native.start(
				this.sourceId,
				this.sourceKind,
				this.width || undefined,
				this.height || undefined,
				this.frameRate || undefined,
				this.sourceKind === 'game' ? this.hookDllPath : undefined,
				this.sourceKind === 'game' ? (this.hookDllPathX86 ?? undefined) : undefined,
				this.sourceKind === 'game' ? (this.injectionMethod ?? undefined) : undefined,
				this.captureId,
				{
					colorRange: this.colorRange,
					colorSpace: this.colorSpace,
					showCursorClicks: this.showCursorClicks,
					captureRect: this.captureRect,
				},
			);
			return result
				? {
						width: result.width,
						height: result.height,
						frameRate: result.frameRate,
						pixelFormat: result.pixelFormat,
					}
				: undefined;
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
			this.native.stop();
		} finally {
			this._emitClosedOnce();
		}
	}

	getDiagnostics() {
		if (!this.native || typeof this.native.getDiagnostics !== 'function') return null;
		try {
			return this.native.getDiagnostics() ?? null;
		} catch (error) {
			console.warn('[win-game-capture] getDiagnostics failed:', error?.message || error);
			return null;
		}
	}

	getSharedTextureHandle() {
		if (!this.native || typeof this.native.getSharedTextureHandle !== 'function') return null;
		try {
			return this.native.getSharedTextureHandle() ?? null;
		} catch (error) {
			console.warn('[win-game-capture] getSharedTextureHandle failed:', error?.message || error);
			return null;
		}
	}

	attachEncoder(width, height) {
		if (!this.native || typeof this.native.attachEncoder !== 'function') {
			throw new Error(`${MODULE_NAME} native binding does not support encoder attachment`);
		}
		this.native.attachEncoder(width, height);
	}

	detachEncoder() {
		if (!this.native || typeof this.native.detachEncoder !== 'function') return;
		this.native.detachEncoder();
	}

	isEncoderAttached() {
		if (!this.native || typeof this.native.isEncoderAttached !== 'function') return false;
		return Boolean(this.native.isEncoderAttached());
	}

	encoderRingFullCount() {
		if (!this.native || typeof this.native.encoderRingFullCount !== 'function') return 0;
		const count = this.native.encoderRingFullCount();
		return Number.isFinite(count) && count > 0 ? Math.floor(count) : 0;
	}

	getEncoderAttachDiagnostics() {
		if (!this.native || typeof this.native.getEncoderAttachDiagnostics !== 'function') return null;
		try {
			return this.native.getEncoderAttachDiagnostics() ?? null;
		} catch (error) {
			console.warn('[win-game-capture] getEncoderAttachDiagnostics failed:', error?.message || error);
			return null;
		}
	}

	getFrameSinkDiagnostics() {
		if (!this.native || typeof this.native.getFrameSinkDiagnostics !== 'function') {
			return {
				accepted: 0,
				coalesced: 0,
				rejected: 0,
				mediaFramesDroppedWithoutSink: 0,
				cpuFallbackFramesDropped: 0,
			};
		}
		try {
			return this.native.getFrameSinkDiagnostics();
		} catch (error) {
			console.warn('[win-game-capture] getFrameSinkDiagnostics failed:', error?.message || error);
			return {
				accepted: 0,
				coalesced: 0,
				rejected: 0,
				mediaFramesDroppedWithoutSink: 0,
				cpuFallbackFramesDropped: 0,
			};
		}
	}

	_emitClosedOnce() {
		if (this.closedEmitted) return;
		this.closedEmitted = true;
		queueMicrotask(() => this.emit('closed'));
	}
}

const FALLBACK_STRATEGY_NAMES = new Set(['game-hook', 'wgc', 'dxgi-duplication', 'window-gdi', 'none']);
function parseFallbackRecommendation(message) {
	if (typeof message !== 'string') return null;
	const match = message.match(/\[next-strategy=([a-z-]+)\]/);
	if (!match) return null;
	const name = match[1];
	return FALLBACK_STRATEGY_NAMES.has(name) ? name : null;
}

async function listSources() {
	if (!binding || typeof binding.listSources !== 'function') return [];
	try {
		const sources = await binding.listSources();
		if (!Array.isArray(sources)) return [];
		return sources
			.filter((source) => {
				return (
					source &&
					(source.kind === 'screen' || source.kind === 'window' || source.kind === 'game') &&
					typeof source.id === 'string' &&
					source.id.length > 0
				);
			})
			.map((source) => ({
				kind: source.kind,
				id: source.id,
				name: typeof source.name === 'string' && source.name.length > 0 ? source.name : source.id,
				width: Number.isFinite(source.width) ? Math.max(0, Math.floor(source.width)) : 0,
				height: Number.isFinite(source.height) ? Math.max(0, Math.floor(source.height)) : 0,
				targetPid: Number.isFinite(source.targetPid) && source.targetPid > 0 ? Math.floor(source.targetPid) : undefined,
			}));
	} catch (error) {
		console.warn('[win-game-capture] listSources failed:', error?.message || error);
		return [];
	}
}

function isSupported() {
	if (!binding) return false;
	return binding.isSupported();
}

function getAvailability() {
	if (!binding) {
		return {available: false, backend: 'windows-game-capture', reason: 'load-failed'};
	}
	return binding.getAvailability();
}

function normalizeProcessId(processId) {
	if (processId === undefined || processId === null) return undefined;
	if (Number.isInteger(processId) && processId > 0) return processId;
	throw new TypeError(`Invalid process id: ${processId}`);
}

function normalizePriorityClass(priorityClass) {
	if (priorityClass === undefined || priorityClass === null) return undefined;
	const normalized = String(priorityClass).trim().toLowerCase();
	if (normalized === 'real-time') return 'realtime';
	if (normalized === 'high' || normalized === 'realtime') return normalized;
	throw new TypeError(`Invalid GPU scheduling priority class: ${priorityClass}`);
}

function elevateGpuSchedulingPriority(processId, priorityClass) {
	if (!binding) return false;
	try {
		binding.elevateGpuSchedulingPriority(normalizeProcessId(processId), normalizePriorityClass(priorityClass));
		return true;
	} catch (error) {
		console.warn('[win-game-capture] elevateGpuSchedulingPriority failed:', error?.message || error);
		return false;
	}
}

function restoreGpuSchedulingPriority(processId) {
	if (!binding) return false;
	try {
		binding.restoreGpuSchedulingPriority(normalizeProcessId(processId));
		return true;
	} catch (error) {
		console.warn('[win-game-capture] restoreGpuSchedulingPriority failed:', error?.message || error);
		return false;
	}
}

function __setBindingForTests(nextBinding) {
	binding = nextBinding;
	loadError = null;
}

module.exports = {
	isSupported,
	getAvailability,
	resolveGameHookPath,
	resolveGameHookPathX86,
	isGameCaptureHookAvailable,
	resolveVulkanLayerManifestPath,
	registerVulkanLayerManifest,
	unregisterVulkanLayerManifest,
	getVulkanLayerRegistrationState,
	listSources,
	ScreenCapture,
	parseFallbackRecommendation,
	elevateGpuSchedulingPriority,
	restoreGpuSchedulingPriority,
	__setBindingForTests,
	get loadError() {
		return loadError;
	},
};
