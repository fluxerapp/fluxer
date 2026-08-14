// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import os from 'node:os';
import {app, powerSaveBlocker} from 'electron';
import log from 'electron-log';
import {retainWindowsScreenCaptureGuard, stopWindowsScreenCaptureGuard} from './WindowsScreenCaptureGuard';

const STREAMING_PRIORITY = os.constants?.priority?.PRIORITY_ABOVE_NORMAL ?? -7;
const CAN_ELEVATE_PROCESS_PRIORITY = process.platform === 'win32';
const GPU_PRIORITY_REFRESH_INTERVAL_MS = 20000;
const GPU_SCHEDULING_PRIORITY_ENV = 'FLUXER_STREAMING_GPU_SCHEDULING_PRIORITY';
const MAX_GPU_PRIORITY_DIAGNOSTIC_TARGETS = 32;

const requireModule = createRequire(import.meta.url);

type WindowsGpuSchedulingPriority = 'high' | 'realtime';
type WindowsGpuPriorityTargetReason =
	| 'native-main-encoder-capture'
	| 'renderer'
	| 'tracked-renderer'
	| 'chromium-gpu'
	| 'chromium-video-capture'
	| 'chromium-video-encode'
	| 'chromium-media-utility';

type GpuPriorityModuleStatus = 'disabled' | 'unsupported-platform' | 'not-loaded' | 'loaded' | 'unavailable';
type GpuPriorityAttemptStatus =
	| 'disabled'
	| 'unsupported-platform'
	| 'native-module-unavailable'
	| 'no-targets'
	| 'succeeded'
	| 'partial'
	| 'failed'
	| 'no-active-priority';

type WindowsGpuPriorityModule = {
	elevateGpuSchedulingPriority?: (processId?: number, priorityClass?: WindowsGpuSchedulingPriority) => boolean;
	restoreGpuSchedulingPriority?: (processId?: number) => boolean;
	loadError?: Error | null;
};

interface GpuPriorityTarget {
	processId: number;
	reasons: Array<WindowsGpuPriorityTargetReason>;
}

interface GpuPriorityFailure {
	processId: number;
	reason: string;
}

interface GpuPriorityAcquireDiagnostics {
	status: GpuPriorityAttemptStatus;
	priorityClass: WindowsGpuSchedulingPriority | null;
	targets: Array<GpuPriorityTarget>;
	elevatedProcessIds: Array<number>;
	skippedProcessIds: Array<number>;
	failedProcessIds: Array<GpuPriorityFailure>;
	detail?: string;
}

interface GpuPriorityRestoreDiagnostics {
	status: GpuPriorityAttemptStatus;
	processIds: Array<number>;
	restoredProcessIds: Array<number>;
	failedProcessIds: Array<GpuPriorityFailure>;
	detail?: string;
}

export interface StreamingPriorityDiagnostics {
	refCount: number;
	powerSaveBlocker: {
		active: boolean;
		id: number | null;
	};
	processPriority: {
		supported: boolean;
		streamingPriority: number;
		savedPriority: number | null;
		elevated: boolean;
	};
	gpuScheduling: {
		supported: boolean;
		priorityClass: WindowsGpuSchedulingPriority | null;
		env: string;
		nativeModuleStatus: GpuPriorityModuleStatus;
		nativeModuleLoadErrorDetail: string | null;
		refreshActive: boolean;
		refreshIntervalMs: number;
		trackedWebContents: number;
		elevatedProcesses: Array<{processId: number; priorityClass: WindowsGpuSchedulingPriority}>;
		lastAcquire: GpuPriorityAcquireDiagnostics | null;
		lastRestore: GpuPriorityRestoreDiagnostics | null;
	};
}

let refCount = 0;
let powerBlockerId: number | null = null;
let savedPriority: number | null = null;
let gpuPriorityModule: WindowsGpuPriorityModule | null | undefined;
let gpuPriorityModuleLoadErrorDetail: string | null = null;
const gpuPriorityElevatedProcessIds = new Map<number, WindowsGpuSchedulingPriority>();
const streamingPriorityWebContents = new Set<Electron.WebContents>();
const streamingPriorityWebContentsCleanup = new Map<Electron.WebContents, () => void>();
const backgroundThrottlingBypassedWebContents = new Set<Electron.WebContents>();
let gpuPriorityRefreshTimer: NodeJS.Timeout | null = null;
let lastGpuPriorityAcquire: GpuPriorityAcquireDiagnostics | null = null;
let lastGpuPriorityRestore: GpuPriorityRestoreDiagnostics | null = null;

function formatErrorDetail(error: unknown): string {
	if (error instanceof Error) return error.message;
	if (
		typeof error === 'object' &&
		error !== null &&
		'message' in error &&
		typeof (error as {message?: unknown}).message === 'string'
	) {
		return (error as {message: string}).message;
	}
	return String(error);
}

const GPU_PRIORITY_MODULE_UNAVAILABLE_DETAIL = 'Windows GPU priority native API unavailable';

function gpuPriorityModuleUnavailableReason(): string {
	if (gpuPriorityModuleLoadErrorDetail === null) return GPU_PRIORITY_MODULE_UNAVAILABLE_DETAIL;
	const lineBreakIndex = gpuPriorityModuleLoadErrorDetail.indexOf('\n');
	if (lineBreakIndex === -1) return gpuPriorityModuleLoadErrorDetail;
	return gpuPriorityModuleLoadErrorDetail.slice(0, lineBreakIndex).trimEnd();
}

function resolveGpuSchedulingPriority(): WindowsGpuSchedulingPriority | null {
	const raw = process.env[GPU_SCHEDULING_PRIORITY_ENV];
	if (raw == null || raw.trim() === '') return 'high';
	const normalized = raw.trim().toLowerCase();
	if (normalized === 'realtime' || normalized === 'real-time') return 'realtime';
	if (normalized === 'high') return 'high';
	if (normalized === 'off' || normalized === 'disabled' || normalized === 'none') return null;
	log.warn('[StreamingPriority] Ignoring invalid GPU scheduling priority override', {
		env: GPU_SCHEDULING_PRIORITY_ENV,
		value: raw,
	});
	return 'high';
}

const GPU_SCHEDULING_PRIORITY = resolveGpuSchedulingPriority();

function startPowerSaveBlocker(): void {
	try {
		powerBlockerId = powerSaveBlocker.start('prevent-display-sleep');
	} catch (error) {
		log.warn('[StreamingPriority] Failed to start power save blocker', {error});
	}
}

function stopPowerSaveBlocker(): void {
	if (powerBlockerId === null) return;
	try {
		powerSaveBlocker.stop(powerBlockerId);
	} catch (error) {
		log.warn('[StreamingPriority] Failed to stop power save blocker', {error});
	}
	powerBlockerId = null;
}

function elevateProcessPriority(): void {
	if (!CAN_ELEVATE_PROCESS_PRIORITY) return;
	try {
		savedPriority = os.getPriority();
		if (savedPriority > STREAMING_PRIORITY) {
			os.setPriority(STREAMING_PRIORITY);
		} else {
			savedPriority = null;
		}
	} catch (error) {
		log.debug('[StreamingPriority] Failed to elevate process priority', {error});
		savedPriority = null;
	}
}

function restoreProcessPriority(): void {
	if (savedPriority === null) return;
	try {
		os.setPriority(savedPriority);
	} catch (error) {
		log.debug('[StreamingPriority] Failed to restore process priority', {error});
	}
	savedPriority = null;
}

function loadWindowsGpuPriorityModule(): WindowsGpuPriorityModule | null {
	if (process.platform !== 'win32') return null;
	if (gpuPriorityModule !== undefined) return gpuPriorityModule;
	try {
		const addon = requireModule('@fluxer/win-game-capture') as WindowsGpuPriorityModule;
		if (addon.loadError) {
			gpuPriorityModuleLoadErrorDetail = formatErrorDetail(addon.loadError);
			gpuPriorityModule = null;
			log.debug('[StreamingPriority] Windows GPU priority module reported load error', {
				detail: gpuPriorityModuleLoadErrorDetail,
			});
		} else {
			gpuPriorityModuleLoadErrorDetail = null;
			gpuPriorityModule = addon;
		}
		return gpuPriorityModule;
	} catch (error) {
		gpuPriorityModuleLoadErrorDetail = formatErrorDetail(error);
		gpuPriorityModule = null;
		log.debug('[StreamingPriority] Failed to load Windows GPU priority module', {
			detail: gpuPriorityModuleLoadErrorDetail,
		});
		return null;
	}
}

function addGpuPriorityTarget(
	targets: Map<number, Set<WindowsGpuPriorityTargetReason>>,
	processId: number | undefined,
	reason: WindowsGpuPriorityTargetReason,
): void {
	if (typeof processId !== 'number' || !Number.isInteger(processId) || processId <= 0) return;
	const reasons = targets.get(processId);
	if (reasons) {
		reasons.add(reason);
	} else {
		targets.set(processId, new Set([reason]));
	}
}

function getRendererProcessId(webContents?: Electron.WebContents): number | undefined {
	if (!webContents || webContents.isDestroyed()) return undefined;
	try {
		return webContents.getOSProcessId();
	} catch {
		return undefined;
	}
}

function setBackgroundThrottlingBypass(webContents: Electron.WebContents, bypass: boolean): void {
	if (webContents.isDestroyed()) return;
	try {
		webContents.setBackgroundThrottling(!bypass);
		if (bypass) {
			backgroundThrottlingBypassedWebContents.add(webContents);
		} else {
			backgroundThrottlingBypassedWebContents.delete(webContents);
		}
	} catch (error) {
		log.debug('[StreamingPriority] Failed to update renderer background throttling', {bypass, error});
	}
}

function restoreBackgroundThrottling(): void {
	for (const webContents of backgroundThrottlingBypassedWebContents) {
		setBackgroundThrottlingBypass(webContents, false);
	}
	backgroundThrottlingBypassedWebContents.clear();
}

function rememberStreamingPriorityWebContents(webContents?: Electron.WebContents): void {
	if (!webContents || webContents.isDestroyed() || streamingPriorityWebContents.has(webContents)) return;
	streamingPriorityWebContents.add(webContents);
	const cleanup = (): void => {
		streamingPriorityWebContents.delete(webContents);
		streamingPriorityWebContentsCleanup.delete(webContents);
		backgroundThrottlingBypassedWebContents.delete(webContents);
	};
	streamingPriorityWebContentsCleanup.set(webContents, cleanup);
	webContents.once('destroyed', cleanup);
}

function clearStreamingPriorityWebContents(): void {
	for (const [webContents, cleanup] of streamingPriorityWebContentsCleanup) {
		if (webContents && !webContents.isDestroyed()) {
			webContents.removeListener('destroyed', cleanup);
		}
	}
	streamingPriorityWebContents.clear();
	streamingPriorityWebContentsCleanup.clear();
}

function getChromiumProcessTargetReasons(metric: Electron.ProcessMetric): Array<WindowsGpuPriorityTargetReason> {
	const type = metric.type.toLowerCase();
	if (type === 'gpu') return ['chromium-gpu'];
	const processName = `${metric.name ?? ''} ${metric.serviceName ?? ''}`.toLowerCase();
	const reasons: Array<WindowsGpuPriorityTargetReason> = [];
	if (processName.includes('video capture') || processName.includes('video_capture')) {
		reasons.push('chromium-video-capture');
	}
	if (processName.includes('video encode') || processName.includes('video_encode')) {
		reasons.push('chromium-video-encode');
	}
	if (type !== 'utility' || reasons.length > 0) return reasons;
	if (
		processName.includes('capture') ||
		processName.includes('encoder') ||
		processName.includes('media') ||
		processName.includes('video') ||
		processName.includes('webrtc')
	) {
		reasons.push('chromium-media-utility');
	}
	return reasons;
}

function collectGpuSchedulingPriorityTargets(webContents?: Electron.WebContents): Array<GpuPriorityTarget> {
	const targets = new Map<number, Set<WindowsGpuPriorityTargetReason>>();
	addGpuPriorityTarget(targets, process.pid, 'native-main-encoder-capture');
	const rendererProcessId = getRendererProcessId(webContents);
	addGpuPriorityTarget(targets, rendererProcessId, 'renderer');
	for (const trackedWebContents of streamingPriorityWebContents) {
		const trackedRendererProcessId = getRendererProcessId(trackedWebContents);
		if (trackedRendererProcessId !== rendererProcessId) {
			addGpuPriorityTarget(targets, trackedRendererProcessId, 'tracked-renderer');
		}
	}
	try {
		for (const metric of app.getAppMetrics()) {
			for (const reason of getChromiumProcessTargetReasons(metric)) {
				addGpuPriorityTarget(targets, metric.pid, reason);
			}
		}
	} catch (error) {
		log.debug('[StreamingPriority] Failed to collect Chromium process metrics', {error});
	}
	return [...targets.entries()].map(([processId, reasons]) => ({processId, reasons: [...reasons]}));
}

function limitGpuPriorityTargets(targets: Array<GpuPriorityTarget>): Array<GpuPriorityTarget> {
	return targets
		.slice(0, MAX_GPU_PRIORITY_DIAGNOSTIC_TARGETS)
		.map((target) => ({processId: target.processId, reasons: [...target.reasons]}));
}

function getGpuPriorityModuleStatus(): GpuPriorityModuleStatus {
	if (GPU_SCHEDULING_PRIORITY === null) return 'disabled';
	if (process.platform !== 'win32') return 'unsupported-platform';
	if (gpuPriorityModule === undefined) return 'not-loaded';
	return gpuPriorityModule === null ? 'unavailable' : 'loaded';
}

function cloneGpuPriorityAcquireDiagnostics(
	diagnostics: GpuPriorityAcquireDiagnostics | null,
): GpuPriorityAcquireDiagnostics | null {
	if (!diagnostics) return null;
	return {
		...diagnostics,
		targets: diagnostics.targets.map((target) => ({processId: target.processId, reasons: [...target.reasons]})),
		elevatedProcessIds: [...diagnostics.elevatedProcessIds],
		skippedProcessIds: [...diagnostics.skippedProcessIds],
		failedProcessIds: diagnostics.failedProcessIds.map((failure) => ({...failure})),
	};
}

function cloneGpuPriorityRestoreDiagnostics(
	diagnostics: GpuPriorityRestoreDiagnostics | null,
): GpuPriorityRestoreDiagnostics | null {
	if (!diagnostics) return null;
	return {
		...diagnostics,
		processIds: [...diagnostics.processIds],
		restoredProcessIds: [...diagnostics.restoredProcessIds],
		failedProcessIds: diagnostics.failedProcessIds.map((failure) => ({...failure})),
	};
}

function elevateGpuSchedulingPriority(webContents?: Electron.WebContents): void {
	if (GPU_SCHEDULING_PRIORITY === null) {
		lastGpuPriorityAcquire = {
			status: 'disabled',
			priorityClass: null,
			targets: [],
			elevatedProcessIds: [],
			skippedProcessIds: [],
			failedProcessIds: [],
			detail: `${GPU_SCHEDULING_PRIORITY_ENV} disabled GPU scheduling priority`,
		};
		return;
	}
	const targets = collectGpuSchedulingPriorityTargets(webContents);
	if (process.platform !== 'win32') {
		lastGpuPriorityAcquire = {
			status: 'unsupported-platform',
			priorityClass: GPU_SCHEDULING_PRIORITY,
			targets: limitGpuPriorityTargets(targets),
			elevatedProcessIds: [],
			skippedProcessIds: [],
			failedProcessIds: [],
			detail: `GPU scheduling priority is Windows-only; current platform is ${process.platform}`,
		};
		return;
	}
	const addon = loadWindowsGpuPriorityModule();
	if (!addon?.elevateGpuSchedulingPriority) {
		lastGpuPriorityAcquire = {
			status: 'native-module-unavailable',
			priorityClass: GPU_SCHEDULING_PRIORITY,
			targets: limitGpuPriorityTargets(targets),
			elevatedProcessIds: [],
			skippedProcessIds: [],
			failedProcessIds: targets.slice(0, MAX_GPU_PRIORITY_DIAGNOSTIC_TARGETS).map((target) => ({
				processId: target.processId,
				reason: gpuPriorityModuleUnavailableReason(),
			})),
			detail: gpuPriorityModuleLoadErrorDetail ?? GPU_PRIORITY_MODULE_UNAVAILABLE_DETAIL,
		};
		log.debug('[StreamingPriority] Cannot elevate GPU scheduling priority; native module unavailable', {
			priorityClass: GPU_SCHEDULING_PRIORITY,
			targets: lastGpuPriorityAcquire.targets,
			detail: lastGpuPriorityAcquire.detail,
		});
		return;
	}
	if (targets.length === 0) {
		lastGpuPriorityAcquire = {
			status: 'no-targets',
			priorityClass: GPU_SCHEDULING_PRIORITY,
			targets: [],
			elevatedProcessIds: [],
			skippedProcessIds: [],
			failedProcessIds: [],
		};
		return;
	}
	const elevatedProcessIds: Array<number> = [];
	const skippedProcessIds: Array<number> = [];
	const failedProcessIds: Array<GpuPriorityFailure> = [];
	for (const {processId} of targets) {
		if (gpuPriorityElevatedProcessIds.get(processId) === GPU_SCHEDULING_PRIORITY) {
			skippedProcessIds.push(processId);
			continue;
		}
		try {
			if (addon.elevateGpuSchedulingPriority(processId, GPU_SCHEDULING_PRIORITY)) {
				gpuPriorityElevatedProcessIds.set(processId, GPU_SCHEDULING_PRIORITY);
				elevatedProcessIds.push(processId);
			} else {
				failedProcessIds.push({processId, reason: 'native API returned false'});
				log.debug('[StreamingPriority] Failed to elevate GPU scheduling priority', {processId});
			}
		} catch (error) {
			failedProcessIds.push({processId, reason: formatErrorDetail(error)});
			log.debug('[StreamingPriority] Failed to elevate GPU scheduling priority', {processId, error});
		}
	}
	lastGpuPriorityAcquire = {
		status: failedProcessIds.length === 0 ? 'succeeded' : elevatedProcessIds.length > 0 ? 'partial' : 'failed',
		priorityClass: GPU_SCHEDULING_PRIORITY,
		targets: limitGpuPriorityTargets(targets),
		elevatedProcessIds: elevatedProcessIds.slice(0, MAX_GPU_PRIORITY_DIAGNOSTIC_TARGETS),
		skippedProcessIds: skippedProcessIds.slice(0, MAX_GPU_PRIORITY_DIAGNOSTIC_TARGETS),
		failedProcessIds: failedProcessIds.slice(0, MAX_GPU_PRIORITY_DIAGNOSTIC_TARGETS),
	};
	if (elevatedProcessIds.length > 0 || failedProcessIds.length > 0) {
		log.info('[StreamingPriority] Elevated GPU scheduling priority', {
			priorityClass: GPU_SCHEDULING_PRIORITY,
			targets: lastGpuPriorityAcquire.targets,
			elevatedProcessIds: lastGpuPriorityAcquire.elevatedProcessIds,
			skippedProcessIds: lastGpuPriorityAcquire.skippedProcessIds,
			failedProcessIds: lastGpuPriorityAcquire.failedProcessIds,
		});
	}
}

function refreshGpuSchedulingPriority(): void {
	if (refCount <= 0) return;
	elevateGpuSchedulingPriority();
}

function startGpuPriorityRefresh(): void {
	if (process.platform !== 'win32' || GPU_SCHEDULING_PRIORITY === null || gpuPriorityRefreshTimer !== null) return;
	gpuPriorityRefreshTimer = setInterval(refreshGpuSchedulingPriority, GPU_PRIORITY_REFRESH_INTERVAL_MS);
	gpuPriorityRefreshTimer.unref?.();
}

function stopGpuPriorityRefresh(): void {
	if (gpuPriorityRefreshTimer === null) return;
	clearInterval(gpuPriorityRefreshTimer);
	gpuPriorityRefreshTimer = null;
}

function restoreGpuSchedulingPriority(): void {
	if (gpuPriorityElevatedProcessIds.size === 0) {
		lastGpuPriorityRestore = {
			status: 'no-active-priority',
			processIds: [],
			restoredProcessIds: [],
			failedProcessIds: [],
		};
		return;
	}
	const processIds = [...gpuPriorityElevatedProcessIds.keys()];
	gpuPriorityElevatedProcessIds.clear();
	const addon = loadWindowsGpuPriorityModule();
	if (!addon?.restoreGpuSchedulingPriority) {
		lastGpuPriorityRestore = {
			status: 'native-module-unavailable',
			processIds,
			restoredProcessIds: [],
			failedProcessIds: processIds.map((processId) => ({
				processId,
				reason: gpuPriorityModuleUnavailableReason(),
			})),
			detail: gpuPriorityModuleLoadErrorDetail ?? GPU_PRIORITY_MODULE_UNAVAILABLE_DETAIL,
		};
		log.debug('[StreamingPriority] Cannot restore GPU scheduling priority; native module unavailable', {
			processIds,
			detail: lastGpuPriorityRestore.detail,
		});
		return;
	}
	const restoredProcessIds: Array<number> = [];
	const failedProcessIds: Array<GpuPriorityFailure> = [];
	for (const processId of processIds) {
		try {
			if (addon.restoreGpuSchedulingPriority(processId)) {
				restoredProcessIds.push(processId);
			} else {
				failedProcessIds.push({processId, reason: 'native API returned false'});
			}
		} catch (error) {
			failedProcessIds.push({processId, reason: formatErrorDetail(error)});
			log.debug('[StreamingPriority] Failed to restore GPU scheduling priority', {processId, error});
		}
	}
	lastGpuPriorityRestore = {
		status: failedProcessIds.length === 0 ? 'succeeded' : restoredProcessIds.length > 0 ? 'partial' : 'failed',
		processIds,
		restoredProcessIds,
		failedProcessIds,
	};
	if (failedProcessIds.length > 0) {
		log.debug('[StreamingPriority] Failed to restore GPU scheduling priority for some processes', {
			processIds: failedProcessIds,
		});
	} else {
		log.info('[StreamingPriority] Restored GPU scheduling priority', {processIds: restoredProcessIds});
	}
}

export function getStreamingPriorityDiagnostics(): StreamingPriorityDiagnostics {
	return {
		refCount,
		powerSaveBlocker: {
			active: powerBlockerId !== null,
			id: powerBlockerId,
		},
		processPriority: {
			supported: CAN_ELEVATE_PROCESS_PRIORITY,
			streamingPriority: STREAMING_PRIORITY,
			savedPriority,
			elevated: savedPriority !== null,
		},
		gpuScheduling: {
			supported: process.platform === 'win32' && GPU_SCHEDULING_PRIORITY !== null,
			priorityClass: GPU_SCHEDULING_PRIORITY,
			env: GPU_SCHEDULING_PRIORITY_ENV,
			nativeModuleStatus: getGpuPriorityModuleStatus(),
			nativeModuleLoadErrorDetail: gpuPriorityModuleLoadErrorDetail,
			refreshActive: gpuPriorityRefreshTimer !== null,
			refreshIntervalMs: GPU_PRIORITY_REFRESH_INTERVAL_MS,
			trackedWebContents: streamingPriorityWebContents.size,
			elevatedProcesses: [...gpuPriorityElevatedProcessIds.entries()].map(([processId, priorityClass]) => ({
				processId,
				priorityClass,
			})),
			lastAcquire: cloneGpuPriorityAcquireDiagnostics(lastGpuPriorityAcquire),
			lastRestore: cloneGpuPriorityRestoreDiagnostics(lastGpuPriorityRestore),
		},
	};
}

export function acquireStreamingPriority(webContents?: Electron.WebContents): void {
	rememberStreamingPriorityWebContents(webContents);
	if (webContents && !webContents.isDestroyed()) {
		setBackgroundThrottlingBypass(webContents, true);
	}
	refCount++;
	if (refCount === 1) {
		startPowerSaveBlocker();
		elevateProcessPriority();
		retainWindowsScreenCaptureGuard();
		startGpuPriorityRefresh();
		log.info('[StreamingPriority] Acquired', {refCount});
	}
	elevateGpuSchedulingPriority(webContents);
}

export function releaseStreamingPriority(): void {
	if (refCount <= 0) return;
	refCount--;
	if (refCount === 0) {
		stopGpuPriorityRefresh();
		restoreBackgroundThrottling();
		clearStreamingPriorityWebContents();
		stopPowerSaveBlocker();
		restoreProcessPriority();
		restoreGpuSchedulingPriority();
		stopWindowsScreenCaptureGuard('streaming-priority-release');
		log.info('[StreamingPriority] Released');
	}
}

export function resetStreamingPriority(): void {
	if (refCount === 0) return;
	refCount = 0;
	stopGpuPriorityRefresh();
	restoreBackgroundThrottling();
	clearStreamingPriorityWebContents();
	stopPowerSaveBlocker();
	restoreProcessPriority();
	restoreGpuSchedulingPriority();
	stopWindowsScreenCaptureGuard('streaming-priority-reset');
	log.info('[StreamingPriority] Reset');
}
