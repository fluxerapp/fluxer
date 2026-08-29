// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {
	NativeScreenCaptureAvailability,
	NativeScreenCaptureDiagnostics,
	NativeScreenCaptureSourceKind,
} from '@app/types/electron.d';

export type ScreenShareCaptureMethod =
	| 'display-media'
	| 'native-screen-capture'
	| 'native-voice-engine-screen-capture'
	| 'device-media';

interface ScreenShareCaptureState {
	active: boolean;
	generation: number;
	method: ScreenShareCaptureMethod;
	startedAtMs: number;
	updatedAtMs: number;
	endedAtMs?: number;
	endReason?: string;
	platform?: string | null;
	sourceId?: string | null;
	desktopSourceId?: string | null;
	sourceKind?: NativeScreenCaptureSourceKind | null;
	captureId?: string | null;
	displayShareEnvironment?: string | null;
	displayMediaSettings?: Record<string, unknown> | null;
	device?: {
		videoDeviceId?: string;
		audioDeviceId?: string;
	};
	nativeAvailability?: NativeScreenCaptureAvailability | null;
	nativeDiagnostics?: NativeScreenCaptureDiagnostics | null;
}

interface CaptureStateUpdate {
	method: ScreenShareCaptureMethod;
	sourceId?: string | null;
	desktopSourceId?: string | null;
	sourceKind?: NativeScreenCaptureSourceKind | null;
	captureId?: string | null;
	displayShareEnvironment?: string | null;
	displayMediaSettings?: Record<string, unknown> | null;
	device?: {
		videoDeviceId?: string;
		audioDeviceId?: string;
	};
	nativeAvailability?: NativeScreenCaptureAvailability | null;
	nativeDiagnostics?: NativeScreenCaptureDiagnostics | null;
}

interface DisplayMediaTrackSettingsUpdate {
	sourceId?: string | null;
	desktopSourceId?: string | null;
	displayShareEnvironment?: string | null;
}

interface MediaTrackSettingsReader {
	getSettings(): MediaTrackSettings;
}

const MAX_CAPTURE_HISTORY = 20;

let activeCapture: ScreenShareCaptureState | null = null;
let lastCapture: ScreenShareCaptureState | null = null;
let captureGeneration = 0;
const captureHistory: Array<ScreenShareCaptureState | null> = new Array(MAX_CAPTURE_HISTORY).fill(null);
let captureHistoryHead = 0;
let captureHistoryLength = 0;

function nowMs(): number {
	return Date.now();
}

function readPlatform(): string | null {
	return getElectronAPI()?.platform ?? null;
}

function cloneCaptureState(state: ScreenShareCaptureState | null): ScreenShareCaptureState | null {
	if (!state) return null;
	const cloned = {...state};
	if (state.device) {
		cloned.device = {...state.device};
	}
	return cloned;
}

function cloneCaptureHistoryTail(limit: number): Array<ScreenShareCaptureState | null> {
	const tailCount = Math.min(captureHistoryLength, limit);
	const tail = new Array<ScreenShareCaptureState | null>(tailCount);
	const startIndex = captureHistoryLength - tailCount;
	for (let i = 0; i < tailCount; i += 1) {
		const index = (captureHistoryHead + startIndex + i) % MAX_CAPTURE_HISTORY;
		tail[i] = cloneCaptureState(captureHistory[index] ?? null);
	}
	return tail;
}

function pushHistory(state: ScreenShareCaptureState): void {
	const cloned = cloneCaptureState(state);
	if (!cloned) return;
	if (captureHistoryLength < MAX_CAPTURE_HISTORY) {
		const index = (captureHistoryHead + captureHistoryLength) % MAX_CAPTURE_HISTORY;
		captureHistory[index] = cloned;
		captureHistoryLength += 1;
		return;
	}
	captureHistory[captureHistoryHead] = cloned;
	captureHistoryHead = (captureHistoryHead + 1) % MAX_CAPTURE_HISTORY;
}

export function markScreenShareCaptureActive(update: CaptureStateUpdate): void {
	const currentTime = nowMs();
	const canUpdateCurrent =
		activeCapture != null &&
		activeCapture.method === update.method &&
		(update.captureId == null || activeCapture.captureId === update.captureId);
	const base = canUpdateCurrent ? activeCapture : null;
	const device = update.device ?? base?.device;
	const nextCapture: ScreenShareCaptureState = {
		active: true,
		generation: base?.generation ?? ++captureGeneration,
		method: update.method,
		startedAtMs: base?.startedAtMs ?? currentTime,
		updatedAtMs: currentTime,
		platform: readPlatform(),
		sourceId: update.sourceId ?? base?.sourceId ?? null,
		desktopSourceId: update.desktopSourceId ?? base?.desktopSourceId ?? null,
		sourceKind: update.sourceKind ?? base?.sourceKind ?? null,
		captureId: update.captureId ?? base?.captureId ?? null,
		displayShareEnvironment: update.displayShareEnvironment ?? base?.displayShareEnvironment ?? null,
		displayMediaSettings: update.displayMediaSettings ?? base?.displayMediaSettings ?? null,
		nativeAvailability: update.nativeAvailability ?? base?.nativeAvailability ?? null,
		nativeDiagnostics: update.nativeDiagnostics ?? base?.nativeDiagnostics ?? null,
	};
	if (device) {
		nextCapture.device = {...device};
	}
	activeCapture = nextCapture;
}

export function updateScreenShareDisplayMediaSettings(
	track: MediaTrackSettingsReader,
	extra?: DisplayMediaTrackSettingsUpdate,
): void {
	const settings = track.getSettings();
	markScreenShareCaptureActive({
		method: 'display-media',
		...extra,
		displayMediaSettings: {
			width: settings.width,
			height: settings.height,
			frameRate: settings.frameRate,
			aspectRatio: settings.aspectRatio,
			deviceId: settings.deviceId,
			displaySurface: (settings as MediaTrackSettings & {displaySurface?: string}).displaySurface,
			cursor: (settings as MediaTrackSettings & {cursor?: string}).cursor,
			logicalSurface: (settings as MediaTrackSettings & {logicalSurface?: boolean}).logicalSurface,
		},
	});
}

export function markScreenShareCaptureEnded(reason: string): void {
	if (!activeCapture) return;
	const ended = {
		...activeCapture,
		active: false,
		updatedAtMs: nowMs(),
		endedAtMs: nowMs(),
		endReason: reason,
	};
	lastCapture = ended;
	pushHistory(ended);
	activeCapture = null;
}

function inferWindowsCaptureMethod(
	capture: ScreenShareCaptureState | null,
	diagnostics: NativeScreenCaptureDiagnostics | null,
): string | null {
	if (!capture || capture.platform !== 'win32') return null;
	if (capture.method === 'display-media') return 'chromium-getDisplayMedia-desktopCapturer';
	if (capture.method === 'device-media') return 'getUserMedia-device';
	if (capture.method !== 'native-screen-capture' && capture.method !== 'native-voice-engine-screen-capture') {
		return null;
	}
	if (diagnostics?.activeStrategy) return diagnostics.activeStrategy;
	if (capture.sourceKind === 'game') return 'wgc';
	if (capture.sourceKind === 'screen') return 'wgc';
	if (capture.sourceKind === 'window') return 'dxgi-duplication';
	return 'native-screen-capture';
}

async function readNativeAvailability(): Promise<NativeScreenCaptureAvailability | null> {
	const api = getElectronAPI()?.nativeScreenCapture;
	if (!api) return null;
	return api.getAvailability().catch(() => null);
}

async function readNativeDiagnostics(
	captureId: string | null | undefined,
): Promise<NativeScreenCaptureDiagnostics | null> {
	const api = getElectronAPI()?.nativeScreenCapture;
	if (!api || !captureId) return null;
	return api.getDiagnostics(captureId).catch(() => null);
}

export async function getScreenShareCaptureDiagnosticSnapshot(): Promise<Record<string, unknown>> {
	const active = cloneCaptureState(activeCapture);
	const nativeAvailability = await readNativeAvailability();
	const nativeDiagnostics = await readNativeDiagnostics(active?.captureId);
	if (active) {
		active.nativeAvailability = nativeAvailability ?? active.nativeAvailability ?? null;
		active.nativeDiagnostics = nativeDiagnostics ?? active.nativeDiagnostics ?? null;
	}
	return {
		active,
		last: cloneCaptureState(lastCapture),
		historyTail: cloneCaptureHistoryTail(5),
		nativeAvailability,
		windowsCaptureMethod: inferWindowsCaptureMethod(active, nativeDiagnostics ?? active?.nativeDiagnostics ?? null),
	};
}
