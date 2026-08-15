// SPDX-License-Identifier: AGPL-3.0-or-later

import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {isNativeVoiceEngineSelected} from '@app/features/voice/engine/native_voice_engine/NativeVoiceEngineSelection';
import {
	markScreenShareCaptureActive,
	markScreenShareCaptureEnded,
	updateScreenShareDisplayMediaSettings,
} from '@app/features/voice/engine/ScreenShareCaptureDiagnostics';
import {
	type CapturedScreenShareTracks,
	stopMediaTrack,
	stopUnselectedStreamTracks,
} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import ActiveScreenShareSource from '@app/features/voice/state/ActiveScreenShareSource';
import type {NativeAudioFramePumpSource} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {createScreenChromiumPreviewBridge} from '@app/features/voice/utils/native_screen_capture_bridge/createChromiumPreviewBridge';
import {
	getNativeScreenCaptureApi,
	markNativeScreenShareTrack,
	type NativeScreenBridgeHandle,
	normalizeNativeScreenCaptureResolution,
} from '@app/features/voice/utils/native_screen_capture_bridge/shared';
import type {
	NativeAudioStartOptions,
	NativeScreenCaptureAvailability,
	NativeScreenCaptureSource,
	NativeScreenCaptureStartOptions,
	NativeScreenCaptureStartResult,
} from '@app/types/electron.d';
import type {ScreenShareCaptureOptions} from 'livekit-client';

type DisplayMediaVideoConstraints = MediaTrackConstraints & {
	cursor?: 'always' | 'motion' | 'never';
	displaySurface?: 'browser' | 'monitor' | 'window';
};
type DisplayMediaAudioConstraints = MediaTrackConstraints & {
	restrictOwnAudio?: boolean;
	suppressLocalAudioPlayback?: boolean;
};
type DisplayMediaTrackSettings = MediaTrackSettings & {
	cursor?: 'always' | 'motion' | 'never';
	displaySurface?: 'browser' | 'monitor' | 'window';
};
interface NativeCaptureAvailabilityReader {
	getAvailability?: () => Promise<NativeScreenCaptureAvailability>;
}

async function readNativeCaptureAvailability(
	api: NativeCaptureAvailabilityReader,
): Promise<NativeScreenCaptureAvailability | null> {
	if (typeof api.getAvailability !== 'function') return null;
	return Promise.resolve(api.getAvailability()).catch(() => null);
}

function resolveDisplayMediaCursorCapture(
	displaySurface: DisplayMediaVideoConstraints['displaySurface'],
): 'always' | 'motion' | 'never' {
	return displaySurface === 'window' ? 'never' : 'always';
}

function getRequestedDisplayMediaVideoConstraints(
	options: ScreenShareCaptureOptions | undefined,
): DisplayMediaVideoConstraints | null {
	if (typeof options?.video !== 'object' || !options.video) return null;
	return options.video as DisplayMediaVideoConstraints;
}

function getNativePreviewBridgeOptions(
	source: NativeScreenCaptureSource,
	startResult: NativeScreenCaptureStartResult,
	resolution?: ScreenShareCaptureOptions['resolution'],
): {
	maxWidth: number;
	maxHeight: number;
	maxFrameRate: number;
	pauseWhenUnfocused: boolean;
	previewPlatform?: NodeJS.Platform;
} {
	const sourceWidth = Math.max(1, startResult.width || resolution?.width || source.width);
	const sourceHeight = Math.max(1, startResult.height || resolution?.height || source.height);
	const sourceFrameRate = Math.max(1, startResult.frameRate || resolution?.frameRate || 60);
	return {
		maxWidth: Math.round(sourceWidth),
		maxHeight: Math.round(sourceHeight),
		maxFrameRate: Math.round(sourceFrameRate),
		pauseWhenUnfocused: false,
		previewPlatform: getElectronAPI()?.platform as NodeJS.Platform | undefined,
	};
}

export function resolveCapturedDisplayMediaCursorCapture(
	track: Pick<MediaStreamTrack, 'getSettings'>,
	options?: ScreenShareCaptureOptions,
): 'always' | 'motion' | 'never' {
	const requestedVideo = getRequestedDisplayMediaVideoConstraints(options);
	const requestedCursor = requestedVideo?.cursor;
	const requestedDisplaySurface = requestedVideo?.displaySurface;
	if (requestedCursor && requestedCursor !== resolveDisplayMediaCursorCapture(requestedDisplaySurface)) {
		return requestedCursor;
	}
	const settings = track.getSettings() as DisplayMediaTrackSettings;
	return resolveDisplayMediaCursorCapture(settings.displaySurface ?? requestedDisplaySurface);
}

export function getDisplayMediaOptions(options?: ScreenShareCaptureOptions): DisplayMediaStreamOptions {
	let videoConstraints: MediaTrackConstraints | boolean = options?.video ?? true;
	const resolution = options?.resolution;
	if (resolution && resolution.width > 0 && resolution.height > 0) {
		videoConstraints = typeof videoConstraints === 'boolean' ? {} : videoConstraints;
		videoConstraints = {
			...videoConstraints,
			width: {ideal: resolution.width},
			height: {ideal: resolution.height},
			frameRate: {ideal: resolution.frameRate, max: resolution.frameRate},
		};
	}
	const base = (typeof videoConstraints === 'boolean' ? {} : videoConstraints) as DisplayMediaVideoConstraints;
	videoConstraints = {
		...base,
		cursor: base.cursor ?? resolveDisplayMediaCursorCapture(base.displaySurface),
	} as MediaTrackConstraints;
	let audioConstraints: DisplayMediaStreamOptions['audio'] = options?.audio ?? false;
	if (audioConstraints) {
		const baseAudio = typeof audioConstraints === 'object' ? audioConstraints : {};
		audioConstraints = {
			...baseAudio,
			channelCount: 2,
			sampleRate: 48000,
			echoCancellation: false,
			noiseSuppression: false,
			autoGainControl: false,
			...(options?.restrictOwnAudio === true ? {restrictOwnAudio: true} : {}),
			...(options?.suppressLocalAudioPlayback === true ? {suppressLocalAudioPlayback: true} : {}),
		} as DisplayMediaAudioConstraints;
	}
	return {
		audio: audioConstraints,
		video: videoConstraints,
		controller: options?.controller,
		selfBrowserSurface: options?.selfBrowserSurface,
		surfaceSwitching: options?.surfaceSwitching,
		systemAudio: options?.systemAudio,
		windowAudio: options?.windowAudio,
		monitorTypeSurfaces: options?.monitorTypeSurfaces,
		preferCurrentTab: options?.preferCurrentTab,
	} as DisplayMediaStreamOptions;
}

export async function createDisplayScreenShareTracks(
	options?: ScreenShareCaptureOptions,
): Promise<CapturedScreenShareTracks> {
	if (!navigator.mediaDevices.getDisplayMedia) {
		throw new Error('getDisplayMedia not supported');
	}
	const stream = await navigator.mediaDevices.getDisplayMedia(getDisplayMediaOptions(options));
	const videoTrack = stream.getVideoTracks()[0];
	if (!videoTrack) {
		stream.getTracks().forEach(stopMediaTrack);
		throw new Error('No video track found in screen share capture');
	}
	if (options?.contentHint) {
		videoTrack.contentHint = options.contentHint;
	}
	await videoTrack.applyConstraints({colorSpace: 'rec709'} as MediaTrackConstraints).catch(() => undefined);
	updateScreenShareDisplayMediaSettings(videoTrack, {
		sourceId: ActiveScreenShareSource.getSourceId(),
	});
	const cursor = resolveCapturedDisplayMediaCursorCapture(videoTrack, options);
	if ((videoTrack.getSettings() as DisplayMediaTrackSettings).cursor !== cursor) {
		await videoTrack.applyConstraints({cursor} as MediaTrackConstraints).catch(() => undefined);
	}
	const audioTrack = stream.getAudioTracks()[0];
	stopUnselectedStreamTracks(stream, [videoTrack, audioTrack]);
	return {
		videoTrack,
		audioTrack,
	};
}

export interface NativeScreenShareOptions {
	source: NativeScreenCaptureSource;
	desktopCaptureSourceId?: string;
	captureId?: string;
	resolution?: ScreenShareCaptureOptions['resolution'];
	contentHint?: ScreenShareCaptureOptions['contentHint'];
	showCursorClicks?: boolean;
	captureRect?: NativeScreenCaptureStartOptions['captureRect'];
	audioTrack?: MediaStreamTrack;
	nativeAudioFramePump?: NativeAudioFramePumpSource;
	nativeAudioLinuxRule?: NonNullable<NativeAudioStartOptions['linuxRule']>;
}

export interface NativeEngineScreenCapture {
	captureId: string;
	width: number;
	height: number;
	previewBridge: NativeScreenBridgeHandle;
}

export async function isNativeScreenCaptureAvailable(): Promise<boolean> {
	if (!isNativeVoiceEngineSelected()) return false;
	const electronApi = getElectronAPI();
	const api = electronApi?.nativeScreenCapture ?? getNativeScreenCaptureApi();
	if (!api) return false;
	try {
		const availability = await api.getAvailability();
		return availability.available;
	} catch {
		return false;
	}
}

export async function startNativeCaptureForEngine(
	options: NativeScreenShareOptions,
): Promise<NativeEngineScreenCapture> {
	const api = getNativeScreenCaptureApi();
	if (!api) {
		throw new Error('Native screen capture API unavailable');
	}
	const {source, resolution} = options;
	const nativeResolution = normalizeNativeScreenCaptureResolution(resolution);
	const availability = await readNativeCaptureAvailability(api);
	const startResult = await api.start({
		sourceId: source.id,
		sourceKind: source.kind,
		width: nativeResolution?.width,
		height: nativeResolution?.height,
		frameRate: resolution?.frameRate,
		injectionMethod: source.kind === 'game' ? DeveloperOptions.gameCaptureInjectionMethod : undefined,
		captureId: options.captureId,
		colorRange: 'full',
		colorSpace: 'rec709',
		showCursorClicks: options.showCursorClicks,
		captureRect: options.captureRect,
		nativeFrameSinkRequired: true,
	});
	markScreenShareCaptureActive({
		method: 'native-voice-engine-screen-capture',
		sourceId: source.id,
		sourceKind: source.kind,
		captureId: startResult.captureId,
		nativeAvailability: availability,
	});
	let previewBridge: NativeScreenBridgeHandle;
	try {
		if (!options.desktopCaptureSourceId) {
			throw new Error('Chromium desktop capture source id unavailable for native screen preview');
		}
		previewBridge = await createScreenChromiumPreviewBridge(
			options.desktopCaptureSourceId,
			getNativePreviewBridgeOptions(source, startResult, resolution),
		);
		markNativeScreenShareTrack(previewBridge.track);
	} catch (error) {
		await api.stop(startResult.captureId).catch(() => undefined);
		markScreenShareCaptureEnded('native-voice-engine-screen-preview-bridge-failed');
		throw error;
	}
	return {
		captureId: startResult.captureId,
		width: startResult.width || source.width,
		height: startResult.height || source.height,
		previewBridge,
	};
}

export async function stopNativeCaptureForEngine(captureId: string): Promise<void> {
	const api = getNativeScreenCaptureApi();
	if (!api) return;
	await api.stop(captureId).catch(() => undefined);
}
