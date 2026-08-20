// SPDX-License-Identifier: AGPL-3.0-or-later

import {updateScreenShareDisplayMediaSettings} from '@app/features/voice/engine/ScreenShareCaptureDiagnostics';
import {
	type CapturedScreenShareTracks,
	stopMediaTrack,
	stopUnselectedStreamTracks,
} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import ActiveScreenShareSource from '@app/features/voice/state/ActiveScreenShareSource';
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
