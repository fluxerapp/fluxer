// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type CapturedScreenShareTracks,
	type DisplayScreenShareCaptureContext,
	stopMediaTrack,
	stopUnselectedStreamTracks,
} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import {ScreenShareAudioCaptureError} from '@app/features/voice/utils/ScreenShareAudioCaptureError';
import type {ScreenShareCaptureOptions} from 'livekit-client';

type DisplayMediaVideoConstraints = MediaTrackConstraints & {
	colorSpace?: string;
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

function buildCapturedDisplayMediaConstraints(
	displayMediaOptions: DisplayMediaStreamOptions,
	cursor: 'always' | 'motion' | 'never',
): MediaTrackConstraints {
	const requestedVideo =
		typeof displayMediaOptions.video === 'object' && displayMediaOptions.video
			? (displayMediaOptions.video as DisplayMediaVideoConstraints)
			: undefined;
	const constraints: DisplayMediaVideoConstraints = {colorSpace: 'rec709', cursor};
	if (requestedVideo?.width !== undefined) constraints.width = requestedVideo.width;
	if (requestedVideo?.height !== undefined) constraints.height = requestedVideo.height;
	if (requestedVideo?.frameRate !== undefined) constraints.frameRate = requestedVideo.frameRate;
	return constraints;
}

export async function createDisplayScreenShareTracks(
	options?: ScreenShareCaptureOptions,
	captureContext?: DisplayScreenShareCaptureContext,
): Promise<CapturedScreenShareTracks> {
	if (!navigator.mediaDevices.getDisplayMedia) {
		throw new Error('getDisplayMedia not supported');
	}
	const displayMediaOptions = getDisplayMediaOptions(options);
	const stream = await navigator.mediaDevices.getDisplayMedia(displayMediaOptions);
	try {
		const videoTrack = stream.getVideoTracks()[0];
		if (!videoTrack) {
			throw new Error('No video track found in screen share capture');
		}
		if (options?.contentHint) {
			videoTrack.contentHint = options.contentHint;
		}
		const cursor = resolveCapturedDisplayMediaCursorCapture(videoTrack, options);
		await videoTrack
			.applyConstraints(buildCapturedDisplayMediaConstraints(displayMediaOptions, cursor))
			.catch(() => undefined);
		const capturedAudioTrack = stream.getAudioTracks()[0];
		const audioTrack = capturedAudioTrack?.readyState === 'live' ? capturedAudioTrack : undefined;
		if (captureContext?.requireAudio && !audioTrack) {
			throw new ScreenShareAudioCaptureError({
				sourceId: captureContext.sourceId,
				reason: 'required-audio-track-missing',
				detail: 'display capture completed without the requested native audio track',
			});
		}
		stopUnselectedStreamTracks(stream, [videoTrack, audioTrack]);
		return {
			videoTrack,
			audioTrack,
			displayCapture: captureContext,
		};
	} catch (error) {
		stream.getTracks().forEach(stopMediaTrack);
		throw error;
	}
}
