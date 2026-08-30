// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type CapturedScreenShareTracks,
	type DeviceScreenShareCaptureOptions,
	logger,
	stopMediaTrack,
	stopUnselectedStreamTracks,
} from '@app/features/voice/engine/voice_screen_share_manager/shared';

interface BuildConstraintsOptions {
	useExactDeviceId: boolean;
	includeResolution: boolean;
}

export function getDeviceMediaConstraints(
	options: DeviceScreenShareCaptureOptions | undefined,
	flags: BuildConstraintsOptions,
): MediaStreamConstraints {
	const resolution = options?.resolution;
	const videoConstraints: MediaTrackConstraints = {};
	if (options?.videoDeviceId && options.videoDeviceId !== 'default') {
		videoConstraints.deviceId = flags.useExactDeviceId
			? {exact: options.videoDeviceId}
			: {ideal: options.videoDeviceId};
	}
	if (resolution && flags.includeResolution) {
		videoConstraints.width = {ideal: resolution.width};
		videoConstraints.height = {ideal: resolution.height};
		videoConstraints.frameRate = resolution.frameRate;
	}
	let audioConstraints: MediaTrackConstraints | false = false;
	if (options?.audioDeviceId !== undefined) {
		audioConstraints = {
			echoCancellation: false,
			noiseSuppression: false,
			autoGainControl: false,
		};
		if (options.audioDeviceId && options.audioDeviceId !== 'default') {
			audioConstraints.deviceId = flags.useExactDeviceId
				? {exact: options.audioDeviceId}
				: {ideal: options.audioDeviceId};
		}
	}
	return {
		video: videoConstraints,
		audio: audioConstraints,
	};
}

const RESOLUTION_CONSTRAINT_NAMES = new Set(['width', 'height', 'frameRate', 'aspectRatio']);

function readOverconstrainedField(error: unknown): string | undefined {
	if (!(error instanceof Error) || !('constraint' in error)) {
		return undefined;
	}
	const constraint = error.constraint;
	return typeof constraint === 'string' && constraint.length > 0 ? constraint : undefined;
}

function getOverconstrainedFieldName(error: unknown): string | undefined {
	if (!(error instanceof Error) || error.name !== 'OverconstrainedError') {
		return undefined;
	}
	return readOverconstrainedField(error);
}

function summarizeGetUserMediaError(error: unknown): Record<string, unknown> {
	if (!(error instanceof Error)) {
		return {error: String(error)};
	}
	const summary: Record<string, unknown> = {name: error.name, message: error.message};
	const constraint = readOverconstrainedField(error);
	if (constraint !== undefined) {
		summary.constraint = constraint;
	}
	return summary;
}

export async function createDeviceReplacementTracks(
	options?: DeviceScreenShareCaptureOptions,
): Promise<CapturedScreenShareTracks> {
	const hasDeviceSelection = Boolean(options?.videoDeviceId || options?.audioDeviceId);
	const exactConstraints = getDeviceMediaConstraints(options, {useExactDeviceId: true, includeResolution: true});
	let stream: MediaStream;
	try {
		stream = await navigator.mediaDevices.getUserMedia(exactConstraints);
	} catch (initialError) {
		const overconstrainedField = getOverconstrainedFieldName(initialError);
		const dropResolutionFirst =
			overconstrainedField !== undefined && RESOLUTION_CONSTRAINT_NAMES.has(overconstrainedField);
		logger.warn('getUserMedia failed for device capture; attempting fallback', {
			...summarizeGetUserMediaError(initialError),
			dropResolutionFirst,
		});
		if (dropResolutionFirst) {
			try {
				stream = await navigator.mediaDevices.getUserMedia(
					getDeviceMediaConstraints(options, {useExactDeviceId: true, includeResolution: false}),
				);
			} catch (resolutionFallbackError) {
				if (!hasDeviceSelection) {
					throw resolutionFallbackError;
				}
				logger.warn('getUserMedia resolution fallback failed; retrying with ideal deviceId', {
					...summarizeGetUserMediaError(resolutionFallbackError),
				});
				stream = await navigator.mediaDevices.getUserMedia(
					getDeviceMediaConstraints(options, {useExactDeviceId: false, includeResolution: false}),
				);
			}
		} else {
			if (!hasDeviceSelection) {
				throw initialError;
			}
			try {
				stream = await navigator.mediaDevices.getUserMedia(
					getDeviceMediaConstraints(options, {useExactDeviceId: false, includeResolution: true}),
				);
			} catch (idealFallbackError) {
				logger.warn('getUserMedia ideal-deviceId fallback failed; retrying without resolution', {
					...summarizeGetUserMediaError(idealFallbackError),
				});
				stream = await navigator.mediaDevices.getUserMedia(
					getDeviceMediaConstraints(options, {useExactDeviceId: false, includeResolution: false}),
				);
			}
		}
	}
	const videoTrack = stream.getVideoTracks()[0];
	if (!videoTrack) {
		stream.getTracks().forEach(stopMediaTrack);
		throw new Error('No video track found in device screen share capture');
	}
	const audioTrack = stream.getAudioTracks()[0];
	stopUnselectedStreamTracks(stream, [videoTrack, audioTrack]);
	return {
		videoTrack,
		audioTrack,
	};
}
