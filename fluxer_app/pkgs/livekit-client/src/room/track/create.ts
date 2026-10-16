// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import DeviceManager from '../DeviceManager.ts';
import {audioDefaults, videoDefaults} from '../defaults.ts';
import {mediaTrackToLocalTrack} from '../participant/publishUtils.ts';
import type {LoggerOptions} from '../types.ts';
import {isAudioTrack, isVideoTrack, unwrapConstraint} from '../utils.ts';
import type LocalAudioTrack from './LocalAudioTrack.ts';
import type LocalTrack from './LocalTrack.ts';
import type LocalVideoTrack from './LocalVideoTrack.ts';
import type {AudioCaptureOptions, CreateLocalTracksOptions, VideoCaptureOptions} from './options.ts';
import {Track} from './Track.ts';
import {constraintsForOptions, extractProcessorsFromOptions, mergeDefaultOptions} from './utils.ts';

export async function createLocalTracks(
	options?: CreateLocalTracksOptions,
	loggerOptions?: LoggerOptions,
): Promise<Array<LocalTrack>> {
	options ??= {};
	let attemptExactMatch = false;

	const {
		audioProcessor,
		videoProcessor,
		optionsWithoutProcessor: internalOptions,
	} = extractProcessorsFromOptions(options);

	let retryAudioOptions: AudioCaptureOptions | undefined | boolean = internalOptions.audio;
	let retryVideoOptions: VideoCaptureOptions | undefined | boolean = internalOptions.video;

	if (audioProcessor && typeof internalOptions.audio === 'object') {
		internalOptions.audio.processor = audioProcessor;
	}
	if (videoProcessor && typeof internalOptions.video === 'object') {
		internalOptions.video.processor = videoProcessor;
	}

	if (
		options.audio &&
		typeof internalOptions.audio === 'object' &&
		typeof internalOptions.audio.deviceId === 'string'
	) {
		const deviceId: string = internalOptions.audio.deviceId;
		internalOptions.audio.deviceId = {exact: deviceId};
		attemptExactMatch = true;
		retryAudioOptions = {
			...internalOptions.audio,
			deviceId: {ideal: deviceId},
		};
	}
	if (
		internalOptions.video &&
		typeof internalOptions.video === 'object' &&
		typeof internalOptions.video.deviceId === 'string'
	) {
		const deviceId: string = internalOptions.video.deviceId;
		internalOptions.video.deviceId = {exact: deviceId};
		attemptExactMatch = true;
		retryVideoOptions = {
			...internalOptions.video,
			deviceId: {ideal: deviceId},
		};
	}
	if (internalOptions.audio === true) {
		internalOptions.audio = {deviceId: 'default'};
	} else if (typeof internalOptions.audio === 'object' && internalOptions.audio !== null) {
		internalOptions.audio = {
			...internalOptions.audio,
			deviceId: internalOptions.audio.deviceId || 'default',
		};
	}
	if (internalOptions.video === true) {
		internalOptions.video = {deviceId: 'default'};
	} else if (typeof internalOptions.video === 'object' && !internalOptions.video.deviceId) {
		internalOptions.video.deviceId = 'default';
	}
	const opts = mergeDefaultOptions(internalOptions, audioDefaults, videoDefaults);
	const constraints = constraintsForOptions(opts);

	const mediaPromise = navigator.mediaDevices.getUserMedia(constraints);

	if (internalOptions.audio) {
		DeviceManager.userMediaPromiseMap.set('audioinput', mediaPromise);
		mediaPromise.catch(() => DeviceManager.userMediaPromiseMap.delete('audioinput'));
	}
	if (internalOptions.video) {
		DeviceManager.userMediaPromiseMap.set('videoinput', mediaPromise);
		mediaPromise.catch(() => DeviceManager.userMediaPromiseMap.delete('videoinput'));
	}
	try {
		const stream = await mediaPromise;
		return await Promise.all(
			stream.getTracks().map(async (mediaStreamTrack) => {
				const isAudio = mediaStreamTrack.kind === 'audio';
				let trackConstraints: MediaTrackConstraints | undefined;
				const conOrBool = isAudio ? constraints.audio : constraints.video;
				if (typeof conOrBool !== 'boolean') {
					trackConstraints = conOrBool;
				}

				const newDeviceId = mediaStreamTrack.getSettings().deviceId;
				if (trackConstraints?.deviceId && unwrapConstraint(trackConstraints.deviceId) !== newDeviceId) {
					trackConstraints.deviceId = newDeviceId;
				} else if (!trackConstraints) {
					trackConstraints = {deviceId: newDeviceId};
				}

				const track = mediaTrackToLocalTrack(mediaStreamTrack, trackConstraints, loggerOptions);
				if (track.kind === Track.Kind.Video) {
					track.source = Track.Source.Camera;
				} else if (track.kind === Track.Kind.Audio) {
					track.source = Track.Source.Microphone;
				}
				track.mediaStream = stream;

				if (isAudioTrack(track) && audioProcessor) {
					await track.setProcessor(audioProcessor);
				} else if (isVideoTrack(track) && videoProcessor) {
					await track.setProcessor(videoProcessor);
				}

				return track;
			}),
		);
	} catch (e) {
		if (!attemptExactMatch) {
			throw e;
		}
		return createLocalTracks(
			{
				...options,
				audio: retryAudioOptions,
				video: retryVideoOptions,
			},
			loggerOptions,
		);
	}
}

export async function createLocalVideoTrack(options?: VideoCaptureOptions): Promise<LocalVideoTrack> {
	const tracks = await createLocalTracks({
		audio: false,
		video: options ?? true,
	});
	return <LocalVideoTrack>tracks[0];
}

export async function createLocalAudioTrack(options?: AudioCaptureOptions): Promise<LocalAudioTrack> {
	const tracks = await createLocalTracks({
		audio: options ?? true,
		video: false,
	});
	return <LocalAudioTrack>tracks[0];
}
