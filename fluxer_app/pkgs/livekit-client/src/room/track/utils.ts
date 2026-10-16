// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {type TrackInfo, TrackPublishedResponse, TrackSource, type VideoQuality} from '@livekit/protocol';
import type {AudioProcessorOptions, TrackProcessor, VideoProcessorOptions} from '../../index.ts';
import {cloneDeep} from '../../utils/cloneDeep.ts';
import {isSafari, sleep} from '../utils.ts';
import type {
	AudioCaptureOptions,
	CreateLocalTracksOptions,
	ScreenShareCaptureOptions,
	VideoCaptureOptions,
	VideoCodec,
} from './options.ts';
import {Track} from './Track.ts';
import type {TrackPublication} from './TrackPublication.ts';
import type {AudioTrack} from './types.ts';

export function mergeDefaultOptions(
	options?: CreateLocalTracksOptions,
	audioDefaults?: AudioCaptureOptions,
	videoDefaults?: VideoCaptureOptions,
): CreateLocalTracksOptions {
	const {optionsWithoutProcessor, audioProcessor, videoProcessor} = extractProcessorsFromOptions(options ?? {});
	const defaultAudioProcessor = audioDefaults?.processor;
	const defaultVideoProcessor = videoDefaults?.processor;
	const clonedOptions: CreateLocalTracksOptions = optionsWithoutProcessor ?? {};
	if (clonedOptions.audio === true) clonedOptions.audio = {};
	if (clonedOptions.video === true) clonedOptions.video = {};

	if (clonedOptions.audio) {
		mergeObjectWithoutOverwriting(
			clonedOptions.audio as Record<string, unknown>,
			audioDefaults as Record<string, unknown>,
		);
		clonedOptions.audio.deviceId ??= {ideal: 'default'};
		if (audioProcessor || defaultAudioProcessor) {
			clonedOptions.audio.processor = audioProcessor ?? defaultAudioProcessor;
		}
	}
	if (clonedOptions.video) {
		mergeObjectWithoutOverwriting(
			clonedOptions.video as Record<string, unknown>,
			videoDefaults as Record<string, unknown>,
		);
		clonedOptions.video.deviceId ??= {ideal: 'default'};
		if (videoProcessor || defaultVideoProcessor) {
			clonedOptions.video.processor = videoProcessor ?? defaultVideoProcessor;
		}
	}
	return clonedOptions;
}

function mergeObjectWithoutOverwriting(
	mainObject: Record<string, unknown>,
	objectToMerge: Record<string, unknown>,
): Record<string, unknown> {
	Object.keys(objectToMerge).forEach((key) => {
		if (mainObject[key] === undefined) mainObject[key] = objectToMerge[key];
	});
	return mainObject;
}

export function constraintsForOptions(options: CreateLocalTracksOptions): MediaStreamConstraints {
	const constraints: MediaStreamConstraints = {};

	if (options.video) {
		if (typeof options.video === 'object') {
			const videoOptions: MediaTrackConstraints = {};
			const target = videoOptions as Record<string, unknown>;
			const source = options.video as Record<string, unknown>;
			Object.keys(source).forEach((key) => {
				switch (key) {
					case 'resolution':
						mergeObjectWithoutOverwriting(target, source.resolution as Record<string, unknown>);
						break;
					default:
						target[key] = source[key];
				}
			});
			constraints.video = videoOptions;
			constraints.video.deviceId ??= {ideal: 'default'};
		} else {
			constraints.video = options.video ? {deviceId: {ideal: 'default'}} : false;
		}
	} else {
		constraints.video = false;
	}

	if (options.audio) {
		if (typeof options.audio === 'object') {
			constraints.audio = options.audio;
			constraints.audio.deviceId ??= {ideal: 'default'};
		} else {
			constraints.audio = {deviceId: {ideal: 'default'}};
		}
	} else {
		constraints.audio = false;
	}
	return constraints;
}
export async function detectSilence(track: AudioTrack, timeOffset = 200): Promise<boolean> {
	const ctx = getNewAudioContext();
	if (ctx) {
		const analyser = ctx.createAnalyser();
		analyser.fftSize = 2048;

		const bufferLength = analyser.frequencyBinCount;
		const dataArray = new Uint8Array(bufferLength);
		const source = ctx.createMediaStreamSource(new MediaStream([track.mediaStreamTrack]));

		source.connect(analyser);
		await sleep(timeOffset);
		analyser.getByteTimeDomainData(dataArray);
		const someNoise = dataArray.some((sample) => sample !== 128 && sample !== 0);
		ctx.close();
		return !someNoise;
	}
	return false;
}

export function getNewAudioContext(): AudioContext | undefined {
	const AudioContext = typeof window !== 'undefined' && (window.AudioContext || window.webkitAudioContext);
	if (AudioContext) {
		const audioContext = new AudioContext({latencyHint: 'interactive'});
		if (audioContext.state === 'suspended' && typeof window !== 'undefined' && window.document?.body) {
			const handleResume = async () => {
				try {
					if (audioContext.state === 'suspended') {
						await audioContext.resume();
					}
				} catch (e) {
					console.warn('Error trying to auto-resume audio context', e);
				} finally {
					window.document.body?.removeEventListener('click', handleResume);
				}
			};

			audioContext.addEventListener('statechange', () => {
				if (audioContext.state === 'closed') {
					window.document.body?.removeEventListener('click', handleResume);
				}
			});

			window.document.body.addEventListener('click', handleResume);
		}
		return audioContext;
	}
	return undefined;
}

export function kindToSource(kind: MediaDeviceKind) {
	if (kind === 'audioinput') {
		return Track.Source.Microphone;
	} else if (kind === 'videoinput') {
		return Track.Source.Camera;
	} else {
		return Track.Source.Unknown;
	}
}

export function sourceToKind(source: Track.Source): MediaDeviceKind | undefined {
	if (source === Track.Source.Microphone) {
		return 'audioinput';
	} else if (source === Track.Source.Camera) {
		return 'videoinput';
	} else {
		return undefined;
	}
}

export function screenCaptureToDisplayMediaStreamOptions(
	options: ScreenShareCaptureOptions,
): DisplayMediaStreamOptions {
	let videoConstraints: MediaTrackConstraints | boolean = options.video ?? true;
	if (options.resolution && options.resolution.width > 0 && options.resolution.height > 0) {
		videoConstraints = typeof videoConstraints === 'boolean' ? {} : videoConstraints;
		if (isSafari()) {
			videoConstraints = {
				...videoConstraints,
				width: {max: options.resolution.width},
				height: {max: options.resolution.height},
				frameRate: options.resolution.frameRate,
			};
		} else {
			videoConstraints = {
				...videoConstraints,
				width: {ideal: options.resolution.width},
				height: {ideal: options.resolution.height},
				frameRate: options.resolution.frameRate,
			};
		}
	}
	let audioConstraints: DisplayMediaStreamOptions['audio'] = options.audio ?? false;
	if (audioConstraints && (options.restrictOwnAudio === true || options.suppressLocalAudioPlayback === true)) {
		const baseAudio = typeof audioConstraints === 'object' ? audioConstraints : {};
		audioConstraints = {
			...baseAudio,
			...(options.restrictOwnAudio === true ? {restrictOwnAudio: true} : {}),
			...(options.suppressLocalAudioPlayback === true ? {suppressLocalAudioPlayback: true} : {}),
		} as MediaTrackConstraints & {
			restrictOwnAudio?: boolean;
			suppressLocalAudioPlayback?: boolean;
		};
	}

	return {
		audio: audioConstraints,
		video: videoConstraints,
		controller: options.controller,
		selfBrowserSurface: options.selfBrowserSurface,
		surfaceSwitching: options.surfaceSwitching,
		systemAudio: options.systemAudio,
		windowAudio: options.windowAudio,
		monitorTypeSurfaces: options.monitorTypeSurfaces,
		preferCurrentTab: options.preferCurrentTab,
	} as DisplayMediaStreamOptions;
}

export function mimeTypeToVideoCodecString(mimeType: string) {
	const normalized = mimeType.split('/')[1].toLowerCase();
	return (normalized === 'av1x' ? 'av1' : normalized) as VideoCodec;
}

export function getTrackPublicationInfo<T extends TrackPublication>(tracks: Array<T>): Array<TrackPublishedResponse> {
	const infos: Array<TrackPublishedResponse> = [];
	tracks.forEach((track: TrackPublication) => {
		if (track.track !== undefined) {
			infos.push(
				new TrackPublishedResponse({
					cid: track.track.mediaStreamID,
					track: track.trackInfo,
				}),
			);
		}
	});
	return infos;
}

export function getLogContextFromTrack(track: Track | TrackPublication): Record<string, unknown> {
	if ('mediaStreamTrack' in track) {
		return {
			trackID: track.sid,
			source: track.source,
			muted: track.isMuted,
			enabled: track.mediaStreamTrack.enabled,
			kind: track.kind,
			streamID: track.mediaStreamID,
			streamTrackID: track.mediaStreamTrack.id,
		};
	} else {
		return {
			trackID: track.trackSid,
			enabled: track.isEnabled,
			muted: track.isMuted,
			trackInfo: {
				mimeType: track.mimeType,
				name: track.trackName,
				encrypted: track.isEncrypted,
				kind: track.kind,
				source: track.source,
				...(track.track ? getLogContextFromTrack(track.track) : {}),
			},
		};
	}
}

export function supportsSynchronizationSources(): boolean {
	return (
		typeof RTCRtpReceiver !== 'undefined' && typeof RTCRtpReceiver.prototype.getSynchronizationSources === 'function'
	);
}

export function diffAttributes(
	oldValues: Record<string, string> | undefined,
	newValues: Record<string, string> | undefined,
) {
	if (oldValues === undefined) {
		oldValues = {};
	}
	if (newValues === undefined) {
		newValues = {};
	}
	const allKeys = [...Object.keys(newValues), ...Object.keys(oldValues)];
	const diff: Record<string, string> = {};

	for (const key of allKeys) {
		if (oldValues[key] !== newValues[key]) {
			diff[key] = newValues[key] ?? '';
		}
	}

	return diff;
}

export function extractProcessorsFromOptions(options: CreateLocalTracksOptions) {
	const newOptions = {...options};
	let audioProcessor: TrackProcessor<Track.Kind.Audio, AudioProcessorOptions> | undefined;
	let videoProcessor: TrackProcessor<Track.Kind.Video, VideoProcessorOptions> | undefined;

	if (typeof newOptions.audio === 'object' && newOptions.audio.processor) {
		audioProcessor = newOptions.audio.processor;
		newOptions.audio = {...newOptions.audio, processor: undefined};
	}
	if (typeof newOptions.video === 'object' && newOptions.video.processor) {
		videoProcessor = newOptions.video.processor;
		newOptions.video = {...newOptions.video, processor: undefined};
	}

	return {audioProcessor, videoProcessor, optionsWithoutProcessor: cloneDeep(newOptions)};
}

export function getTrackSourceFromProto(source: TrackSource): Track.Source {
	switch (source) {
		case TrackSource.CAMERA:
			return Track.Source.Camera;
		case TrackSource.MICROPHONE:
			return Track.Source.Microphone;
		case TrackSource.SCREEN_SHARE:
			return Track.Source.ScreenShare;
		case TrackSource.SCREEN_SHARE_AUDIO:
			return Track.Source.ScreenShareAudio;
		default:
			return Track.Source.Unknown;
	}
}

export function areDimensionsSmaller(a: Track.Dimensions, b: Track.Dimensions): boolean {
	return a.width * a.height < b.width * b.height;
}

export function layerDimensionsFor(trackInfo: TrackInfo, quality: VideoQuality): Track.Dimensions | undefined {
	return trackInfo.layers?.find((l) => l.quality === quality);
}
