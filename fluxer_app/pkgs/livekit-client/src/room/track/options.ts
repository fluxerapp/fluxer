// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {FrameMetadataPublishOptions} from '../../frameMetadata/types.ts';
import type {AudioProcessorOptions, TrackProcessor, VideoProcessorOptions} from './processor/types.ts';
import type {Track} from './Track.ts';

export interface TrackPublishDefaults {
	videoEncoding?: VideoEncoding;

	backupCodec?: true | false | {codec: BackupVideoCodec; encoding?: VideoEncoding};

	backupCodecPolicy?: BackupCodecPolicy;

	screenShareEncoding?: VideoEncoding;

	videoCodec?: VideoCodec;

	audioPreset?: AudioPreset;

	dtx?: boolean;

	red?: boolean;

	forceStereo?: boolean;

	simulcast?: boolean;

	scalabilityMode?: ScalabilityMode;

	degradationPreference?: RTCDegradationPreference;

	videoSimulcastLayers?: Array<VideoPreset>;

	screenShareSimulcastLayers?: Array<VideoPreset>;

	stopMicTrackOnMute?: boolean;

	preConnectBuffer?: boolean;

	frameMetadata?: FrameMetadataPublishOptions;

	packetTrailer?: FrameMetadataPublishOptions;
}

export interface TrackPublishOptions extends TrackPublishDefaults {
	name?: string;

	source?: Track.Source;

	stream?: string;
}

export interface CreateLocalTracksOptions {
	audio?: boolean | AudioCaptureOptions;

	video?: boolean | VideoCaptureOptions;
}

export interface VideoCaptureOptions {
	deviceId?: ConstrainDOMString;

	frameRate?: ConstrainDouble;

	facingMode?: 'user' | 'environment' | 'left' | 'right';

	resolution?: VideoResolution;

	processor?: TrackProcessor<Track.Kind.Video, VideoProcessorOptions>;
}

export interface ScreenShareCaptureOptions {
	audio?: boolean | AudioCaptureOptions;

	video?: true | {displaySurface?: 'window' | 'browser' | 'monitor'};

	resolution?: VideoResolution;

	controller?: unknown;

	selfBrowserSurface?: 'include' | 'exclude';

	surfaceSwitching?: 'include' | 'exclude';

	systemAudio?: 'include' | 'exclude';

	windowAudio?: 'system' | 'window' | 'exclude';

	monitorTypeSurfaces?: 'include' | 'exclude';

	contentHint?: 'detail' | 'text' | 'motion';

	restrictOwnAudio?: boolean;

	suppressLocalAudioPlayback?: boolean;

	preferCurrentTab?: boolean;
}

export interface AudioCaptureOptions {
	autoGainControl?: ConstrainBoolean;

	channelCount?: ConstrainULong;

	deviceId?: ConstrainDOMString;

	echoCancellation?: ConstrainBoolean;

	latency?: ConstrainDouble;

	noiseSuppression?: ConstrainBoolean;

	voiceIsolation?: ConstrainBoolean;

	restrictOwnAudio?: ConstrainBoolean;

	sampleRate?: ConstrainULong;

	sampleSize?: ConstrainULong;

	processor?: TrackProcessor<Track.Kind.Audio, AudioProcessorOptions>;
}

export interface AudioOutputOptions {
	deviceId?: string;
}

export interface VideoResolution {
	width: number;
	height: number;
	frameRate?: number;
	aspectRatio?: number;
}

export interface VideoEncoding {
	maxBitrate: number;
	maxFramerate?: number;
	priority?: RTCPriorityType;
}

export interface VideoPresetOptions {
	width: number;
	height: number;
	aspectRatio?: number;
	maxBitrate: number;
	maxFramerate?: number;
	priority?: RTCPriorityType;
}

export class VideoPreset {
	encoding: VideoEncoding;

	width: number;

	height: number;

	aspectRatio?: number;

	constructor(videoPresetOptions: VideoPresetOptions);
	constructor(width: number, height: number, maxBitrate: number, maxFramerate?: number, priority?: RTCPriorityType);
	constructor(
		widthOrOptions: number | VideoPresetOptions,
		height?: number,
		maxBitrate?: number,
		maxFramerate?: number,
		priority?: RTCPriorityType,
	) {
		if (typeof widthOrOptions === 'object') {
			this.width = widthOrOptions.width;
			this.height = widthOrOptions.height;
			this.aspectRatio = widthOrOptions.aspectRatio;
			this.encoding = {
				maxBitrate: widthOrOptions.maxBitrate,
				maxFramerate: widthOrOptions.maxFramerate,
				priority: widthOrOptions.priority,
			};
		} else if (height !== undefined && maxBitrate !== undefined) {
			this.width = widthOrOptions;
			this.height = height;
			this.aspectRatio = widthOrOptions / height;
			this.encoding = {
				maxBitrate,
				maxFramerate,
				priority,
			};
		} else {
			throw new TypeError('Unsupported options: provide at least width, height and maxBitrate');
		}
	}

	get resolution(): VideoResolution {
		return {
			width: this.width,
			height: this.height,
			frameRate: this.encoding.maxFramerate,
			aspectRatio: this.aspectRatio,
		};
	}
}

export interface AudioPreset {
	maxBitrate: number;
	priority?: RTCPriorityType;
}

export const audioCodecs = ['opus', 'red'] as const;

export type AudioCodec = (typeof audioCodecs)[number];

const backupVideoCodecs = ['vp8', 'h264'] as const;

export const videoCodecs = ['vp8', 'h264', 'vp9', 'av1', 'h265'] as const;

export type VideoCodec = (typeof videoCodecs)[number];

export type BackupVideoCodec = (typeof backupVideoCodecs)[number];

export function isBackupVideoCodec(codec: string): codec is BackupVideoCodec {
	return !!backupVideoCodecs.find((backup) => backup === codec);
}

export const isBackupCodec = isBackupVideoCodec;

export enum BackupCodecPolicy {
	PREFER_REGRESSION = 0,
	SIMULCAST = 1,
	REGRESSION = 2,
}

export type ScalabilityMode =
	| 'L1T1'
	| 'L1T2'
	| 'L1T3'
	| 'L2T1'
	| 'L2T1h'
	| 'L2T1_KEY'
	| 'L2T2'
	| 'L2T2h'
	| 'L2T2_KEY'
	| 'L2T3'
	| 'L2T3h'
	| 'L2T3_KEY'
	| 'L3T1'
	| 'L3T1h'
	| 'L3T1_KEY'
	| 'L3T2'
	| 'L3T2h'
	| 'L3T2_KEY'
	| 'L3T3'
	| 'L3T3h'
	| 'L3T3_KEY';

export namespace AudioPresets {
	export const telephone: AudioPreset = {
		maxBitrate: 12_000,
	};
	export const speech: AudioPreset = {
		maxBitrate: 24_000,
	};
	export const music: AudioPreset = {
		maxBitrate: 48_000,
	};
	export const musicStereo: AudioPreset = {
		maxBitrate: 64_000,
	};
	export const musicHighQuality: AudioPreset = {
		maxBitrate: 96_000,
	};
	export const musicHighQualityStereo: AudioPreset = {
		maxBitrate: 510_000,
	};
}

export const VideoPresets = {
	h90: new VideoPreset(160, 90, 90_000, 20),
	h180: new VideoPreset(320, 180, 160_000, 20),
	h216: new VideoPreset(384, 216, 180_000, 20),
	h360: new VideoPreset(640, 360, 450_000, 20),
	h540: new VideoPreset(960, 540, 800_000, 25),
	h720: new VideoPreset(1280, 720, 1_700_000, 30),
	h1080: new VideoPreset(1920, 1080, 3_000_000, 30),
	h1440: new VideoPreset(2560, 1440, 5_000_000, 30),
	h2160: new VideoPreset(3840, 2160, 8_000_000, 30),
} as const;

export const VideoPresets43 = {
	h120: new VideoPreset(160, 120, 70_000, 20),
	h180: new VideoPreset(240, 180, 125_000, 20),
	h240: new VideoPreset(320, 240, 140_000, 20),
	h360: new VideoPreset(480, 360, 330_000, 20),
	h480: new VideoPreset(640, 480, 500_000, 20),
	h540: new VideoPreset(720, 540, 600_000, 25),
	h720: new VideoPreset(960, 720, 1_300_000, 30),
	h1080: new VideoPreset(1440, 1080, 2_300_000, 30),
	h1440: new VideoPreset(1920, 1440, 3_800_000, 30),
} as const;

export const ScreenSharePresets = {
	h360fps3: new VideoPreset(640, 360, 200_000, 3, 'medium'),
	h360fps15: new VideoPreset(640, 360, 400_000, 15, 'medium'),
	h720fps5: new VideoPreset(1280, 720, 800_000, 5, 'medium'),
	h720fps15: new VideoPreset(1280, 720, 1_500_000, 15, 'medium'),
	h720fps30: new VideoPreset(1280, 720, 2_000_000, 30, 'medium'),
	h1080fps15: new VideoPreset(1920, 1080, 2_500_000, 15, 'medium'),
	h1080fps30: new VideoPreset(1920, 1080, 5_000_000, 30, 'medium'),
	original: new VideoPreset(0, 0, 20_000_000, 60, 'high'),
} as const;
