// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {E2EEOptions} from './e2ee/types.ts';
import type {FrameMetadataOptions} from './frameMetadata/FrameMetadataManager.ts';
import type {ReconnectPolicy} from './room/ReconnectPolicy.ts';
import type {
	AudioCaptureOptions,
	AudioOutputOptions,
	TrackPublishDefaults,
	VideoCaptureOptions,
	VideoCodec,
} from './room/track/options.ts';
import type {AdaptiveStreamSettings} from './room/track/types.ts';

export interface WebAudioSettings {
	audioContext: AudioContext;
}

export interface InternalRoomOptions {
	adaptiveStream: AdaptiveStreamSettings | boolean;

	dynacast: boolean;

	audioCaptureDefaults?: AudioCaptureOptions;

	videoCaptureDefaults?: VideoCaptureOptions;

	publishDefaults?: TrackPublishDefaults;

	audioOutput?: AudioOutputOptions;

	stopLocalTrackOnUnpublish: boolean;

	reconnectPolicy: ReconnectPolicy;

	disconnectOnPageLeave: boolean;

	expSignalLatency?: number;

	webAudioMix: boolean | WebAudioSettings;

	e2ee?: E2EEOptions;

	encryption?: E2EEOptions;

	loggerName?: string;

	frameMetadata?: FrameMetadataOptions;

	packetTrailer?: FrameMetadataOptions;

	singlePeerConnection: boolean;
	subscriberVideoCodecExclusions?: Array<VideoCodec>;
	h264HardwareProfiles?: ReadonlySet<string>;
	dataStream?: RoomDataStreamOptions;
}
export interface RoomDataStreamOptions {
	maxPayloadByteLength?: number;
}

export interface RoomOptions extends Partial<InternalRoomOptions> {}

export interface InternalRoomConnectOptions {
	autoSubscribe: boolean;

	peerConnectionTimeout: number;

	rtcConfig?: RTCConfiguration;

	maxRetries: number;

	websocketTimeout: number;
}

export interface RoomConnectOptions extends Partial<InternalRoomConnectOptions> {}
