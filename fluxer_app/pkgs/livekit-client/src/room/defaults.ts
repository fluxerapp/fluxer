// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {InternalRoomConnectOptions, InternalRoomOptions} from '../options.ts';
import DefaultReconnectPolicy from './DefaultReconnectPolicy.ts';
import type {AudioCaptureOptions, TrackPublishDefaults, VideoCaptureOptions} from './track/options.ts';
import {AudioPresets, BackupCodecPolicy, ScreenSharePresets, VideoPresets} from './track/options.ts';

export const defaultVideoCodec = 'h264';

export const publishDefaults: TrackPublishDefaults = {
	audioPreset: AudioPresets.music,
	dtx: false,
	red: true,
	forceStereo: false,
	simulcast: true,
	screenShareEncoding: ScreenSharePresets.original.encoding,
	stopMicTrackOnMute: false,
	videoCodec: defaultVideoCodec,
	backupCodec: {codec: 'h264'},
	backupCodecPolicy: BackupCodecPolicy.SIMULCAST,
	degradationPreference: 'maintain-resolution',
	preConnectBuffer: false,
} as const;

export const audioDefaults: AudioCaptureOptions = {
	deviceId: {ideal: 'default'},
	autoGainControl: true,
	echoCancellation: true,
	noiseSuppression: true,
	voiceIsolation: true,
};

export const videoDefaults: VideoCaptureOptions = {
	deviceId: {ideal: 'default'},
	resolution: VideoPresets.h720.resolution,
};

export const roomOptionDefaults: InternalRoomOptions = {
	adaptiveStream: false,
	dynacast: true,
	stopLocalTrackOnUnpublish: true,
	reconnectPolicy: new DefaultReconnectPolicy(),
	disconnectOnPageLeave: true,
	webAudioMix: false,
	singlePeerConnection: true,
} as const;

export const roomConnectOptionDefaults: InternalRoomConnectOptions = {
	autoSubscribe: true,
	maxRetries: 1,
	peerConnectionTimeout: 15_000,
	websocketTimeout: 15_000,
} as const;
