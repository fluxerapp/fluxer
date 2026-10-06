// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {DataStream_Chunk, Encryption_Type} from '@livekit/protocol';

export type SimulationOptions = {
	publish?: {
		audio?: boolean;
		video?: boolean;
		useRealTracks?: boolean;
	};
	participants?: {
		count?: number;
		aspectRatios?: Array<number>;
		audio?: boolean;
		video?: boolean;
	};
};

export interface SendTextOptions {
	topic?: string;
	destinationIdentities?: Array<string>;
	attachments?: Array<File>;
	onProgress?: (progress: number) => void;
	attributes?: Record<string, string>;
	compress?: boolean;
}

export interface SendBytesOptions {
	topic?: string;
	destinationIdentities?: Array<string>;
	attributes?: Record<string, string>;
	onProgress?: (progress: number) => void;
	compress?: boolean;
	name?: string;
	mimeType?: string;
}

export interface StreamTextOptions {
	topic?: string;
	destinationIdentities?: Array<string>;
	type?: 'create' | 'update';
	streamId?: string;
	version?: number;
	attachedStreamIds?: Array<string>;
	replyToStreamId?: string;
	totalSize?: number;
	attributes?: Record<string, string>;
}

export type StreamBytesOptions = {
	name?: string;
	topic?: string;
	attributes?: Record<string, string>;
	destinationIdentities?: Array<string>;
	streamId?: string;
	mimeType?: string;
	totalSize?: number;
};

export type SendFileOptions = Pick<StreamBytesOptions, 'topic' | 'mimeType' | 'destinationIdentities'> & {
	onProgress?: (progress: number) => void;
	encryptionType?: Encryption_Type.NONE;
	compress?: boolean;
};

export type DataPublishOptions = {
	reliable?: boolean;
	destinationIdentities?: Array<string>;
	topic?: string;
};

export type LiveKitReactNativeInfo = {
	platform: 'ios' | 'android' | 'windows' | 'macos' | 'web' | 'native';
	devicePixelRatio: number;
};

export type SimulationScenario =
	| 'signal-reconnect'
	| 'speaker'
	| 'node-failure'
	| 'server-leave'
	| 'migration'
	| 'resume-reconnect'
	| 'force-tcp'
	| 'force-tls'
	| 'full-reconnect'
	| 'subscriber-bandwidth'
	| 'disconnect-signal-on-resume'
	| 'disconnect-signal-on-resume-no-messages'
	| 'leave-full-reconnect'
	| 'fail-on-v1-path';

export type LoggerOptions = {
	loggerName?: string;
	loggerContextCb?: () => Record<string, unknown>;
};

export interface TranscriptionSegment {
	id: string;
	text: string;
	language: string;
	startTime: number;
	endTime: number;
	final: boolean;
	firstReceivedTime: number;
	lastReceivedTime: number;
}

export interface ChatMessage {
	id: string;
	timestamp: number;
	message: string;
	editTimestamp?: number;
	attachedFiles?: Array<File>;
}

export interface StreamController<T extends DataStream_Chunk> {
	info: BaseStreamInfo;
	controller: ReadableStreamDefaultController<T>;
	startTime: number;
	endTime?: number;
	sendingParticipantIdentity: string;
}

export interface BaseStreamInfo {
	id: string;
	mimeType: string;
	topic: string;
	timestamp: number;
	size?: number;
	attributes?: Record<string, string>;
	encryptionType: Encryption_Type;
}
export interface ByteStreamInfo extends BaseStreamInfo {
	name: string;
}

export interface TextStreamInfo extends BaseStreamInfo {
	attachedStreamIds?: Array<string>;
}
