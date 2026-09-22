// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {FrameMetadataPayload} from './frameMetadata.ts';

export interface FrameMetadata {
	userTimestamp: bigint;
	frameId: number;
	userData?: Uint8Array;
}

export type PacketTrailerMetadata = FrameMetadata;

export interface FrameMetadataPublishOptions {
	timestamp?: boolean;
	frameId?: boolean;
}

export type PacketTrailerPublishOptions = FrameMetadataPublishOptions;

export interface PTBaseMessage {
	kind: string;
	data?: unknown;
}

export interface PTInitMessage extends PTBaseMessage {
	kind: 'init';
}

export interface PTInitAck extends PTBaseMessage {
	kind: 'initAck';
}

export interface PTDecodeMessage extends PTBaseMessage {
	kind: 'decode';
	data: {
		readableStream: ReadableStream;
		writableStream: WritableStream;
		trackId: string;
		hasPacketTrailer: boolean;
	};
}

export interface PTEncodeMessage extends PTBaseMessage {
	kind: 'encode';
	data: {
		readableStream: ReadableStream;
		writableStream: WritableStream;
		packetTrailer?: FrameMetadataPublishOptions;
	};
}

export type PTScriptTransformOptions =
	| {
			kind: 'decode';
			trackId: string;
	  }
	| {
			kind: 'encode';
			packetTrailer?: FrameMetadataPublishOptions;
	  };

export interface PTMetadataMessage extends PTBaseMessage {
	kind: 'metadata';
	data: FrameMetadataPayload;
}

export interface PTUpdateTrackIdMessage extends PTBaseMessage {
	kind: 'updateTrackId';
	data: {
		oldTrackId: string;
		newTrackId: string;
		hasPacketTrailer: boolean;
	};
}

export type PTWorkerMessage =
	| PTInitMessage
	| PTInitAck
	| PTDecodeMessage
	| PTEncodeMessage
	| PTUpdateTrackIdMessage
	| PTMetadataMessage;
