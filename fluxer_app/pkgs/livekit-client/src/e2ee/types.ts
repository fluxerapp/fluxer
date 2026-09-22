// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {FrameMetadataPayload} from '../frameMetadata/frameMetadata.ts';
import type {FrameMetadataPublishOptions} from '../frameMetadata/types.ts';
import type {LogLevel} from '../logger.ts';
import type {VideoCodec} from '../room/track/options.ts';
import type {NonSharedUint8Array} from '../type-polyfills/non-shared-typed-arrays.ts';
import type {BaseE2EEManager} from './E2eeManager.ts';
import type {BaseKeyProvider} from './KeyProvider.ts';

export interface BaseMessage {
	kind: string;
	data?: unknown;
}

export interface InitMessage extends BaseMessage {
	kind: 'init';
	data: {
		keyProviderOptions: KeyProviderOptions;
		loglevel: LogLevel;
	};
}

export interface SetKeyMessage extends BaseMessage {
	kind: 'setKey';
	data: {
		participantIdentity?: string;
		isPublisher: boolean;
		key: CryptoKey;
		keyIndex?: number;
		updateCurrentKeyIndex: boolean;
	};
}

export interface RTPVideoMapMessage extends BaseMessage {
	kind: 'setRTPMap';
	data: {
		map: Map<number, VideoCodec>;
		participantIdentity: string;
	};
}

export interface SifTrailerMessage extends BaseMessage {
	kind: 'setSifTrailer';
	data: {
		trailer: NonSharedUint8Array;
	};
}

export interface EncodeMessage extends BaseMessage {
	kind: 'decode' | 'encode';
	data: {
		participantIdentity: string;
		readableStream: ReadableStream;
		writableStream: WritableStream;
		trackId: string;
		codec?: VideoCodec;
		hasPacketTrailer: boolean;
		packetTrailer?: FrameMetadataPublishOptions;
	};
}

export interface RemoveTransformMessage extends BaseMessage {
	kind: 'removeTransform';
	data: {
		participantIdentity: string;
		trackId: string;
	};
}

export interface UpdateCodecMessage extends BaseMessage {
	kind: 'updateCodec';
	data: {
		participantIdentity: string;
		trackId: string;
		codec?: VideoCodec;
		previousTrackId?: string;
		hasPacketTrailer: boolean;
	};
}

export interface RatchetRequestMessage extends BaseMessage {
	kind: 'ratchetRequest';
	data: {
		participantIdentity?: string;
		keyIndex?: number;
	};
}

export interface RatchetMessage extends BaseMessage {
	kind: 'ratchetKey';
	data: {
		participantIdentity: string;
		keyIndex?: number;
		ratchetResult: RatchetResult;
	};
}

export interface ErrorMessage extends BaseMessage {
	kind: 'error';
	data: {
		error: Error;
		participantIdentity?: string;
		uuid?: string;
	};
}

export interface EnableMessage extends BaseMessage {
	kind: 'enable';
	data: {
		participantIdentity: string;
		enabled: boolean;
	};
}

export interface InitAck extends BaseMessage {
	kind: 'initAck';
	data: {
		enabled: boolean;
	};
}

export interface DecryptDataRequestMessage extends BaseMessage {
	kind: 'decryptDataRequest';
	data: {
		uuid: string;
		payload: NonSharedUint8Array;
		iv: NonSharedUint8Array;
		participantIdentity: string;
		keyIndex: number;
	};
}

export interface DecryptDataResponseMessage extends BaseMessage {
	kind: 'decryptDataResponse';
	data: {
		uuid: string;
		payload: NonSharedUint8Array;
	};
}

export interface EncryptDataRequestMessage extends BaseMessage {
	kind: 'encryptDataRequest';
	data: {
		uuid: string;
		payload: NonSharedUint8Array;
		participantIdentity: string;
	};
}

export interface EncryptDataResponseMessage extends BaseMessage {
	kind: 'encryptDataResponse';
	data: {
		uuid: string;
		payload: NonSharedUint8Array;
		iv: NonSharedUint8Array;
		keyIndex: number;
	};
}

export interface PTMetadataFromE2EEMessage extends BaseMessage {
	kind: 'packetTrailerMetadata';
	data: FrameMetadataPayload;
}

export interface LogMessage extends BaseMessage {
	kind: 'log';
	data: {
		level: 'trace' | 'debug' | 'info' | 'warn' | 'error';
		msg: string;
		context?: object;
	};
}

export interface SetLogLevelMessage extends BaseMessage {
	kind: 'setLogLevel';
	data: {
		level: LogLevel;
	};
}

export type E2EEWorkerMessage =
	| InitMessage
	| SetKeyMessage
	| EncodeMessage
	| ErrorMessage
	| EnableMessage
	| RemoveTransformMessage
	| RTPVideoMapMessage
	| UpdateCodecMessage
	| RatchetRequestMessage
	| RatchetMessage
	| SifTrailerMessage
	| InitAck
	| DecryptDataRequestMessage
	| DecryptDataResponseMessage
	| EncryptDataRequestMessage
	| EncryptDataResponseMessage
	| PTMetadataFromE2EEMessage
	| LogMessage
	| SetLogLevelMessage;

export type KeySet = {material: CryptoKey; encryptionKey: CryptoKey};

export type RatchetResult = {
	chainKey: ArrayBuffer;
	cryptoKey: CryptoKey;
};

export type KeyProviderOptions = {
	sharedKey: boolean;
	ratchetSalt: string;
	ratchetWindowSize: number;
	failureTolerance: number;
	keyringSize: number;
	keySize: 128 | 256;
};

export type KeyInfo = {
	key: CryptoKey;
	participantIdentity?: string;
	keyIndex?: number;
};

export type E2EEManagerOptions = {
	keyProvider: BaseKeyProvider;
	worker: Worker;
};

export type E2EEOptions =
	| E2EEManagerOptions
	| {
			e2eeManager: BaseE2EEManager;
	  };

export type DecodeRatchetOptions = {
	ratchetCount: number;
	encryptionKey?: CryptoKey;
};

export type ScriptTransformOptions = {
	kind: 'decode' | 'encode';
	participantIdentity: string;
	trackId: string;
	codec?: VideoCodec;
	hasPacketTrailer: boolean;
	packetTrailer?: FrameMetadataPublishOptions;
};
