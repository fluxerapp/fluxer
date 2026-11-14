// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {
	DataPacket,
	DataStream_ByteHeader,
	DataStream_CompressionType,
	DataStream_Header,
	DataStream_OperationType,
	DataStream_TextHeader,
} from '@livekit/protocol';
import type {ByteStreamInfo, StreamTextOptions, TextStreamInfo} from '../../types.ts';
import {numberToBigInt} from '../../utils.ts';

export interface StreamHeaderV2Fields {
	compression?: DataStream_CompressionType;
	inlineContent?: Uint8Array;
}

export function buildTextStreamHeader(
	info: TextStreamInfo,
	options?: Pick<StreamTextOptions, 'version' | 'replyToStreamId' | 'type'>,
	v2?: StreamHeaderV2Fields,
): DataStream_Header {
	return new DataStream_Header({
		streamId: info.id,
		mimeType: info.mimeType,
		topic: info.topic,
		timestamp: numberToBigInt(info.timestamp),
		totalLength: numberToBigInt(info.size),
		attributes: info.attributes,
		compression: v2?.compression ?? DataStream_CompressionType.NONE,
		inlineContent: v2?.inlineContent,
		contentHeader: {
			case: 'textHeader',
			value: new DataStream_TextHeader({
				version: options?.version,
				attachedStreamIds: info.attachedStreamIds,
				replyToStreamId: options?.replyToStreamId,
				operationType: options?.type === 'update' ? DataStream_OperationType.UPDATE : DataStream_OperationType.CREATE,
			}),
		},
	});
}

export function buildByteStreamHeader(info: ByteStreamInfo, v2?: StreamHeaderV2Fields): DataStream_Header {
	return new DataStream_Header({
		streamId: info.id,
		mimeType: info.mimeType,
		topic: info.topic,
		timestamp: numberToBigInt(info.timestamp),
		totalLength: numberToBigInt(info.size),
		attributes: info.attributes,
		compression: v2?.compression ?? DataStream_CompressionType.NONE,
		inlineContent: v2?.inlineContent,
		contentHeader: {
			case: 'byteHeader',
			value: new DataStream_ByteHeader({
				name: info.name,
			}),
		},
	});
}

export function createStreamHeaderPacket(header: DataStream_Header, destinationIdentities?: Array<string>): DataPacket {
	return new DataPacket({
		destinationIdentities,
		value: {
			case: 'streamHeader',
			value: header,
		},
	});
}
