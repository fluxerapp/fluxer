// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {
	type DataPacket,
	DataStream_Chunk,
	DataStream_CompressionType,
	type DataStream_Header,
	type DataStream_Trailer,
	type Encryption_Type,
} from '@livekit/protocol';
import log from '../../../logger.ts';
import type {NonSharedUint8Array} from '../../../type-polyfills/non-shared-typed-arrays.ts';
import {DataStreamError, DataStreamErrorReason} from '../../errors.ts';
import type {ByteStreamInfo, StreamController, TextStreamInfo} from '../../types.ts';
import {bigIntToNumber, isCompressionStreamSupported, numberToBigInt} from '../../utils.ts';
import {deflateRawDecompress, inflateRawTransform} from '../compression.ts';
import {DEFAULT_MAX_PAYLOAD_BYTE_LENGTH} from '../constants.ts';
import {type ByteStreamHandler, ByteStreamReader, type TextStreamHandler, TextStreamReader} from './StreamReader.ts';

export default class IncomingDataStreamManager {
	private log = log;

	private maxPayloadByteLength: number;

	constructor(maxPayloadByteLength: number = DEFAULT_MAX_PAYLOAD_BYTE_LENGTH) {
		this.maxPayloadByteLength = maxPayloadByteLength;
	}

	private byteStreamControllers = new Map<string, StreamController<DataStream_Chunk>>();

	private textStreamControllers = new Map<string, StreamController<DataStream_Chunk>>();

	private byteStreamHandlers = new Map<string, ByteStreamHandler>();

	private textStreamHandlers = new Map<string, TextStreamHandler>();

	private isConnected = false;

	private bufferedPackets: Array<{packet: DataPacket; encryptionType: Encryption_Type}> = [];

	setConnected(connected: boolean) {
		this.isConnected = connected;
		if (connected) {
			this.flushBufferedPackets();
		}
	}

	private flushBufferedPackets() {
		const packets = this.bufferedPackets;
		this.bufferedPackets = [];
		for (const {packet, encryptionType} of packets) {
			this.handleDataStreamPacket(packet, encryptionType);
		}
	}

	registerTextStreamHandler(topic: string, callback: TextStreamHandler) {
		if (this.textStreamHandlers.has(topic)) {
			throw new DataStreamError(
				`A text stream handler for topic "${topic}" has already been set.`,
				DataStreamErrorReason.HandlerAlreadyRegistered,
			);
		}
		this.textStreamHandlers.set(topic, callback);
	}

	unregisterTextStreamHandler(topic: string) {
		this.textStreamHandlers.delete(topic);
	}

	registerByteStreamHandler(topic: string, callback: ByteStreamHandler) {
		if (this.byteStreamHandlers.has(topic)) {
			throw new DataStreamError(
				`A byte stream handler for topic "${topic}" has already been set.`,
				DataStreamErrorReason.HandlerAlreadyRegistered,
			);
		}
		this.byteStreamHandlers.set(topic, callback);
	}

	unregisterByteStreamHandler(topic: string) {
		this.byteStreamHandlers.delete(topic);
	}

	clearControllers() {
		this.byteStreamControllers.clear();
		this.textStreamControllers.clear();
		this.bufferedPackets = [];
	}

	validateParticipantHasNoActiveDataStreams(participantIdentity: string) {
		const textStreamsBeingSentByDisconnectingParticipant = Array.from(this.textStreamControllers.entries()).filter(
			(entry) => entry[1].sendingParticipantIdentity === participantIdentity,
		);
		const byteStreamsBeingSentByDisconnectingParticipant = Array.from(this.byteStreamControllers.entries()).filter(
			(entry) => entry[1].sendingParticipantIdentity === participantIdentity,
		);

		if (
			textStreamsBeingSentByDisconnectingParticipant.length > 0 ||
			byteStreamsBeingSentByDisconnectingParticipant.length > 0
		) {
			const abnormalEndError = new DataStreamError(
				`Participant ${participantIdentity} unexpectedly disconnected in the middle of sending data`,
				DataStreamErrorReason.AbnormalEnd,
			);
			for (const [id, controller] of byteStreamsBeingSentByDisconnectingParticipant) {
				controller.controller.error(abnormalEndError);
				this.byteStreamControllers.delete(id);
			}
			for (const [id, controller] of textStreamsBeingSentByDisconnectingParticipant) {
				controller.controller.error(abnormalEndError);
				this.textStreamControllers.delete(id);
			}
		}
	}

	handleDataStreamPacket(packet: DataPacket, encryptionType: Encryption_Type) {
		if (!this.isConnected) {
			this.bufferedPackets.push({packet, encryptionType});
			return;
		}
		switch (packet.value.case) {
			case 'streamHeader':
				return this.handleStreamHeader(packet.value.value, packet.participantIdentity, encryptionType);
			case 'streamChunk':
				return this.handleStreamChunk(packet.value.value, encryptionType);
			case 'streamTrailer':
				return this.handleStreamTrailer(packet.value.value, encryptionType);
			default:
				throw new Error(`DataPacket of value "${packet.value.case}" is not data stream related!`);
		}
	}

	private handleStreamHeader(
		streamHeader: DataStream_Header,
		participantIdentity: string,
		encryptionType: Encryption_Type,
	) {
		switch (streamHeader.contentHeader.case) {
			case 'byteHeader': {
				const streamHandlerCallback = this.byteStreamHandlers.get(streamHeader.topic);
				if (!streamHandlerCallback) {
					this.log.debug('ignoring incoming byte stream due to no handler for topic', streamHeader.topic);
					return;
				}

				let streamController: ReadableStreamDefaultController<DataStream_Chunk>;

				const info: ByteStreamInfo = {
					id: streamHeader.streamId,
					name: streamHeader.contentHeader.value.name ?? 'unknown',
					mimeType: streamHeader.mimeType,
					size: streamHeader.totalLength ? Number(streamHeader.totalLength) : undefined,
					topic: streamHeader.topic,
					timestamp: bigIntToNumber(streamHeader.timestamp),
					attributes: streamHeader.attributes,
					encryptionType,
				};

				let compressed: boolean;
				switch (streamHeader.compression) {
					case DataStream_CompressionType.DEFLATE_RAW:
						if (!isCompressionStreamSupported()) {
							log.warn(
								`Data stream ${streamHeader.streamId} received with deflate-raw compression, but this browser does not have support for DecompressionStream. Dropping...`,
							);
							return;
						}
						compressed = true;
						break;
					case DataStream_CompressionType.NONE:
						compressed = false;
						break;
					default:
						log.warn(
							`Data stream ${streamHeader.streamId} received with unknown compression type ${streamHeader.compression}, dropping...`,
						);
						return;
				}

				const inlineContent = streamHeader.inlineContent as NonSharedUint8Array;
				if (typeof inlineContent !== 'undefined') {
					streamHandlerCallback(
						new ByteStreamReader(
							info,
							createInlineStream(
								streamHeader.streamId,
								compressed ? deflateRawDecompress(inlineContent, this.maxPayloadByteLength) : inlineContent,
							),
							bigIntToNumber(streamHeader.totalLength),
						),
						{identity: participantIdentity},
					);
					return;
				}

				const stream = new ReadableStream<DataStream_Chunk>({
					start: (controller) => {
						streamController = controller;

						if (this.byteStreamControllers.has(streamHeader.streamId)) {
							throw new DataStreamError(
								`A data stream read is already in progress for a stream with id ${streamHeader.streamId}.`,
								DataStreamErrorReason.AlreadyOpened,
							);
						}

						this.byteStreamControllers.set(streamHeader.streamId, {
							info,
							controller: streamController,
							startTime: Date.now(),
							sendingParticipantIdentity: participantIdentity,
						});
					},
				});
				streamHandlerCallback(
					new ByteStreamReader(
						info,
						compressed
							? inflateRawByteChunkStream(stream, streamHeader.streamId, this.maxPayloadByteLength)
							: stream.pipeThrough(ensureOrderedChunks(streamHeader.streamId)),
						bigIntToNumber(streamHeader.totalLength),
					),
					{
						identity: participantIdentity,
					},
				);
				return;
			}
			case 'textHeader': {
				const streamHandlerCallback = this.textStreamHandlers.get(streamHeader.topic);
				if (!streamHandlerCallback) {
					this.log.debug('ignoring incoming text stream due to no handler for topic', streamHeader.topic);
					return;
				}

				let streamController: ReadableStreamDefaultController<DataStream_Chunk>;

				const info: TextStreamInfo = {
					id: streamHeader.streamId,
					mimeType: streamHeader.mimeType,
					size: streamHeader.totalLength ? Number(streamHeader.totalLength) : undefined,
					topic: streamHeader.topic,
					timestamp: Number(streamHeader.timestamp),
					attributes: streamHeader.attributes,
					encryptionType,
					attachedStreamIds: streamHeader.contentHeader.value.attachedStreamIds,
				};

				let compressed: boolean;
				switch (streamHeader.compression) {
					case DataStream_CompressionType.DEFLATE_RAW:
						if (!isCompressionStreamSupported()) {
							log.warn(
								`Data stream ${streamHeader.streamId} received with deflate-raw compression, but this browser does not have support for DecompressionStream. Dropping...`,
							);
							return;
						}
						compressed = true;
						break;
					case DataStream_CompressionType.NONE:
						compressed = false;
						break;
					default:
						log.warn(
							`Data stream ${streamHeader.streamId} received with unknown compression type ${streamHeader.compression}, dropping...`,
						);
						return;
				}

				const inlineContent = streamHeader.inlineContent as NonSharedUint8Array;
				if (typeof inlineContent !== 'undefined') {
					const content = compressed ? deflateRawDecompress(inlineContent, this.maxPayloadByteLength) : inlineContent;
					streamHandlerCallback(
						new TextStreamReader(
							info,
							createInlineStream(streamHeader.streamId, content),
							bigIntToNumber(streamHeader.totalLength),
						),
						{identity: participantIdentity},
					);
					return;
				}

				const stream = new ReadableStream<DataStream_Chunk>({
					start: (controller) => {
						streamController = controller;

						if (this.textStreamControllers.has(streamHeader.streamId)) {
							throw new DataStreamError(
								`A data stream read is already in progress for a stream with id ${streamHeader.streamId}.`,
								DataStreamErrorReason.AlreadyOpened,
							);
						}

						this.textStreamControllers.set(streamHeader.streamId, {
							info,
							controller: streamController,
							startTime: Date.now(),
							sendingParticipantIdentity: participantIdentity,
						});
					},
				});
				streamHandlerCallback(
					new TextStreamReader(
						info,
						compressed
							? inflateRawChunkStream(stream, streamHeader.streamId, this.maxPayloadByteLength)
							: stream.pipeThrough(ensureOrderedChunks(streamHeader.streamId)),
						bigIntToNumber(streamHeader.totalLength),
					),
					{identity: participantIdentity},
				);
				return;
			}
		}
	}

	private handleStreamChunk(chunk: DataStream_Chunk, encryptionType: Encryption_Type) {
		const fileBuffer = this.byteStreamControllers.get(chunk.streamId);
		if (fileBuffer) {
			if (fileBuffer.info.encryptionType !== encryptionType) {
				fileBuffer.controller.error(
					new DataStreamError(
						`Encryption type mismatch for stream ${chunk.streamId}. Expected ${encryptionType}, got ${fileBuffer.info.encryptionType}`,
						DataStreamErrorReason.EncryptionTypeMismatch,
					),
				);
				this.byteStreamControllers.delete(chunk.streamId);
			} else {
				fileBuffer.controller.enqueue(chunk);
			}
		}
		const textBuffer = this.textStreamControllers.get(chunk.streamId);
		if (textBuffer) {
			if (textBuffer.info.encryptionType !== encryptionType) {
				textBuffer.controller.error(
					new DataStreamError(
						`Encryption type mismatch for stream ${chunk.streamId}. Expected ${encryptionType}, got ${textBuffer.info.encryptionType}`,
						DataStreamErrorReason.EncryptionTypeMismatch,
					),
				);
				this.textStreamControllers.delete(chunk.streamId);
			} else {
				textBuffer.controller.enqueue(chunk);
			}
		}
	}

	private handleStreamTrailer(trailer: DataStream_Trailer, encryptionType: Encryption_Type) {
		const textBuffer = this.textStreamControllers.get(trailer.streamId);
		if (textBuffer) {
			if (textBuffer.info.encryptionType !== encryptionType) {
				textBuffer.controller.error(
					new DataStreamError(
						`Encryption type mismatch for stream ${trailer.streamId}. Expected ${encryptionType}, got ${textBuffer.info.encryptionType}`,
						DataStreamErrorReason.EncryptionTypeMismatch,
					),
				);
			} else {
				textBuffer.info.attributes = {...textBuffer.info.attributes, ...trailer.attributes};
				if (trailer.reason) {
					textBuffer.controller.error(
						new DataStreamError(
							`Data stream ${trailer.streamId} closed abnormally: ${trailer.reason}`,
							DataStreamErrorReason.AbnormalEnd,
						),
					);
				} else {
					textBuffer.controller.close();
				}
			}
			this.textStreamControllers.delete(trailer.streamId);
		}

		const fileBuffer = this.byteStreamControllers.get(trailer.streamId);
		if (fileBuffer) {
			if (fileBuffer.info.encryptionType !== encryptionType) {
				fileBuffer.controller.error(
					new DataStreamError(
						`Encryption type mismatch for stream ${trailer.streamId}. Expected ${encryptionType}, got ${fileBuffer.info.encryptionType}`,
						DataStreamErrorReason.EncryptionTypeMismatch,
					),
				);
			} else {
				fileBuffer.info.attributes = {...fileBuffer.info.attributes, ...trailer.attributes};
				if (trailer.reason) {
					fileBuffer.controller.error(
						new DataStreamError(
							`Data stream ${trailer.streamId} closed abnormally: ${trailer.reason}`,
							DataStreamErrorReason.AbnormalEnd,
						),
					);
				} else {
					fileBuffer.controller.close();
				}
			}
			this.byteStreamControllers.delete(trailer.streamId);
		}
	}
}

function createInlineStream(
	streamId: string,
	content: Uint8Array | Promise<Uint8Array>,
): ReadableStream<DataStream_Chunk> {
	return new ReadableStream<DataStream_Chunk>({
		start: async (controller) => {
			const bytes = await content;
			controller.enqueue(new DataStream_Chunk({streamId, chunkIndex: BigInt(0), content: bytes}));
			controller.close();
		},
	});
}

function ensureOrderedChunks(streamId: string): TransformStream<DataStream_Chunk, DataStream_Chunk> {
	let lastChunkIndex = -1;
	return new TransformStream({
		transform: (value, controller) => {
			const index = bigIntToNumber(value.chunkIndex);
			if (index <= lastChunkIndex) {
				log.warn(
					`ignoring duplicate chunk ${index} ${value.version > 0 ? `(version ${value.version})` : ''} for data stream ${streamId} (last processed: ${lastChunkIndex})`,
				);
				return;
			}
			if (index > lastChunkIndex + 1) {
				throw new DataStreamError(
					`Missing chunk(s) ${lastChunkIndex + 1}..${index - 1} for data stream ${streamId} - cannot reassemble payload`,
					DataStreamErrorReason.Incomplete,
				);
			}
			lastChunkIndex = index;
			if (value.content.length === 0) {
				return;
			}
			controller.enqueue(value);
		},
	});
}

function chunksToBytes(): TransformStream<DataStream_Chunk, Uint8Array> {
	return new TransformStream({
		transform: (value, controller) => {
			controller.enqueue(value.content);
		},
	});
}

function bytesToChunks(streamId: string): TransformStream<Uint8Array, DataStream_Chunk> {
	let outIndex = 0;
	return new TransformStream({
		transform: (value, controller) => {
			if (value.byteLength > 0) {
				controller.enqueue(
					new DataStream_Chunk({
						streamId,
						chunkIndex: numberToBigInt(outIndex),
						content: value,
					}),
				);
				outIndex += 1;
			}
		},
	});
}

function bytesToDecodedUtf8(streamId: string): TransformStream<Uint8Array, DataStream_Chunk> {
	const decoder = new TextDecoder('utf-8');
	const encoder = new TextEncoder();

	let outIndex = 0;
	const decodeOrThrow = (bytes?: Uint8Array): string => {
		try {
			return bytes ? decoder.decode(bytes, {stream: true}) : decoder.decode();
		} catch (err) {
			throw new DataStreamError(
				`Cannot decode compressed data stream ${streamId} as text: ${err}`,
				DataStreamErrorReason.DecodeFailed,
			);
		}
	};

	return new TransformStream({
		transform: (value, controller) => {
			const text = decodeOrThrow(value);
			if (text.length > 0) {
				controller.enqueue(
					new DataStream_Chunk({
						streamId,
						chunkIndex: numberToBigInt(outIndex),
						content: encoder.encode(text),
					}),
				);
				outIndex += 1;
			}
		},
		flush: (controller) => {
			const tail = decodeOrThrow();
			if (tail.length > 0) {
				controller.enqueue(
					new DataStream_Chunk({
						streamId,
						chunkIndex: numberToBigInt(outIndex),
						content: encoder.encode(tail),
					}),
				);
				outIndex += 1;
			}
		},
	});
}

function inflateRawByteChunkStream(
	raw: ReadableStream<DataStream_Chunk>,
	streamId: string,
	maxPayloadByteLength: number,
): ReadableStream<DataStream_Chunk> {
	return raw
		.pipeThrough(ensureOrderedChunks(streamId))
		.pipeThrough(chunksToBytes())
		.pipeThrough(inflateRawTransform())
		.pipeThrough(maxDecompressedLengthGuard(streamId, maxPayloadByteLength))
		.pipeThrough(bytesToChunks(streamId));
}

function inflateRawChunkStream(
	raw: ReadableStream<DataStream_Chunk>,
	streamId: string,
	maxPayloadByteLength: number,
): ReadableStream<DataStream_Chunk> {
	return raw
		.pipeThrough(ensureOrderedChunks(streamId))
		.pipeThrough(chunksToBytes())
		.pipeThrough(inflateRawTransform())
		.pipeThrough(maxDecompressedLengthGuard(streamId, maxPayloadByteLength))
		.pipeThrough(bytesToDecodedUtf8(streamId));
}

function maxDecompressedLengthGuard(streamId: string, maxByteLength: number): TransformStream<Uint8Array, Uint8Array> {
	let total = 0;
	return new TransformStream({
		transform: (value, controller) => {
			total += value.byteLength;
			if (total > maxByteLength) {
				throw new DataStreamError(
					`Data stream ${streamId} exceeds the maximum payload size of ${maxByteLength} bytes`,
					DataStreamErrorReason.PayloadTooLarge,
				);
			}
			controller.enqueue(value);
		},
	});
}
