// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import {
	ClientInfo_Capability,
	DataPacket,
	DataStream_Chunk,
	DataStream_CompressionType,
	DataStream_Trailer,
	Encryption_Type,
} from '@livekit/protocol';
import type {StructuredLogger} from '../../../logger.ts';
import type {NonSharedUint8Array} from '../../../type-polyfills/non-shared-typed-arrays.ts';
import {CLIENT_PROTOCOL_DATA_STREAM_V2} from '../../../version.ts';
import {DataStreamError, DataStreamErrorReason} from '../../errors.ts';
import {EngineEvent} from '../../events.ts';
import type RTCEngine from '../../RTCEngine.ts';
import {DataChannelKind} from '../../RTCEngine.ts';
import type {
	ByteStreamInfo,
	SendBytesOptions,
	SendFileOptions,
	SendTextOptions,
	StreamBytesOptions,
	StreamTextOptions,
	TextStreamInfo,
} from '../../types.ts';
import {
	isCompressionStreamSupported,
	numberToBigInt,
	readableFromBytes,
	readBytesInChunks,
	splitUtf8,
} from '../../utils.ts';
import {collect, deflateRawTransform} from '../compression.ts';
import {STREAM_CHUNK_SIZE_BYTES} from '../constants.ts';
import {buildByteStreamHeader, buildTextStreamHeader, createStreamHeaderPacket} from './header-utils.ts';
import {ByteStreamWriter, TextStreamWriter} from './StreamWriter.ts';

const textEncoder = new TextEncoder();

export default class OutgoingDataStreamManager {
	protected engine: RTCEngine;

	protected log: StructuredLogger;

	protected getRemoteParticipantClientProtocol: (identity: string) => number;

	protected getRemoteParticipantCapabilities: (identity: string) => Array<ClientInfo_Capability>;

	protected getAllRemoteParticipantIdentities: () => Array<string>;

	constructor(
		engine: RTCEngine,
		log: StructuredLogger,
		getRemoteParticipantClientProtocol: (identity: string) => number,
		getRemoteParticipantCapabilities: (identity: string) => Array<ClientInfo_Capability>,
		getAllRemoteParticipantIdentities: () => Array<string>,
	) {
		this.engine = engine;
		this.log = log;
		this.getRemoteParticipantClientProtocol = getRemoteParticipantClientProtocol;
		this.getRemoteParticipantCapabilities = getRemoteParticipantCapabilities;
		this.getAllRemoteParticipantIdentities = getAllRemoteParticipantIdentities;
	}

	setupEngine(engine: RTCEngine) {
		this.engine = engine;
	}

	async sendText(text: string, options?: SendTextOptions): Promise<TextStreamInfo> {
		const streamId = crypto.randomUUID();
		const textInBytes = textEncoder.encode(text);
		const totalTextLength = textInBytes.byteLength;
		const compress = options?.compress ?? true;

		let info: TextStreamInfo = {
			id: streamId,
			mimeType: 'text/plain',
			timestamp: Date.now(),
			topic: options?.topic ?? '',
			size: totalTextLength,
			attributes: options?.attributes,
			encryptionType: this.engine.e2eeManager?.isDataChannelEncryptionEnabled
				? Encryption_Type.GCM
				: Encryption_Type.NONE,
		};

		const compressEligible =
			compress &&
			isCompressionStreamSupported() &&
			this.allRecipientsSupportV2(options?.destinationIdentities) &&
			this.allRecipientsSupportCompression(options?.destinationIdentities);
		const compressedStream = compressEligible
			? MaybeCollectedStream.fromStream(readableFromBytes(textInBytes).pipeThrough(deflateRawTransform()))
			: null;

		const noAttachments = !options?.attachments || options.attachments.length === 0;
		if (noAttachments && this.allRecipientsSupportV2(options?.destinationIdentities)) {
			let inlineContent: Uint8Array = textInBytes;
			let compression = DataStream_CompressionType.NONE;
			if (compressedStream) {
				const collectedBytes = await compressedStream.collect();
				if (collectedBytes.byteLength < textInBytes.byteLength) {
					inlineContent = collectedBytes;
					compression = DataStream_CompressionType.DEFLATE_RAW;
				}
			}

			const header = buildTextStreamHeader(info, undefined, {compression, inlineContent});
			const packet = createStreamHeaderPacket(header, options?.destinationIdentities);

			if (packet.toBinary().byteLength <= STREAM_CHUNK_SIZE_BYTES) {
				await this.engine.sendDataPacket(packet, DataChannelKind.RELIABLE);
				options?.onProgress?.(1);
				return info;
			}
		}

		const fileIds = options?.attachments?.map(() => crypto.randomUUID());

		const parts = fileIds ? fileIds.length + 1 : 1;
		const progresses = new Array<number>(parts).fill(0);

		const handleProgress = (progress: number, idx: number) => {
			progresses[idx] = progress;
			options?.onProgress?.(progresses.reduce((acc, val) => acc + val, 0) / parts);
		};

		if (compressedStream) {
			info.attachedStreamIds = fileIds;

			const header = buildTextStreamHeader(info, undefined, {
				compression: DataStream_CompressionType.DEFLATE_RAW,
			});
			const packet = createStreamHeaderPacket(header, options?.destinationIdentities);
			await this.sendChunkedByteStream(
				packet,
				streamId,
				options?.destinationIdentities,
				compressedStream
					.stream()
					.pipeThrough(progressReportingStream(textInBytes.length, (progress) => handleProgress(progress, 0))),
			);

			if (textInBytes.length === 0) {
				handleProgress(1, 0);
			}
		} else {
			const writer = await this.streamText({
				streamId,
				totalSize: totalTextLength,
				destinationIdentities: options?.destinationIdentities,
				topic: options?.topic,
				attachedStreamIds: fileIds,
				attributes: options?.attributes,
			});

			await writer.write(text);
			handleProgress(1, 0);

			await writer.close();
			info = writer.info;
		}

		if (options?.attachments && fileIds) {
			await Promise.all(
				options.attachments.map(async (file, idx) =>
					this._sendFile(fileIds[idx], file, {
						topic: options.topic,
						mimeType: file.type,
						destinationIdentities: options.destinationIdentities,
						compress: options.compress,
						onProgress: (progress) => {
							handleProgress(progress, idx + 1);
						},
					}),
				),
			);
		}
		return info;
	}

	async sendBytes(bytes: Uint8Array, options?: SendBytesOptions): Promise<ByteStreamInfo> {
		const streamId = crypto.randomUUID();
		const destinationIdentities = options?.destinationIdentities;
		const compress = options?.compress ?? true;

		const info: ByteStreamInfo = {
			id: streamId,
			name: options?.name ?? 'unknown',
			mimeType: options?.mimeType ?? 'application/octet-stream',
			timestamp: Date.now(),
			topic: options?.topic ?? '',
			size: bytes.byteLength,
			attributes: options?.attributes,
			encryptionType: this.engine.e2eeManager?.isDataChannelEncryptionEnabled
				? Encryption_Type.GCM
				: Encryption_Type.NONE,
		};

		const progressMonitorTap = progressReportingStream(bytes.length, options?.onProgress);

		const compressEligible =
			compress &&
			isCompressionStreamSupported() &&
			this.allRecipientsSupportV2(destinationIdentities) &&
			this.allRecipientsSupportCompression(destinationIdentities);
		const compressedStream = compressEligible
			? MaybeCollectedStream.fromStream(
					readableFromBytes(bytes as NonSharedUint8Array)
						.pipeThrough(progressMonitorTap)
						.pipeThrough(deflateRawTransform()),
				)
			: null;

		if (this.allRecipientsSupportV2(destinationIdentities)) {
			let inlineContent: Uint8Array = bytes;
			let compression = DataStream_CompressionType.NONE;
			if (compressedStream) {
				const collectedBytes = await compressedStream.collect();
				if (collectedBytes.byteLength < bytes.byteLength) {
					inlineContent = collectedBytes;
					compression = DataStream_CompressionType.DEFLATE_RAW;
				}
			}

			const header = buildByteStreamHeader(info, {compression, inlineContent});
			const packet = createStreamHeaderPacket(header, destinationIdentities);

			if (packet.toBinary().byteLength <= STREAM_CHUNK_SIZE_BYTES) {
				await this.engine.sendDataPacket(packet, DataChannelKind.RELIABLE);
				options?.onProgress?.(1);
				return info;
			}
		}

		const header = buildByteStreamHeader(info, {
			compression: compressedStream ? DataStream_CompressionType.DEFLATE_RAW : DataStream_CompressionType.NONE,
		});
		const packet = createStreamHeaderPacket(header, destinationIdentities);
		const source = compressedStream
			? compressedStream.stream()
			: readableFromBytes(bytes as NonSharedUint8Array).pipeThrough(progressMonitorTap);
		await this.sendChunkedByteStream(packet, streamId, destinationIdentities, source);

		if (bytes.length === 0) {
			options?.onProgress?.(1);
		}

		return info;
	}

	private allRecipientsSupportV2(destinationIdentities?: Array<string>): boolean {
		const identities =
			destinationIdentities && destinationIdentities.length > 0
				? destinationIdentities
				: this.getAllRemoteParticipantIdentities();
		return identities.every(
			(identity) => this.getRemoteParticipantClientProtocol(identity) >= CLIENT_PROTOCOL_DATA_STREAM_V2,
		);
	}

	private allRecipientsSupportCompression(destinationIdentities?: Array<string>): boolean {
		const identities =
			destinationIdentities && destinationIdentities.length > 0
				? destinationIdentities
				: this.getAllRemoteParticipantIdentities();
		return identities.every((identity) =>
			this.getRemoteParticipantCapabilities(identity).includes(ClientInfo_Capability.CAP_COMPRESSION_DEFLATE_RAW),
		);
	}

	private async sendChunkedByteStream(
		headerPacket: DataPacket,
		streamId: string,
		destinationIdentities: Array<string> | undefined,
		source: ReadableStream<NonSharedUint8Array>,
	): Promise<void> {
		const engine = this.engine;
		await sendHeaderPacket(engine, headerPacket);

		let chunkId = 0;
		for await (const chunk of readBytesInChunks(source, STREAM_CHUNK_SIZE_BYTES)) {
			const chunkPacket = new DataPacket({
				destinationIdentities,
				value: {
					case: 'streamChunk',
					value: new DataStream_Chunk({
						content: chunk,
						streamId,
						chunkIndex: numberToBigInt(chunkId),
					}),
				},
			});
			await engine.sendDataPacket(chunkPacket, DataChannelKind.RELIABLE);
			chunkId += 1;
		}

		await sendStreamTrailer(streamId, destinationIdentities, engine);
	}

	async streamText(options?: StreamTextOptions): Promise<TextStreamWriter> {
		const streamId = options?.streamId ?? crypto.randomUUID();
		const destinationIdentities = options?.destinationIdentities;

		const info: TextStreamInfo = {
			id: streamId,
			mimeType: 'text/plain',
			timestamp: Date.now(),
			topic: options?.topic ?? '',
			size: options?.totalSize,
			attributes: options?.attributes,
			encryptionType: this.engine.e2eeManager?.isDataChannelEncryptionEnabled
				? Encryption_Type.GCM
				: Encryption_Type.NONE,
			attachedStreamIds: options?.attachedStreamIds,
		};
		const header = buildTextStreamHeader(info, options);
		const packet = createStreamHeaderPacket(header, destinationIdentities);
		await sendHeaderPacket(this.engine, packet);

		let chunkId = 0;
		const engine = this.engine;

		const writableStream = new WritableStream<string>({
			async write(text) {
				for (const textByteChunk of splitUtf8(text, STREAM_CHUNK_SIZE_BYTES)) {
					const chunk = new DataStream_Chunk({
						content: textByteChunk,
						streamId,
						chunkIndex: numberToBigInt(chunkId),
					});
					const chunkPacket = new DataPacket({
						destinationIdentities,
						value: {
							case: 'streamChunk',
							value: chunk,
						},
					});
					await engine.sendDataPacket(chunkPacket, DataChannelKind.RELIABLE);

					chunkId += 1;
				}
			},
			async close() {
				await sendStreamTrailer(streamId, destinationIdentities, engine);
			},
			abort(_err) {},
		});

		const onEngineClose = async () => {
			await writer.close();
		};

		engine.once(EngineEvent.Closing, onEngineClose);

		const writer = new TextStreamWriter(writableStream, info, () =>
			this.engine.off(EngineEvent.Closing, onEngineClose),
		);

		return writer;
	}

	async sendFile(file: File, options?: SendFileOptions): Promise<{id: string}> {
		const streamId = crypto.randomUUID();
		await this._sendFile(streamId, file, options);
		return {id: streamId};
	}

	private async _sendFile(streamId: string, file: File, options?: SendFileOptions): Promise<ByteStreamInfo> {
		const destinationIdentities = options?.destinationIdentities;
		const compress =
			(options?.compress ?? true) &&
			isCompressionStreamSupported() &&
			this.allRecipientsSupportV2(destinationIdentities) &&
			this.allRecipientsSupportCompression(destinationIdentities);

		const info: ByteStreamInfo = {
			id: streamId,
			name: file.name,
			mimeType: options?.mimeType ?? file.type,
			topic: options?.topic ?? '',
			timestamp: Date.now(),
			size: file.size,
			encryptionType: this.engine.e2eeManager?.isDataChannelEncryptionEnabled
				? Encryption_Type.GCM
				: Encryption_Type.NONE,
		};

		const header = buildByteStreamHeader(info, {
			compression: compress ? DataStream_CompressionType.DEFLATE_RAW : DataStream_CompressionType.NONE,
		});
		const packet = createStreamHeaderPacket(header, destinationIdentities);

		const tapped = file.stream().pipeThrough(progressReportingStream(file.size, options?.onProgress));
		const source = compress ? tapped.pipeThrough(deflateRawTransform()) : tapped;
		await this.sendChunkedByteStream(packet, streamId, destinationIdentities, source);

		if (file.size === 0) {
			options?.onProgress?.(1);
		}

		return info;
	}

	async streamBytes(options?: StreamBytesOptions) {
		const streamId = options?.streamId ?? crypto.randomUUID();
		const destinationIdentities = options?.destinationIdentities;

		const info: ByteStreamInfo = {
			id: streamId,
			mimeType: options?.mimeType ?? 'application/octet-stream',
			topic: options?.topic ?? '',
			timestamp: Date.now(),
			attributes: options?.attributes,
			size: options?.totalSize,
			name: options?.name ?? 'unknown',
			encryptionType: this.engine.e2eeManager?.isDataChannelEncryptionEnabled
				? Encryption_Type.GCM
				: Encryption_Type.NONE,
		};

		const header = buildByteStreamHeader(info);
		const packet = createStreamHeaderPacket(header, destinationIdentities);

		await sendHeaderPacket(this.engine, packet);

		let chunkId = 0;
		const writeMutex = new Mutex();
		const engine = this.engine;
		const logLocal = this.log;

		const writableStream = new WritableStream<Uint8Array>({
			async write(chunk) {
				const unlock = await writeMutex.lock();

				let byteOffset = 0;
				try {
					while (byteOffset < chunk.byteLength) {
						const subChunk = chunk.slice(byteOffset, byteOffset + STREAM_CHUNK_SIZE_BYTES);
						const chunkPacket = new DataPacket({
							destinationIdentities,
							value: {
								case: 'streamChunk',
								value: new DataStream_Chunk({
									content: subChunk,
									streamId,
									chunkIndex: numberToBigInt(chunkId),
								}),
							},
						});
						await engine.sendDataPacket(chunkPacket, DataChannelKind.RELIABLE);
						chunkId += 1;
						byteOffset += subChunk.byteLength;
					}
				} finally {
					unlock();
				}
			},
			async close() {
				await sendStreamTrailer(streamId, destinationIdentities, engine);
			},
			abort(err) {
				logLocal.error('Sink error:', err);
			},
		});

		const byteWriter = new ByteStreamWriter(writableStream, info);

		return byteWriter;
	}
}

class MaybeCollectedStream {
	private state:
		| {type: 'stream'; stream: ReadableStream<NonSharedUint8Array>}
		| {type: 'collected'; bytes: NonSharedUint8Array};

	private constructor(state: typeof this.state) {
		this.state = state;
	}

	static fromStream(stream: ReadableStream<NonSharedUint8Array>) {
		return new MaybeCollectedStream({type: 'stream', stream});
	}

	async collect() {
		switch (this.state.type) {
			case 'stream': {
				const bytes = await collect(this.state.stream);
				this.state = {type: 'collected', bytes};
				return bytes;
			}
			case 'collected':
				return this.state.bytes;
		}
	}

	stream() {
		switch (this.state.type) {
			case 'stream':
				return this.state.stream;
			case 'collected':
				return readableFromBytes(this.state.bytes);
		}
	}
}

function progressReportingStream(
	totalPreCompressionLength: number | undefined,
	onProgress?: (progress: number) => void,
): ReadableWritablePair<NonSharedUint8Array, NonSharedUint8Array> {
	let sent = 0;
	return new TransformStream<NonSharedUint8Array, NonSharedUint8Array>({
		transform(chunk, controller) {
			sent += chunk.byteLength;
			if (onProgress && typeof totalPreCompressionLength === 'number' && totalPreCompressionLength > 0) {
				onProgress(Math.min(sent / totalPreCompressionLength, 1));
			}
			controller.enqueue(chunk);
		},
	});
}

async function sendHeaderPacket(engine: RTCEngine, packet: DataPacket): Promise<void> {
	if (packet.toBinary().byteLength > STREAM_CHUNK_SIZE_BYTES) {
		throw new DataStreamError(
			`data stream header exceeds the ${STREAM_CHUNK_SIZE_BYTES}-byte limit; reduce attribute size`,
			DataStreamErrorReason.HeaderTooLarge,
		);
	}
	await engine.sendDataPacket(packet, DataChannelKind.RELIABLE);
}

async function sendStreamTrailer(
	streamId: string,
	destinationIdentities: Array<string> | undefined,
	engine: RTCEngine,
): Promise<void> {
	const trailerPacket = new DataPacket({
		destinationIdentities,
		value: {case: 'streamTrailer', value: new DataStream_Trailer({streamId})},
	});
	await engine.sendDataPacket(trailerPacket, DataChannelKind.RELIABLE);
}
