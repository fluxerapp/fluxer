// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0

import type {NonSharedUint8Array} from '../../../type-polyfills/non-shared-typed-arrays.ts';
import type {Throws} from '../../../utils/throws.ts';
import {DataTrackHandle, DataTrackHandleError, DataTrackHandleErrorReason} from '../handle.ts';
import {coerceToDataView, DataTrackTimestamp, type U16_MAX_SIZE, WrapAroundUnsignedInt} from '../utils.ts';
import {
	BASE_HEADER_LEN,
	EXT_FLAG_MASK,
	EXT_FLAG_SHIFT,
	EXT_WORDS_INDICATOR_SIZE,
	FRAME_MARKER_FINAL,
	FRAME_MARKER_INTER,
	FRAME_MARKER_MASK,
	FRAME_MARKER_SHIFT,
	FRAME_MARKER_SINGLE,
	FRAME_MARKER_START,
	SUPPORTED_VERSION,
	U8_LENGTH_BYTES,
	U16_LENGTH_BYTES,
	U32_LENGTH_BYTES,
	VERSION_MASK,
	VERSION_SHIFT,
} from './constants.ts';
import {DataTrackDeserializeError, DataTrackSerializeError, type DataTrackSerializeErrorReason} from './errors.ts';
import {DataTrackExtensions} from './extensions.ts';
import Serializable from './serializable.ts';

export class DataTrackPacketHeader extends Serializable {
	marker: FrameMarker;

	trackHandle: DataTrackHandle;

	sequence: WrapAroundUnsignedInt<typeof U16_MAX_SIZE>;

	frameNumber: WrapAroundUnsignedInt<typeof U16_MAX_SIZE>;

	timestamp: DataTrackTimestamp<90_000>;

	extensions: DataTrackExtensions;

	constructor(opts: {
		marker: FrameMarker;
		trackHandle: DataTrackHandle;
		sequence: WrapAroundUnsignedInt<typeof U16_MAX_SIZE>;
		frameNumber: WrapAroundUnsignedInt<typeof U16_MAX_SIZE>;
		timestamp: DataTrackTimestamp<90_000>;
		extensions?: DataTrackExtensions;
	}) {
		super();
		this.marker = opts.marker;
		this.trackHandle = opts.trackHandle;
		this.sequence = opts.sequence;
		this.frameNumber = opts.frameNumber;
		this.timestamp = opts.timestamp;
		this.extensions = opts.extensions ?? new DataTrackExtensions();
	}

	private extensionsMetrics() {
		const lengthBytes = this.extensions.toBinaryLengthBytes();
		const lengthWords = Math.ceil((EXT_WORDS_INDICATOR_SIZE + lengthBytes) / 4);
		const paddingLengthBytes = lengthWords * 4 - EXT_WORDS_INDICATOR_SIZE - lengthBytes;

		return {lengthBytes, lengthWords, paddingLengthBytes};
	}

	toBinaryLengthBytes() {
		const {lengthBytes: extLengthBytes, paddingLengthBytes: extPaddingLengthBytes} = this.extensionsMetrics();

		let totalLengthBytes = BASE_HEADER_LEN;
		if (extLengthBytes > 0) {
			totalLengthBytes += EXT_WORDS_INDICATOR_SIZE + extLengthBytes + extPaddingLengthBytes;
		}
		return totalLengthBytes;
	}

	toBinaryInto(
		dataView: DataView,
	): Throws<number, DataTrackSerializeError<DataTrackSerializeErrorReason.TooSmallForHeader>> {
		if (dataView.byteLength < this.toBinaryLengthBytes()) {
			throw DataTrackSerializeError.tooSmallForHeader();
		}

		let initial = SUPPORTED_VERSION << VERSION_SHIFT;

		let marker: number;
		switch (this.marker) {
			case FrameMarker.Inter:
				marker = FRAME_MARKER_INTER;
				break;
			case FrameMarker.Final:
				marker = FRAME_MARKER_FINAL;
				break;
			case FrameMarker.Start:
				marker = FRAME_MARKER_START;
				break;
			case FrameMarker.Single:
				marker = FRAME_MARKER_SINGLE;
				break;
		}
		initial |= marker << FRAME_MARKER_SHIFT;

		const {
			lengthBytes: extensionsLengthBytes,
			lengthWords: extensionsLengthWords,
			paddingLengthBytes: extensionsPaddingLengthBytes,
		} = this.extensionsMetrics();

		if (extensionsLengthBytes > 0) {
			initial |= 1 << EXT_FLAG_SHIFT;
		}

		let byteIndex = 0;
		dataView.setUint8(byteIndex, initial);
		byteIndex += U8_LENGTH_BYTES;
		dataView.setUint8(byteIndex, 0);
		byteIndex += U8_LENGTH_BYTES;

		dataView.setUint16(byteIndex, this.trackHandle);
		byteIndex += U16_LENGTH_BYTES;
		dataView.setUint16(byteIndex, this.sequence.value);
		byteIndex += U16_LENGTH_BYTES;
		dataView.setUint16(byteIndex, this.frameNumber.value);
		byteIndex += U16_LENGTH_BYTES;
		dataView.setUint32(byteIndex, this.timestamp.asTicks());
		byteIndex += U32_LENGTH_BYTES;

		if (extensionsLengthBytes > 0) {
			const rtpOrientedExtensionLengthWords = extensionsLengthWords - 1;

			dataView.setUint16(byteIndex, rtpOrientedExtensionLengthWords);
			byteIndex += U16_LENGTH_BYTES;
			const extensionBytes = this.extensions.toBinaryInto(
				new DataView(dataView.buffer, dataView.byteOffset + byteIndex),
			);
			byteIndex += extensionBytes;
			for (let i = 0; i < extensionsPaddingLengthBytes; i += 1) {
				dataView.setUint8(byteIndex, 0);
				byteIndex += U8_LENGTH_BYTES;
			}
		}

		const totalLengthBytes = this.toBinaryLengthBytes();
		if (byteIndex !== totalLengthBytes) {
			throw new Error(
				`DataTrackPacketHeader.toBinaryInto: Wrote ${byteIndex} bytes but expected length was ${totalLengthBytes} bytes`,
			);
		}

		return totalLengthBytes;
	}

	static fromBinary<Input extends DataView | ArrayBuffer | Uint8Array>(
		input: Input,
	): Throws<[header: DataTrackPacketHeader, byteLength: number], DataTrackDeserializeError> {
		const dataView = coerceToDataView(input);

		if (dataView.byteLength < BASE_HEADER_LEN) {
			throw DataTrackDeserializeError.tooShort();
		}

		let byteIndex = 0;

		const initial = dataView.getUint8(byteIndex);
		byteIndex += U8_LENGTH_BYTES;

		const version = (initial >> VERSION_SHIFT) & VERSION_MASK;
		if (version > SUPPORTED_VERSION) {
			throw DataTrackDeserializeError.unsupportedVersion(version);
		}

		let marker: FrameMarker;
		switch ((initial >> FRAME_MARKER_SHIFT) & FRAME_MARKER_MASK) {
			case FRAME_MARKER_START:
				marker = FrameMarker.Start;
				break;
			case FRAME_MARKER_FINAL:
				marker = FrameMarker.Final;
				break;
			case FRAME_MARKER_SINGLE:
				marker = FrameMarker.Single;
				break;
			default:
				marker = FrameMarker.Inter;
				break;
		}

		const extensionsFlag = ((initial >> EXT_FLAG_SHIFT) & EXT_FLAG_MASK) > 0;

		byteIndex += U8_LENGTH_BYTES;

		let trackHandle: DataTrackHandle | undefined;
		try {
			trackHandle = DataTrackHandle.fromNumber(dataView.getUint16(byteIndex));
		} catch (e) {
			if (
				e instanceof DataTrackHandleError &&
				(e.isReason(DataTrackHandleErrorReason.Reserved) || e.isReason(DataTrackHandleErrorReason.TooLarge))
			) {
				throw DataTrackDeserializeError.invalidHandle(e);
			} else {
				throw e;
			}
		}
		byteIndex += U16_LENGTH_BYTES;

		const sequence = WrapAroundUnsignedInt.u16(dataView.getUint16(byteIndex));
		byteIndex += U16_LENGTH_BYTES;

		const frameNumber = WrapAroundUnsignedInt.u16(dataView.getUint16(byteIndex));
		byteIndex += U16_LENGTH_BYTES;

		const timestamp = DataTrackTimestamp.fromRtpTicks(dataView.getUint32(byteIndex));
		byteIndex += U32_LENGTH_BYTES;

		let extensions = new DataTrackExtensions();
		if (extensionsFlag) {
			if (dataView.byteLength - byteIndex < U16_LENGTH_BYTES) {
				throw DataTrackDeserializeError.missingExtWords();
			}
			const rtpOrientedExtensionWords = dataView.getUint16(byteIndex);
			byteIndex += U16_LENGTH_BYTES;

			const extensionWords = rtpOrientedExtensionWords + 1;

			const extensionLengthBytes = 4 * extensionWords - EXT_WORDS_INDICATOR_SIZE;

			if (byteIndex + extensionLengthBytes > dataView.byteLength) {
				throw DataTrackDeserializeError.headerOverrun();
			}

			const extensionDataView = new DataView(dataView.buffer, dataView.byteOffset + byteIndex, extensionLengthBytes);

			const [result, readBytes] = DataTrackExtensions.fromBinary(extensionDataView);
			extensions = result;
			byteIndex += readBytes;
		}

		return [
			new DataTrackPacketHeader({
				marker,
				trackHandle: trackHandle!,
				sequence,
				frameNumber,
				timestamp,
				extensions,
			}),
			byteIndex,
		];
	}

	toJSON() {
		return {
			marker: this.marker,
			trackHandle: this.trackHandle,
			sequence: this.sequence.value,
			frameNumber: this.frameNumber.value,
			timestamp: this.timestamp.asTicks(),
			extensions: this.extensions.toJSON(),
		};
	}
}

export enum FrameMarker {
	Start = 0,
	Inter = 1,
	Final = 2,
	Single = 3,
}

export class DataTrackPacket extends Serializable {
	header: DataTrackPacketHeader;

	payload: NonSharedUint8Array;

	constructor(header: DataTrackPacketHeader, payload: NonSharedUint8Array) {
		super();
		this.header = header;
		this.payload = payload;
	}

	toBinaryLengthBytes() {
		return this.header.toBinaryLengthBytes() + this.payload.byteLength;
	}

	toBinaryInto(dataView: DataView): Throws<number, DataTrackSerializeError> {
		let byteIndex = 0;
		const headerLengthBytes = this.header.toBinaryInto(dataView);
		byteIndex += headerLengthBytes;

		if (dataView.byteLength - byteIndex < this.payload.byteLength) {
			throw DataTrackSerializeError.tooSmallForPayload();
		}

		for (let index = 0; index < this.payload.length; index += 1) {
			dataView.setUint8(byteIndex, this.payload[index]);
			byteIndex += U8_LENGTH_BYTES;
		}

		const totalLengthBytes = this.toBinaryLengthBytes();
		if (byteIndex !== totalLengthBytes) {
			throw new Error(
				`DataTrackPacket.toBinaryInto: Wrote ${byteIndex} bytes but expected length was ${totalLengthBytes} bytes`,
			);
		}

		return totalLengthBytes;
	}

	static fromBinary<Input extends DataView | ArrayBuffer | Uint8Array>(
		input: Input,
	): Throws<[packet: DataTrackPacket, byteLength: number], DataTrackDeserializeError> {
		const dataView = coerceToDataView(input);

		const [header, headerByteLength] = DataTrackPacketHeader.fromBinary(dataView);

		const payload = dataView.buffer.slice(
			dataView.byteOffset + headerByteLength,
			dataView.byteOffset + dataView.byteLength,
		);

		return [new DataTrackPacket(header, new Uint8Array(payload) as NonSharedUint8Array), dataView.byteLength] as [
			DataTrackPacket,
			number,
		];
	}

	toJSON() {
		return {header: this.header.toJSON(), payload: this.payload};
	}
}
