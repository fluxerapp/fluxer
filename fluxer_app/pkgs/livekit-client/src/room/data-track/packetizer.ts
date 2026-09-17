// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {Throws} from '../../utils/throws.ts';
import {LivekitReasonedError} from '../errors.ts';
import type {DataTrackFrameInternal} from './frame.ts';
import type {DataTrackHandle} from './handle.ts';
import {DataTrackPacket, DataTrackPacketHeader, FrameMarker} from './packet/index.ts';
import {DataTrackClock, DataTrackTimestamp, WrapAroundUnsignedInt} from './utils.ts';

type PacketizeOptions = {
	now?: DataTrackTimestamp<90_000>;
};

export class DataTrackPacketizerError<
	Reason extends DataTrackPacketizerReason = DataTrackPacketizerReason,
> extends LivekitReasonedError<Reason> {
	override readonly name = 'DataTrackPacketizerError';

	reason: Reason;

	reasonName: string;

	constructor(message: string, reason: Reason, options?: {cause?: unknown}) {
		super(19, message, options);
		this.reason = reason;
		this.reasonName = DataTrackPacketizerReason[reason];
	}

	static mtuTooShort() {
		return new DataTrackPacketizerError('MTU is too short to send frame', DataTrackPacketizerReason.MtuTooShort);
	}
}

export enum DataTrackPacketizerReason {
	MtuTooShort = 0,
}

export default class DataTrackPacketizer {
	private handle: DataTrackHandle;

	private mtuSizeBytes: number;

	private sequence = WrapAroundUnsignedInt.u16(0);

	private frameNumber = WrapAroundUnsignedInt.u16(0);

	private clock = DataTrackClock.rtpStartingNow(DataTrackTimestamp.rtpRandom());

	constructor(trackHandle: DataTrackHandle, mtuSizeBytes: number) {
		this.handle = trackHandle;
		this.mtuSizeBytes = mtuSizeBytes;
	}

	static computeFrameMarker(index: number, packetCount: number) {
		if (packetCount <= 1) {
			return FrameMarker.Single;
		}
		if (index === 0) {
			return FrameMarker.Start;
		} else if (index === packetCount - 1) {
			return FrameMarker.Final;
		} else {
			return FrameMarker.Inter;
		}
	}

	*packetize(
		frame: DataTrackFrameInternal,
		options?: PacketizeOptions,
	): Throws<Generator<DataTrackPacket>, DataTrackPacketizerError> {
		const frameNumber = this.frameNumber.getThenIncrement();
		const headerParams = {
			marker: FrameMarker.Inter,
			trackHandle: this.handle,
			sequence: WrapAroundUnsignedInt.u16(0),
			frameNumber,
			timestamp: options?.now ?? this.clock.now(),
			extensions: frame.extensions,
		};
		const headerSerializedLengthBytes = new DataTrackPacketHeader(headerParams).toBinaryLengthBytes();
		if (headerSerializedLengthBytes >= this.mtuSizeBytes) {
			throw DataTrackPacketizerError.mtuTooShort();
		}

		const maxPayloadSizeBytes = this.mtuSizeBytes - headerSerializedLengthBytes;

		const packetCount = Math.ceil(frame.payload.byteLength / maxPayloadSizeBytes);

		for (
			let index = 0, indexBytes = 0;
			indexBytes < frame.payload.byteLength;
			[index, indexBytes] = [index + 1, indexBytes + maxPayloadSizeBytes]
		) {
			const sequence = this.sequence.getThenIncrement();
			const packetHeader = new DataTrackPacketHeader({
				...headerParams,
				marker: DataTrackPacketizer.computeFrameMarker(index, packetCount),
				sequence,
			});

			const packetPayloadLengthBytes = Math.min(maxPayloadSizeBytes, frame.payload.byteLength - indexBytes);
			const packetPayload = new Uint8Array(
				frame.payload.buffer,
				frame.payload.byteOffset + indexBytes,
				packetPayloadLengthBytes,
			);

			yield new DataTrackPacket(packetHeader, packetPayload);
		}
	}
}
