// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0

import {getLogger, LoggerNames} from '../../logger.ts';
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';
import type {Throws} from '../../utils/throws.ts';
import {LivekitReasonedError} from '../errors.ts';
import type {DataTrackFrameInternal} from './frame.ts';
import type {DataTrackExtensions} from './packet/extensions.ts';
import {type DataTrackPacket, FrameMarker} from './packet/index.ts';
import type {U16_MAX_SIZE, WrapAroundUnsignedInt} from './utils.ts';

const log = getLogger(LoggerNames.DataTracks);

type PartialFrame = {
	startSequence: WrapAroundUnsignedInt<typeof U16_MAX_SIZE>;
	extensions: DataTrackExtensions;
	payloads: Map<number, NonSharedUint8Array>;
};

export class DataTrackDepacketizerDropError<
	Reason extends DataTrackDepacketizerDropReason = DataTrackDepacketizerDropReason,
> extends LivekitReasonedError<Reason> {
	override readonly name = 'DataTrackDepacketizerDropError';

	reason: Reason;

	reasonName: string;

	frameNumber: number;

	constructor(message: string, reason: Reason, frameNumber: number, options?: {cause?: unknown}) {
		super(19, `Frame ${frameNumber} dropped: ${message}`, options);
		this.reason = reason;
		this.reasonName = DataTrackDepacketizerDropReason[reason];
		this.frameNumber = frameNumber;
	}

	static interrupted(frameNumber: number, newFrameNumber: number) {
		return new DataTrackDepacketizerDropError(
			`Interrupted by the start of a new frame ${newFrameNumber}`,
			DataTrackDepacketizerDropReason.Interrupted,
			frameNumber,
		);
	}

	static unknownFrame(frameNumber: number) {
		return new DataTrackDepacketizerDropError(
			'Initial packet was never received.',
			DataTrackDepacketizerDropReason.UnknownFrame,
			frameNumber,
		);
	}

	static bufferFull(frameNumber: number) {
		return new DataTrackDepacketizerDropError(
			'Reorder buffer is full.',
			DataTrackDepacketizerDropReason.BufferFull,
			frameNumber,
		);
	}

	static incomplete(frameNumber: number, receivedPackets: number, expectedPackets: number) {
		return new DataTrackDepacketizerDropError(
			`Not all packets received before final packet. Received ${receivedPackets} packets, expected ${expectedPackets} packets.`,
			DataTrackDepacketizerDropReason.Incomplete,
			frameNumber,
		);
	}
}

export enum DataTrackDepacketizerDropReason {
	Interrupted = 0,
	UnknownFrame = 1,
	BufferFull = 2,
	Incomplete = 3,
}

type PushOptions = {
	throwOnInterruption: boolean;

	maxPartialFrames?: number;
};

export default class DataTrackDepacketizer {
	static MAX_BUFFER_PACKETS = 128;

	private partials: Map<number, PartialFrame> = new Map();

	push(
		packet: DataTrackPacket,
		options?: PushOptions,
	): Throws<DataTrackFrameInternal | null, DataTrackDepacketizerDropError> {
		switch (packet.header.marker) {
			case FrameMarker.Single:
				return this.frameFromSingle(packet, options);
			case FrameMarker.Start:
				return this.beginPartial(packet, options);
			case FrameMarker.Inter:
			case FrameMarker.Final:
				return this.pushToPartial(packet);
		}
	}

	reset() {
		this.partials.clear();
	}

	private peekOldestPartialFrameNumber(): number | null {
		const first = this.partials.keys().next();
		return first.done ? null : first.value;
	}

	private frameFromSingle(
		packet: DataTrackPacket,
		options?: PushOptions,
	): Throws<DataTrackFrameInternal, DataTrackDepacketizerDropError<DataTrackDepacketizerDropReason.Interrupted>> {
		if (packet.header.marker !== FrameMarker.Single) {
			throw new Error(
				`Depacketizer.frameFromSingle: packet.header.marker was not FrameMarker.Single, found ${packet.header.marker}.`,
			);
		}

		const maxPartialFrames = options?.maxPartialFrames ?? 1;
		if (this.partials.size >= maxPartialFrames) {
			const oldestPartialFrameNumber = this.peekOldestPartialFrameNumber();
			if (typeof oldestPartialFrameNumber !== 'number') {
				throw new Error(
					`Depacketizer.frameFromSingle: no oldest frame number found, but partials.size is ${this.partials.size}.`,
				);
			}
			this.partials.delete(oldestPartialFrameNumber);
			if (options?.throwOnInterruption) {
				throw DataTrackDepacketizerDropError.interrupted(oldestPartialFrameNumber, packet.header.frameNumber.value);
			}
			log.warn(
				`Data track frame ${oldestPartialFrameNumber} was interrupted by single-packet frame ${packet.header.frameNumber.value}, dropping.`,
			);
		}

		return {payload: packet.payload, extensions: packet.header.extensions};
	}

	private beginPartial(
		packet: DataTrackPacket,
		options?: PushOptions,
	): Throws<null, DataTrackDepacketizerDropError<DataTrackDepacketizerDropReason.Interrupted>> {
		if (packet.header.marker !== FrameMarker.Start) {
			throw new Error(
				`Depacketizer.beginPartial: packet.header.marker was not FrameMarker.Start, found ${packet.header.marker}.`,
			);
		}

		const startSequence = packet.header.sequence;
		const frameNumber = packet.header.frameNumber.value;
		const partial: PartialFrame = {
			startSequence,
			extensions: packet.header.extensions,
			payloads: new Map([[startSequence.value, packet.payload]]),
		};

		const maxPartialFrames = options?.maxPartialFrames ?? 1;
		while (this.partials.size >= maxPartialFrames) {
			const oldestPartialFrameNumber = this.peekOldestPartialFrameNumber();
			if (typeof oldestPartialFrameNumber !== 'number') {
				break;
			}
			this.partials.delete(oldestPartialFrameNumber);

			if (options?.throwOnInterruption) {
				throw DataTrackDepacketizerDropError.interrupted(oldestPartialFrameNumber, frameNumber);
			}
			log.warn(
				`Data track partials full (max ${maxPartialFrames}), evicted oldest frame ${oldestPartialFrameNumber} to make room for new frame ${frameNumber}.`,
			);
		}
		this.partials.set(frameNumber, partial);

		return null;
	}

	private pushToPartial(
		packet: DataTrackPacket,
	): Throws<DataTrackFrameInternal | null, DataTrackDepacketizerDropError> {
		if (packet.header.marker !== FrameMarker.Inter && packet.header.marker !== FrameMarker.Final) {
			throw new Error(
				`Depacketizer.pushToPartial: packet.header.marker was not FrameMarker.Inter or FrameMarker.Final, found ${packet.header.marker}.`,
			);
		}

		const packetFrameNumber = packet.header.frameNumber.value;
		const matchingPartial = this.partials.get(packetFrameNumber);
		if (!matchingPartial) {
			this.partials.delete(packetFrameNumber);
			throw DataTrackDepacketizerDropError.unknownFrame(packetFrameNumber);
		}

		if (matchingPartial.payloads.size >= DataTrackDepacketizer.MAX_BUFFER_PACKETS) {
			this.partials.delete(packetFrameNumber);
			throw DataTrackDepacketizerDropError.bufferFull(packetFrameNumber);
		}

		if (matchingPartial.payloads.has(packet.header.sequence.value)) {
			log.warn(
				`Data track frame ${packetFrameNumber} received duplicate packet for sequence ${packet.header.sequence.value}, so replacing with newly received packet.`,
			);
		}
		matchingPartial.payloads.set(packet.header.sequence.value, packet.payload);

		if (packet.header.marker === FrameMarker.Final) {
			return this.finalize(packetFrameNumber, matchingPartial, packet.header.sequence.value);
		}

		return null;
	}

	private finalize(
		partialFrameNumber: number,
		partial: PartialFrame,
		endSequence: number,
	): Throws<DataTrackFrameInternal, DataTrackDepacketizerDropError<DataTrackDepacketizerDropReason.Incomplete>> {
		const received = partial.payloads.size;

		let payloadLengthBytes = 0;
		for (const p of partial.payloads.values()) {
			payloadLengthBytes += p.length;
		}
		const payload = new Uint8Array(payloadLengthBytes);

		const sequencePointer = partial.startSequence.clone();
		let payloadOffsetPointerBytes = 0;
		while (true) {
			const partialPayload = partial.payloads.get(sequencePointer.value);
			if (!partialPayload) {
				break;
			}
			partial.payloads.delete(sequencePointer.value);

			const payloadRemainingBytes = payload.length - payloadOffsetPointerBytes;
			if (partialPayload.length > payloadRemainingBytes) {
				throw new Error(
					`Depacketizer.finalize: Expected at least ${partialPayload.length} more bytes left in the payload buffer, only got ${payloadRemainingBytes} bytes.`,
				);
			}

			payload.set(partialPayload, payloadOffsetPointerBytes);
			payloadOffsetPointerBytes += partialPayload.length;

			if (sequencePointer.value !== endSequence) {
				sequencePointer.increment();
				continue;
			}

			this.partials.delete(partialFrameNumber);
			return {payload, extensions: partial.extensions};
		}

		this.partials.delete(partialFrameNumber);
		throw DataTrackDepacketizerDropError.incomplete(
			partialFrameNumber,
			received,
			endSequence - partial.startSequence.value + 1,
		);
	}
}
