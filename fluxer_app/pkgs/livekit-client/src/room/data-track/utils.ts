// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
export const U16_MAX_SIZE = 0xffff;
export const U32_MAX_SIZE = 0xffffffff;

export class WrapAroundUnsignedInt<MaxSize extends number> {
	value: number;

	private maxSize: MaxSize;

	static u16(raw: number) {
		return new WrapAroundUnsignedInt(raw, U16_MAX_SIZE);
	}

	static u32(raw: number) {
		return new WrapAroundUnsignedInt(raw, U32_MAX_SIZE);
	}

	constructor(raw: number, maxSize: MaxSize) {
		this.value = raw;
		if (raw < 0) {
			throw new Error('WrapAroundUnsignedInt: cannot faithfully represent an integer smaller than 0');
		}
		if (maxSize > Number.MAX_SAFE_INTEGER) {
			throw new Error('WrapAroundUnsignedInt: cannot faithfully represent an integer bigger than MAX_SAFE_INTEGER.');
		}
		this.maxSize = maxSize;
		this.clamp();
	}

	clamp() {
		while (this.value > this.maxSize) {
			this.value -= this.maxSize + 1;
		}
		while (this.value < 0) {
			this.value += this.maxSize + 1;
		}
	}

	clone() {
		return new WrapAroundUnsignedInt(this.value, this.maxSize);
	}

	update(updateFn: (value: number) => number) {
		this.value = updateFn(this.value);
		this.clamp();
	}

	increment(n = 1) {
		this.update((value) => value + n);
	}

	decrement(n = 1) {
		this.update((value) => value - n);
	}

	getThenIncrement() {
		const previousValue = this.value;
		this.increment();
		return new WrapAroundUnsignedInt(previousValue, this.maxSize);
	}

	isBefore(other: WrapAroundUnsignedInt<MaxSize>) {
		const a = this.value >>> 0;
		const b = other.value >>> 0;
		const diff = (b - a) >>> 0;
		return diff !== 0 && diff < this.maxSize + 1;
	}
}

export class DataTrackTimestamp<RateInHz extends number> {
	rateInHz: RateInHz;

	timestamp: WrapAroundUnsignedInt<typeof U32_MAX_SIZE>;

	static fromRtpTicks(rtpTicks: number) {
		return new DataTrackTimestamp(rtpTicks, 90_000);
	}

	static rtpRandom() {
		const randomValue = Math.round(Math.random() * U32_MAX_SIZE);
		return DataTrackTimestamp.fromRtpTicks(randomValue);
	}

	private constructor(raw: number, rateInHz: RateInHz) {
		this.timestamp = WrapAroundUnsignedInt.u32(raw);
		this.rateInHz = rateInHz;
	}

	asTicks() {
		return this.timestamp.value;
	}

	clone() {
		return new DataTrackTimestamp(this.timestamp.value, this.rateInHz);
	}

	wrappingAdd(n: number) {
		this.timestamp.increment(n);
	}

	isBefore(other: DataTrackTimestamp<RateInHz>) {
		return this.timestamp.isBefore(other.timestamp);
	}
}

export class DataTrackClock<RateInHz extends number> {
	epoch: Date;

	base: DataTrackTimestamp<RateInHz>;

	previous: DataTrackTimestamp<RateInHz>;

	rateInHz: RateInHz;

	private constructor(rateInHz: RateInHz, epoch: Date, base: DataTrackTimestamp<RateInHz>) {
		this.epoch = epoch;
		this.base = base;
		this.previous = base.clone();
		this.rateInHz = rateInHz;
	}

	static startingNow<RateInHz extends number>(base: DataTrackTimestamp<RateInHz>, rateInHz: RateInHz) {
		return new DataTrackClock(rateInHz, new Date(), base);
	}

	static startingAtTime<RateInHz extends number>(epoch: Date, base: DataTrackTimestamp<RateInHz>, rateInHz: RateInHz) {
		return new DataTrackClock(rateInHz, epoch, base);
	}

	static rtpStartingNow(base: DataTrackTimestamp<90_000>) {
		return DataTrackClock.startingNow(base, 90_000);
	}

	static rtpStartingAtTime(epoch: Date, base: DataTrackTimestamp<90_000>) {
		return DataTrackClock.startingAtTime(epoch, base, 90_000);
	}

	now(): DataTrackTimestamp<RateInHz> {
		return this.at(new Date());
	}

	at(timestamp: Date) {
		const elapsedMs = timestamp.getTime() - this.epoch.getTime();
		const durationTicks = DataTrackClock.durationInMsToTicks(elapsedMs, this.rateInHz);

		let result = this.base.clone();
		result.wrappingAdd(durationTicks);

		if (result.isBefore(this.previous)) {
			result = this.previous;
		}
		this.previous = result.clone();
		return result.clone();
	}

	static durationInMsToTicks(durationMilliseconds: number, rateInHz: number) {
		const durationNanoseconds = durationMilliseconds * 1e6;
		const ticks = (durationNanoseconds * rateInHz + 500_000_000) / 1_000_000_000;
		return Math.round(ticks);
	}
}

export function coerceToDataView<Input extends DataView | ArrayBuffer | Uint8Array>(input: Input): DataView {
	if (input instanceof DataView) {
		return input;
	} else if (input instanceof ArrayBuffer) {
		return new DataView(input);
	} else if (input instanceof Uint8Array) {
		return new DataView(input.buffer, input.byteOffset, input.byteLength);
	} else {
		throw new Error(`Error coercing ${input} to DataView - input was not DataView, ArrayBuffer, or Uint8Array.`);
	}
}
