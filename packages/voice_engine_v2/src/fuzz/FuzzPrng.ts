// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';

const FUZZ_PRNG_BYTES_MAX = 1 << 20;

const FUZZ_PRNG_BYTES_MIN = 16;

const FUZZ_PRNG_SEED_MULTIPLIER_HI = 0x9e3779b9;

const FUZZ_PRNG_SEED_MULTIPLIER_LO = 0x85ebca6b;

const FUZZ_PRNG_SEED_XOR_MASK = 0xc2b2ae35;

const FUZZ_PRNG_BYTE_MASK = 0xff;

const FUZZ_PRNG_U32_BYTES = 4;

const FUZZ_PRNG_U32_MAX = 0xffffffff;

interface FuzzPrngOptions {
	seed: number;
	budgetBytes?: number;
}

export class FuzzPrng {
	private readonly seedValue: number;
	private readonly bytes: Uint8Array;
	private readonly capacity: number;
	private cursor: number;

	constructor(options: FuzzPrngOptions) {
		assert.equal(typeof options.seed, 'number', 'seed must be a number');
		assert.ok(Number.isInteger(options.seed), 'seed must be an integer');
		assert.ok(options.seed >= 0, 'seed must be non-negative');
		assert.ok(options.seed <= FUZZ_PRNG_U32_MAX, 'seed must fit in a u32');
		const budget = options.budgetBytes ?? FUZZ_PRNG_BYTES_MAX;
		assert.ok(Number.isInteger(budget), 'budgetBytes must be an integer');
		assert.ok(budget >= FUZZ_PRNG_BYTES_MIN, 'budgetBytes must be >= FUZZ_PRNG_BYTES_MIN');
		assert.ok(budget <= FUZZ_PRNG_BYTES_MAX, 'budgetBytes must be <= FUZZ_PRNG_BYTES_MAX');
		this.seedValue = options.seed;
		this.capacity = budget;
		this.bytes = fillSeededBytes(options.seed, budget);
		this.cursor = 0;
		assert.equal(this.bytes.length, budget, 'seeded byte buffer must match capacity');
	}

	get seed(): number {
		return this.seedValue;
	}

	get bytesConsumed(): number {
		assert.ok(this.cursor >= 0, 'cursor must be non-negative');
		return this.cursor;
	}

	nextByte(): number {
		assert.ok(this.capacity > 0, 'PRNG must have positive capacity');
		const value = this.bytes[this.cursor % this.capacity];
		assert.equal(typeof value, 'number', 'byte slot must contain a number');
		assert.ok(value >= 0, 'byte value must be non-negative');
		assert.ok(value <= FUZZ_PRNG_BYTE_MASK, 'byte value must fit in a byte');
		this.cursor = (this.cursor + 1) % this.capacity;
		return value;
	}

	nextU32(): number {
		assert.ok(this.capacity >= FUZZ_PRNG_U32_BYTES, 'PRNG must hold at least a u32 worth of bytes');
		let accumulator = 0;
		for (let index = 0; index < FUZZ_PRNG_U32_BYTES; index += 1) {
			const byte = this.nextByte();
			accumulator = ((accumulator << 8) | byte) >>> 0;
		}
		assert.ok(accumulator >= 0, 'u32 must be non-negative');
		assert.ok(accumulator <= FUZZ_PRNG_U32_MAX, 'u32 must fit in a u32');
		return accumulator;
	}

	nextRange(maxExclusive: number): number {
		assert.equal(typeof maxExclusive, 'number', 'maxExclusive must be a number');
		assert.ok(Number.isInteger(maxExclusive), 'maxExclusive must be an integer');
		assert.ok(maxExclusive >= 1, 'maxExclusive must be >= 1');
		assert.ok(maxExclusive <= FUZZ_PRNG_U32_MAX, 'maxExclusive must fit in a u32');
		const value = this.nextU32() % maxExclusive;
		assert.ok(value >= 0, 'range value must be non-negative');
		assert.ok(value < maxExclusive, 'range value must be below maxExclusive');
		return value;
	}

	nextChoice<T>(items: ReadonlyArray<T>): T {
		assert.ok(Array.isArray(items), 'items must be an array');
		assert.ok(items.length >= 1, 'items must contain at least one element');
		const index = this.nextRange(items.length);
		const value = items[index];
		assert.ok(value !== undefined, 'choice slot must not be undefined');
		return value;
	}

	nextBool(probability: number): boolean {
		assert.equal(typeof probability, 'number', 'probability must be a number');
		assert.ok(Number.isFinite(probability), 'probability must be finite');
		assert.ok(probability >= 0, 'probability must be >= 0');
		assert.ok(probability <= 1, 'probability must be <= 1');
		if (probability === 0) return false;
		if (probability === 1) return true;
		const u32 = this.nextU32();
		const threshold = Math.floor(probability * (FUZZ_PRNG_U32_MAX + 1));
		return u32 < threshold;
	}

	reset(): void {
		this.cursor = 0;
		assert.equal(this.cursor, 0, 'reset must zero the cursor');
	}
}

function fillSeededBytes(seed: number, length: number): Uint8Array {
	assert.equal(typeof seed, 'number', 'seed must be a number');
	assert.ok(Number.isInteger(length), 'length must be an integer');
	assert.ok(length >= FUZZ_PRNG_BYTES_MIN, 'length must be at least the minimum');
	assert.ok(length <= FUZZ_PRNG_BYTES_MAX, 'length must be at most the maximum');
	const out = new Uint8Array(length);
	let stateHi = (seed ^ FUZZ_PRNG_SEED_XOR_MASK) >>> 0;
	let stateLo = ((seed + FUZZ_PRNG_SEED_MULTIPLIER_HI) ^ FUZZ_PRNG_SEED_MULTIPLIER_LO) >>> 0;
	for (let index = 0; index < length; index += 1) {
		stateHi = Math.imul(stateHi ^ (stateHi >>> 16), FUZZ_PRNG_SEED_MULTIPLIER_LO) >>> 0;
		stateLo = Math.imul(stateLo ^ (stateLo >>> 13), FUZZ_PRNG_SEED_MULTIPLIER_HI) >>> 0;
		const mixed = (stateHi ^ stateLo) >>> 0;
		out[index] = mixed & FUZZ_PRNG_BYTE_MASK;
	}
	assert.equal(out.length, length, 'seeded byte buffer must match length');
	return out;
}
