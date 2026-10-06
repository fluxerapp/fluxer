// SPDX-License-Identifier: AGPL-3.0-or-later

import {FuzzPrng} from '@fluxer/voice_engine_v2/src/fuzz/FuzzPrng';
import {
	FUZZ_REDUCER_ARBITRARY_ITERATIONS,
	FUZZ_REDUCER_NEGATIVE_ITERATIONS,
	FUZZ_REDUCER_POSITIVE_ITERATIONS,
	ReducerFuzzer,
} from '@fluxer/voice_engine_v2/src/fuzz/ReducerFuzzer';
import {describe, expect, it} from 'vitest';

const SEEDS: ReadonlyArray<number> = [1, 2, 7, 13];

describe('ReducerFuzzer.fuzzPositive', () => {
	it('dispatches every generated positive event without throwing', () => {
		for (const seed of SEEDS) {
			const fuzzer = new ReducerFuzzer(seed);
			const report = fuzzer.fuzzPositive(FUZZ_REDUCER_POSITIVE_ITERATIONS);
			expect(report.mode).toBe('positive');
			expect(report.iterations).toBe(FUZZ_REDUCER_POSITIVE_ITERATIONS);
			expect(report.dispatched).toBe(FUZZ_REDUCER_POSITIVE_ITERATIONS);
			expect(report.failures).toEqual([]);
		}
	});

	it('produces identical traces for the same seed (determinism)', () => {
		const first = new ReducerFuzzer(99).fuzzPositive(64);
		const second = new ReducerFuzzer(99).fuzzPositive(64);
		expect(first.dispatched).toBe(second.dispatched);
		expect(first.rejected).toBe(second.rejected);
		expect(first.failures.length).toBe(second.failures.length);
	});
});

describe('ReducerFuzzer.fuzzNegative', () => {
	it('handles almost-valid events without crashing', () => {
		for (const seed of SEEDS) {
			const fuzzer = new ReducerFuzzer(seed);
			const report = fuzzer.fuzzNegative(FUZZ_REDUCER_NEGATIVE_ITERATIONS);
			expect(report.mode).toBe('negative');
			expect(report.iterations).toBe(FUZZ_REDUCER_NEGATIVE_ITERATIONS);
			expect(report.failures).toEqual([]);
			expect(report.dispatched + report.rejected).toBe(FUZZ_REDUCER_NEGATIVE_ITERATIONS);
		}
	});
});

describe('ReducerFuzzer.fuzzQualitative', () => {
	it('runs the idealised scenario with no failures', async () => {
		for (const seed of SEEDS) {
			const fuzzer = new ReducerFuzzer(seed);
			const report = await fuzzer.fuzzQualitative();
			expect(report.mode).toBe('qualitative');
			expect(report.iterations).toBeGreaterThan(0);
			expect(report.failures).toEqual([]);
			expect(report.dispatched).toBe(report.iterations);
		}
	});
});

describe('ReducerFuzzer.fuzzArbitraryOrder', () => {
	it('invokes events in arbitrary orders without crashing the runtime', () => {
		for (const seed of SEEDS) {
			const fuzzer = new ReducerFuzzer(seed);
			const report = fuzzer.fuzzArbitraryOrder(FUZZ_REDUCER_ARBITRARY_ITERATIONS);
			expect(report.mode).toBe('arbitraryOrder');
			expect(report.iterations).toBe(FUZZ_REDUCER_ARBITRARY_ITERATIONS);
			expect(report.failures).toEqual([]);
			expect(report.dispatched + report.rejected).toBe(FUZZ_REDUCER_ARBITRARY_ITERATIONS);
		}
	});
});

describe('FuzzPrng determinism', () => {
	it('produces the same byte sequence for the same seed', () => {
		const first = new FuzzPrng({seed: 42, budgetBytes: 64});
		const second = new FuzzPrng({seed: 42, budgetBytes: 64});
		const firstBytes: Array<number> = [];
		const secondBytes: Array<number> = [];
		for (let i = 0; i < 64; i += 1) firstBytes.push(first.nextByte());
		for (let i = 0; i < 64; i += 1) secondBytes.push(second.nextByte());
		expect(firstBytes).toEqual(secondBytes);
	});

	it('produces different byte sequences for different seeds', () => {
		const first = new FuzzPrng({seed: 1, budgetBytes: 64});
		const second = new FuzzPrng({seed: 2, budgetBytes: 64});
		const firstBytes: Array<number> = [];
		const secondBytes: Array<number> = [];
		for (let i = 0; i < 64; i += 1) firstBytes.push(first.nextByte());
		for (let i = 0; i < 64; i += 1) secondBytes.push(second.nextByte());
		expect(firstBytes).not.toEqual(secondBytes);
	});

	it('wraps the cursor when the byte budget is exhausted', () => {
		const prng = new FuzzPrng({seed: 17, budgetBytes: 16});
		const head = prng.nextByte();
		for (let i = 0; i < 15; i += 1) prng.nextByte();
		expect(prng.bytesConsumed).toBe(0);
		const wrapped = prng.nextByte();
		expect(head).toBe(wrapped);
		expect(prng.bytesConsumed).toBe(1);
	});
});
