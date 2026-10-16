// SPDX-License-Identifier: AGPL-3.0-or-later

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
