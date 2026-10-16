// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	FUZZ_REDUCER_ARBITRARY_ITERATIONS,
	FUZZ_REDUCER_EXTERNAL_CONNECTION_ITERATIONS,
	ReducerFuzzer,
} from '@fluxer/voice_engine_v2/src/fuzz/ReducerFuzzer';
import {describe, expect, it} from 'vitest';

const SEEDS: ReadonlyArray<number> = [1, 2, 7, 13, 99];

describe('ReducerFuzzer.fuzzExternalConnectionCycles', () => {
	it('survives weighted establish/disconnect cycles without invariant failures', () => {
		for (const seed of SEEDS) {
			const fuzzer = new ReducerFuzzer(seed);
			const report = fuzzer.fuzzExternalConnectionCycles(FUZZ_REDUCER_EXTERNAL_CONNECTION_ITERATIONS);
			expect(report.mode).toBe('externalConnection');
			expect(report.iterations).toBe(FUZZ_REDUCER_EXTERNAL_CONNECTION_ITERATIONS);
			expect(report.failures).toEqual([]);
			expect(report.dispatched + report.rejected).toBe(FUZZ_REDUCER_EXTERNAL_CONNECTION_ITERATIONS);
			expect(report.dispatched).toBeGreaterThan(0);
		}
	});
});

describe('ReducerFuzzer arbitrary-order generation with external connection events', () => {
	it('keeps the arbitrary-order mode crash-free now that external events are in the pool', () => {
		for (const seed of SEEDS) {
			const fuzzer = new ReducerFuzzer(seed);
			const report = fuzzer.fuzzArbitraryOrder(FUZZ_REDUCER_ARBITRARY_ITERATIONS);
			expect(report.failures).toEqual([]);
			expect(report.dispatched + report.rejected).toBe(FUZZ_REDUCER_ARBITRARY_ITERATIONS);
		}
	});
});
