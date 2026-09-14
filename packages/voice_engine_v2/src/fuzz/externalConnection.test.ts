// SPDX-License-Identifier: AGPL-3.0-or-later

import {FuzzPrng} from '@fluxer/voice_engine_v2/src/fuzz/FuzzPrng';
import {
	EXTERNAL_CONNECTION_EVENT_KINDS,
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

	it('produces an identical snapshot hash and counters for the same seed (determinism)', () => {
		for (const seed of SEEDS) {
			const first = new ReducerFuzzer(seed).fuzzExternalConnectionCycles(256);
			const second = new ReducerFuzzer(seed).fuzzExternalConnectionCycles(256);
			expect(first.snapshotHash).toBeDefined();
			expect(first.snapshotHash).toBe(second.snapshotHash);
			expect(first.dispatched).toBe(second.dispatched);
			expect(first.rejected).toBe(second.rejected);
			expect(first.failures.length).toBe(second.failures.length);
		}
	});

	it('produces diverging traces for different seeds', () => {
		const first = new ReducerFuzzer(1).fuzzExternalConnectionCycles(256);
		const second = new ReducerFuzzer(2).fuzzExternalConnectionCycles(256);
		expect(first.snapshotHash === second.snapshotHash && first.dispatched === second.dispatched).toBe(false);
	});

	it('rejects an iteration budget above the global cap', () => {
		const fuzzer = new ReducerFuzzer(1);
		expect(() => fuzzer.fuzzExternalConnectionCycles(1025)).toThrow('iterations must respect FUZZ_ITERATIONS_MAX');
	});

	it('rejects a non-positive iteration budget', () => {
		const fuzzer = new ReducerFuzzer(1);
		expect(() => fuzzer.fuzzExternalConnectionCycles(0)).toThrow('iterations must be >= 1');
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

	it('remains deterministic across runs with the widened event pool', () => {
		const first = new ReducerFuzzer(42).fuzzArbitraryOrder(128);
		const second = new ReducerFuzzer(42).fuzzArbitraryOrder(128);
		expect(first.dispatched).toBe(second.dispatched);
		expect(first.rejected).toBe(second.rejected);
	});
});

describe('external connection event kind catalogue', () => {
	it('contains exactly the externally-driven connection lifecycle events', () => {
		expect(EXTERNAL_CONNECTION_EVENT_KINDS).toEqual([
			'connection.externallyEstablished',
			'connection.remoteDisconnected',
		]);
	});

	it('keeps the seeded PRNG as the only source of randomness for kind selection', () => {
		const prng = new FuzzPrng({seed: 7});
		const picks: Array<string> = [];
		for (let index = 0; index < 32; index += 1) {
			picks.push(prng.nextChoice(EXTERNAL_CONNECTION_EVENT_KINDS));
		}
		const replayPrng = new FuzzPrng({seed: 7});
		const replays: Array<string> = [];
		for (let index = 0; index < 32; index += 1) {
			replays.push(replayPrng.nextChoice(EXTERNAL_CONNECTION_EVENT_KINDS));
		}
		expect(picks).toEqual(replays);
		expect(new Set(picks).size).toBe(2);
	});
});
