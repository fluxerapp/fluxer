// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';

const {MODULE_POLL_INTERVAL_MS, nextModulePollDelay, resolveModulePollPlan} = await import('./ModulePollPlan.ts');

describe('how often a client re-checks the module manifest', () => {
	test('a dev override polls fast and exactly, with no jitter', () => {
		const plan = resolveModulePollPlan(2000);
		assert.deepEqual(plan, {intervalMs: 2000, jitterRatio: 0});
		assert.equal(
			nextModulePollDelay(plan, () => 0),
			2000,
		);
		assert.equal(
			nextModulePollDelay(plan, () => 1),
			2000,
		);
	});

	test('a shipped build polls every ten minutes, not every two seconds', () => {
		assert.equal(resolveModulePollPlan(null).intervalMs, MODULE_POLL_INTERVAL_MS);
		assert.equal(MODULE_POLL_INTERVAL_MS, 10 * 60 * 1000);
	});

	test('a shipped build spreads the poll so clients do not stampede the manifest', () => {
		const plan = resolveModulePollPlan(null);
		assert.ok(
			plan.jitterRatio > 0,
			'a fixed interval aligns every client that booted together onto the same tick, so every release turns into a thundering herd against the manifest endpoint',
		);
		const low = nextModulePollDelay(plan, () => 0);
		const mid = nextModulePollDelay(plan, () => 0.5);
		const high = nextModulePollDelay(plan, () => 1);
		assert.equal(mid, plan.intervalMs);
		assert.equal(low, plan.intervalMs * (1 - plan.jitterRatio));
		assert.equal(high, plan.intervalMs * (1 + plan.jitterRatio));
		assert.ok(low < mid && mid < high);
	});

	test('a delay is never zero or negative whatever the jitter draws', () => {
		for (const draw of [0, 0.25, 0.5, 0.75, 1]) {
			assert.ok(nextModulePollDelay({intervalMs: 1, jitterRatio: 5}, () => draw) >= 1);
		}
	});
});
