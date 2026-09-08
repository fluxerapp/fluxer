// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import {MockKVProvider} from '../../test/mocks/MockKVProvider';
import {createKvIpInfoLookupBudget} from '../IpInfoBudget';

const BUDGET_ENV_KEYS = [
	'FLUXER_IPINFO_BUDGET_ENABLED',
	'FLUXER_IPINFO_BUDGET_MONTHLY_MAX',
	'FLUXER_IPINFO_BUDGET_BACKGROUND_MONTHLY_PCT',
	'FLUXER_IPINFO_BUDGET_STANDARD_MONTHLY_PCT',
	'FLUXER_IPINFO_BUDGET_CRITICAL_BURST',
	'FLUXER_IPINFO_BUDGET_STANDARD_BURST',
	'FLUXER_IPINFO_BUDGET_BACKGROUND_BURST',
	'FLUXER_IPINFO_BUDGET_CRITICAL_REFILL_PER_MIN',
	'FLUXER_IPINFO_BUDGET_STANDARD_REFILL_PER_MIN',
	'FLUXER_IPINFO_BUDGET_BACKGROUND_REFILL_PER_MIN',
];

function monthKey(): string {
	const now = new Date();
	const month = String(now.getUTCMonth() + 1).padStart(2, '0');
	return `ipinfo:budget:month:${now.getUTCFullYear()}-${month}`;
}

describe('IpInfoBudget', () => {
	const savedEnv = new Map<string, string | undefined>();

	beforeEach(() => {
		for (const key of BUDGET_ENV_KEYS) {
			savedEnv.set(key, process.env[key]);
			delete process.env[key];
		}
		process.env.FLUXER_IPINFO_BUDGET_MONTHLY_MAX = '100';
	});

	afterEach(() => {
		for (const [key, value] of savedEnv) {
			if (value === undefined) {
				delete process.env[key];
			} else {
				process.env[key] = value;
			}
		}
		savedEnv.clear();
	});

	it('sheds background lookups at the background monthly ceiling while standard still admits', async () => {
		const kv = new MockKVProvider();
		await kv.set(monthKey(), '60');
		const budget = createKvIpInfoLookupBudget({getKvClient: () => kv});

		expect(await budget.tryConsume('background')).toBe(false);
		expect(await budget.tryConsume('standard')).toBe(true);
	});

	it('sheds standard lookups at the standard monthly ceiling', async () => {
		const kv = new MockKVProvider();
		await kv.set(monthKey(), '90');
		const budget = createKvIpInfoLookupBudget({getKvClient: () => kv});

		expect(await budget.tryConsume('standard')).toBe(false);
	});

	it('admits critical lookups above the standard ceiling', async () => {
		const kv = new MockKVProvider();
		await kv.set(monthKey(), '95');
		const budget = createKvIpInfoLookupBudget({getKvClient: () => kv});

		expect(await budget.tryConsume('critical')).toBe(true);
	});

	it('never increments the monthly counter for a shed lookup', async () => {
		const kv = new MockKVProvider();
		await kv.set(monthKey(), '60');
		const budget = createKvIpInfoLookupBudget({getKvClient: () => kv});

		const outcomes = [
			await budget.tryConsume('background'),
			await budget.tryConsume('background'),
			await budget.tryConsume('standard'),
			await budget.tryConsume('critical'),
		];

		expect(outcomes).toEqual([false, false, true, true]);
		expect(kv.incrSpy).toHaveBeenCalledTimes(2);
		expect(await kv.get(monthKey())).toBe('62');
	});

	it('sheds once the per-priority burst bucket is drained', async () => {
		process.env.FLUXER_IPINFO_BUDGET_BACKGROUND_BURST = '3';
		process.env.FLUXER_IPINFO_BUDGET_BACKGROUND_REFILL_PER_MIN = '1';
		const kv = new MockKVProvider();
		const budget = createKvIpInfoLookupBudget({getKvClient: () => kv});

		expect(await budget.tryConsume('background')).toBe(true);
		expect(await budget.tryConsume('background')).toBe(true);
		expect(await budget.tryConsume('background')).toBe(true);
		expect(await budget.tryConsume('background')).toBe(false);
		expect(await budget.tryConsume('standard')).toBe(true);
	});

	it('expires the month key only on the first increment', async () => {
		const kv = new MockKVProvider();
		const budget = createKvIpInfoLookupBudget({getKvClient: () => kv});

		expect(await budget.tryConsume('standard')).toBe(true);
		expect(await budget.tryConsume('standard')).toBe(true);
		expect(await budget.tryConsume('standard')).toBe(true);

		expect(kv.expireSpy).toHaveBeenCalledTimes(1);
		expect(kv.expireSpy).toHaveBeenCalledWith(monthKey(), 40 * 24 * 3600);
	});

	it('fails open for every priority when the KV provider throws', async () => {
		const kv = new MockKVProvider();
		vi.spyOn(kv, 'get').mockRejectedValue(new Error('kv unavailable'));
		const budget = createKvIpInfoLookupBudget({getKvClient: () => kv});

		expect(await budget.tryConsume('background')).toBe(true);
		expect(await budget.tryConsume('standard')).toBe(true);
		expect(await budget.tryConsume('critical')).toBe(true);
	});

	it('admits everything when the budget is disabled', async () => {
		process.env.FLUXER_IPINFO_BUDGET_ENABLED = '0';
		const kv = new MockKVProvider();
		await kv.set(monthKey(), '1000');
		const budget = createKvIpInfoLookupBudget({getKvClient: () => kv});

		expect(await budget.tryConsume('background')).toBe(true);
		expect(kv.tryConsumeTokensSpy).not.toHaveBeenCalled();
	});
});
