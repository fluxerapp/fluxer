// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createHistoricalOutcomeAdapter,
	type HistoricalOutcomeAdapterContext,
} from '@app/api/risk/adapters/HistoricalOutcomeAdapter';
import type {IRiskHistoryRepository} from '@app/api/risk/HistoricalOutcomeRepository';
import type {HistoricalOutcomeRecord} from '@app/api/risk/RiskHistoryTypes';
import {describe, expect, it} from 'vitest';

function makeRepository(records: ReadonlyArray<HistoricalOutcomeRecord>): IRiskHistoryRepository {
	return {
		upsertLatestContext: async () => undefined,
		recordOutcomeForUser: async () => undefined,
		listByIp: async (ip, sinceTime) =>
			records.filter((record) => record.createdAt >= sinceTime && ip === '203.0.113.10'),
		listBySubnet: async () => [],
		listByEmailDomain: async () => [],
		listByAsn: async () => [],
	};
}

describe('HistoricalOutcomeAdapter', () => {
	it('deduplicates outcomes per user and aggregates challenge/enforcement counts', async () => {
		const now = new Date('2026-04-12T20:00:00Z');
		const records: ReadonlyArray<HistoricalOutcomeRecord> = [
			{
				userId: '1',
				createdAt: new Date('2026-04-12T19:30:00Z'),
				outcomeCode: 'challenged',
				source: 'registration_risk',
			},
			{
				userId: '1',
				createdAt: new Date('2026-04-12T19:35:00Z'),
				outcomeCode: 'disabled_suspicious',
				source: 'admin_disable_suspicious_activity',
			},
			{
				userId: '2',
				createdAt: new Date('2026-04-12T19:40:00Z'),
				outcomeCode: 'spammer',
				source: 'admin_spammer',
			},
		];
		const adapter = createHistoricalOutcomeAdapter({
			repository: makeRepository(records),
			now: () => now,
		} satisfies HistoricalOutcomeAdapterContext);
		const result = await adapter.getHistoricalOutcomesByIp({
			ip: '203.0.113.10',
			windowHours: 24,
		});
		expect(result.sampledRegistrations).toBe(3);
		expect(result.sampledUsers).toBe(2);
		expect(result.resolvedUsers).toBe(2);
		expect(result.challengedUsers).toBe(1);
		expect(result.enforcedUsers).toBe(2);
		expect(result.disabledSuspiciousUsers).toBe(1);
		expect(result.spammerUsers).toBe(1);
	});
});
