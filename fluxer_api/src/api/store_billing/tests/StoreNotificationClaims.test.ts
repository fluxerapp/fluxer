// SPDX-License-Identifier: AGPL-3.0-or-later

import {KvOnceClaims} from '@app/api/infrastructure/KvOnceClaims';
import {appStoreNotificationClaimKey} from '@app/api/store_billing/StoreNotificationClaims';
import {WORKER_LANES} from '@app/api/worker/WorkerLaneConfig';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms} from 'itty-time';
import {describe, expect, it, vi} from 'vitest';

const LIFECYCLE_REDELIVERY_DELAY_MS = ms('5 seconds');

describe('StoreNotificationClaims', () => {
	it.each([
		{options: {}, id: 'app-store-notification:notification', key: 'app-store-notification:notification', ttl: 90},
		{
			options: {keyPrefix: 'stripe-webhook:', inFlightTtlSeconds: 600},
			id: 'evt_1',
			key: 'stripe-webhook:evt_1',
			ttl: 600,
		},
	])('preserves durable claim keys and expiry times for $key', async ({options, id, key, ttl}) => {
		let value: string | null = null;
		const setnx = vi.fn(async (_key: string, next: string, _ttl?: number) => {
			if (value !== null) return false;
			value = next;
			return true;
		});
		const get = vi.fn(async (_key: string) => value);
		const setex = vi.fn(async (_key: string, _ttl: number, next: string) => {
			value = next;
		});
		const del = vi.fn(async (_key: string) => {
			value = null;
			return 1;
		});
		const claims = new KvOnceClaims({setnx, get, setex, del} as unknown as IKVProvider, options);
		expect(await claims.tryClaim(id)).toBe('claimed');
		expect(setnx).toHaveBeenCalledWith(key, 'in_flight', ttl);
		expect(await claims.tryClaim(id)).toBe('in_flight');
		expect(get).toHaveBeenCalledWith(key);
		await claims.markProcessed(id);
		expect(setex).toHaveBeenCalledWith(key, 604800, 'processed');
		expect(await claims.tryClaim(id)).toBe('already_processed');
		await claims.releaseClaim(id);
		expect(del).toHaveBeenCalledWith(key);
		expect(await claims.tryClaim(id)).toBe('claimed');
	});

	it('lets an abandoned claim expire before the lifecycle lane stops redelivering', async () => {
		const ttls: Array<number> = [];
		const kv = {
			async setnx(_key: string, _value: string, ttlSeconds?: number): Promise<boolean> {
				ttls.push(ttlSeconds ?? 0);
				return true;
			},
		} as unknown as IKVProvider;
		const claims = new KvOnceClaims(kv);
		expect(await claims.tryClaim(appStoreNotificationClaimKey('7b4d9c1e-0000-4000-8000-000000000001'))).toBe('claimed');
		const lane = WORKER_LANES.find((entry) => entry.name === 'lifecycle');
		expect(lane?.taskTypes).toContain('processAppStoreNotification');
		expect(lane?.taskTypes).toContain('processGooglePlayNotification');
		const retryWindowMs = lane!.ackWaitMs + (lane!.maxDeliver - 1) * LIFECYCLE_REDELIVERY_DELAY_MS;
		expect(ttls).toHaveLength(1);
		expect(ttls[0] * 1000).toBeGreaterThanOrEqual(lane!.ackWaitMs);
		expect(ttls[0] * 1000).toBeLessThanOrEqual(retryWindowMs / 2);
	});
});
