// SPDX-License-Identifier: AGPL-3.0-or-later

import {appStoreNotificationClaimKey, StoreNotificationClaims} from '@app/api/store_billing/StoreNotificationClaims';
import {WORKER_LANES} from '@app/api/worker/WorkerLaneConfig';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms} from 'itty-time';
import {describe, expect, it} from 'vitest';

const LIFECYCLE_REDELIVERY_DELAY_MS = ms('5 seconds');

describe('StoreNotificationClaims', () => {
	it('lets an abandoned claim expire before the lifecycle lane stops redelivering', async () => {
		const ttls: Array<number> = [];
		const kv = {
			async setnx(_key: string, _value: string, ttlSeconds?: number): Promise<boolean> {
				ttls.push(ttlSeconds ?? 0);
				return true;
			},
		} as unknown as IKVProvider;
		const claims = new StoreNotificationClaims(kv);
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
