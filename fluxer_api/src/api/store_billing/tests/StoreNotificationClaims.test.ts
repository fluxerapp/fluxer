// SPDX-License-Identifier: AGPL-3.0-or-later

import {KvOnceClaims} from '@app/api/infrastructure/KvOnceClaims';
import {appStoreNotificationClaimKey} from '@app/api/store_billing/StoreNotificationClaims';
import {WORKER_LANES} from '@app/api/worker/WorkerLaneConfig';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {ms} from 'itty-time';
import {describe, expect, it} from 'vitest';

const LIFECYCLE_REDELIVERY_DELAY_MS = ms('5 seconds');

describe('StoreNotificationClaims', () => {
	it('lets an abandoned claim expire before the lifecycle lane stops redelivering', async () => {
		const kv = new MockKVProvider();
		const key = appStoreNotificationClaimKey('7b4d9c1e-0000-4000-8000-000000000001');
		expect(await new KvOnceClaims(kv).tryClaim(key)).toBe('claimed');
		const lane = WORKER_LANES.find((entry) => entry.name === 'lifecycle');
		expect(lane?.taskTypes).toContain('processAppStoreNotification');
		expect(lane?.taskTypes).toContain('processGooglePlayNotification');
		const retryWindowMs = lane!.ackWaitMs + (lane!.maxDeliver - 1) * LIFECYCLE_REDELIVERY_DELAY_MS;
		const ttlMs = (await kv.ttl(key)) * 1000;
		expect(ttlMs).toBeGreaterThanOrEqual(lane!.ackWaitMs);
		expect(ttlMs).toBeLessThanOrEqual(retryWindowMs / 2);
	});
});
