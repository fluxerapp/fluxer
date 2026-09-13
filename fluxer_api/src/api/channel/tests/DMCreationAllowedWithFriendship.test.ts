// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createDmChannel, createFriendship} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('DM creation allowed with friendship', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('allows friends to create DMs with each other', async () => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		await createFriendship(harness, user1, user2);
		const dm = await createDmChannel(harness, user1.token, user2.userId);
		expect(dm.id).toBeTruthy();
		expect(dm.id.length).toBeGreaterThan(0);
	});
});
