// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Invite Validation', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it('should reject getting nonexistent invite', async () => {
		await createBuilder(harness, '').get('/invites/nonexistent_code').expect(HTTP_STATUS.NOT_FOUND).execute();
	});
	it('should reject accepting nonexistent invite', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/invites/nonexistent_code')
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	it('should reject deleting nonexistent invite', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.delete('/invites/nonexistent_code')
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	it('should reject invalid max_uses value when creating invite', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Invite Validation Guild');
		await createBuilder(harness, account.token)
			.post(`/channels/${guild.system_channel_id}/invites`)
			.body({max_uses: -1})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('should reject invalid max_age value when creating invite', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Invite Validation Guild');
		await createBuilder(harness, account.token)
			.post(`/channels/${guild.system_channel_id}/invites`)
			.body({max_age: -1})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('should accept valid max_uses and max_age values', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Invite Validation Guild');
		await createBuilder(harness, account.token)
			.post(`/channels/${guild.system_channel_id}/invites`)
			.body({max_uses: 10, max_age: 3600})
			.expect(HTTP_STATUS.OK)
			.execute();
	});
});
