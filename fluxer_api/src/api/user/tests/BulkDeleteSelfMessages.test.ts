// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const DEFAULT_BODY = {
	scope: 'selected' as const,
	include_dms: true,
	include_dms_closed: true,
	include_group_dms: true,
	include_guilds: true,
	guild_filter_mode: 'exclude' as const,
	excluded_guild_ids: [] as Array<string>,
	included_guild_ids: [] as Array<string>,
	start_date: null,
	end_date: null,
};

describe('Bulk delete my messages (filtered)', () => {
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
	it('requires sudo verification', async () => {
		const owner = await createTestAccount(harness);
		const response = await harness.requestJson({
			path: '/users/@me/messages/bulk-delete-mine',
			method: 'POST',
			headers: {authorization: owner.token},
			body: {...DEFAULT_BODY},
		});
		expect([400, 401, 403]).toContain(response.status);
	});
});
