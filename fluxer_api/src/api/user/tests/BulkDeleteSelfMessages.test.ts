// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
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

function postBulkDelete(harness: ApiTestHarness, token: string, body: Record<string, unknown>) {
	return createBuilder(harness, token).post('/users/@me/messages/bulk-delete-mine').body(body);
}

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
	it('rejects when no inclusion toggles are set in selected mode', async () => {
		const owner = await createTestAccount(harness);
		await postBulkDelete(harness, owner.token, {
			password: owner.password,
			scope: 'selected',
			include_dms: false,
			include_dms_closed: false,
			include_group_dms: false,
			include_guilds: false,
		})
			.expect(400)
			.execute();
	});
	it('rejects when start_date is on or after end_date', async () => {
		const owner = await createTestAccount(harness);
		await postBulkDelete(harness, owner.token, {
			...DEFAULT_BODY,
			password: owner.password,
			start_date: '2030-01-02T00:00:00.000Z',
			end_date: '2030-01-01T00:00:00.000Z',
		})
			.expect(400)
			.execute();
	});
	it('accepts a valid request with sudo and the default everything filter', async () => {
		const owner = await createTestAccount(harness);
		await postBulkDelete(harness, owner.token, {
			...DEFAULT_BODY,
			password: owner.password,
		})
			.expect(202)
			.execute();
	});
	it('accepts a valid inaccessible_only request', async () => {
		const owner = await createTestAccount(harness);
		await postBulkDelete(harness, owner.token, {
			...DEFAULT_BODY,
			password: owner.password,
			scope: 'inaccessible_only',
		})
			.expect(202)
			.execute();
	});
	it('accepts a valid custom date range', async () => {
		const owner = await createTestAccount(harness);
		await postBulkDelete(harness, owner.token, {
			...DEFAULT_BODY,
			password: owner.password,
			start_date: '2020-01-01T00:00:00.000Z',
			end_date: '2025-01-01T00:00:00.000Z',
		})
			.expect(202)
			.execute();
	});
});
