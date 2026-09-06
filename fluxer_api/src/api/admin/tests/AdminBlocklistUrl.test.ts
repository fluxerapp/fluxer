// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';

interface ValidationErrorResponse {
	code: string;
	message: string;
	errors?: Array<{
		path: string;
		code: string;
		message: string;
	}>;
}

describe('Admin url blocklist canonicalization', () => {
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

	async function createBlocklistAdminToken(): Promise<string> {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'ban:url:add', 'ban:url:remove']);
		return `${admin.token}`;
	}

	it('rejects an uncanonicalizable url on add with INVALID_FORM_BODY', async () => {
		const token = await createBlocklistAdminToken();
		const json = await createBuilder<ValidationErrorResponse>(harness, token)
			.post('/admin/blocklists/url/entries')
			.body({url: 'http://'})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		expect(json.errors?.[0].path).toBe('url');
		expect(json.errors?.[0].code).toBe('INVALID_URL_FORMAT');
	});

	it('rejects an uncanonicalizable url on update with INVALID_FORM_BODY', async () => {
		const token = await createBlocklistAdminToken();
		const json = await createBuilder<ValidationErrorResponse>(harness, token)
			.patch('/admin/blocklists/url/entries/not-a-url')
			.body({})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		expect(json.errors?.[0].path).toBe('url');
		expect(json.errors?.[0].code).toBe('INVALID_URL_FORMAT');
	});

	it('rejects an uncanonicalizable url on remove with INVALID_FORM_BODY', async () => {
		const token = await createBlocklistAdminToken();
		const json = await createBuilder<ValidationErrorResponse>(harness, token)
			.delete('/admin/blocklists/url/entries/not-a-url')
			.body(null)
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		expect(json.errors?.[0].path).toBe('url');
		expect(json.errors?.[0].code).toBe('INVALID_URL_FORMAT');
	});

	it('still adds a url that canonicalizes', async () => {
		const token = await createBlocklistAdminToken();
		await createBuilder(harness, token)
			.post('/admin/blocklists/url/entries')
			.body({url: 'https://Example.com/?utm_source=x'})
			.expect(204)
			.execute();
	});
});
