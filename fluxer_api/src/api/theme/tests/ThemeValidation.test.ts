// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Theme validation', () => {
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
	it('rejects request with missing css field', async () => {
		const user = await createTestAccount(harness);
		await createBuilder(harness, user.token)
			.post('/users/@me/themes')
			.body({})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('rejects request with empty css string', async () => {
		const user = await createTestAccount(harness);
		await createBuilder(harness, user.token)
			.post('/users/@me/themes')
			.body({css: ''})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('rejects request with null css value', async () => {
		const user = await createTestAccount(harness);
		await createBuilder(harness, user.token)
			.post('/users/@me/themes')
			.body({css: null})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('rejects request with numeric css value', async () => {
		const user = await createTestAccount(harness);
		await createBuilder(harness, user.token)
			.post('/users/@me/themes')
			.body({css: 12345})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('rejects request with array css value', async () => {
		const user = await createTestAccount(harness);
		await createBuilder(harness, user.token)
			.post('/users/@me/themes')
			.body({css: ['body { color: red; }']})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('rejects request with object css value', async () => {
		const user = await createTestAccount(harness);
		await createBuilder(harness, user.token)
			.post('/users/@me/themes')
			.body({css: {content: 'body { color: red; }'}})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('rejects request with boolean css value', async () => {
		const user = await createTestAccount(harness);
		await createBuilder(harness, user.token)
			.post('/users/@me/themes')
			.body({css: true})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
});
