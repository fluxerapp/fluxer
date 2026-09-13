// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Theme bot user denied', () => {
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
	it('rejects theme creation from bot users', async () => {
		const botAccount = await createTestBotAccount(harness);
		await createBuilder(harness, `Bot ${botAccount.botToken}`)
			.post('/users/@me/themes')
			.body({css: '.test { color: red; }'})
			.expect(HTTP_STATUS.FORBIDDEN, 'ACCESS_DENIED')
			.execute();
	});
});
