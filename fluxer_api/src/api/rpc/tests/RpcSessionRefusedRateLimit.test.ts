// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const ATTEMPTS = 25;

describe('RpcService session refusals past the session rate limit', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	async function sessionStatuses(token: string): Promise<Array<number>> {
		const statuses: Array<number> = [];
		for (let attempt = 0; attempt < ATTEMPTS; attempt++) {
			const {response} = await createBuilder(harness, '')
				.post('/test/rpc-session-init')
				.body({type: 'session', token, version: 1, ip: '127.0.0.1'})
				.executeRaw();
			statuses.push(response.status);
		}
		return statuses;
	}

	test('keeps answering a refused bot token with 401 once the limiter denies it', async () => {
		const bot = await createTestBotAccount(harness);
		const statuses = await sessionStatuses(`Bot ${bot.appId}.deadSecretThatIsRefused`);
		expect(statuses).toEqual(Array(ATTEMPTS).fill(401));
	});

	test('still answers a valid bot token with 429 once the limiter denies it', async () => {
		const bot = await createTestBotAccount(harness);
		const statuses = await sessionStatuses(`Bot ${bot.botToken}`);
		expect(statuses.slice(0, 20)).toEqual(Array(20).fill(200));
		expect(statuses.slice(20)).toEqual(Array(ATTEMPTS - 20).fill(429));
	});
});
