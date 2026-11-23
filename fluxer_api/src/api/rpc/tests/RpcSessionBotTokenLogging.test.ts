// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {Logger} from '@app/api/Logger';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, type MockInstance, test, vi} from 'vitest';

const VALIDATION_FAILED_MESSAGE = 'RPC session token validation failed';
const DEAD_SECRET = 'deadSecretThatMustNeverBeLogged';

describe('RpcService session bot token rejection logging', () => {
	let harness: ApiTestHarness;
	let warnSpy: MockInstance;
	beforeEach(async () => {
		harness = await createApiTestHarness();
		warnSpy = vi.spyOn(Logger.child({}), 'warn');
	});
	afterEach(async () => {
		warnSpy.mockRestore();
		await harness?.shutdown();
	});

	async function rejectSession(token: string): Promise<Record<string, unknown>> {
		await createBuilder(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token, version: 1, ip: '127.0.0.1'})
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
		const call = warnSpy.mock.calls.find(([, message]) => message === VALIDATION_FAILED_MESSAGE);
		expect(call).toBeDefined();
		return call![0] as Record<string, unknown>;
	}

	function expectNoSecret(fields: Record<string, unknown>, secret: string): void {
		const serialized = JSON.stringify(fields);
		expect(serialized).not.toContain(secret);
		expect(serialized).not.toContain(secret.slice(0, 8));
	}

	test('logs the application id and secret mismatch for a well-formed bot token with a dead secret', async () => {
		const bot = await createTestBotAccount(harness);
		const fields = await rejectSession(`Bot ${bot.appId}.${DEAD_SECRET}`);
		expect(fields['tokenType']).toBe('bot');
		expect(fields['applicationId']).toBe(bot.appId);
		expect(fields['rejectionReason']).toBe('secret_mismatch');
		expect(fields['tokenHashPrefix']).toMatch(/^[0-9a-f]{12}$/);
		expectNoSecret(fields, DEAD_SECRET);
	});

	test('logs unknown application for a well-formed bot token of a missing application', async () => {
		const fields = await rejectSession(`1100000000000000001.${DEAD_SECRET}`);
		expect(fields['applicationId']).toBe('1100000000000000001');
		expect(fields['rejectionReason']).toBe('unknown_application');
		expectNoSecret(fields, DEAD_SECRET);
	});

	test('omits the application id when the part before the dot is not a snowflake', async () => {
		const fields = await rejectSession(`123456789012345678901234567890.${DEAD_SECRET}`);
		expect(fields['tokenType']).toBe('bot');
		expect(fields).not.toHaveProperty('applicationId');
		expectNoSecret(fields, DEAD_SECRET);
	});

	test('omits the application id for a token that is not shaped like a bot token', async () => {
		const fields = await rejectSession(`notanid.${DEAD_SECRET}`);
		expect(fields['tokenType']).toBe('unknown');
		expect(fields).not.toHaveProperty('applicationId');
		expect(fields).not.toHaveProperty('rejectionReason');
		expectNoSecret(fields, DEAD_SECRET);
	});
});
