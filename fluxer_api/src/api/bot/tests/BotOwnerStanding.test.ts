// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const HOUR_MS = 3_600_000;

async function setOwnerStanding(
	ownerUserId: string,
	change: {addFlags?: bigint; tempBannedUntil?: Date | null},
): Promise<void> {
	const users = getUserRepository();
	const owner = (await users.findUnique(createUserID(BigInt(ownerUserId))))!;
	await users.patchUpsert(
		owner.id,
		{
			flags: owner.flags | (change.addFlags ?? 0n),
			...(change.tempBannedUntil !== undefined ? {temp_banned_until: change.tempBannedUntil} : {}),
		},
		owner.toRow(),
	);
}

describe('Bot tokens follow the owner account standing', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	async function botRestStatus(botToken: string): Promise<number> {
		const {response} = await createBuilder(harness, `Bot ${botToken}`).get('/users/@me').executeRaw();
		return response.status;
	}

	async function botIdentifyStatus(botToken: string): Promise<number> {
		const response = await harness.requestJson({
			path: '/test/rpc-session-init',
			method: 'POST',
			body: {type: 'session', token: `Bot ${botToken}`, version: 1, ip: '127.0.0.1'},
		});
		return response.status;
	}

	test('a bot of an owner in good standing works over REST and the gateway', async () => {
		const bot = await createTestBotAccount(harness);
		expect(await botRestStatus(bot.botToken)).toBe(HTTP_STATUS.OK);
		expect(await botIdentifyStatus(bot.botToken)).toBe(HTTP_STATUS.OK);
	});

	test('a bot of an owner with the DELETED flag stops working', async () => {
		const bot = await createTestBotAccount(harness);
		await setOwnerStanding(bot.ownerUserId, {addFlags: UserFlags.DELETED});
		expect(await botRestStatus(bot.botToken)).toBe(HTTP_STATUS.UNAUTHORIZED);
		expect(await botIdentifyStatus(bot.botToken)).toBe(HTTP_STATUS.UNAUTHORIZED);
	});

	test('a bot of a temporarily banned owner stops working until the ban ends', async () => {
		const bot = await createTestBotAccount(harness);
		await setOwnerStanding(bot.ownerUserId, {
			addFlags: UserFlags.DISABLED,
			tempBannedUntil: new Date(Date.now() + HOUR_MS),
		});
		expect(await botRestStatus(bot.botToken)).toBe(HTTP_STATUS.UNAUTHORIZED);
		expect(await botIdentifyStatus(bot.botToken)).toBe(HTTP_STATUS.UNAUTHORIZED);
		await setOwnerStanding(bot.ownerUserId, {tempBannedUntil: new Date(Date.now() - HOUR_MS)});
		expect(await botRestStatus(bot.botToken)).toBe(HTTP_STATUS.OK);
	});

	test('a bot of a disabled owner stops working', async () => {
		const bot = await createTestBotAccount(harness);
		await createBuilder(harness, bot.ownerToken)
			.post('/users/@me/disable')
			.body({password: bot.ownerPassword})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(await botRestStatus(bot.botToken)).toBe(HTTP_STATUS.UNAUTHORIZED);
	});
});
