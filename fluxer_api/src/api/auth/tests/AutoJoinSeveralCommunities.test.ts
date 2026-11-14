// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUniqueEmail, createUniqueUsername} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {createChannelInvite, createGuild, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {parseInviteCodeList} from '@app/api/instance/AutoJoinInviteCodes';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('auto-join invite list', () => {
	it('splits on commas and spaces and drops repeats', () => {
		expect(parseInviteCodeList(' abc, def ,,abc  ghi ')).toEqual(['abc', 'def', 'ghi']);
		expect(parseInviteCodeList('')).toEqual([]);
		expect(parseInviteCodeList(undefined)).toEqual([]);
	});
});

describe('auto-join several communities on sign-up', () => {
	let harness: ApiTestHarness;
	const original = Config.instance.autoJoinInviteCode;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterEach(() => {
		Config.instance.autoJoinInviteCode = original;
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	it('joins every community in the list and skips a code that does not work', async () => {
		const {owner, guild: first} = await setupTestGuildWithMembers(harness, 0);
		const second = await createGuild(harness, owner.token, 'Second community');
		const firstInvite = await createChannelInvite(harness, owner.token, first.system_channel_id!);
		const secondInvite = await createChannelInvite(harness, owner.token, second.system_channel_id!);
		Config.instance.autoJoinInviteCode = `${firstInvite.code}, notacode, ${secondInvite.code}`;
		const registered = await createBuilderWithoutAuth<{user_id: string}>(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail('autojoin'),
				username: createUniqueUsername('autojoin'),
				global_name: 'New member',
				password: 'joins-two-communities',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.execute();
		for (const guild of [first, second]) {
			await createBuilder(harness, owner.token)
				.get(`/guilds/${guild.id}/members/${registered.user_id}`)
				.expect(HTTP_STATUS.OK)
				.execute();
		}
	});
});
