// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {createGuild, createRole} from '@app/api/guild/tests/GuildTestUtils';
import {banUser} from '@app/api/moderation/tests/ModerationTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, test} from 'vitest';

let harness: ApiTestHarness;
const original = {
	selfHosted: Config.instance.selfHosted,
	guildId: Config.instance.visionariesGuildId,
	roleId: Config.instance.visionariesGuildVisionaryRoleId,
};

beforeAll(async () => {
	harness = await createApiTestHarness();
	Config.instance.selfHosted = false;
});

afterAll(async () => {
	await harness.shutdown();
	Config.instance.selfHosted = original.selfHosted;
	Config.instance.visionariesGuildId = original.guildId;
	Config.instance.visionariesGuildVisionaryRoleId = original.roleId;
});

test('visionary rejoin follows the community ban list', async () => {
	await harness.resetData();
	const owner = await createTestAccount(harness);
	const guild = await createGuild(harness, owner.token, 'Visionaries');
	const role = await createRole(harness, owner.token, guild.id, {name: 'Visionary'});
	Config.instance.visionariesGuildId = guild.id;
	Config.instance.visionariesGuildVisionaryRoleId = role.id;
	const visionary = await createTestAccount(harness);
	await createBuilder(harness, visionary.token)
		.post(`/test/users/${visionary.userId}/premium`)
		.body({premium_type: UserPremiumTypes.LIFETIME, premium_lifetime_sequence: 1})
		.execute();
	await banUser(harness, owner.token, guild.id, visionary.userId);
	await createBuilder(harness, visionary.token)
		.post('/premium/visionary/rejoin')
		.expect(403, APIErrorCodes.USER_BANNED_FROM_GUILD)
		.execute();
	await createBuilder(harness, owner.token)
		.get(`/guilds/${guild.id}/members/${visionary.userId}`)
		.expect(404)
		.execute();
});
