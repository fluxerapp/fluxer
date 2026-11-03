// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {createChannelInvite, createGuild, getGuildChannels} from '@app/api/guild/tests/GuildTestUtils';
import {bumpGuildBanGeneration} from '@app/api/guild/visits/GuildVisitLifecycle';
import {guildVisitRepository} from '@app/api/guild/visits/GuildVisitorAccess';
import {getGuildRepository, getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface SessionData {
	data: {visit_guild_ids: Array<string>};
}

const VISITOR_IP = '198.51.100.23';

describe('Guild visit session ban checks', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await getInstanceConfigRepository().updateGuildVisitsConfig({enabled: true, user_basis_points: 10000});
	});
	afterAll(async () => {
		await harness.shutdown();
	});

	async function createVisit() {
		const owner = await createTestAccount(harness);
		const visitor = await createTestAccount(harness, {ipAddress: VISITOR_IP});
		const guild = await createGuild(harness, owner.token, 'Session Visits');
		const [channel] = await getGuildChannels(harness, owner.token, guild.id);
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await createBuilder(harness, visitor.token)
			.post(`/invites/${invite.code}/visit`)
			.header('x-forwarded-for', VISITOR_IP)
			.body(null)
			.execute();
		const guildId = createGuildID(BigInt(guild.id));
		await bumpGuildBanGeneration(guildId);
		return {owner, visitor, guildId};
	}

	function restoreSession(visitor: TestAccount) {
		return createBuilder<SessionData>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: visitor.token, version: 1, ip: VISITOR_IP, guild_visits_capable: true});
	}

	it.each([
		['account', 'visitor', null],
		['IP address', 'owner', VISITOR_IP],
	] as const)('ends the visit when the %s is banned', async (_label, target, ip) => {
		const {owner, visitor, guildId} = await createVisit();
		const userId = createUserID(BigInt(visitor.userId));
		const targets = {owner: owner.userId, visitor: visitor.userId};
		await getGuildRepository().upsertBan({
			guild_id: guildId,
			user_id: createUserID(BigInt(targets[target])),
			moderator_id: createUserID(BigInt(owner.userId)),
			banned_at: new Date(),
			expires_at: null,
			reason: null,
			ip,
			email: null,
		});

		const session = await restoreSession(visitor).execute();

		expect(session.data.visit_guild_ids).toEqual([]);
		expect(await guildVisitRepository().get(userId, guildId)).toBeNull();
	});
});
