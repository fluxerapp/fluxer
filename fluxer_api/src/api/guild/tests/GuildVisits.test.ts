// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {sendChannelMessage} from '@app/api/channel/tests/ChannelTestUtils';
import {guildVisitRepository} from '@app/api/guild/visits/GuildVisitorAccess';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import type {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DiscoveryCategories} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {GuildVisitSources} from '@fluxer/constants/src/GuildVisitConstants';
import {Headers} from '@fluxer/constants/src/Headers';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildVisitResponse, GuildVisitStartResponse} from '@fluxer/schema/src/domains/guild/GuildVisitSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface Registered {
	token: string;
	user_id: string;
	entry?: {guild_id: string; mode: string} | null;
}

interface VisitGuild {
	owner: TestAccount;
	guildId: string;
	channelId: string;
	code: string;
}

function yearsAgo(years: number): string {
	const date = new Date();
	date.setUTCFullYear(date.getUTCFullYear() - years);
	date.setUTCDate(date.getUTCDate() - 1);
	return date.toISOString().slice(0, 10);
}

describe('Guild visits', () => {
	let harness: ApiTestHarness;
	let gateway: NoopGatewayService;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		gateway = getGatewayService() as NoopGatewayService;
		gateway.visitEvents.length = 0;
		await getInstanceConfigRepository().updateGuildVisitsConfig({enabled: true, user_basis_points: 10000});
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function createVisitGuild(name = 'Visit Guild', inviteBody: Record<string, unknown> = {}): Promise<VisitGuild> {
		const owner = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, owner.token).post('/guilds').body({name}).execute();
		const channelId = guild.system_channel_id!;
		const invite = await createBuilder<{code: string}>(harness, owner.token)
			.post(`/channels/${channelId}/invites`)
			.body(inviteBody)
			.execute();
		return {owner, guildId: guild.id, channelId, code: invite.code};
	}

	async function createUnclaimed(dateOfBirth = yearsAgo(30)): Promise<Registered> {
		return await createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({username: 'visitor', consent: true, date_of_birth: dateOfBirth})
			.execute();
	}

	function startVisit(token: string, code: string) {
		return createBuilder<GuildVisitStartResponse>(harness, token).post(`/invites/${code}/visit`).body(null);
	}

	async function setFeatures(owner: TestAccount, guildId: string, features: Array<string>): Promise<void> {
		await createBuilder(harness, owner.token).patch(`/guilds/${guildId}`).body({features}).execute();
	}

	it('turns visits on for new guilds and lets the owner turn them off', async () => {
		const {owner, guildId} = await createVisitGuild();
		const guild = await createBuilder<GuildResponse>(harness, owner.token).get(`/guilds/${guildId}`).execute();
		expect(guild.features).toContain(GuildFeatures.VISITS_ENABLED);
		await setFeatures(owner, guildId, []);
		const after = await createBuilder<GuildResponse>(harness, owner.token).get(`/guilds/${guildId}`).execute();
		expect(after.features).not.toContain(GuildFeatures.VISITS_ENABLED);
	});

	it('starts a visit from an invite without making a member', async () => {
		const {guildId, code} = await createVisitGuild();
		const visitor = await createUnclaimed();
		const started = await startVisit(visitor.token, code).execute();
		expect(started).toMatchObject({guild_id: guildId, member: false, visit: {guild_id: guildId, source: 1}});
		expect(gateway.visitEvents).toEqual([
			{kind: 'start', userId: createUserID(BigInt(visitor.user_id)), guildId: createGuildID(BigInt(guildId))},
		]);
		expect(
			await gateway.hasGuildMember({
				guildId: createGuildID(BigInt(guildId)),
				userId: createUserID(BigInt(visitor.user_id)),
			}),
		).toBe(false);
		const visits = await createBuilder<Array<GuildVisitResponse>>(harness, visitor.token)
			.get('/users/@me/guild-visits')
			.execute();
		expect(visits.map((visit) => visit.guild_id)).toEqual([guildId]);
		const again = await startVisit(visitor.token, code).execute();
		expect(again.visit?.started_at).toBe(started.visit?.started_at);
	});

	it('answers as a member when the user already joined', async () => {
		const {owner, guildId, code} = await createVisitGuild();
		const started = await startVisit(owner.token, code).execute();
		expect(started).toEqual({guild_id: guildId, member: true, visit: null});
		expect(gateway.visitEvents).toEqual([]);
	});

	it('hides the visit route while the switch or the assignment is off', async () => {
		const {code} = await createVisitGuild();
		const visitor = await createUnclaimed();
		await getInstanceConfigRepository().updateGuildVisitsConfig({enabled: false});
		await startVisit(visitor.token, code).expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_INVITE).execute();
		await getInstanceConfigRepository().updateGuildVisitsConfig({enabled: true, user_basis_points: 0});
		await startVisit(visitor.token, code).expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_INVITE).execute();
		const listed = await createBuilder<Array<GuildVisitResponse>>(harness, visitor.token)
			.get('/users/@me/guild-visits')
			.execute();
		expect(listed).toEqual([]);
	});

	it('refuses guilds that turned visits off or paused invites', async () => {
		const {owner, guildId, code} = await createVisitGuild();
		const visitor = await createUnclaimed();
		await setFeatures(owner, guildId, []);
		await startVisit(visitor.token, code).expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.GUILD_VISITS_DISABLED).execute();
		await setFeatures(owner, guildId, [GuildFeatures.VISITS_ENABLED, GuildFeatures.INVITES_DISABLED]);
		await startVisit(visitor.token, code).expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.INVITES_DISABLED).execute();
	});

	it('keeps adults-only guilds away from younger accounts', async () => {
		const {owner, guildId, code} = await createVisitGuild();
		await createBuilder(harness, owner.token).patch(`/guilds/${guildId}`).body({nsfw: true}).execute();
		const teen = await createUnclaimed(yearsAgo(15));
		await startVisit(teen.token, code)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.GUILD_VISIT_AGE_RESTRICTED)
			.execute();
		const adult = await createUnclaimed(yearsAgo(30));
		await startVisit(adult.token, code).execute();
	});

	it('asks for a date of birth before any visit', async () => {
		const {code} = await createVisitGuild();
		const account = await createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({username: 'birthdayless', consent: true})
			.execute();
		await startVisit(account.token, code).expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.DATE_OF_BIRTH_REQUIRED).execute();
	});

	it('refuses banned users and accounts that cannot start anything new', async () => {
		const {owner, guildId, code} = await createVisitGuild();
		const banned = await createTestAccount(harness);
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guildId}/bans/${banned.userId}`)
			.body({delete_message_days: 0})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await startVisit(banned.token, code).expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.USER_BANNED_FROM_GUILD).execute();
	});

	it('spends a limited invite use only on joining', async () => {
		const {guildId, code} = await createVisitGuild('Limited', {max_uses: 1});
		const first = await createTestAccount(harness);
		const second = await createTestAccount(harness);
		const started = await startVisit(first.token, code).execute();
		expect(started.visit).not.toBeNull();
		expect((await startVisit(second.token, code).execute()).visit).not.toBeNull();
		const joined = await createBuilder<GuildResponse>(harness, first.token)
			.post(`/users/@me/guild-visits/${guildId}/join`)
			.body(null)
			.execute();
		expect(joined.id).toBe(guildId);
		expect(
			await guildVisitRepository().get(createUserID(BigInt(first.userId)), createGuildID(BigInt(guildId))),
		).toBeNull();
		await createBuilder(harness, second.token)
			.post(`/invites/${code}`)
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_INVITE)
			.execute();
	});

	it('joins from a visit through the invite accept route without a second use', async () => {
		const {guildId, code} = await createVisitGuild('Accept', {max_uses: 1});
		const visitor = await createTestAccount(harness);
		await startVisit(visitor.token, code).execute();
		await createBuilder(harness, visitor.token).post(`/invites/${code}`).body(null).execute();
		expect(
			await gateway.hasGuildMember({
				guildId: createGuildID(BigInt(guildId)),
				userId: createUserID(BigInt(visitor.userId)),
			}),
		).toBe(true);
		expect(
			await guildVisitRepository().get(createUserID(BigInt(visitor.userId)), createGuildID(BigInt(guildId))),
		).toBeNull();
	});

	it('asks unclaimed visitors to finish their account before joining, and keeps the visit', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({interaction_tier: 'enforced'});
		const {guildId, code} = await createVisitGuild();
		const visitor = await createUnclaimed();
		await startVisit(visitor.token, code).execute();
		const refused = await createBuilder<{action: string}>(harness, visitor.token)
			.post(`/users/@me/guild-visits/${guildId}/join`)
			.header(Headers.X_FLUXER_CAPABILITIES, 'guild_visits')
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCOUNT_VERIFICATION_REQUIRED)
			.execute();
		expect(refused.action).toBe('guild_join');
		expect(
			await guildVisitRepository().get(createUserID(BigInt(visitor.user_id)), createGuildID(BigInt(guildId))),
		).not.toBeNull();
	});

	it('joins a visited guild as a member with a member count change', async () => {
		const {owner, guildId, code} = await createVisitGuild();
		const visitor = await createTestAccount(harness);
		await startVisit(visitor.token, code).execute();
		const before = await createBuilder<GuildResponse>(harness, owner.token).get(`/guilds/${guildId}`).execute();
		await createBuilder(harness, visitor.token).post(`/users/@me/guild-visits/${guildId}/join`).body(null).execute();
		const after = await createBuilder<GuildResponse>(harness, owner.token).get(`/guilds/${guildId}`).execute();
		expect(after.member_count).toBe((before.member_count ?? 0) + 1);
		const visits = await createBuilder<Array<GuildVisitResponse>>(harness, visitor.token)
			.get('/users/@me/guild-visits')
			.execute();
		expect(visits).toEqual([]);
		await createBuilder(harness, visitor.token)
			.post(`/users/@me/guild-visits/${guildId}/join`)
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.GUILD_VISIT_NOT_FOUND)
			.execute();
	});

	it('ends a visit when the user leaves it', async () => {
		const {guildId, code} = await createVisitGuild();
		const visitor = await createUnclaimed();
		await startVisit(visitor.token, code).execute();
		await createBuilder(harness, visitor.token)
			.delete(`/users/@me/guild-visits/${guildId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, visitor.token)
			.delete(`/users/@me/guild-visits/${guildId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(gateway.visitEvents.map((event) => event.kind)).toEqual(['start', 'end', 'end']);
		expect(
			await guildVisitRepository().get(createUserID(BigInt(visitor.user_id)), createGuildID(BigInt(guildId))),
		).toBeNull();
	});

	it('replaces the least recently opened visit past the cap for unclaimed accounts', async () => {
		const visitor = await createUnclaimed();
		const guilds: Array<VisitGuild> = [];
		for (let index = 0; index < 6; index++) {
			const guild = await createVisitGuild(`Cap ${index}`);
			guilds.push(guild);
			await startVisit(visitor.token, guild.code).execute();
		}
		const visits = await createBuilder<Array<GuildVisitResponse>>(harness, visitor.token)
			.get('/users/@me/guild-visits')
			.execute();
		expect(visits).toHaveLength(5);
		expect(visits.map((visit) => visit.guild_id)).not.toContain(guilds[0]!.guildId);
		expect(
			gateway.visitEvents.filter((event) => event.kind === 'end').map((event) => event.guildId.toString()),
		).toEqual([guilds[0]!.guildId]);
	});

	it('offers a visit on invite payloads only to users who can visit', async () => {
		const {owner, guildId, code} = await createVisitGuild();
		const visitor = await createUnclaimed();
		const seen = await createBuilder<{guild: {visits_enabled?: boolean}}>(harness, visitor.token)
			.get(`/invites/${code}`)
			.execute();
		expect(seen.guild.visits_enabled).toBe(true);
		await setFeatures(owner, guildId, []);
		const off = await createBuilder<{guild: {visits_enabled?: boolean}}>(harness, visitor.token)
			.get(`/invites/${code}`)
			.execute();
		expect(off.guild.visits_enabled).toBe(false);
		await getInstanceConfigRepository().updateGuildVisitsConfig({user_basis_points: 0});
		await setFeatures(owner, guildId, [GuildFeatures.VISITS_ENABLED]);
		const anonymous = await createBuilderWithoutAuth<{guild: {visits_enabled?: boolean}}>(harness)
			.get(`/invites/${code}`)
			.execute();
		expect(anonymous.guild).not.toHaveProperty('visits_enabled');
	});

	it('turns an invite used at sign-up into a visit after the date of birth step', async () => {
		const {guildId, code} = await createVisitGuild();
		const account = await createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.header(Headers.X_FLUXER_CAPABILITIES, 'guild_visits')
			.body({username: 'latecomer', consent: true, invite_code: code})
			.execute();
		expect(account.entry ?? null).toBeNull();
		expect(await guildVisitRepository().list(createUserID(BigInt(account.user_id)))).toEqual([]);
		const result = await createBuilder<{entry: {guild_id: string; mode: string} | null}>(harness, account.token)
			.put('/users/@me/date-of-birth')
			.header(Headers.X_FLUXER_CAPABILITIES, 'guild_visits')
			.body({date_of_birth: yearsAgo(25)})
			.execute();
		expect(result.entry).toEqual({guild_id: guildId, mode: 'visit'});
		const rows = await guildVisitRepository().list(createUserID(BigInt(account.user_id)));
		expect(rows.map((row) => [row.guild_id.toString(), row.source])).toEqual([[guildId, GuildVisitSources.INVITE]]);
	});

	it('ends visits when the guild turns them off later', async () => {
		const {owner, guildId, code, channelId} = await createVisitGuild();
		const visitor = await createUnclaimed();
		await startVisit(visitor.token, code).execute();
		await sendChannelMessage(harness, owner.token, channelId, 'hello visitors');
		await createBuilder(harness, visitor.token).get(`/channels/${channelId}/messages`).execute();
		await setFeatures(owner, guildId, []);
		await createBuilder(harness, visitor.token)
			.get(`/channels/${channelId}/messages`)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
			.execute();
	});

	it('lets the owner turn previews off for discovery too', async () => {
		const {owner, guildId} = await createVisitGuild('Listed Guild');
		await createBuilder(harness, '').post(`/test/guilds/${guildId}/member-count`).body({member_count: 10}).execute();
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
		await createBuilder(harness, owner.token)
			.post(`/guilds/${guildId}/discovery`)
			.body({description: 'A listed community for previews', category_type: DiscoveryCategories.GAMING})
			.execute();
		await createBuilder(harness, admin.token)
			.patch(`/admin/discovery/applications/${guildId}`)
			.body({status: 'approved'})
			.execute();
		const visitor = await createUnclaimed();
		const discoveryVisit = () =>
			createBuilder<GuildVisitStartResponse>(harness, visitor.token)
				.post(`/discovery/guilds/${guildId}/visit`)
				.body(null);
		expect((await discoveryVisit().execute()).visit).not.toBeNull();
		await createBuilder(harness, visitor.token)
			.delete(`/users/@me/guild-visits/${guildId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await setFeatures(owner, guildId, []);
		await discoveryVisit().expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.GUILD_VISITS_DISABLED).execute();
	});

	it.each([0, 60])('stores the invite expiry of a %i-second invite on the visit', async (maxAge) => {
		const {guildId, code} = await createVisitGuild('Invite expiry', {max_age: maxAge, unique: true});
		const visitor = await createUnclaimed();
		const invite = await createBuilderWithoutAuth<{expires_at: string | null}>(harness)
			.get(`/invites/${code}`)
			.execute();
		expect(invite.expires_at === null).toBe(maxAge === 0);
		await startVisit(visitor.token, code).execute();
		const row = await guildVisitRepository().get(createUserID(BigInt(visitor.user_id)), createGuildID(BigInt(guildId)));
		expect(row?.invite_code).toBe(code);
		expect(row?.invite_expires_at ?? null).toEqual(invite.expires_at === null ? null : new Date(invite.expires_at));
	});
});
