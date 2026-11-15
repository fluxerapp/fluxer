// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {sendChannelMessage} from '@app/api/channel/tests/ChannelTestUtils';
import {threadsRequest} from '@app/api/channel/tests/ThreadTestUtils';
import {guildVisitRepository} from '@app/api/guild/visits/GuildVisitorAccess';
import {getGatewayService, setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import type {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {SyncTaskWorkerService} from '@app/api/test/SyncTaskWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {
	endGuildVisitsForCodeTask,
	endUnassignedGuildVisitsTask,
	pruneGuildVisitsTask,
} from '@app/api/worker/tasks/GuildVisitTasks';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {DiscoveryCategories} from '@fluxer/constants/src/DiscoveryConstants';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildVisitStartResponse} from '@fluxer/schema/src/domains/guild/GuildVisitSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface Registered {
	token: string;
	user_id: string;
}

interface Scene {
	owner: TestAccount;
	guildId: string;
	channelId: string;
	code: string;
}

interface SessionData {
	data: {
		visit_guild_ids?: Array<string>;
		operator_visit_guild_ids?: Array<string>;
		adult_content_allowed?: boolean;
		account_tier?: string;
		ip_key?: string | null;
		guilds: Array<{id: string}>;
	};
}

function yearsAgo(years: number): string {
	const date = new Date();
	date.setUTCFullYear(date.getUTCFullYear() - years);
	date.setUTCDate(date.getUTCDate() - 1);
	return date.toISOString().slice(0, 10);
}

describe('Guild visit lifecycle', () => {
	let harness: ApiTestHarness;
	let gateway: NoopGatewayService;
	let worker: SyncTaskWorkerService;
	beforeAll(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	beforeEach(async () => {
		await harness.reset();
		gateway = getGatewayService() as NoopGatewayService;
		gateway.visitEvents.length = 0;
		worker = new SyncTaskWorkerService({
			endGuildVisitsForCode: endGuildVisitsForCodeTask,
			endUnassignedGuildVisits: endUnassignedGuildVisitsTask,
			pruneGuildVisits: pruneGuildVisitsTask,
		});
		setInjectedWorkerService(worker);
		await getInstanceConfigRepository().updateGuildVisitsConfig({enabled: true, user_basis_points: 10000});
	});
	afterEach(() => {
		setInjectedWorkerService(new NoopWorkerService());
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function createScene(): Promise<Scene> {
		const owner = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, owner.token)
			.post('/guilds')
			.body({name: 'Lifecycle Guild'})
			.execute();
		const channelId = guild.system_channel_id!;
		const invite = await createBuilder<{code: string}>(harness, owner.token)
			.post(`/channels/${channelId}/invites`)
			.body({})
			.execute();
		return {owner, guildId: guild.id, channelId, code: invite.code};
	}

	async function createUnclaimed(dateOfBirth = yearsAgo(30), ip?: string): Promise<Registered> {
		const builder = createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({username: 'wanderer', consent: true, date_of_birth: dateOfBirth});
		if (ip) builder.header('x-forwarded-for', ip);
		return await builder.execute();
	}

	function visitRow(userId: string, guildId: string) {
		return guildVisitRepository().get(createUserID(BigInt(userId)), createGuildID(BigInt(guildId)));
	}

	it('ends every visit an invite started when the invite is deleted', async () => {
		const {owner, guildId, code} = await createScene();
		const first = await createUnclaimed();
		const second = await createUnclaimed();
		for (const visitor of [first, second]) {
			await createBuilder(harness, visitor.token).post(`/invites/${code}/visit`).body(null).execute();
		}
		await createBuilder(harness, owner.token).delete(`/invites/${code}`).expect(HTTP_STATUS.NO_CONTENT).execute();
		expect(await visitRow(first.user_id, guildId)).toBeNull();
		expect(await visitRow(second.user_id, guildId)).toBeNull();
		expect(gateway.visitEvents.filter((event) => event.kind === 'end')).toHaveLength(2);
	});

	it('ends visits and evicts other visitors on the address when someone is banned', async () => {
		const {owner, guildId, code} = await createScene();
		const ip = '198.51.100.23';
		const target = await createTestAccount(harness, {ipAddress: ip});
		await createBuilder(harness, target.token)
			.post(`/invites/${code}/visit`)
			.header('x-forwarded-for', ip)
			.body(null)
			.execute();
		const alt = await createUnclaimed(yearsAgo(30), ip);
		await createBuilder(harness, alt.token)
			.post(`/invites/${code}/visit`)
			.header('x-forwarded-for', ip)
			.body(null)
			.execute();
		gateway.setLiveVisitors(createGuildID(BigInt(guildId)), [
			{userId: createUserID(BigInt(alt.user_id)), ipKey: getSameIpDecisionKey(ip)!},
		]);
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guildId}/bans/${target.userId}`)
			.body({delete_message_days: 0})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(await visitRow(target.userId, guildId)).toBeNull();
		expect(await visitRow(alt.user_id, guildId)).toBeNull();
		const late = await createUnclaimed(yearsAgo(30), ip);
		await createBuilder(harness, late.token)
			.post(`/invites/${code}/visit`)
			.header('x-forwarded-for', ip)
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.USER_IP_BANNED_FROM_GUILD)
			.execute();
	});

	it('prunes visitors who are too young when a guild becomes adults only', async () => {
		const {owner, guildId, code} = await createScene();
		const teen = await createUnclaimed(yearsAgo(15));
		const adult = await createUnclaimed(yearsAgo(30));
		for (const visitor of [teen, adult]) {
			await createBuilder(harness, visitor.token).post(`/invites/${code}/visit`).body(null).execute();
		}
		await createBuilder(harness, owner.token).patch(`/guilds/${guildId}`).body({nsfw: true}).execute();
		expect(await visitRow(teen.user_id, guildId)).toBeNull();
		expect(await visitRow(adult.user_id, guildId)).not.toBeNull();
		await createBuilder(harness, owner.token).patch(`/guilds/${guildId}`).body({features: []}).execute();
		expect(await visitRow(adult.user_id, guildId)).toBeNull();
	});

	it('restores visits for visit-capable sessions and drops stale ones', async () => {
		const {owner, guildId, code} = await createScene();
		const visitor = await createUnclaimed();
		await createBuilder(harness, visitor.token).post(`/invites/${code}/visit`).body(null).execute();
		const session = await createBuilder<SessionData>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: visitor.token, version: 1, ip: '127.0.0.1', guild_visits_capable: true})
			.execute();
		expect(session.data.visit_guild_ids).toEqual([guildId]);
		expect(session.data.operator_visit_guild_ids).toEqual([]);
		expect(session.data.guilds).toEqual([]);
		expect(session.data).toMatchObject({adult_content_allowed: true, account_tier: 'unclaimed'});
		const legacy = await createBuilder<SessionData>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: visitor.token, version: 1, ip: '127.0.0.1'})
			.execute();
		expect(legacy.data.visit_guild_ids).toEqual([]);
		await createBuilder(harness, owner.token).patch(`/guilds/${guildId}`).body({features: []}).execute();
		const disabled = await createBuilder<SessionData>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: visitor.token, version: 1, ip: '127.0.0.1', guild_visits_capable: true})
			.execute();
		expect(disabled.data.visit_guild_ids).toEqual([]);
	});

	it('refuses every visit surface and ends live visits once the experiment is off, and restores them when it is back on', async () => {
		const {owner, guildId, channelId, code} = await createScene();
		await sendChannelMessage(harness, owner.token, channelId, 'hello visitors');
		await createBuilder(harness, '').post(`/test/guilds/${guildId}/member-count`).body({member_count: 10}).execute();
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review', 'instance:config:update']);
		await createBuilder(harness, owner.token)
			.post(`/guilds/${guildId}/discovery`)
			.body({description: 'A listed community for previews', category_type: DiscoveryCategories.GAMING})
			.execute();
		await createBuilder(harness, admin.token)
			.patch(`/admin/discovery/applications/${guildId}`)
			.body({status: 'approved'})
			.execute();
		const visitor = await createUnclaimed();
		await createBuilder(harness, visitor.token).post(`/invites/${code}/visit`).body(null).execute();
		await createBuilder(harness, visitor.token).get(`/channels/${channelId}/messages`).execute();
		gateway.visitEvents.length = 0;
		const sessionVisits = async () =>
			(
				await createBuilder<SessionData>(harness, '')
					.post('/test/rpc-session-init')
					.body({type: 'session', token: visitor.token, version: 1, ip: '127.0.0.1', guild_visits_capable: true})
					.execute()
			).data.visit_guild_ids;

		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({guild_visits: {enabled: false}})
			.execute();

		expect(gateway.visitEvents).toEqual([
			{kind: 'end', userId: createUserID(BigInt(visitor.user_id)), guildId: createGuildID(BigInt(guildId))},
		]);
		await createBuilder(harness, visitor.token)
			.post(`/invites/${code}/visit`)
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_INVITE)
			.execute();
		await createBuilder(harness, visitor.token)
			.post(`/discovery/guilds/${guildId}/visit`)
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
			.execute();
		await createBuilder(harness, visitor.token)
			.post(`/users/@me/guild-visits/${guildId}/join`)
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.GUILD_VISIT_NOT_FOUND)
			.execute();
		expect(await createBuilder(harness, visitor.token).get('/users/@me/guild-visits').execute()).toEqual([]);
		for (const path of [`/guilds/${guildId}`, `/guilds/${guildId}/channels`, `/channels/${channelId}/messages`]) {
			const response = await harness.requestJson({path, method: 'GET', headers: {Authorization: visitor.token}});
			await response.text();
			expect([HTTP_STATUS.FORBIDDEN, HTTP_STATUS.NOT_FOUND]).toContain(response.status);
		}
		const invite = await createBuilder<{guild: {visits_enabled?: boolean}}>(harness, visitor.token)
			.get(`/invites/${code}`)
			.execute();
		expect(invite.guild).not.toHaveProperty('visits_enabled');
		const listing = await createBuilder<{guilds: Array<{id: string}>}>(harness, visitor.token)
			.get('/discovery/guilds')
			.execute();
		const listed = listing.guilds.find((entry) => entry.id === guildId);
		expect(listed).toBeDefined();
		expect(listed).not.toHaveProperty('visits_enabled');
		expect(await sessionVisits()).toEqual([]);
		expect(gateway.visitEvents.filter((event) => event.kind === 'start')).toEqual([]);

		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({guild_visits: {enabled: true}})
			.execute();
		expect(await sessionVisits()).toEqual([guildId]);
		await createBuilder(harness, visitor.token).get(`/channels/${channelId}/messages`).execute();
	});

	it('visits a discoverable guild and refuses guilds that are not listed', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, owner.token)
			.post('/guilds')
			.body({name: 'Listed Guild'})
			.execute();
		const visitor = await createUnclaimed();
		await createBuilder(harness, visitor.token)
			.post(`/discovery/guilds/${guild.id}/visit`)
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
			.execute();
		await createBuilder(harness, '').post(`/test/guilds/${guild.id}/member-count`).body({member_count: 10}).execute();
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
		await createBuilder(harness, owner.token)
			.post(`/guilds/${guild.id}/discovery`)
			.body({description: 'A listed place for visitors', category_type: DiscoveryCategories.GAMING})
			.execute();
		await createBuilder(harness, admin.token)
			.patch(`/admin/discovery/applications/${guild.id}`)
			.body({status: 'approved'})
			.execute();
		const started = await createBuilder<GuildVisitStartResponse>(harness, visitor.token)
			.post(`/discovery/guilds/${guild.id}/visit`)
			.body(null)
			.execute();
		expect(started.visit?.source).toBe(3);
		const listing = await createBuilder<{guilds: Array<{id: string; visits_enabled?: boolean}>}>(harness, visitor.token)
			.get('/discovery/guilds')
			.execute();
		expect(listing.guilds.find((entry) => entry.id === guild.id)?.visits_enabled).toBe(true);
	});

	it('reads public threads but never private ones', async () => {
		const {owner, channelId, code} = await createScene();
		const message = await sendChannelMessage(harness, owner.token, channelId, 'thread starter');
		const visitor = await createUnclaimed();
		await createBuilder(harness, visitor.token).post(`/invites/${code}/visit`).body(null).execute();
		const publicThread = await threadsRequest<{id: string}>(harness, owner.token)
			.post(`/channels/${channelId}/messages/${message.id}/threads`)
			.body({name: 'public side'})
			.expect(201)
			.execute();
		await threadsRequest(harness, owner.token)
			.post(`/channels/${publicThread.id}/messages`)
			.body({content: 'inside the thread'})
			.execute();
		const read = await threadsRequest<Array<{content: string}>>(harness, visitor.token)
			.get(`/channels/${publicThread.id}/messages`)
			.execute();
		expect(read.map((entry) => entry.content)).toContain('inside the thread');
		const privateThread = await threadsRequest<{id: string}>(harness, owner.token)
			.post(`/channels/${channelId}/threads`)
			.body({name: 'private side', type: ChannelTypes.PRIVATE_THREAD})
			.expect(201)
			.execute();
		await threadsRequest(harness, visitor.token)
			.get(`/channels/${privateThread.id}/messages`)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_CHANNEL)
			.execute();
	});

	it('removes visit rows when the account is deleted', async () => {
		const {guildId, code} = await createScene();
		const visitor = await createTestAccount(harness);
		await createBuilder(harness, visitor.token).post(`/invites/${code}/visit`).body(null).execute();
		const removed = await guildVisitRepository().deleteAllForUser(createUserID(BigInt(visitor.userId)));
		expect(removed.map((id) => id.toString())).toEqual([guildId]);
		expect(await visitRow(visitor.userId, guildId)).toBeNull();
	});
});
