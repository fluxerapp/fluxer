// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import type {GuildID} from '@app/api/BrandedTypes';
import {createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {createPermissionOverwrite} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {createChannel, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {setInjectedGatewayService} from '@app/api/middleware/ServiceRegistry';
import {getGuildRepository} from '@app/api/middleware/ServiceSingletons';
import {banUser} from '@app/api/moderation/tests/ModerationTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import syncDiscoveryIndex from '@app/api/worker/tasks/SyncDiscoveryIndex';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {DiscoveryCategories} from '@fluxer/constants/src/DiscoveryConstants';
import type {
	DiscoveryApplicationResponse,
	DiscoveryChannelPreviewResponse,
	DiscoveryGuildListResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

async function setGuildMemberCount(harness: ApiTestHarness, guildId: string, memberCount: number): Promise<void> {
	await createBuilder(harness, '')
		.post(`/test/guilds/${guildId}/member-count`)
		.body({member_count: memberCount})
		.execute();
}

interface LiveGuildCounts {
	memberCount: number;
	onlineCount: number;
}

const WORKER_HELPERS = {logger: new NoopLogger()} as unknown as WorkerTaskHelpers;

class LiveCountsGatewayService extends NoopGatewayService {
	constructor(private readonly liveCounts: Map<string, LiveGuildCounts>) {
		super();
	}

	override async getDiscoveryGuildCounts(guildIds: Array<GuildID>): Promise<Map<GuildID, LiveGuildCounts>> {
		const counts = new Map<GuildID, LiveGuildCounts>();
		for (const guildId of guildIds) {
			const live = this.liveCounts.get(guildId.toString());
			if (live) {
				counts.set(guildId, live);
			}
		}
		return counts;
	}
}

async function applyAndApprove(
	harness: ApiTestHarness,
	ownerToken: string,
	adminToken: string,
	guildId: string,
	description: string,
	categoryId: number,
): Promise<void> {
	await createBuilder<DiscoveryApplicationResponse>(harness, ownerToken)
		.post(`/guilds/${guildId}/discovery`)
		.body({description, category_type: categoryId})
		.expect(HTTP_STATUS.OK)
		.execute();
	await createBuilder(harness, `${adminToken}`)
		.patch(`/admin/discovery/applications/${guildId}`)
		.body({status: 'approved'})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function createApprovedDiscoveryGuild(
	harness: ApiTestHarness,
	adminToken: string,
	name: string,
	memberCount: number,
): Promise<string> {
	const owner = await createTestAccount(harness);
	const guild = await createGuild(harness, owner.token, name);
	await setGuildMemberCount(harness, guild.id, memberCount);
	await applyAndApprove(
		harness,
		owner.token,
		adminToken,
		guild.id,
		`${name} welcomes everyone`,
		DiscoveryCategories.GAMING,
	);
	return guild.id;
}

describe('Discovery Search and Join', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		clearWorkerDependencies();
		resetChannelThreadsConfig();
		await harness?.shutdown();
	});
	describe('search', () => {
		test('should not return pending guilds in search results', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Pending Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Pending application guild', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			const searcher = await createTestAccount(harness);
			const results = await createBuilder<DiscoveryGuildListResponse>(harness, searcher.token)
				.get('/discovery/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			const found = results.guilds.find((g) => g.id === guild.id);
			expect(found).toBeUndefined();
		});
		test('should not repeat guilds across pages when the discovery index is resynced', async () => {
			const liveCounts = new Map<string, LiveGuildCounts>();
			const gatewayService = new LiveCountsGatewayService(liveCounts);
			setInjectedGatewayService(gatewayService);
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			const guildIds: Array<string> = [];
			for (const [index, memberCount] of [60, 50, 40, 40, 30, 30].entries()) {
				guildIds.push(await createApprovedDiscoveryGuild(harness, admin.token, `Paged Guild ${index}`, memberCount));
			}
			const searcher = await createTestAccount(harness);
			const firstPage = await createBuilder<DiscoveryGuildListResponse>(harness, searcher.token)
				.get('/discovery/guilds?sort_by=member_count&limit=2&offset=0')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(firstPage.guilds.map((guild) => guild.id)).toEqual([guildIds[0], guildIds[1]]);
			liveCounts.set(guildIds[0], {memberCount: 5, onlineCount: 0});
			setWorkerDependenciesForTest({guildRepository: getGuildRepository(), gatewayService});
			await syncDiscoveryIndex({}, WORKER_HELPERS);
			const secondPage = await createBuilder<DiscoveryGuildListResponse>(harness, searcher.token)
				.get('/discovery/guilds?sort_by=member_count&limit=2&offset=2')
				.expect(HTTP_STATUS.OK)
				.execute();
			const thirdPage = await createBuilder<DiscoveryGuildListResponse>(harness, searcher.token)
				.get('/discovery/guilds?sort_by=member_count&limit=2&offset=4')
				.expect(HTTP_STATUS.OK)
				.execute();
			const paged = [...firstPage.guilds, ...secondPage.guilds, ...thirdPage.guilds].map((guild) => guild.id);
			expect(new Set(paged).size).toBe(paged.length);
			expect([...paged].sort()).toEqual([...guildIds].sort());
		});
	});
	describe('join', () => {
		test('should block discovery join for same /64 IPv6 guild ban', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'IPv6 Discovery Ban');
			await setGuildMemberCount(harness, guild.id, 10);
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			await applyAndApprove(
				harness,
				owner.token,
				admin.token,
				guild.id,
				'Join this community',
				DiscoveryCategories.GAMING,
			);
			const bannedUser = await createTestAccount(harness, {ipAddress: '2a01:e0a:d10:95b0:9231:a3e4:939:e8e3'});
			await createBuilder(harness, bannedUser.token)
				.post(`/discovery/guilds/${guild.id}/join`)
				.header('x-forwarded-for', bannedUser.ipAddress!)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await banUser(harness, owner.token, guild.id, bannedUser.userId, 0);
			const altUser = await createTestAccount(harness, {ipAddress: '2a01:e0a:d10:95b0:2415:acac:7521:2b4b'});
			await createBuilder(harness, altUser.token)
				.post(`/discovery/guilds/${guild.id}/join`)
				.header('x-forwarded-for', altUser.ipAddress!)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.USER_IP_BANNED_FROM_GUILD)
				.execute();
		});
		test('should not allow joining non-discoverable guild', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Not Discoverable');
			const joiner = await createTestAccount(harness);
			await createBuilder(harness, joiner.token)
				.post(`/discovery/guilds/${guild.id}/join`)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE)
				.execute();
		});
		test('should not allow joining guild with only pending application', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Pending Join Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Pending but not yet approved', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			const joiner = await createTestAccount(harness);
			await createBuilder(harness, joiner.token)
				.post(`/discovery/guilds/${guild.id}/join`)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE)
				.execute();
		});
		test('should not allow bot accounts to join discoverable guilds', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'No Bots Guild');
			await setGuildMemberCount(harness, guild.id, 10);
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			await applyAndApprove(
				harness,
				owner.token,
				admin.token,
				guild.id,
				'No bots allowed to join via discovery',
				DiscoveryCategories.GAMING,
			);
			const botAccount = await createTestBotAccount(harness);
			const botToken = `Bot ${botAccount.botToken}`;
			await createBuilder(harness, botToken)
				.post(`/discovery/guilds/${guild.id}/join`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('channel preview', () => {
		async function createListedGuild(name: string): Promise<{ownerToken: string; guildId: string; channelId: string}> {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, name);
			await setGuildMemberCount(harness, guild.id, 10);
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			await applyAndApprove(
				harness,
				owner.token,
				admin.token,
				guild.id,
				`${name} welcomes everyone`,
				DiscoveryCategories.GAMING,
			);
			return {ownerToken: owner.token, guildId: guild.id, channelId: guild.system_channel_id!};
		}
		test('should not preview a channel hidden from everyone', async () => {
			const {ownerToken, guildId} = await createListedGuild('Hidden Channel Guild');
			const hidden = await createChannel(harness, ownerToken, guildId, 'staff');
			await createPermissionOverwrite(harness, ownerToken, hidden.id, guildId, {
				type: 0,
				allow: '0',
				deny: Permissions.VIEW_CHANNEL.toString(),
			});
			const viewer = await createTestAccount(harness);
			await createBuilder(harness, viewer.token)
				.get(`/discovery/guilds/${guildId}/channels/${hidden.id}`)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE)
				.execute();
		});
		test('should not preview a channel from another guild', async () => {
			const {guildId} = await createListedGuild('Listed Guild');
			const other = await createListedGuild('Other Listed Guild');
			const viewer = await createTestAccount(harness);
			await createBuilder(harness, viewer.token)
				.get(`/discovery/guilds/${guildId}/channels/${other.channelId}`)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE)
				.execute();
		});
		test('should only preview a forum for viewers in the threads experiment', async () => {
			await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
			const {ownerToken, guildId} = await createListedGuild('Forum Guild');
			const forum = await threadsRequest<{id: string}>(harness, ownerToken)
				.post(`/guilds/${guildId}/channels`)
				.body({name: 'forum', type: ChannelTypes.GUILD_FORUM})
				.execute();
			const viewer = await createTestAccount(harness);
			const preview = await threadsRequest<DiscoveryChannelPreviewResponse>(harness, viewer.token)
				.get(`/discovery/guilds/${guildId}/channels/${forum.id}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(preview.channel.type).toBe(ChannelTypes.GUILD_FORUM);
			await createBuilder(harness, viewer.token)
				.get(`/discovery/guilds/${guildId}/channels/${forum.id}`)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE)
				.execute();
		});
		test('should never preview a thread', async () => {
			await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
			const {ownerToken, guildId, channelId} = await createListedGuild('Thread Guild');
			const viewer = await createTestAccount(harness);
			for (const type of [ChannelTypes.PUBLIC_THREAD, ChannelTypes.PRIVATE_THREAD]) {
				const thread = await threadsRequest<{id: string}>(harness, ownerToken)
					.post(`/channels/${channelId}/threads`)
					.body({name: 'secret plans', type})
					.expect(201)
					.execute();
				await threadsRequest(harness, viewer.token)
					.get(`/discovery/guilds/${guildId}/channels/${thread.id}`)
					.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE)
					.execute();
			}
		});
		test('should not preview a guild that is not listed', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Unlisted Guild');
			const viewer = await createTestAccount(harness);
			await createBuilder(harness, viewer.token)
				.get(`/discovery/guilds/${guild.id}/channels/${guild.system_channel_id}`)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE)
				.execute();
		});
	});
});
