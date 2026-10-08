// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createRoleID} from '@app/api/BrandedTypes';
import {authorizeBot, createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {resetChannelThreadsConfig, setChannelThreadsConfig} from '@app/api/channel/tests/ThreadTestUtils';
import {fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {GuildThreadStateRow} from '@app/api/database/types/ThreadTypes';
import {GuildRoleRepository} from '@app/api/guild/repositories/GuildRoleRepository';
import {
	acceptInvite,
	addMemberRole,
	createChannelInvite,
	createGuild,
	createRole,
	getChannel,
	updateRolePositions,
} from '@app/api/guild/tests/GuildTestUtils';
import {GuildThreadState} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes, DEFAULT_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	DEFAULT_THREAD_PERMISSIONS,
	THREAD_PERMISSIONS,
	ThreadPermissionFlags,
} from '@fluxer/constants/src/ThreadPermissionUtils';
import type {ChannelThreadsConfig} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

const FEATURES = 'X-Fluxer-Features';
const CAPABLE = 'channel_threads';
const ACTIVE: Partial<ChannelThreadsConfig> = {
	enabled: true,
	guild_basis_points: 10000,
	user_basis_points: 10000,
};

const FETCH_MARKER = GuildThreadState.selectCql({where: GuildThreadState.where.eq('guild_id'), limit: 1});

function bits(value: string | undefined): bigint {
	return BigInt(value ?? '0');
}

describe('thread permission bits across guild modes', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
	});

	afterEach(() => {
		resetChannelThreadsConfig();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function listRoles(token: string, guildId: string, capable: boolean): Promise<Array<GuildRoleResponse>> {
		const builder = createBuilder<Array<GuildRoleResponse>>(harness, token).get(`/guilds/${guildId}/roles`);
		return (capable ? builder.header(FEATURES, CAPABLE) : builder).execute();
	}

	async function roleBits(token: string, guildId: string, roleId: string): Promise<bigint> {
		const roles = await listRoles(token, guildId, true);
		return bits(roles.find((role) => role.id === roleId)?.permissions);
	}

	test('control guilds keep today behaviour for requested and stored thread bits', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Control');
		const everyone = await roleBits(owner.token, guild.id, guild.id);
		expect(everyone).toBe(DEFAULT_PERMISSIONS);
		expect(await fetchOne<GuildThreadStateRow>(FETCH_MARKER, {guild_id: createGuildID(BigInt(guild.id))})).toBeNull();
		const role = await createBuilder<GuildRoleResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/roles`)
			.header(FEATURES, CAPABLE)
			.body({name: 'r', permissions: (Permissions.SEND_MESSAGES | ThreadPermissionFlags.MANAGE_THREADS).toString()})
			.execute();
		expect(bits(role.permissions)).toBe(Permissions.SEND_MESSAGES);
		const roleRepository = new GuildRoleRepository();
		const stored = await roleRepository.getRole(createRoleID(BigInt(role.id)), createGuildID(BigInt(guild.id)));
		await roleRepository.upsertRole({...stored!.toRow(), permissions: Permissions.SEND_MESSAGES | (1n << 35n)});
		expect(await roleBits(owner.token, guild.id, role.id)).toBe(Permissions.SEND_MESSAGES | (1n << 35n));
		expect(bits((await listRoles(owner.token, guild.id, false)).find((r) => r.id === role.id)?.permissions)).toBe(
			Permissions.SEND_MESSAGES | (1n << 35n),
		);
		const updated = await createBuilder<GuildRoleResponse>(harness, owner.token)
			.patch(`/guilds/${guild.id}/roles/${role.id}`)
			.body({permissions: Permissions.VIEW_CHANNEL.toString()})
			.execute();
		expect(bits(updated.permissions)).toBe(Permissions.VIEW_CHANNEL);
	});

	test('active guilds seed @everyone, write bits for capable users and restore them for others', async () => {
		await setChannelThreadsConfig(ACTIVE);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Active');
		expect(await roleBits(owner.token, guild.id, guild.id)).toBe(DEFAULT_PERMISSIONS | DEFAULT_THREAD_PERMISSIONS);
		const marker = await fetchOne<GuildThreadStateRow>(FETCH_MARKER, {guild_id: createGuildID(BigInt(guild.id))});
		expect(marker?.perms_seeded_at).toBeInstanceOf(Date);
		const role = await createBuilder<GuildRoleResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/roles`)
			.header(FEATURES, CAPABLE)
			.body({name: 'mods', permissions: (Permissions.SEND_MESSAGES | ThreadPermissionFlags.MANAGE_THREADS).toString()})
			.execute();
		expect(bits(role.permissions)).toBe(Permissions.SEND_MESSAGES | ThreadPermissionFlags.MANAGE_THREADS);
		const legacy = await createBuilder<GuildRoleResponse>(harness, owner.token)
			.patch(`/guilds/${guild.id}/roles/${role.id}`)
			.body({permissions: (Permissions.VIEW_CHANNEL | ThreadPermissionFlags.CREATE_PUBLIC_THREADS).toString()})
			.execute();
		expect(bits(legacy.permissions) & THREAD_PERMISSIONS).toBe(0n);
		expect(await roleBits(owner.token, guild.id, role.id)).toBe(
			Permissions.VIEW_CHANNEL | ThreadPermissionFlags.MANAGE_THREADS,
		);
		const masked = await listRoles(owner.token, guild.id, false);
		expect(masked.every((r) => (bits(r.permissions) & THREAD_PERMISSIONS) === 0n)).toBe(true);
		const cleared = await createBuilder<GuildRoleResponse>(harness, owner.token)
			.patch(`/guilds/${guild.id}/roles/${role.id}`)
			.header(FEATURES, CAPABLE)
			.body({permissions: Permissions.VIEW_CHANNEL.toString()})
			.execute();
		expect(bits(cleared.permissions)).toBe(Permissions.VIEW_CHANNEL);
	});

	test('bots write thread bits and bot invites keep them only for capable active users in active guilds', async () => {
		await setChannelThreadsConfig(ACTIVE);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bots');
		const bot = await createTestBotAccount(harness);
		const requested =
			Permissions.MANAGE_ROLES | Permissions.SEND_MESSAGES | ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS;
		await createBuilder<{redirect_to: string}>(harness, owner.token)
			.post('/oauth2/authorize/consent')
			.header(FEATURES, CAPABLE)
			.body({client_id: bot.appId, scope: 'bot', guild_id: guild.id, permissions: requested.toString()})
			.execute();
		const botRole = (await listRoles(owner.token, guild.id, true)).find((role) => role.name !== '@everyone');
		expect(bits(botRole?.permissions)).toBe(requested);
		const legacyBot = await createTestBotAccount(harness);
		await authorizeBot(harness, owner.token, legacyBot.appId, ['bot'], guild.id, requested.toString());
		const legacyRole = (await listRoles(owner.token, guild.id, true)).find(
			(role) => role.name !== '@everyone' && role.id !== botRole?.id,
		);
		expect(bits(legacyRole?.permissions)).toBe(Permissions.MANAGE_ROLES | Permissions.SEND_MESSAGES);
		const written = await createBuilder<GuildRoleResponse>(harness, `Bot ${bot.botToken}`)
			.post(`/guilds/${guild.id}/roles`)
			.body({name: 'bot-made', permissions: ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS.toString()})
			.execute();
		expect(bits(written.permissions)).toBe(ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS);
		const vcm = await createBuilder<GuildRoleResponse>(harness, `Bot ${bot.botToken}`)
			.post(`/guilds/${guild.id}/roles`)
			.body({name: 'bot-vcm', permissions: Permissions.VIEW_CHANNEL_MEMBERS.toString()})
			.execute();
		expect(bits(vcm.permissions)).toBe(0n);

		await setChannelThreadsConfig({enabled: false});
		const controlOwner = await createTestAccount(harness);
		const controlGuild = await createGuild(harness, controlOwner.token, 'Bots control');
		const controlBot = await createTestBotAccount(harness);
		await authorizeBot(harness, controlOwner.token, controlBot.appId, ['bot'], controlGuild.id, requested.toString());
		const controlRole = (await listRoles(controlOwner.token, controlGuild.id, true)).find(
			(role) => role.name !== '@everyone',
		);
		expect(bits(controlRole?.permissions)).toBe(Permissions.MANAGE_ROLES | Permissions.SEND_MESSAGES);
	});

	test('retired guilds keep stored thread bits read-only without escalation errors', async () => {
		await setChannelThreadsConfig(ACTIVE);
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Retired');
		const target = await createBuilder<GuildRoleResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/roles`)
			.header(FEATURES, CAPABLE)
			.body({
				name: 'target',
				permissions: (Permissions.SEND_MESSAGES | ThreadPermissionFlags.MANAGE_THREADS).toString(),
			})
			.execute();
		const mod = await createRole(harness, owner.token, guild.id, {
			name: 'mod',
			permissions: (Permissions.MANAGE_ROLES | Permissions.SEND_MESSAGES | Permissions.ADD_REACTIONS).toString(),
		});
		await updateRolePositions(harness, owner.token, guild.id, [
			{id: mod.id, position: 2},
			{id: target.id, position: 1},
		]);
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		await addMemberRole(harness, owner.token, guild.id, member.userId, mod.id);

		await setChannelThreadsConfig({enabled: false});
		const updated = await createBuilder<GuildRoleResponse>(harness, member.token)
			.patch(`/guilds/${guild.id}/roles/${target.id}`)
			.header(FEATURES, CAPABLE)
			.body({
				permissions: (
					Permissions.SEND_MESSAGES |
					Permissions.ADD_REACTIONS |
					ThreadPermissionFlags.CREATE_PUBLIC_THREADS
				).toString(),
			})
			.execute();
		expect(bits(updated.permissions) & ~THREAD_PERMISSIONS).toBe(Permissions.SEND_MESSAGES | Permissions.ADD_REACTIONS);
		expect(await roleBits(owner.token, guild.id, target.id)).toBe(
			Permissions.SEND_MESSAGES | Permissions.ADD_REACTIONS,
		);
		const stored = await new GuildRoleRepository().getRole(
			createRoleID(BigInt(target.id)),
			createGuildID(BigInt(guild.id)),
		);
		expect(stored?.permissions).toBe(
			Permissions.SEND_MESSAGES | Permissions.ADD_REACTIONS | ThreadPermissionFlags.MANAGE_THREADS,
		);

		const channel = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.header(FEATURES, CAPABLE)
			.body({
				name: 'retired-overwrites',
				type: ChannelTypes.GUILD_TEXT,
				permission_overwrites: [
					{
						id: guild.id,
						type: 0,
						allow: (Permissions.SEND_MESSAGES | ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS).toString(),
						deny: '0',
					},
				],
			})
			.execute();
		expect(channel.permission_overwrites?.map((overwrite) => bits(overwrite.allow))).toEqual([
			Permissions.SEND_MESSAGES,
		]);
	});

	test('active channel creates store thread-aware overwrites and mask them for non-viewers', async () => {
		await setChannelThreadsConfig(ACTIVE);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Overwrites');
		const allow = Permissions.SEND_MESSAGES | ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS;
		const channel = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.header(FEATURES, CAPABLE)
			.body({
				name: 'active-overwrites',
				type: ChannelTypes.GUILD_TEXT,
				permission_overwrites: [{id: guild.id, type: 0, allow: allow.toString(), deny: '0'}],
			})
			.execute();
		expect(channel.permission_overwrites?.map((overwrite) => bits(overwrite.allow))).toEqual([allow]);
		const viewerList = await createBuilder<Array<ChannelResponse>>(harness, owner.token)
			.get(`/guilds/${guild.id}/channels`)
			.header(FEATURES, CAPABLE)
			.execute();
		const legacyList = await createBuilder<Array<ChannelResponse>>(harness, owner.token)
			.get(`/guilds/${guild.id}/channels`)
			.execute();
		const pick = (list: Array<ChannelResponse>) =>
			bits(list.find((entry) => entry.id === channel.id)?.permission_overwrites?.[0]?.allow);
		expect(pick(viewerList)).toBe(allow);
		expect(pick(legacyList)).toBe(Permissions.SEND_MESSAGES);
		const single = async (capable: boolean) => {
			const builder = createBuilder<ChannelResponse>(harness, owner.token).get(`/channels/${channel.id}`);
			return bits(
				(await (capable ? builder.header(FEATURES, CAPABLE) : builder).execute()).permission_overwrites?.[0]?.allow,
			);
		};
		expect(await single(true)).toBe(allow);
		expect(await single(false)).toBe(Permissions.SEND_MESSAGES);
		const patched = await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${channel.id}`)
			.body({topic: 'masked'})
			.execute();
		expect(bits(patched.permission_overwrites?.[0]?.allow)).toBe(Permissions.SEND_MESSAGES);
	});

	test('overwrite removals keep thread bits the actor cannot write, without escalation errors', async () => {
		await setChannelThreadsConfig(ACTIVE);
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Removals');
		const target = await createRole(harness, owner.token, guild.id, {name: 'target', permissions: '0'});
		const mod = await createRole(harness, owner.token, guild.id, {
			name: 'mod',
			permissions: (Permissions.MANAGE_ROLES | Permissions.MANAGE_CHANNELS).toString(),
		});
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		await addMemberRole(harness, owner.token, guild.id, member.userId, mod.id);
		const inThreads = ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS;
		const manageThreads = ThreadPermissionFlags.MANAGE_THREADS;
		const channel = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.header(FEATURES, CAPABLE)
			.body({
				name: 'removals',
				type: ChannelTypes.GUILD_TEXT,
				permission_overwrites: [
					{id: guild.id, type: 0, allow: (Permissions.SEND_MESSAGES | inThreads).toString(), deny: '0'},
					{id: target.id, type: 0, allow: '0', deny: (Permissions.ADD_REACTIONS | manageThreads).toString()},
				],
			})
			.execute();
		const channelId = createChannelID(BigInt(channel.id));
		const stored = async () => {
			const overwrites = (await new ChannelRepository().findUnique(channelId))!.permissionOverwrites;
			return {
				everyone: overwrites.get(createRoleID(BigInt(guild.id))),
				target: overwrites.get(createRoleID(BigInt(target.id))),
			};
		};
		await createBuilder(harness, owner.token)
			.patch(`/channels/${channel.id}`)
			.body({permission_overwrites: []})
			.execute();
		let after = await stored();
		expect([after.everyone?.allow, after.everyone?.deny]).toEqual([inThreads, 0n]);
		expect([after.target?.allow, after.target?.deny]).toEqual([0n, manageThreads]);

		await setChannelThreadsConfig({enabled: false});
		await createBuilder(harness, member.token)
			.patch(`/channels/${channel.id}`)
			.header(FEATURES, CAPABLE)
			.body({permission_overwrites: []})
			.execute();
		after = await stored();
		expect([after.everyone?.allow, after.target?.deny]).toEqual([inThreads, manageThreads]);
		await createBuilder(harness, member.token)
			.delete(`/channels/${channel.id}/permissions/${target.id}`)
			.expect(204)
			.execute();
		expect((await stored()).target?.deny).toBe(manageThreads);
	});

	test('deleting a forum removes its thread-only index row', async () => {
		await setChannelThreadsConfig(ACTIVE);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Forum delete');
		const guildId = createGuildID(BigInt(guild.id));
		const text = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'seed', type: ChannelTypes.GUILD_TEXT})
			.execute();
		const repository = new ChannelRepository();
		const before = await repository.channelData.countGuildChannels(guildId);
		const source = (await repository.findUnique(createChannelID(BigInt(text.id))))!;
		const forumId = createChannelID(BigInt(text.id) + 1n);
		await repository.upsert({...source.toRow(), channel_id: forumId, type: ChannelTypes.GUILD_FORUM, name: 'forum'});
		expect(await repository.channelData.countGuildChannels(guildId)).toBe(before + 1);
		await createBuilder(harness, owner.token)
			.delete(`/channels/${forumId}`)
			.header(FEATURES, CAPABLE)
			.expect(204)
			.execute();
		expect(await repository.channelData.countGuildChannels(guildId)).toBe(before);
	});
	test('category sync and delete in a retired guild update forums without dispatching them', async () => {
		await setChannelThreadsConfig(ACTIVE);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Retired category');
		const guildId = createGuildID(BigInt(guild.id));
		const category = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'cat', type: ChannelTypes.GUILD_CATEGORY})
			.execute();
		const text = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'text', type: ChannelTypes.GUILD_TEXT, parent_id: category.id})
			.execute();
		const repository = new ChannelRepository();
		const source = (await repository.findUnique(createChannelID(BigInt(text.id))))!;
		const forumId = createChannelID(BigInt(text.id) + 1n);
		await repository.upsert({...source.toRow(), channel_id: forumId, type: ChannelTypes.GUILD_FORUM, name: 'forum'});
		expect(await fetchOne<GuildThreadStateRow>(FETCH_MARKER, {guild_id: guildId})).not.toBeNull();
		await setChannelThreadsConfig({enabled: false});
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		const updatedIds = () =>
			dispatchSpy.mock.calls
				.filter(([params]) => params.event === 'CHANNEL_UPDATE')
				.map(([params]) => (params.data as ChannelResponse).id);
		try {
			await createBuilder(harness, owner.token)
				.patch(`/channels/${category.id}`)
				.body({permission_overwrites: [{id: guild.id, type: 0, allow: '0', deny: Permissions.VIEW_CHANNEL.toString()}]})
				.execute();
			expect((await repository.findUnique(forumId))?.permissionOverwrites.size).toBe(1);
			expect(updatedIds()).toContain(text.id);
			expect(updatedIds()).not.toContain(forumId.toString());
			dispatchSpy.mockClear();
			await createBuilder(harness, owner.token).delete(`/channels/${category.id}`).expect(204).execute();
			expect((await repository.findUnique(forumId))?.parentId).toBeNull();
			expect(updatedIds()).toContain(text.id);
			expect(updatedIds()).not.toContain(forumId.toString());
		} finally {
			dispatchSpy.mockRestore();
		}
	});
	test('channel overwrite edits keep thread bits for clients that cannot write them', async () => {
		await setChannelThreadsConfig(ACTIVE);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Overwrite edits');
		const inThreads = ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS;
		const channel = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.header(FEATURES, CAPABLE)
			.body({
				name: 'edits',
				type: ChannelTypes.GUILD_TEXT,
				permission_overwrites: [
					{id: guild.id, type: 0, allow: (Permissions.SEND_MESSAGES | inThreads).toString(), deny: '0'},
				],
			})
			.execute();
		const channelId = createChannelID(BigInt(channel.id));
		const everyoneId = createRoleID(BigInt(guild.id));
		const storedAllow = async () =>
			(await new ChannelRepository().findUnique(channelId))?.permissionOverwrites.get(everyoneId)?.allow;
		await createBuilder(harness, owner.token)
			.patch(`/channels/${channel.id}`)
			.body({permission_overwrites: [{id: guild.id, type: 0, allow: Permissions.VIEW_CHANNEL.toString(), deny: '0'}]})
			.execute();
		expect(await storedAllow()).toBe(Permissions.VIEW_CHANNEL | inThreads);
		await createBuilder(harness, owner.token)
			.put(`/channels/${channel.id}/permissions/${guild.id}`)
			.body({type: 0, allow: Permissions.SEND_MESSAGES.toString(), deny: '0'})
			.expect(204)
			.execute();
		expect(await storedAllow()).toBe(Permissions.SEND_MESSAGES | inThreads);
		await createBuilder(harness, owner.token)
			.delete(`/channels/${channel.id}/permissions/${guild.id}`)
			.expect(204)
			.execute();
		expect(await storedAllow()).toBe(inThreads);
		await createBuilder(harness, owner.token)
			.delete(`/channels/${channel.id}/permissions/${guild.id}`)
			.header(FEATURES, CAPABLE)
			.expect(204)
			.execute();
		expect(await storedAllow()).toBeUndefined();
	});
	test('lock_permissions moves copy parent thread bits only for writers', async () => {
		await setChannelThreadsConfig(ACTIVE);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Lock sync');
		const inThreads = ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS;
		const category = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.header(FEATURES, CAPABLE)
			.body({
				name: 'locked',
				type: ChannelTypes.GUILD_CATEGORY,
				permission_overwrites: [
					{id: guild.id, type: 0, allow: (Permissions.SEND_MESSAGES | inThreads).toString(), deny: '0'},
				],
			})
			.execute();
		const everyoneId = createRoleID(BigInt(guild.id));
		const moveAndRead = async (capable: boolean) => {
			const child = await createBuilder<ChannelResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: capable ? 'capable' : 'legacy', type: ChannelTypes.GUILD_TEXT})
				.execute();
			const builder = createBuilder(harness, owner.token)
				.patch(`/guilds/${guild.id}/channels`)
				.body([{id: child.id, parent_id: category.id, lock_permissions: true}])
				.expect(204);
			await (capable ? builder.header(FEATURES, CAPABLE) : builder).execute();
			const stored = await new ChannelRepository().findUnique(createChannelID(BigInt(child.id)));
			return stored?.permissionOverwrites.get(everyoneId)?.allow;
		};
		expect(await moveAndRead(true)).toBe(Permissions.SEND_MESSAGES | inThreads);
		expect(await moveAndRead(false)).toBe(Permissions.SEND_MESSAGES);
	});
	test('template imports keep thread bits only for guilds created while active', async () => {
		const templated = async (
			token: string,
			everyonePermissions: bigint = Permissions.VIEW_CHANNEL | ThreadPermissionFlags.CREATE_PUBLIC_THREADS,
		) =>
			createBuilder<GuildResponse>(harness, token)
				.post('/guilds')
				.body({
					name: 'Templated',
					template: {
						name: 'Source',
						description: null,
						verification_level: 0,
						default_message_notifications: 0,
						explicit_content_filter: 0,
						system_channel_id: 1,
						afk_timeout: 300,
						system_channel_flags: 0,
						roles: [
							{
								id: 0,
								name: '@everyone',
								permissions: everyonePermissions.toString(),
							},
							{
								id: 2,
								name: 'mods',
								permissions: (Permissions.KICK_MEMBERS | ThreadPermissionFlags.MANAGE_THREADS).toString(),
							},
						],
						channels: [{id: 1, type: ChannelTypes.GUILD_TEXT, name: 'general', position: 0}],
					},
				})
				.execute();
		const controlOwner = await createTestAccount(harness);
		const controlGuild = await templated(controlOwner.token);
		const controlRoles = await listRoles(controlOwner.token, controlGuild.id, true);
		expect(controlRoles.map((role) => bits(role.permissions) & THREAD_PERMISSIONS)).toEqual([0n, 0n]);

		await setChannelThreadsConfig(ACTIVE);
		const owner = await createTestAccount(harness);
		const guild = await templated(owner.token);
		const roles = await listRoles(owner.token, guild.id, true);
		expect(bits(roles.find((role) => role.id === guild.id)?.permissions)).toBe(
			Permissions.VIEW_CHANNEL | ThreadPermissionFlags.CREATE_PUBLIC_THREADS,
		);
		expect(bits(roles.find((role) => role.name === 'mods')?.permissions)).toBe(
			Permissions.KICK_MEMBERS | ThreadPermissionFlags.MANAGE_THREADS,
		);
		const everyoneOf = async (everyonePermissions: bigint) => {
			const created = await templated(owner.token, everyonePermissions);
			const createdRoles = await listRoles(owner.token, created.id, true);
			return bits(createdRoles.find((role) => role.id === created.id)?.permissions);
		};
		expect(await everyoneOf(Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES)).toBe(
			Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES | DEFAULT_THREAD_PERMISSIONS,
		);
		expect(await everyoneOf(Permissions.VIEW_CHANNEL)).toBe(Permissions.VIEW_CHANNEL);
	});
});
