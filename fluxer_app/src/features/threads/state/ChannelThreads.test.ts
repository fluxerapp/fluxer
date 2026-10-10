// SPDX-License-Identifier: AGPL-3.0-or-later

import {Channel, type ChannelWire} from '@app/features/channel/models/Channel';
import {ChannelTypes, MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import {observable, runInAction} from 'mobx';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const channelStore = new Map<string, Channel>();
const viewable = new Set<string>();
const moderatorParents = new Set<string>();
const navigation = observable({
	guildId: null as string | null,
	channelId: null as string | null,
	threadId: null as string | null,
});
const selectedGuild = observable({selectedGuildId: null as string | null});
const updateGuildSubscriptions = vi.fn();
const gateway = observable({isReady: false, socket: {isConnected: () => true, updateGuildSubscriptions}});

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: unknown) => descriptor,
	t: (descriptor: unknown) => descriptor,
}));
vi.mock('@app/features/app/state/RuntimeConfig', () => ({default: {localInstanceDomain: 'fluxer.test'}}));
vi.mock('@app/features/user/state/Users', () => ({
	default: {getUser: () => undefined, cacheUsers: () => {}, getCurrentUser: () => ({id: 'me', mfaEnabled: true})},
}));
vi.mock('@app/features/auth/state/Authentication', () => ({default: {currentUserId: 'me'}}));
vi.mock('@app/app/Routes', () => ({
	Routes: {guildChannel: (guildId: string, channelId?: string) => `/channels/${guildId}/${channelId ?? ''}`},
}));
vi.mock('@app/features/channel/events/ChannelDelete', () => ({
	cleanupChannelLocalState: vi.fn((channel: {id: string}) => channelStore.delete(channel.id)),
}));
vi.mock('@app/features/channel/state/Channels', () => ({
	default: {
		getChannel: (id: string) => channelStore.get(id),
		upsertThread: (wire: ChannelWire) => channelStore.set(wire.id, new Channel(wire)),
		removeThread: (id: string) => channelStore.delete(id),
		handleChannelCreate: ({channel}: {channel: Channel | ChannelWire}) =>
			channelStore.set(channel.id, channel instanceof Channel ? channel : new Channel(channel)),
		getGuildChannels: (guildId: string) =>
			[...channelStore.values()].filter((channel) => channel.guildId === guildId && !channel.isThread()),
	},
}));
vi.mock('@app/features/guild/state/GuildReadState', () => ({default: {handleGenericUpdate: vi.fn()}}));
vi.mock('@app/features/read_state/state/ReadStates', () => ({default: {handleChannelCreate: vi.fn()}}));
vi.mock('@app/features/messaging/state/MessagingMessages', () => ({
	default: {handleCleanup: vi.fn(), handleGuildThreadsPurged: vi.fn()},
}));
vi.mock('@app/features/ui/commands/ContextMenuCommands', () => ({close: vi.fn()}));
vi.mock('@app/features/ui/commands/PopoutCommands', () => ({closeAll: vi.fn()}));
vi.mock('@app/features/guild/state/Guilds', () => ({default: {getGuild: () => ({ownerId: 'owner', mfaLevel: 0})}}));
vi.mock('@app/features/member/state/GuildMembers', () => ({
	default: {getMember: () => null, hydrateOrRefresh: () => {}},
}));
vi.mock('@app/features/permissions/state/Permission', () => ({
	default: {
		can: (_permission: bigint, channel: {id: string}) => viewable.has(channel.id),
		getChannelPermissions: (id: string) => {
			if (!viewable.has(id)) return 0n;
			const base = Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY | Permissions.SEND_MESSAGES;
			return moderatorParents.has(id) ? base | ThreadPermissionFlags.MANAGE_THREADS : base;
		},
		handleChannelUpdate: vi.fn(),
	},
}));
vi.mock('@app/features/navigation/state/Navigation', () => ({default: navigation}));
vi.mock('@app/features/navigation/state/SelectedGuild', () => ({default: selectedGuild}));
vi.mock('@app/features/navigation/utils/RouterUtils', () => ({replaceWith: vi.fn(), transitionTo: vi.fn()}));
vi.mock('@app/features/gateway/transport/GatewayConnection', () => ({default: gateway}));
vi.mock('@app/features/user/utils/DateFormatting', () => ({getFormattedFullDate: () => ''}));
vi.mock('@app/features/moderation/state/LocalUserSpamOverride', () => ({
	default: {isUserMarkedAsSpammer: () => false},
}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: vi.fn(), post: vi.fn(), put: vi.fn(), patch: vi.fn(), delete: vi.fn()},
}));

const {default: ChannelThreads} = await import('@app/features/threads/state/ChannelThreads');
const {default: ThreadGuilds} = await import('@app/features/threads/state/ThreadGuilds');
const {default: ThreadMemberships} = await import('@app/features/threads/state/ThreadMemberships');
const {default: ThreadRoster} = await import('@app/features/threads/state/ThreadRoster');
const {default: ThreadSubscriptions} = await import('@app/features/threads/state/ThreadSubscriptions');
const {isSyncExcludedChannelId} = await import('@app/features/threads/utils/SyncedPreferenceGuard');
const {cleanupChannelLocalState} = await import('@app/features/channel/events/ChannelDelete');
const RouterUtils = await import('@app/features/navigation/utils/RouterUtils');
const {http} = await import('@app/features/platform/transport/RestTransport');
const {isNewMessageGroup} = await import('@app/features/messaging/utils/MessageGroupingUtils');

const GUILD = '1500000000000000000';
const PARENT = '1500000000000000001';
const OTHER_PARENT = '1500000000000000002';
const FORUM = '1500000000000000003';
const THREAD_A = '1500000000000000010';
const THREAD_B = '1500000000000000011';
const PRIVATE_THREAD = '1500000000000000012';
const MEMBER = {join_timestamp: '2026-09-28T00:00:00.000Z', flags: 0};

function metadata(overrides: Partial<{archived: boolean; locked: boolean}> = {}) {
	return {
		archived: false,
		locked: false,
		auto_archive_duration: 4320,
		archive_timestamp: '2026-09-28T00:00:00.000Z',
		create_timestamp: '2026-09-28T00:00:00.000Z',
		...overrides,
	};
}

function thread(id: string, overrides: Partial<ChannelWire> = {}): ChannelWire {
	return {
		id,
		guild_id: GUILD,
		parent_id: PARENT,
		type: ChannelTypes.PUBLIC_THREAD,
		name: `thread-${id}`,
		last_message_id: id,
		thread_metadata: metadata(),
		message_count: 0,
		total_message_sent: 0,
		...overrides,
	};
}

function seedParents(): void {
	channelStore.set(PARENT, new Channel({id: PARENT, guild_id: GUILD, type: ChannelTypes.GUILD_TEXT, name: 'general'}));
	channelStore.set(OTHER_PARENT, new Channel({id: OTHER_PARENT, guild_id: GUILD, type: ChannelTypes.GUILD_TEXT}));
	channelStore.set(FORUM, new Channel({id: FORUM, guild_id: GUILD, type: ChannelTypes.GUILD_FORUM, name: 'forum'}));
	viewable.add(PARENT);
	viewable.add(OTHER_PARENT);
	viewable.add(FORUM);
}

function guildObject(threads?: Array<ChannelWire>) {
	return {
		id: GUILD,
		properties: {},
		channels: [],
		emojis: [],
		members: [],
		member_count: 0,
		roles: [],
		joined_at: '2026-09-28T00:00:00.000Z',
		...(threads ? {threads} : {}),
	} as unknown as Parameters<typeof ThreadGuilds.handleGuild>[0];
}

beforeEach(() => {
	channelStore.clear();
	viewable.clear();
	moderatorParents.clear();
	ChannelThreads.handleGatewayReady([]);
	ThreadGuilds.reset();
	runInAction(() => {
		navigation.guildId = null;
		navigation.channelId = null;
		navigation.threadId = null;
		selectedGuild.selectedGuildId = null;
		gateway.isReady = false;
	});
	updateGuildSubscriptions.mockClear();
	vi.mocked(cleanupChannelLocalState).mockClear();
	vi.mocked(RouterUtils.replaceWith).mockClear();
	seedParents();
});

afterEach(() => {
	vi.mocked(http.delete).mockClear();
});

describe('ChannelThreads ingest and upsert', () => {
	beforeEach(() => {
		ThreadGuilds.handleGuild(guildObject([]));
	});

	it('indexes READY threads by parent and records the viewer membership', () => {
		ChannelThreads.handleGatewayReady([
			{id: GUILD, threads: [thread(THREAD_A, {member: MEMBER}), thread(THREAD_B, {parent_id: OTHER_PARENT})]},
		]);
		expect(ChannelThreads.getThreadIdsForParent(PARENT)).toEqual([THREAD_A]);
		expect(ChannelThreads.getThreadIdsForParent(OTHER_PARENT)).toEqual([THREAD_B]);
		expect(ChannelThreads.getGuildThreadIds(GUILD)).toEqual([THREAD_A, THREAD_B]);
		expect(ThreadMemberships.isMember(THREAD_A)).toBe(true);
		expect(ThreadMemberships.isMember(THREAD_B)).toBe(false);
		expect(channelStore.get(THREAD_A)?.isThread()).toBe(true);
	});

	it('never walks the last message id back on an update', () => {
		ChannelThreads.upsert(thread(THREAD_A, {last_message_id: '1500000000000000099'}));
		ChannelThreads.upsert(thread(THREAD_A, {last_message_id: '1500000000000000050', name: 'renamed'}));
		expect(channelStore.get(THREAD_A)?.lastMessageId).toBe('1500000000000000099');
		expect(channelStore.get(THREAD_A)?.name).toBe('renamed');
	});

	it('keeps archived threads out of the active and sidebar lists unless open', () => {
		ChannelThreads.upsert(thread(THREAD_A, {member: MEMBER}));
		ChannelThreads.upsert(thread(THREAD_A, {thread_metadata: metadata({archived: true})}));
		expect(ChannelThreads.getThreadsForParent(PARENT).map((entry) => entry.id)).toEqual([THREAD_A]);
		expect(ChannelThreads.getActiveThreadsForParent(PARENT)).toEqual([]);
		expect(ChannelThreads.getSidebarThreads(PARENT, null)).toEqual([]);
		expect(ChannelThreads.getSidebarThreads(PARENT, THREAD_A).map((entry) => entry.id)).toEqual([THREAD_A]);
	});

	it('filters every getter on parent VIEW_CHANNEL', () => {
		ChannelThreads.upsert(thread(THREAD_A, {member: MEMBER}));
		viewable.delete(PARENT);
		expect(ChannelThreads.getThread(THREAD_A)).toBeUndefined();
		expect(ChannelThreads.getThreadsForParent(PARENT)).toEqual([]);
		expect(ChannelThreads.getGuildThreads(GUILD)).toEqual([]);
		expect(ChannelThreads.getSidebarThreads(PARENT, THREAD_A)).toEqual([]);
	});

	it('shows private threads only to members and thread moderators', () => {
		ChannelThreads.upsert(thread(PRIVATE_THREAD, {type: ChannelTypes.PRIVATE_THREAD}));
		expect(ChannelThreads.getThread(PRIVATE_THREAD)).toBeUndefined();
		moderatorParents.add(PARENT);
		expect(ChannelThreads.getThread(PRIVATE_THREAD)?.id).toBe(PRIVATE_THREAD);
		moderatorParents.delete(PARENT);
		ChannelThreads.upsert(thread(PRIVATE_THREAD, {type: ChannelTypes.PRIVATE_THREAD, member: MEMBER}));
		expect(ChannelThreads.getThread(PRIVATE_THREAD)?.id).toBe(PRIVATE_THREAD);
	});

	it('ingests the thread attached to a loaded source message', () => {
		ChannelThreads.ingestMessageThreads([
			{
				id: THREAD_A,
				channel_id: PARENT,
				type: MessageTypes.DEFAULT,
				thread: thread(THREAD_A),
			} as unknown as Parameters<typeof ChannelThreads.ingestMessageThreads>[0][number],
		]);
		expect(ChannelThreads.getThread(THREAD_A)?.parentId).toBe(PARENT);
	});
});

describe('ChannelThreads cascades', () => {
	beforeEach(() => {
		ThreadGuilds.handleGuild(guildObject([]));
	});

	it('cleans up every thread of a deleted parent', () => {
		ChannelThreads.upsert(thread(THREAD_A, {member: MEMBER}));
		ChannelThreads.upsert(thread(THREAD_B));
		ChannelThreads.handleParentDelete(PARENT);
		expect(
			vi
				.mocked(cleanupChannelLocalState)
				.mock.calls.map(([channel]) => channel.id)
				.sort(),
		).toEqual([THREAD_A, THREAD_B]);
		expect(ChannelThreads.getThreadIdsForParent(PARENT)).toEqual([]);
		expect(ThreadMemberships.isMember(THREAD_A)).toBe(false);
	});

	it('drops a deleted thread and its membership', () => {
		ChannelThreads.upsert(thread(THREAD_A, {member: MEMBER}));
		ChannelThreads.handleThreadDelete({id: THREAD_A, guild_id: GUILD, parent_id: PARENT, type: 11});
		expect(ChannelThreads.hasThread(THREAD_A)).toBe(false);
		expect(ThreadMemberships.isMember(THREAD_A)).toBe(false);
		expect(channelStore.has(THREAD_A)).toBe(false);
	});

	it('returns a full view of a deleted thread to its parent before the generic fallback', () => {
		ChannelThreads.upsert(thread(THREAD_A));
		runInAction(() => {
			navigation.guildId = GUILD;
			navigation.channelId = THREAD_A;
		});
		const replaceWith = vi.mocked(RouterUtils.replaceWith);
		const cleanup = vi.mocked(cleanupChannelLocalState);
		ChannelThreads.handleThreadDelete({id: THREAD_A, guild_id: GUILD, parent_id: PARENT, type: 11});
		expect(replaceWith).toHaveBeenCalledWith(`/channels/${GUILD}/${PARENT}`);
		expect(replaceWith.mock.invocationCallOrder[0]).toBeLessThan(cleanup.mock.invocationCallOrder[0]);
	});

	it('removes stale active threads only for the synced parents', () => {
		ChannelThreads.upsert(thread(THREAD_A));
		ChannelThreads.upsert(thread(THREAD_B, {parent_id: OTHER_PARENT}));
		ChannelThreads.handleListSync(
			{guild_id: GUILD, channel_ids: [PARENT], threads: [], members: []},
			new Set<string>(),
		);
		expect(ChannelThreads.hasThread(THREAD_A)).toBe(false);
		expect(ChannelThreads.hasThread(THREAD_B)).toBe(true);
	});

	it('drops stale memberships on a guild resync but keeps the threads', () => {
		ChannelThreads.upsert(thread(THREAD_A, {member: MEMBER}));
		ChannelThreads.upsert(thread(THREAD_B, {member: MEMBER, thread_metadata: metadata({archived: true})}));
		ChannelThreads.ingestGuildThreads(GUILD, []);
		expect(ThreadMemberships.isMember(THREAD_A)).toBe(false);
		expect(ThreadMemberships.isMember(THREAD_B)).toBe(true);
		expect(ChannelThreads.hasThread(THREAD_A)).toBe(true);
	});

	it('keeps the thread that is open in a view through a list sync', () => {
		ChannelThreads.upsert(thread(THREAD_A));
		ChannelThreads.handleListSync({guild_id: GUILD, threads: [], members: []}, new Set([THREAD_A]));
		expect(ChannelThreads.hasThread(THREAD_A)).toBe(true);
	});
});

describe('ThreadGuilds flip', () => {
	it('activates from the threads key and purges on the kill', () => {
		ThreadGuilds.handleGuild(guildObject([thread(THREAD_A, {member: MEMBER})]));
		ChannelThreads.ingestGuildThreads(GUILD, [thread(THREAD_A, {member: MEMBER})]);
		expect(ThreadGuilds.isActive(GUILD)).toBe(true);
		runInAction(() => {
			navigation.guildId = GUILD;
			navigation.channelId = PARENT;
			navigation.threadId = THREAD_A;
		});
		ThreadGuilds.handleGuild(guildObject());
		expect(ThreadGuilds.isActive(GUILD)).toBe(false);
		const cleaned = vi.mocked(cleanupChannelLocalState).mock.calls.map(([channel]) => channel.id);
		expect(cleaned).toContain(THREAD_A);
		expect(cleaned).toContain(FORUM);
		expect(cleaned).not.toContain(PARENT);
		expect(ChannelThreads.getGuildThreadIds(GUILD)).toEqual([]);
		expect(ThreadMemberships.isMember(THREAD_A)).toBe(false);
		expect(vi.mocked(RouterUtils.replaceWith)).toHaveBeenCalledWith(`/channels/${GUILD}/${PARENT}`);
		expect(ThreadGuilds.isPurgedEvent({channel_id: THREAD_A})).toBe(true);
		expect(ThreadGuilds.isPurgedEvent({channel_id: PARENT})).toBe(false);
		expect(vi.mocked(http.delete)).not.toHaveBeenCalled();
		ThreadGuilds.handleGuild(guildObject([]));
		expect(ThreadGuilds.isPurgedEvent({channel_id: THREAD_A})).toBe(false);
	});

	it('routes a forum post panel to the guild root on the kill', () => {
		const post = thread(THREAD_B, {parent_id: FORUM, member: MEMBER});
		ThreadGuilds.handleGuild(guildObject([post]));
		ChannelThreads.ingestGuildThreads(GUILD, [post]);
		runInAction(() => {
			navigation.guildId = GUILD;
			navigation.channelId = FORUM;
			navigation.threadId = THREAD_B;
		});
		ThreadGuilds.handleGuild(guildObject());
		expect(vi.mocked(cleanupChannelLocalState).mock.calls.map(([channel]) => channel.id)).toContain(FORUM);
		expect(vi.mocked(RouterUtils.replaceWith)).toHaveBeenLastCalledWith(`/channels/${GUILD}/`);
	});

	it('routes a full view forum post to the guild root on the kill', () => {
		const post = thread(THREAD_B, {parent_id: FORUM, member: MEMBER});
		ThreadGuilds.handleGuild(guildObject([post]));
		ChannelThreads.ingestGuildThreads(GUILD, [post]);
		runInAction(() => {
			navigation.guildId = GUILD;
			navigation.channelId = THREAD_B;
		});
		ThreadGuilds.handleGuild(guildObject());
		expect(vi.mocked(RouterUtils.replaceWith)).toHaveBeenLastCalledWith(`/channels/${GUILD}/`);
	});

	it('routes a full view text thread to its parent on the kill', () => {
		const child = thread(THREAD_A, {member: MEMBER});
		ThreadGuilds.handleGuild(guildObject([child]));
		ChannelThreads.ingestGuildThreads(GUILD, [child]);
		runInAction(() => {
			navigation.guildId = GUILD;
			navigation.channelId = THREAD_A;
		});
		ThreadGuilds.handleGuild(guildObject());
		expect(vi.mocked(RouterUtils.replaceWith)).toHaveBeenLastCalledWith(`/channels/${GUILD}/${PARENT}`);
	});

	it('routes a thread view to the guild root when a new READY drops its forum', () => {
		runInAction(() => {
			navigation.guildId = GUILD;
			navigation.channelId = FORUM;
			navigation.threadId = THREAD_B;
		});
		const forum = {id: FORUM, guild_id: GUILD, type: ChannelTypes.GUILD_FORUM};
		ThreadGuilds.handleGatewayReady([{...guildObject(), channels: [forum]}]);
		expect(vi.mocked(RouterUtils.replaceWith)).toHaveBeenLastCalledWith(`/channels/${GUILD}/`);
		const parent = {id: PARENT, guild_id: GUILD, type: ChannelTypes.GUILD_TEXT};
		runInAction(() => {
			navigation.channelId = PARENT;
		});
		ThreadGuilds.handleGatewayReady([{...guildObject(), channels: [parent]}]);
		expect(vi.mocked(RouterUtils.replaceWith)).toHaveBeenLastCalledWith(`/channels/${GUILD}/${PARENT}`);
	});

	it('purges a guild that is missing from a new READY', () => {
		ThreadGuilds.handleGuild(guildObject([thread(THREAD_A, {member: MEMBER})]));
		ChannelThreads.ingestGuildThreads(GUILD, [thread(THREAD_A, {member: MEMBER})]);
		ThreadGuilds.handleGatewayReady([]);
		expect(ThreadGuilds.isActive(GUILD)).toBe(false);
		const cleaned = vi.mocked(cleanupChannelLocalState).mock.calls.map(([channel]) => channel.id);
		expect(cleaned).toContain(FORUM);
		expect(ThreadGuilds.isPurgedEvent({channel_id: THREAD_A})).toBe(true);
	});

	it('ignores unavailable guild stubs and deletes for the gate', () => {
		ThreadGuilds.handleGuild(guildObject([]));
		ThreadGuilds.handleGatewayReady([
			{id: GUILD, unavailable: true} as unknown as Parameters<typeof ThreadGuilds.handleGuild>[0],
		]);
		ThreadGuilds.handleGuildDelete(GUILD, true);
		expect(ThreadGuilds.isActive(GUILD)).toBe(true);
		ThreadGuilds.handleGuildDelete(GUILD, false);
		expect(ThreadGuilds.isActive(GUILD)).toBe(false);
	});
});

describe('control arm', () => {
	it('stays inert for guilds without the threads key', async () => {
		ThreadGuilds.handleGatewayReady([guildObject()]);
		runInAction(() => {
			selectedGuild.selectedGuildId = GUILD;
			gateway.isReady = true;
		});
		ThreadSubscriptions.handleConnectionReady();
		await Promise.resolve();
		expect(ThreadGuilds.anyActive).toBe(false);
		expect(ThreadGuilds.isPurgedEvent({channel_id: PARENT})).toBe(false);
		expect(updateGuildSubscriptions).not.toHaveBeenCalled();
		expect(vi.mocked(http.get)).not.toHaveBeenCalled();
		expect(vi.mocked(http.post)).not.toHaveBeenCalled();
		ThreadGuilds.handleGuild(guildObject([]));
		await Promise.resolve();
		expect(updateGuildSubscriptions).toHaveBeenCalledWith({
			subscriptions: {[GUILD]: {threads: true, thread_member_lists: []}},
		});
	});

	it('sends thread member lists only for subscribed threads of the active guild', async () => {
		ThreadGuilds.handleGuild(guildObject([]));
		runInAction(() => {
			selectedGuild.selectedGuildId = GUILD;
			gateway.isReady = true;
		});
		ThreadSubscriptions.handleConnectionReady();
		const release = ThreadRoster.subscribe(GUILD, THREAD_A);
		await Promise.resolve();
		expect(updateGuildSubscriptions).toHaveBeenLastCalledWith({
			subscriptions: {[GUILD]: {threads: true, thread_member_lists: [THREAD_A]}},
		});
		release();
		await Promise.resolve();
	});

	it('switches thread member lists in a single request', async () => {
		ThreadGuilds.handleGuild(guildObject([]));
		runInAction(() => {
			selectedGuild.selectedGuildId = GUILD;
			gateway.isReady = true;
		});
		ThreadSubscriptions.handleConnectionReady();
		const releaseA = ThreadRoster.subscribe(GUILD, THREAD_A);
		await Promise.resolve();
		updateGuildSubscriptions.mockClear();
		releaseA();
		const releaseB = ThreadRoster.subscribe(GUILD, THREAD_B);
		await Promise.resolve();
		expect(updateGuildSubscriptions).toHaveBeenCalledTimes(1);
		expect(updateGuildSubscriptions).toHaveBeenCalledWith({
			subscriptions: {[GUILD]: {threads: true, thread_member_lists: [THREAD_B]}},
		});
		releaseB();
		await Promise.resolve();
	});

	it('clears the previous guild thread member lists when the selected guild changes', async () => {
		const otherGuild = '1500000000000000099';
		ThreadGuilds.handleGuild(guildObject([]));
		ThreadGuilds.handleGuild({...guildObject([]), id: otherGuild});
		runInAction(() => {
			selectedGuild.selectedGuildId = GUILD;
			gateway.isReady = true;
		});
		ThreadSubscriptions.handleConnectionReady();
		const release = ThreadRoster.subscribe(GUILD, THREAD_A);
		await Promise.resolve();
		updateGuildSubscriptions.mockClear();
		runInAction(() => {
			selectedGuild.selectedGuildId = otherGuild;
		});
		await Promise.resolve();
		expect(updateGuildSubscriptions).toHaveBeenCalledWith({
			subscriptions: {
				[GUILD]: {thread_member_lists: []},
				[otherGuild]: {threads: true, thread_member_lists: []},
			},
		});
		release();
		await Promise.resolve();
	});

	it('serializes a control channel without any new keys', () => {
		const wire = {
			id: PARENT,
			guild_id: GUILD,
			type: ChannelTypes.GUILD_TEXT,
			name: 'general',
			topic: null,
			position: 1,
			parent_id: null,
			last_message_id: null,
			permission_overwrites: [],
			nsfw: false,
			rate_limit_per_user: 0,
		};
		const keys = Object.keys(new Channel(wire).toJSON()).sort();
		expect(keys).toEqual(
			[
				'bitrate',
				'content_warning_level',
				'content_warning_text',
				'guild_id',
				'icon',
				'id',
				'last_message_id',
				'last_pin_timestamp',
				'name',
				'nicks',
				'nsfw',
				'nsfw_override',
				'owner_id',
				'parent_id',
				'permission_overwrites',
				'position',
				'rate_limit_per_user',
				'recipients',
				'rtc_p2p',
				'rtc_region',
				'topic',
				'type',
				'url',
				'user_limit',
				'voice_connection_limit',
			].sort(),
		);
		expect(new Channel(wire).threadFields).toBeNull();
	});

	it('refuses a thread for a guild that is not active', () => {
		expect(ChannelThreads.upsert(thread(THREAD_A, {member: MEMBER}))).toBeUndefined();
		expect(channelStore.has(THREAD_A)).toBe(false);
		expect(ChannelThreads.hasThread(THREAD_A)).toBe(false);
		expect(ThreadMemberships.isMember(THREAD_A)).toBe(false);
	});

	it('sends no thread keys on a new connection until READY was ingested', () => {
		ThreadGuilds.handleGuild(guildObject([]));
		runInAction(() => {
			selectedGuild.selectedGuildId = GUILD;
			gateway.isReady = true;
		});
		expect(updateGuildSubscriptions).not.toHaveBeenCalled();
		ThreadGuilds.handleGatewayReady([guildObject()]);
		ThreadSubscriptions.handleConnectionReady();
		expect(updateGuildSubscriptions).not.toHaveBeenCalled();
	});

	it('refuses thread and forum ids for synced preferences only', () => {
		ThreadGuilds.handleGuild(guildObject([]));
		ChannelThreads.upsert(thread(THREAD_A));
		expect(isSyncExcludedChannelId(THREAD_A)).toBe(true);
		expect(isSyncExcludedChannelId(FORUM)).toBe(true);
		expect(isSyncExcludedChannelId(PARENT)).toBe(false);
		expect(isSyncExcludedChannelId('1500000000000000999')).toBe(false);
	});

	it('clears the previous guild thread member lists after a guild switch during a reconnect', async () => {
		const otherGuild = '1500000000000000099';
		ThreadGuilds.handleGuild(guildObject([]));
		ThreadGuilds.handleGuild({...guildObject([]), id: otherGuild});
		runInAction(() => {
			selectedGuild.selectedGuildId = GUILD;
			gateway.isReady = true;
		});
		ThreadSubscriptions.handleConnectionReady();
		const release = ThreadRoster.subscribe(GUILD, THREAD_A);
		await Promise.resolve();
		runInAction(() => {
			gateway.isReady = false;
		});
		release();
		runInAction(() => {
			selectedGuild.selectedGuildId = otherGuild;
		});
		await Promise.resolve();
		updateGuildSubscriptions.mockClear();
		runInAction(() => {
			gateway.isReady = true;
		});
		ThreadSubscriptions.handleConnectionReady();
		await Promise.resolve();
		expect(updateGuildSubscriptions).toHaveBeenCalledTimes(1);
		expect(updateGuildSubscriptions).toHaveBeenCalledWith({
			subscriptions: {
				[GUILD]: {thread_member_lists: []},
				[otherGuild]: {threads: true, thread_member_lists: []},
			},
		});
	});
});

describe('thread roster', () => {
	const ONLINE = '1600000000000000001';
	const IDLE = '1600000000000000002';
	const OFFLINE = '1600000000000000003';

	function listUpdate(members: Array<{user_id: string; presence: Record<string, unknown>}>) {
		return {
			guild_id: GUILD,
			thread_id: THREAD_A,
			members: members.map((entry) => ({...entry, join_timestamp: null, flags: 0, member: null})),
		};
	}

	it('keeps each member status and custom status from the list update', () => {
		const release = ThreadRoster.subscribe(GUILD, THREAD_A);
		ThreadRoster.handleListUpdate(
			listUpdate([
				{user_id: ONLINE, presence: {status: 'online', afk: false, mobile: true, custom_status: null}},
				{
					user_id: IDLE,
					presence: {
						status: 'idle',
						afk: true,
						mobile: false,
						custom_status: {emoji_id: '1471401583408697449', emoji_name: 'nahua_smug', text: 'im spooky goop'},
					},
				},
				{user_id: OFFLINE, presence: {status: 'offline', afk: false, mobile: false}},
			]),
		);
		expect(ThreadRoster.getMembers(THREAD_A)).toEqual([
			{userId: ONLINE, joinTimestamp: null, status: 'online', mobile: true, customStatus: null},
			{
				userId: IDLE,
				joinTimestamp: null,
				status: 'idle',
				mobile: false,
				customStatus: expect.objectContaining({text: 'im spooky goop', emojiName: 'nahua_smug'}),
			},
			{userId: OFFLINE, joinTimestamp: null, status: 'offline', mobile: false, customStatus: null},
		]);
		release();
	});

	it('replaces the roster on every list update and ignores updates after release', () => {
		const release = ThreadRoster.subscribe(GUILD, THREAD_A);
		ThreadRoster.handleListUpdate(
			listUpdate([
				{user_id: ONLINE, presence: {status: 'online'}},
				{user_id: IDLE, presence: {status: 'idle'}},
			]),
		);
		ThreadRoster.handleListUpdate(listUpdate([{user_id: ONLINE, presence: {status: 'dnd'}}]));
		expect(ThreadRoster.getMembers(THREAD_A)).toEqual([
			{userId: ONLINE, joinTimestamp: null, status: 'dnd', mobile: false, customStatus: null},
		]);
		release();
		ThreadRoster.handleListUpdate(listUpdate([{user_id: ONLINE, presence: {status: 'online'}}]));
		expect(ThreadRoster.getMembers(THREAD_A)).toBeUndefined();
	});
});

describe('control arm message grouping', () => {
	const SYSTEM_TYPES = [1, 2, 3, 4, 5, 6, 7, MessageTypes.THREAD_CREATED, MessageTypes.CLIENT_SYSTEM];

	function fakeMessage(type: number, offsetMs: number) {
		return {
			type,
			author: {id: 'author', username: 'author'},
			webhookId: undefined,
			timestamp: new Date(Date.UTC(2026, 8, 28, 12, 0, 0) + offsetMs),
			mentions: [],
			mentionRoles: [],
			mentionEveryone: false,
			hasFlag: () => false,
			isUserMessage: () =>
				type === MessageTypes.DEFAULT || type === MessageTypes.REPLY || type === MessageTypes.CLIENT_SYSTEM,
		} as unknown as Parameters<typeof isNewMessageGroup>[2];
	}

	it("keeps today's grouping for system, reply and client messages while inactive", () => {
		expect(ThreadGuilds.anyActive).toBe(false);
		for (const type of SYSTEM_TYPES) {
			expect(isNewMessageGroup(undefined, fakeMessage(MessageTypes.DEFAULT, 0), fakeMessage(type, 1000))).toBe(true);
			expect(isNewMessageGroup(undefined, fakeMessage(type, 0), fakeMessage(MessageTypes.DEFAULT, 1000))).toBe(true);
			expect(isNewMessageGroup(undefined, fakeMessage(type, 0), fakeMessage(type, 1000))).toBe(false);
		}
		expect(
			isNewMessageGroup(undefined, fakeMessage(MessageTypes.DEFAULT, 0), fakeMessage(MessageTypes.REPLY, 1000)),
		).toBe(true);
		expect(
			isNewMessageGroup(undefined, fakeMessage(MessageTypes.DEFAULT, 0), fakeMessage(MessageTypes.DEFAULT, 1000)),
		).toBe(false);
	});
});
