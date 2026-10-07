// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelID,
	createChannelID,
	createGuildID,
	createMessageID,
	createUserID,
	type UserID,
} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {syncChannelThreadsConfig} from '@app/api/experiment/ChannelThreadsGate';
import type {GatewayMentionSourceEntry, IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {ThreadState} from '@app/api/models/ThreadState';
import type {ReadStateMentionUpdate} from '@app/api/read_state/IReadStateRepository';
import type {ReadStateService} from '@app/api/read_state/ReadStateService';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import handleMentionChunk from '@app/api/worker/tasks/HandleMentionChunk';
import handleMentions from '@app/api/worker/tasks/HandleMentions';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	MAX_ROLE_MENTION_THREAD_ADDS,
	MAX_THREAD_MEMBERS,
	ReadStateFlags,
	ServerMessageFlags,
} from '@fluxer/constants/src/ThreadConstants';
import {MaxThreadMembersError} from '@fluxer/errors/src/domains/channel/MaxThreadMembersError';
import {ChannelThreadsConfigSchema} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@app/api/channel/services/message/MessageGatewayDispatch', () => ({
	buildBroadcastMessageData: async ({message}: {message: {id: bigint; flags: number}}) => ({
		id: message.id.toString(),
		flags: message.flags,
	}),
}));

vi.mock('@app/api/channel/services/thread/ThreadDispatch', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/api/channel/services/thread/ThreadDispatch')>()),
	threadMembersUpdateEvent: (
		view: {state: {appliedTags: Array<bigint>}},
		change: {added?: Array<{userId: UserID}>},
	) => ({
		event: 'THREAD_MEMBERS_UPDATE',
		data: {
			added: (change.added ?? []).map((member) => member.userId.toString()),
			...(view.state.appliedTags.length > 0 ? {applied_tags: view.state.appliedTags.map(String)} : {}),
		},
	}),
}));

const GUILD = createGuildID(4000n);
const THREAD = createChannelID(5000n);
const PARENT = createChannelID(4500n);
const MESSAGE = createMessageID(6000n);
const AUTHOR = createUserID(1n);

interface EnqueuedChunk {
	thread?: boolean;
	mentions: Array<{userId: string; direct: boolean; role: boolean; everyone: boolean}>;
}

function activate(active: boolean): void {
	const config = ChannelThreadsConfigSchema.parse(
		active ? {enabled: true, guild_basis_points: 10000, user_basis_points: 10000} : {},
	);
	syncChannelThreadsConfig(JSON.stringify(config), (raw) => ChannelThreadsConfigSchema.parse(JSON.parse(raw ?? '{}')));
}

function createHarness(params: {
	channelType?: number;
	members?: Array<string>;
	isPrivate?: boolean;
	memberCount?: number;
	addRejected?: boolean;
	joinedConcurrently?: Array<string>;
	forumTags?: {applied: Array<bigint>; live: Array<bigint>};
	mentions: Array<GatewayMentionSourceEntry>;
}) {
	const members = new Set(params.members ?? []);
	const added: Array<string> = [];
	const dispatched: Array<{event: string; data: unknown}> = [];
	const upserts: Array<number> = [];
	const message = {
		id: MESSAGE,
		flags: 0,
		mentionEveryone: true,
		mentionedRoleIds: new Set(),
		mentionedUserIds: new Set(),
	};
	const joinedConcurrently = new Set(params.joinedConcurrently ?? []);
	const state = new ThreadState({
		thread_id: THREAD,
		guild_id: GUILD,
		parent_id: PARENT,
		type: params.isPrivate ? ChannelTypes.PRIVATE_THREAD : ChannelTypes.PUBLIC_THREAD,
		archived: false,
		locked: false,
		invitable: null,
		auto_archive_duration: 1440,
		archive_timestamp: null,
		created_at: new Date(0),
		flags: 0,
		applied_tags: params.forumTags?.applied ?? null,
		member_count: params.memberCount ?? members.size,
		member_ids_preview: null,
		has_starter: false,
		state_version: 1,
	});
	const parentType = params.forumTags ? ChannelTypes.GUILD_FORUM : ChannelTypes.GUILD_TEXT;
	const channel = {
		id: THREAD,
		guildId: GUILD,
		parentId: PARENT,
		type: params.channelType ?? ChannelTypes.PUBLIC_THREAD,
	};
	const channelRepository = {
		getMessage: async () => message,
		findUnique: async () => channel,
		threads: {
			getState: async () => state,
			getMembers: async (_threadId: ChannelID, userIds: Array<UserID>) =>
				userIds.filter((userId) => members.has(userId.toString())).map((userId) => ({userId})),
			addMembers: async (_threadId: ChannelID, entries: Array<{userId: UserID}>) => {
				if (params.addRejected) throw new MaxThreadMembersError(MAX_THREAD_MEMBERS);
				for (const userId of joinedConcurrently) members.add(userId);
				const fresh = entries.filter((entry) => !members.has(entry.userId.toString()));
				for (const entry of fresh) added.push(entry.userId.toString());
				return {added: fresh.map((entry) => ({userId: entry.userId})), state};
			},
			getStats: async () => ({}),
			getParentConfig: async () => ({availableTags: (params.forumTags?.live ?? []).map((id) => ({id}))}),
		},
		channelData: {
			findUnique: async () => ({
				id: PARENT,
				type: parentType,
				isThreadOnly: () => parentType === ChannelTypes.GUILD_FORUM,
			}),
		},
		messages: {
			getMessage: async () => ({...message, toRow: () => ({flags: message.flags})}),
			upsertMessage: async (row: {flags: number}) => {
				upserts.push(row.flags);
				return {...message, flags: row.flags};
			},
		},
	} as unknown as ChannelRepository;
	const gatewayService = {
		resolveMentionSourcesPage: async () => ({mentions: params.mentions, nextCursor: null}),
		dispatchGuild: async ({event, data}: {event: string; data: unknown}) => {
			dispatched.push({event, data});
		},
		dispatchGuildMany: async ({events}: {events: Array<{event: string; data: unknown}>}) => {
			dispatched.push(...events);
		},
	} as unknown as IGatewayService;
	const userRepository = {listUsers: async () => []} as unknown as UserRepository;
	setWorkerDependenciesForTest({channelRepository, gatewayService, userRepository});
	return {added, dispatched, upserts};
}

async function run(): Promise<Array<EnqueuedChunk>> {
	const chunks: Array<EnqueuedChunk> = [];
	const helpers: WorkerTaskHelpers = {
		logger: new NoopLogger(),
		jobId: 1n,
		addJob: async (_name: string, payload: unknown) => {
			chunks.push(payload as EnqueuedChunk);
			return 0n;
		},
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	} as unknown as WorkerTaskHelpers;
	await handleMentions(
		{
			channelId: THREAD.toString(),
			messageId: MESSAGE.toString(),
			authorId: AUTHOR.toString(),
			guildId: GUILD.toString(),
		},
		helpers,
	);
	return chunks;
}

function entry(userId: number, source: Partial<GatewayMentionSourceEntry>): GatewayMentionSourceEntry {
	return {userId: createUserID(BigInt(userId)), direct: false, role: false, everyone: false, ...source};
}

describe('thread mentions', () => {
	beforeEach(() => activate(true));

	afterEach(() => {
		clearWorkerDependencies();
		activate(false);
	});

	test('notifies members for @everyone, keeps direct mentions and adds role members', async () => {
		const harness = createHarness({
			members: ['10'],
			mentions: [
				entry(10, {everyone: true}),
				entry(11, {everyone: true}),
				entry(12, {direct: true}),
				entry(13, {role: true, everyone: true}),
			],
		});
		const chunks = await run();
		expect(chunks).toHaveLength(1);
		expect(chunks[0]?.thread).toBe(true);
		expect(chunks[0]?.mentions).toEqual([
			{userId: '10', direct: false, role: false, everyone: true},
			{userId: '12', direct: true, role: false, everyone: false},
			{userId: '13', direct: false, role: true, everyone: false},
		]);
		expect(harness.added).toEqual(['13']);
		expect(harness.dispatched.map((event) => event.event)).toEqual(['THREAD_MEMBERS_UPDATE']);
		expect(harness.upserts).toEqual([]);
	});

	test('keeps a role mention for a member who joined during the add', async () => {
		const harness = createHarness({
			joinedConcurrently: ['13'],
			mentions: [entry(13, {role: true}), entry(14, {role: true})],
		});
		const chunks = await run();
		expect(harness.added).toEqual(['14']);
		expect(chunks.flatMap((chunk) => chunk.mentions)).toEqual([
			{userId: '13', direct: false, role: true, everyone: false},
			{userId: '14', direct: false, role: true, everyone: false},
		]);
		expect(harness.upserts).toEqual([]);
	});

	test('drops deleted forum tags from the role add THREAD_MEMBERS_UPDATE', async () => {
		const harness = createHarness({forumTags: {applied: [7n, 8n], live: [7n]}, mentions: [entry(13, {role: true})]});
		await run();
		expect(harness.dispatched).toEqual([{event: 'THREAD_MEMBERS_UPDATE', data: {added: ['13'], applied_tags: ['7']}}]);
	});

	test('never adds role members to a private thread', async () => {
		const harness = createHarness({isPrivate: true, mentions: [entry(13, {role: true})]});
		expect(await run()).toEqual([]);
		expect(harness.added).toEqual([]);
	});

	test('caps role adds and flags the message once', async () => {
		const mentions = Array.from({length: MAX_ROLE_MENTION_THREAD_ADDS + 5}, (_, index) =>
			entry(100 + index, {role: true}),
		);
		const harness = createHarness({mentions});
		const chunks = await run();
		expect(harness.added).toHaveLength(MAX_ROLE_MENTION_THREAD_ADDS);
		expect(chunks.flatMap((chunk) => chunk.mentions)).toHaveLength(MAX_ROLE_MENTION_THREAD_ADDS);
		expect(harness.upserts).toEqual([ServerMessageFlags.FAILED_TO_MENTION_SOME_ROLES_IN_THREAD]);
		expect(harness.dispatched.at(-1)).toEqual({
			event: 'MESSAGE_UPDATE',
			data: {id: MESSAGE.toString(), flags: ServerMessageFlags.FAILED_TO_MENTION_SOME_ROLES_IN_THREAD},
		});
	});

	test('mentions only role members that fit under the thread member cap', async () => {
		const harness = createHarness({
			memberCount: MAX_THREAD_MEMBERS - 1,
			mentions: [entry(13, {role: true}), entry(14, {role: true})],
		});
		const chunks = await run();
		expect(harness.added).toEqual(['13']);
		expect(chunks.flatMap((chunk) => chunk.mentions)).toEqual([
			{userId: '13', direct: false, role: true, everyone: false},
		]);
		expect(harness.upserts).toEqual([ServerMessageFlags.FAILED_TO_MENTION_SOME_ROLES_IN_THREAD]);
	});

	test('mentions no role members when the add hits the member cap', async () => {
		const harness = createHarness({addRejected: true, mentions: [entry(13, {role: true}), entry(12, {direct: true})]});
		const chunks = await run();
		expect(chunks.flatMap((chunk) => chunk.mentions)).toEqual([
			{userId: '12', direct: true, role: false, everyone: false},
		]);
		expect(harness.upserts).toEqual([ServerMessageFlags.FAILED_TO_MENTION_SOME_ROLES_IN_THREAD]);
	});

	test('mentions nobody in a thread once the guild leaves the experiment', async () => {
		activate(false);
		const harness = createHarness({mentions: [entry(12, {direct: true})]});
		expect(await run()).toEqual([]);
		expect(harness.added).toEqual([]);
	});

	test('leaves ordinary channels untouched', async () => {
		createHarness({channelType: ChannelTypes.GUILD_TEXT, mentions: [entry(11, {everyone: true})]});
		const chunks = await run();
		expect(chunks).toHaveLength(1);
		expect(chunks[0]).not.toHaveProperty('thread');
		expect(chunks[0]?.mentions).toEqual([{userId: '11', direct: false, role: false, everyone: true}]);
	});
});

describe('thread mention chunks', () => {
	afterEach(() => {
		clearWorkerDependencies();
		activate(false);
	});

	function chunkHarness() {
		const increments: Array<ReadStateMentionUpdate> = [];
		const readStateService = {
			bulkIncrementMentionCounts: async (updates: Array<ReadStateMentionUpdate>) => {
				increments.push(...updates);
			},
		} as unknown as ReadStateService;
		const userRepository = {
			findGuildSettings: async () => null,
			createRecentMentions: async () => {},
		} as unknown as UserRepository;
		setWorkerDependenciesForTest({readStateService, userRepository});
		return increments;
	}

	async function runChunk() {
		await handleMentionChunk(
			{
				channelId: THREAD.toString(),
				messageId: MESSAGE.toString(),
				guildId: GUILD.toString(),
				thread: true,
				mentions: [{userId: '12', direct: true}],
			},
			{logger: new NoopLogger()} as unknown as WorkerTaskHelpers,
		);
	}

	test('stamps thread read states on increment', async () => {
		activate(true);
		const increments = chunkHarness();
		await runChunk();
		expect(increments).toEqual([
			expect.objectContaining({
				userId: createUserID(12n),
				marker: {flags: ReadStateFlags.IS_GUILD_CHANNEL | ReadStateFlags.IS_THREAD, guildId: GUILD},
			}),
		]);
	});

	test('drops a thread chunk that lands after the guild left the experiment', async () => {
		const increments = chunkHarness();
		await runChunk();
		expect(increments).toEqual([]);
	});
});
