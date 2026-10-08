// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import type {CrosspostSourceService} from '@app/api/channel/services/message/CrosspostSourceService';
import {MessageRequestService} from '@app/api/channel/services/message/MessageRequestService';
import type {MessageResponseDataService} from '@app/api/channel/services/message/MessageResponseDataService';
import {setCassandraQueryExecutorForTesting, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, PreparedQuery} from '@app/api/database/CassandraTypes';
import {
	clearChannelThreadsTaintCacheForTesting,
	syncChannelThreadsConfig,
	type ThreadViewer,
} from '@app/api/experiment/ChannelThreadsGate';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {GuildThreadState} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {ServerMessageFlags} from '@fluxer/constants/src/ThreadConstants';
import {ChannelThreadsConfigSchema} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

class MarkerCountingExecutor extends InMemoryCassandraQueryExecutor {
	markerReads = 0;

	override async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		if (query.cql.trimStart().toUpperCase().startsWith('SELECT') && query.cql.includes('guild_thread_state')) {
			this.markerReads++;
		}
		return super.executeQuery<T>(query);
	}
}

const GUILD = createGuildID(100n);
const CHANNEL = createChannelID(200n);
const USER = createUserID(10n);
const VIEWER: ThreadViewer = {kind: 'user', userId: USER, bot: false, capable: false};

function message(overrides: Partial<MessageResponse> = {}): MessageResponse {
	return {
		id: '300',
		channel_id: CHANNEL.toString(),
		type: MessageTypes.DEFAULT,
		flags: 0,
		content: 'hello',
		mentions: [],
		mention_roles: [],
		...overrides,
	} as MessageResponse;
}

function build(responses: Array<MessageResponse>) {
	const listMessages = vi.fn(async (_params: {threadsMask?: boolean}) => responses);
	const getMessage = vi.fn(async (_params: {threadsMask?: boolean}) => responses[0] ?? null);
	const channelService = {
		messages: {
			retrieval: {
				getResponseAccess: async () => ({
					access: {sourceGuildId: GUILD, messageHistoryCutoff: null, canReadMessageHistory: true},
					authChannel: {channel: {guildId: GUILD, isThreadOnly: () => false}},
				}),
				threadResponses: {
					shape: async (params: {responses: Array<MessageResponse>}) => params.responses,
					getStarter: async () => null,
				},
			},
		},
	} as unknown as ChannelService;
	const service = new MessageRequestService(
		channelService,
		{listMessages, getMessage} as unknown as MessageResponseDataService,
		{} as CrosspostSourceService,
	);
	return {service, listMessages, getMessage};
}

function list(service: MessageRequestService) {
	return service.listMessages({
		userId: USER,
		viewer: VIEWER,
		channelId: CHANNEL,
		query: {limit: 50},
		requestCache: {} as RequestCache,
	});
}

function get(service: MessageRequestService) {
	return service.getMessage({
		userId: USER,
		viewer: VIEWER,
		channelId: CHANNEL,
		messageId: createMessageID(300n),
		requestCache: {} as RequestCache,
	});
}

async function taint(): Promise<void> {
	await upsertOne(
		GuildThreadState.upsertAll({
			guild_id: GUILD,
			first_active_at: new Date(),
			perms_seeded_at: null,
			search_backfilled_at: null,
		}),
	);
}

describe('MessageRequestService thread masking', () => {
	let executor: MarkerCountingExecutor;

	beforeEach(() => {
		executor = new MarkerCountingExecutor();
		setCassandraQueryExecutorForTesting(executor);
		clearChannelThreadsTaintCacheForTesting();
		const raw = JSON.stringify(ChannelThreadsConfigSchema.parse({enabled: false, ever_enabled: true}));
		syncChannelThreadsConfig(raw, (value) => ChannelThreadsConfigSchema.parse(JSON.parse(value ?? '{}')));
	});

	afterEach(() => {
		syncChannelThreadsConfig(null, () => ChannelThreadsConfigSchema.parse({}));
		setCassandraQueryExecutorForTesting(null);
	});

	it('never reads the marker for a page without thread data', async () => {
		const {service, listMessages, getMessage} = build([message()]);
		await list(service);
		await get(service);
		expect(executor.markerReads).toBe(0);
		expect(listMessages).toHaveBeenCalledTimes(1);
		expect(listMessages.mock.calls[0]![0].threadsMask).toBeUndefined();
		expect(getMessage).toHaveBeenCalledTimes(1);
		expect(getMessage.mock.calls[0]![0].threadsMask).toBeUndefined();
	});

	it('refetches masked when a tainted guild page carries thread data', async () => {
		await taint();
		const {service, listMessages, getMessage} = build([message({flags: ServerMessageFlags.HAS_THREAD})]);
		await list(service);
		await get(service);
		expect(executor.markerReads).toBe(1);
		expect(listMessages.mock.calls.map(([params]) => params.threadsMask)).toEqual([undefined, true]);
		expect(getMessage.mock.calls.map(([params]) => params.threadsMask)).toEqual([undefined, true]);
	});

	it('treats a mentioned forum as thread data', async () => {
		await taint();
		const {service, listMessages} = build([
			message({mention_channels: [{id: '400', name: 'forum', type: ChannelTypes.GUILD_FORUM}]}),
		]);
		await list(service);
		expect(listMessages.mock.calls.map(([params]) => params.threadsMask)).toEqual([undefined, true]);
	});

	it('keeps raw data in an untainted guild', async () => {
		const {service, listMessages} = build([message({flags: ServerMessageFlags.HAS_THREAD})]);
		await list(service);
		expect(executor.markerReads).toBe(1);
		expect(listMessages).toHaveBeenCalledTimes(1);
	});
});
