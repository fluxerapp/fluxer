// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {createSnowflakeFromTimestamp} from '@fluxer/snowflake/src/Snowflake';
import {describe, expect, it} from 'vitest';
import type {ChannelID, MessageID} from '../../../BrandedTypes';
import {createChannelID, createGuildID, createMessageID, createUserID} from '../../../BrandedTypes';
import type {GuildAuditLogService} from '../../../guild/GuildAuditLogService';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../../infrastructure/ISnowflakeService';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import {Message} from '../../../models/Message';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from '../AuthenticatedChannel';
import type {MessagePersistenceService} from '../message/MessagePersistenceService';
import {MessagePinService} from './MessagePinService';

const CHANNEL_ID = createChannelID(10n);
const GUILD_ID = createGuildID(20n);
const VIEWER_ID = createUserID(7n);
const CUTOFF = new Date('2026-01-10T00:00:00.000Z');
const CUTOFF_ISO: string | null = CUTOFF.toISOString();
const HIDDEN_CREATED_AT = new Date('2026-01-01T00:00:00.000Z');
const VISIBLE_CREATED_AT = new Date('2026-01-20T00:00:00.000Z');
const PIN_EPOCH = new Date('2026-02-01T00:00:00.000Z');

function makeMessageId(timestamp: Date, sequence: number): MessageID {
	return createMessageID(createSnowflakeFromTimestamp(timestamp.getTime()) + BigInt(sequence));
}

function makePin(createdAt: Date, sequence: number, pinnedAtOffsetMs: number): Message {
	return new Message({
		channel_id: CHANNEL_ID,
		bucket: 0,
		message_id: makeMessageId(createdAt, sequence),
		author_id: null,
		type: MessageTypes.DEFAULT,
		webhook_id: null,
		webhook_name: null,
		webhook_avatar_hash: null,
		content: '',
		edited_timestamp: null,
		pinned_timestamp: new Date(PIN_EPOCH.getTime() + pinnedAtOffsetMs),
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: null,
		embeds: null,
		sticker_items: null,
		message_reference: null,
		message_snapshots: null,
		call: null,
		has_reaction: null,
		version: 1,
	});
}

function createPinService({canReadHistory, pins}: {canReadHistory: boolean; pins: Array<Message>}) {
	const calls: Array<{before: Date; limit: number}> = [];
	const channelRepository = {
		messageInteractions: {
			listChannelPins: async (_channelId: ChannelID, before: Date, limit: number) => {
				calls.push({before, limit});
				return pins
					.filter((pin) => pin.pinnedTimestamp!.getTime() < before.getTime())
					.sort((a, b) => b.pinnedTimestamp!.getTime() - a.pinnedTimestamp!.getTime())
					.slice(0, limit);
			},
		},
	} as unknown as IChannelRepositoryAggregate;
	const channel = {id: CHANNEL_ID, type: ChannelTypes.GUILD_TEXT, guildId: GUILD_ID};
	const authChannel: AuthenticatedChannel = {
		channel: channel as AuthenticatedChannel['channel'],
		guild: {message_history_cutoff: CUTOFF_ISO} as AuthenticatedChannel['guild'],
		member: null,
		hasPermission: async () => canReadHistory,
		checkPermission: async () => {},
	};
	const service = new MessagePinService(
		{} as unknown as IGatewayService,
		channelRepository,
		{} as unknown as ISnowflakeService,
		{} as unknown as MessagePersistenceService,
		{} as unknown as GuildAuditLogService,
	);
	return {
		authChannel,
		calls,
		listPins: (limit: number) =>
			service.getChannelPins({
				authChannel,
				userId: VIEWER_ID,
				requestCache: {} as unknown as RequestCache,
				limit,
			}),
	};
}

describe('MessagePinService.getChannelPins', () => {
	it('walks past a full page of pins hidden by the history cutoff', async () => {
		const hidden = Array.from({length: 12}, (_value, index) => makePin(HIDDEN_CREATED_AT, index, 1_000 + index));
		const visible = Array.from({length: 5}, (_value, index) => makePin(VISIBLE_CREATED_AT, index, index));
		const {calls, listPins} = createPinService({canReadHistory: false, pins: [...hidden, ...visible]});

		const page = await listPins(10);

		expect(page.items.map((item) => item.message.id)).toEqual(
			[...visible].reverse().map((message) => message.id.toString()),
		);
		expect(page.has_more).toBe(false);
		expect(calls).toHaveLength(2);
	});

	it('reports has_more when visible pins behind the hidden run outnumber the page', async () => {
		const hidden = Array.from({length: 12}, (_value, index) => makePin(HIDDEN_CREATED_AT, index, 1_000 + index));
		const visible = Array.from({length: 11}, (_value, index) => makePin(VISIBLE_CREATED_AT, index, index));
		const {listPins} = createPinService({canReadHistory: false, pins: [...hidden, ...visible]});

		const page = await listPins(10);

		expect(page.items).toHaveLength(10);
		expect(page.has_more).toBe(true);
		expect(page.items.map((item) => item.message.id)).toEqual(
			[...visible]
				.reverse()
				.slice(0, 10)
				.map((message) => message.id.toString()),
		);
	});

	it('reads the pin index once for a caller that can read message history', async () => {
		const pins = Array.from({length: 11}, (_value, index) => makePin(HIDDEN_CREATED_AT, index, index));
		const {calls, listPins} = createPinService({canReadHistory: true, pins});

		const page = await listPins(10);

		expect(page.items).toHaveLength(10);
		expect(page.has_more).toBe(true);
		expect(calls).toHaveLength(1);
	});
});
