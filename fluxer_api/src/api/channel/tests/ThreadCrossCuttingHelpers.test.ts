// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createChannelID, createGuildID} from '@app/api/BrandedTypes';
import {MessageContentService} from '@app/api/channel/services/message/MessageContentService';
import {maskThreadArtifacts} from '@app/api/channel/services/message/ThreadMessageResponses';
import {resolveNsfwScopeChannel} from '@app/api/channel/utils/ThreadNsfwScope';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import {ChannelHelpers} from '@app/api/guild/services/channel/ChannelHelpers';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {Channel} from '@app/api/models/Channel';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {GuildExplicitContentFilterTypes} from '@fluxer/constants/src/GuildConstants';
import {ServerMessageFlags} from '@fluxer/constants/src/ThreadConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {describe, expect, it} from 'vitest';

const GUILD_ID = createGuildID(1_920_000_000_000_000_000n);

function channel(id: bigint, type: number, overrides: Partial<ChannelRow> = {}): Channel {
	return new Channel({
		channel_id: createChannelID(id),
		guild_id: GUILD_ID,
		type,
		name: `c${id}`,
		topic: null,
		icon_hash: null,
		url: null,
		parent_id: null,
		position: 0,
		owner_id: null,
		recipient_ids: null,
		nsfw: null,
		rate_limit_per_user: 0,
		bitrate: null,
		user_limit: null,
		voice_connection_limit: null,
		rtc_region: null,
		rtc_p2p: null,
		last_message_id: null,
		last_pin_timestamp: null,
		permission_overwrites: null,
		nicks: null,
		soft_deleted: false,
		indexed_at: null,
		version: 1,
		...overrides,
	});
}

function message(overrides: Partial<MessageResponse>): MessageResponse {
	return {
		id: '10',
		channel_id: '1',
		author: {
			id: '2',
			username: 'a',
			discriminator: '0001',
			global_name: null,
			avatar: null,
			avatar_color: null,
			flags: 0,
		},
		type: MessageTypes.DEFAULT,
		flags: 0,
		content: '',
		timestamp: new Date(0).toISOString(),
		pinned: false,
		mention_everyone: false,
		tts: false,
		mentions: [],
		mention_roles: [],
		...overrides,
	};
}

describe('thread NSFW scope', () => {
	const contentService = new MessageContentService(
		{} as IUserRepository,
		{} as IGuildRepositoryAggregate,
		{} as LimitConfigService,
	);
	const guild = {
		nsfw_level: 0,
		features: [],
		explicit_content_filter: GuildExplicitContentFilterTypes.ALL_MEMBERS,
	} as unknown as GuildResponse;

	it('resolves a thread to its parent and leaves other channels alone', async () => {
		const parent = channel(1n, ChannelTypes.GUILD_TEXT, {nsfw: true});
		const thread = channel(2n, ChannelTypes.PUBLIC_THREAD, {parent_id: parent.id});
		const find = async (id: ChannelID) => (id === parent.id ? parent : null);
		expect(await resolveNsfwScopeChannel(thread, find)).toBe(parent);
		expect(await resolveNsfwScopeChannel(parent, find)).toBe(parent);
		expect(contentService.isNSFWContentAllowed({channel: thread, guild})).toBe(false);
		expect(contentService.isNSFWContentAllowed({channel: await resolveNsfwScopeChannel(thread, find), guild})).toBe(
			true,
		);
	});

	it('treats forum and media parents like text for NSFW', () => {
		expect(
			contentService.isNSFWContentAllowed({channel: channel(3n, ChannelTypes.GUILD_FORUM, {nsfw: true}), guild}),
		).toBe(true);
	});
});

describe('forum positioning', () => {
	it('ranks forums with text channels above voice channels', () => {
		const category = channel(1n, ChannelTypes.GUILD_CATEGORY, {position: 1});
		const text = channel(2n, ChannelTypes.GUILD_TEXT, {parent_id: category.id, position: 2});
		const forum = channel(3n, ChannelTypes.GUILD_FORUM, {parent_id: category.id, position: 3});
		const voice = channel(4n, ChannelTypes.GUILD_VOICE, {parent_id: category.id, position: 4});
		const existing = [category, text, forum, voice];
		expect(ChannelHelpers.getNextGlobalChannelPosition(ChannelTypes.GUILD_TEXT, category.id, existing)).toBe(4);
		const parents = new Map(existing.map((c) => [c.id, c.parentId]));
		expect(() => ChannelHelpers.validateChannelVoicePlacement([category, text, voice, forum], parents)).toThrow();
		expect(() => ChannelHelpers.validateChannelVoicePlacement([category, forum, text, voice], parents)).not.toThrow();
	});
});

describe('thread artifact masking', () => {
	it('drops thread created messages and clears thread bits including nested ones', () => {
		const masked = maskThreadArtifacts([
			message({id: '1', type: MessageTypes.THREAD_CREATED}),
			message({
				id: '2',
				flags: ServerMessageFlags.HAS_THREAD,
				referenced_message: message({id: '3', flags: ServerMessageFlags.FAILED_TO_MENTION_SOME_ROLES_IN_THREAD}),
			}),
			message({id: '4', referenced_message: message({id: '5', type: MessageTypes.THREAD_CREATED})}),
		]);
		expect(masked.map((m) => m.id)).toEqual(['2', '4']);
		expect(masked[0]?.flags).toBe(0);
		expect(masked[0]?.referenced_message?.flags).toBe(0);
		expect(masked[1]?.referenced_message).toBeNull();
	});

	it('returns the same array when nothing carries a thread artifact', () => {
		const responses = [message({id: '1'})];
		expect(maskThreadArtifacts(responses)).toBe(responses);
	});
});
