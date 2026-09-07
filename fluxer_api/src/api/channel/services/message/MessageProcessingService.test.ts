// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';
import {
	type ChannelID,
	createChannelID,
	createMessageID,
	createUserID,
	type MessageID,
	type UserID,
} from '../../../BrandedTypes';
import type {ChannelRow} from '../../../database/types/ChannelTypes';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {UserCacheService} from '../../../infrastructure/UserCacheService';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import {Channel} from '../../../models/Channel';
import type {IUserRepository} from '../../../user/IUserRepository';
import {MessageProcessingService} from './MessageProcessingService';

const CHANNEL_ID = createChannelID(1532860318772891648n);
const AUTHOR_ID = createUserID(1471426754353995881n);
const RECIPIENT_ID = createUserID(1485344055661987728n);
const MESSAGE_ID = createMessageID(1546325276953149440n);

function dmChannelRow(lastMessageId: MessageID | null): ChannelRow {
	return {
		channel_id: CHANNEL_ID,
		guild_id: null,
		type: ChannelTypes.DM,
		name: null,
		topic: null,
		icon_hash: null,
		url: null,
		parent_id: null,
		position: null,
		owner_id: null,
		recipient_ids: new Set<UserID>([AUTHOR_ID, RECIPIENT_ID]),
		nsfw: null,
		content_warning_level: null,
		content_warning_text: null,
		rate_limit_per_user: null,
		bitrate: null,
		user_limit: null,
		voice_connection_limit: null,
		rtc_region: null,
		last_message_id: lastMessageId,
		last_pin_timestamp: null,
		permission_overwrites: null,
		nicks: null,
		soft_deleted: false,
		indexed_at: null,
		version: 0,
	};
}

function buildService(): {service: MessageProcessingService; opened: Array<Channel>} {
	const opened: Array<Channel> = [];
	const userRepository = {
		isDmChannelOpen: async (userId: UserID, _channelId: ChannelID) => userId === AUTHOR_ID,
		openPrivateChannelForUser: async (_userId: UserID, channel: Channel) => {
			opened.push(channel);
		},
	} as unknown as IUserRepository;
	const userCacheService = {
		getUserPartialResponses: async (userIds: Array<UserID>) =>
			new Map(userIds.map((userId) => [userId, {id: userId.toString()}])),
	} as unknown as UserCacheService;
	const gatewayService = {
		dispatchPresence: async () => {},
	} as unknown as IGatewayService;
	const service = new MessageProcessingService(
		undefined as never,
		userRepository,
		userCacheService,
		gatewayService,
		undefined as never,
		undefined as never,
	);
	return {service, opened};
}

describe('MessageProcessingService.updateDMRecipients', () => {
	it('snapshots the new message id when the in-request channel is stale', async () => {
		const {service, opened} = buildService();
		await service.updateDMRecipients({
			channel: new Channel(dmChannelRow(null)),
			channelId: CHANNEL_ID,
			messageId: MESSAGE_ID,
			requestCache: {} as RequestCache,
		});
		expect(opened).toHaveLength(1);
		expect(opened[0].lastMessageId).toBe(MESSAGE_ID);
	});

	it('keeps a newer last message id already present on the channel', async () => {
		const {service, opened} = buildService();
		const newer = createMessageID(MESSAGE_ID + 10n);
		await service.updateDMRecipients({
			channel: new Channel(dmChannelRow(newer)),
			channelId: CHANNEL_ID,
			messageId: MESSAGE_ID,
			requestCache: {} as RequestCache,
		});
		expect(opened).toHaveLength(1);
		expect(opened[0].lastMessageId).toBe(newer);
	});
});
