// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelID, UserID} from '../../BrandedTypes';
import {userIdToChannelId} from '../../BrandedTypes';
import type {ChannelRow} from '../../database/types/ChannelTypes';
import type {Channel} from '../../models/Channel';

interface PersonalNotesChannelRepository {
	findUnique(channelId: ChannelID): Promise<Channel | null>;
	upsert(data: ChannelRow): Promise<Channel>;
}

export function isPersonalNotesChannelId({userId, channelId}: {userId: UserID; channelId: ChannelID}): boolean {
	return userIdToChannelId(userId) === channelId;
}

function buildPersonalNotesChannelRow(userId: UserID): ChannelRow {
	return {
		channel_id: userIdToChannelId(userId),
		guild_id: null,
		type: ChannelTypes.DM_PERSONAL_NOTES,
		name: '',
		topic: null,
		icon_hash: null,
		url: null,
		parent_id: null,
		position: 0,
		owner_id: userId,
		recipient_ids: new Set<UserID>(),
		nsfw: false,
		content_warning_level: null,
		content_warning_text: null,
		rate_limit_per_user: 0,
		bitrate: null,
		user_limit: null,
		voice_connection_limit: null,
		rtc_region: null,
		last_message_id: null,
		last_pin_timestamp: null,
		permission_overwrites: null,
		nicks: null,
		soft_deleted: false,
		indexed_at: null,
		thread_metadata: null,
		message_count: null,
		total_message_sent: null,
		member_count: null,
		version: 1,
	};
}

export async function ensurePersonalNotesChannelExists({
	channelRepository,
	userId,
}: {
	channelRepository: PersonalNotesChannelRepository;
	userId: UserID;
}): Promise<Channel> {
	const channelId = userIdToChannelId(userId);
	const existingChannel = await channelRepository.findUnique(channelId);
	if (existingChannel) {
		return existingChannel;
	}
	return channelRepository.upsert(buildPersonalNotesChannelRow(userId));
}
