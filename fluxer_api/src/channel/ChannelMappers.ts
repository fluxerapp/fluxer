/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {UserID} from '~/BrandedTypes';
import {ChannelTypes} from '~/Constants';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Channel} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {getCachedUserPartialResponses} from '~/user/UserCacheHelpers';
import type {ChannelOverwriteResponse, ChannelPartialResponse, ChannelResponse} from './ChannelTypes';

interface MapChannelToResponseParams {
	channel: Channel;
	currentUserId: UserID | null;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
}

function serializeBaseChannelFields(channel: Channel) {
	return {
		id: channel.id.toString(),
		type: channel.type,
	};
}

function serializeMessageableFields(channel: Channel) {
	return {
		last_message_id: channel.lastMessageId ? channel.lastMessageId.toString() : null,
		last_pin_timestamp: channel.lastPinTimestamp ? channel.lastPinTimestamp.toISOString() : null,
	};
}

function serializeGuildChannelFields(channel: Channel) {
	return {
		guild_id: channel.guildId?.toString(),
		name: channel.name ?? undefined,
		position: channel.position ?? undefined,
		permission_overwrites: serializePermissionOverwrites(channel),
	};
}

function serializePositionableGuildChannelFields(channel: Channel) {
	return {
		...serializeGuildChannelFields(channel),
		parent_id: channel.parentId ? channel.parentId.toString() : null,
	};
}

function serializePermissionOverwrites(channel: Channel): Array<ChannelOverwriteResponse> {
	if (!channel.permissionOverwrites) return [];
	return Array.from(channel.permissionOverwrites).map(([targetId, overwrite]) => ({
		id: targetId.toString(),
		type: overwrite.type,
		allow: (overwrite.allow ?? 0n).toString(),
		deny: (overwrite.deny ?? 0n).toString(),
	}));
}

function serializeGuildTextChannel(channel: Channel): ChannelResponse {
	return {
		...serializeBaseChannelFields(channel),
		...serializeMessageableFields(channel),
		...serializePositionableGuildChannelFields(channel),
		topic: channel.topic,
		nsfw: channel.isNsfw,
		rate_limit_per_user: channel.rateLimitPerUser,
	};
}

function serializeGuildVoiceChannel(channel: Channel): ChannelResponse {
	return {
		...serializeBaseChannelFields(channel),
		...serializePositionableGuildChannelFields(channel),
		bitrate: channel.bitrate,
		user_limit: channel.userLimit,
		rtc_region: channel.rtcRegion,
	};
}

function serializeGuildCategoryChannel(channel: Channel): ChannelResponse {
	return {
		...serializeBaseChannelFields(channel),
		...serializeGuildChannelFields(channel),
	};
}

function serializeGuildLinkChannel(channel: Channel): ChannelResponse {
	return {
		...serializeBaseChannelFields(channel),
		...serializePositionableGuildChannelFields(channel),
		url: channel.url,
	};
}

function serializeDMChannel(channel: Channel): ChannelResponse {
	return {
		...serializeBaseChannelFields(channel),
		...serializeMessageableFields(channel),
	};
}

function serializeGroupDMChannel(channel: Channel): ChannelResponse {
	const nicknameMap = channel.nicknames ?? new Map<string, string>();
	const nicks: Record<string, string> = {};
	if (nicknameMap.size > 0) {
		for (const [userId, nickname] of nicknameMap) {
			const key = String(userId);
			nicks[key] = nickname;
		}
	}

	return {
		...serializeBaseChannelFields(channel),
		...serializeMessageableFields(channel),
		name: channel.name ?? undefined,
		icon: channel.iconHash ?? null,
		owner_id: channel.ownerId ? channel.ownerId.toString() : null,
		nicks: nicknameMap.size > 0 ? nicks : undefined,
	};
}

function serializeDMPersonalNotesChannel(channel: Channel): ChannelResponse {
	return {
		...serializeBaseChannelFields(channel),
		...serializeMessageableFields(channel),
	};
}

async function addDMRecipients(
	response: ChannelResponse,
	channel: Channel,
	currentUserId: UserID | null,
	userCacheService: UserCacheService,
	requestCache: RequestCache,
): Promise<void> {
	if (
		channel.guildId == null &&
		channel.type !== ChannelTypes.DM_PERSONAL_NOTES &&
		currentUserId != null &&
		channel.recipientIds &&
		channel.recipientIds.size > 0
	) {
		const recipientIds = Array.from(channel.recipientIds).filter((id) => id !== currentUserId);
		if (recipientIds.length > 0) {
			const userPartials = await getCachedUserPartialResponses({
				userIds: recipientIds,
				userCacheService,
				requestCache,
			});
			response.recipients = recipientIds.map((userId) => userPartials.get(userId)!);
		}
	}
}

export async function mapChannelToResponse({
	channel,
	currentUserId,
	userCacheService,
	requestCache,
}: MapChannelToResponseParams): Promise<ChannelResponse> {
	let response: ChannelResponse;

	switch (channel.type) {
		case ChannelTypes.GUILD_TEXT:
			response = serializeGuildTextChannel(channel);
			break;
		case ChannelTypes.GUILD_VOICE:
			response = serializeGuildVoiceChannel(channel);
			break;
		case ChannelTypes.GUILD_CATEGORY:
			response = serializeGuildCategoryChannel(channel);
			break;
		case ChannelTypes.GUILD_LINK:
			response = serializeGuildLinkChannel(channel);
			break;
		case ChannelTypes.DM:
			response = serializeDMChannel(channel);
			await addDMRecipients(response, channel, currentUserId, userCacheService, requestCache);
			break;
		case ChannelTypes.GROUP_DM:
			response = serializeGroupDMChannel(channel);
			await addDMRecipients(response, channel, currentUserId, userCacheService, requestCache);
			break;
		case ChannelTypes.DM_PERSONAL_NOTES:
			response = serializeDMPersonalNotesChannel(channel);
			break;
		default:
			response = {
				...serializeBaseChannelFields(channel),
				...serializeMessageableFields(channel),
				guild_id: channel.guildId?.toString(),
				name: channel.name ?? undefined,
				topic: channel.topic,
				url: channel.url ?? undefined,
				icon: channel.iconHash ?? null,
				owner_id: channel.ownerId ? channel.ownerId.toString() : null,
				position: channel.position ?? undefined,
				parent_id: channel.parentId ? channel.parentId.toString() : null,
				permission_overwrites: channel.guildId ? serializePermissionOverwrites(channel) : undefined,
			};
	}

	return response;
}

export const mapChannelToPartialResponse = (channel: Channel): ChannelPartialResponse => ({
	id: channel.id.toString(),
	name: channel.name,
	type: channel.type,
});
