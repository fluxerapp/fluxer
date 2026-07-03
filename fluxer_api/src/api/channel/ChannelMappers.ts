// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ThreadStates} from '@fluxer/constants/src/ChannelConstants';
import type {
	ChannelOverwriteResponse,
	ChannelPartialResponse,
	ChannelResponse,
	ThreadResponse,
} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {UserID} from '../BrandedTypes';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {Channel} from '../models/Channel';
import {getCachedUserPartialResponses} from '../user/UserCacheHelpers';

interface MapChannelToResponseParams {
	channel: Channel;
	currentUserId: UserID | null;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	effectiveNsfw?: boolean;
	effectiveContentWarningLevel?: number;
	effectiveContentWarningText?: string | null;
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
		type: overwrite.type === 1 ? 1 : 0,
		allow: (overwrite.allow ?? 0n).toString(),
		deny: (overwrite.deny ?? 0n).toString(),
	}));
}

function serializeContentWarningFields(channel: Channel, ctx: ContentWarningCtx) {
	return {
		nsfw: ctx.effectiveNsfw,
		nsfw_override: channel.nsfwOverride,
		content_warning_level: ctx.effectiveContentWarningLevel as 0 | 1,
		content_warning_text: ctx.effectiveContentWarningText,
	};
}

interface ContentWarningCtx {
	effectiveNsfw: boolean;
	effectiveContentWarningLevel: number;
	effectiveContentWarningText: string | null;
}

function deriveContentWarningCtx(channel: Channel, params: MapChannelToResponseParams): ContentWarningCtx {
	return {
		effectiveNsfw: params.effectiveNsfw ?? channel.isNsfw,
		effectiveContentWarningLevel: params.effectiveContentWarningLevel ?? channel.contentWarningLevel,
		effectiveContentWarningText:
			params.effectiveContentWarningText !== undefined
				? params.effectiveContentWarningText
				: channel.contentWarningText,
	};
}

function serializeGuildTextChannel(channel: Channel, ctx: ContentWarningCtx): ChannelResponse {
	return {
		...serializeBaseChannelFields(channel),
		...serializeMessageableFields(channel),
		...serializePositionableGuildChannelFields(channel),
		topic: channel.topic,
		...serializeContentWarningFields(channel, ctx),
		rate_limit_per_user: channel.rateLimitPerUser,
	};
}

function serializeGuildVoiceChannel(channel: Channel, ctx: ContentWarningCtx): ChannelResponse {
	return {
		...serializeBaseChannelFields(channel),
		...serializeMessageableFields(channel),
		...serializePositionableGuildChannelFields(channel),
		topic: channel.topic,
		bitrate: channel.bitrate,
		user_limit: channel.userLimit,
		voice_connection_limit: channel.voiceConnectionLimit,
		rtc_region: channel.rtcRegion,
		...serializeContentWarningFields(channel, ctx),
		rate_limit_per_user: channel.rateLimitPerUser,
	};
}

function serializeGuildCategoryChannel(channel: Channel, ctx: ContentWarningCtx): ChannelResponse {
	return {
		...serializeBaseChannelFields(channel),
		...serializeGuildChannelFields(channel),
		...serializeContentWarningFields(channel, ctx),
	};
}

function serializeGuildLinkChannel(channel: Channel, ctx: ContentWarningCtx): ChannelResponse {
	return {
		...serializeBaseChannelFields(channel),
		...serializePositionableGuildChannelFields(channel),
		url: channel.url,
		...serializeContentWarningFields(channel, ctx),
	};
}

function serializeGuildThreadChannel(channel: Channel, ctx: ContentWarningCtx): ThreadResponse {
	return {
		...serializeBaseChannelFields(channel),
		...serializeMessageableFields(channel),
		guild_id: channel.guildId?.toString(),
		name: channel.name ?? undefined,
		parent_id: channel.threadParentChannelId?.toString() ?? null,
		permission_overwrites: serializePermissionOverwrites(channel),
		...serializeContentWarningFields(channel, ctx),
		rate_limit_per_user: channel.rateLimitPerUser,
		thread_state: channel.threadState ?? ThreadStates.OPEN,
		thread_parent_channel_id: channel.threadParentChannelId?.toString() ?? '',
		thread_creator_id: channel.ownerId?.toString() ?? null,
		thread_creator_username: channel.threadCreatorUsername ?? null,
		thread_expires_at: channel.threadExpiresAt?.toISOString() ?? null,
		thread_source_message_id: channel.threadSourceMessageId?.toString() ?? null,
		thread_metadata: {
			archived: channel.threadArchived,
			locked: channel.threadLocked,
			auto_archive_duration: ([60, 1440, 4320, 10080].includes(channel.threadAutoArchiveDuration ?? 10080)
				? channel.threadAutoArchiveDuration
				: 10080) as 60 | 1440 | 4320 | 10080,
			archive_timestamp: channel.threadArchiveTimestamp?.toISOString() ?? null,
		},
		thread_total_message_sent: channel.threadTotalMessageSent,
		thread_member_count_actual: channel.threadMemberCountActual,
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

export async function mapChannelToResponse(params: MapChannelToResponseParams): Promise<ChannelResponse> {
	const {channel, currentUserId, userCacheService, requestCache} = params;
	const ctx = deriveContentWarningCtx(channel, params);
	let response: ChannelResponse;
	switch (channel.type) {
		case ChannelTypes.GUILD_TEXT:
			response = serializeGuildTextChannel(channel, ctx);
			break;
		case ChannelTypes.GUILD_VOICE:
			response = serializeGuildVoiceChannel(channel, ctx);
			break;
		case ChannelTypes.GUILD_CATEGORY:
			response = serializeGuildCategoryChannel(channel, ctx);
			break;
		case ChannelTypes.GUILD_LINK:
			response = serializeGuildLinkChannel(channel, ctx);
			break;
		case ChannelTypes.GUILD_THREAD:
			response = serializeGuildThreadChannel(channel, ctx);
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

export function mapChannelToPartialResponse(channel: Channel): ChannelPartialResponse {
	return {
		id: channel.id.toString(),
		name: channel.name,
		type: channel.type,
	};
}
