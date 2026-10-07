// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/api/models/Channel';
import type {ThreadMember} from '@app/api/models/ThreadMember';
import type {ThreadState} from '@app/api/models/ThreadState';
import type {ThreadStats} from '@app/api/models/ThreadStats';
import {THREAD_MEMBER_COUNT_DISPLAY_CAP, THREAD_ONLY_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {ThreadMemberResponse, ThreadMemberRpcResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';

export interface ThreadView {
	channel: Channel;
	state: ThreadState;
	stats: ThreadStats;
	parentType: number | null;
}

export function mapThreadMemberToResponse(
	member: ThreadMember,
	options: {self: boolean; guildMember?: GuildMemberResponse; withIds?: boolean},
): ThreadMemberResponse {
	const response: ThreadMemberResponse = {
		...(options.withIds === false ? {} : {id: member.threadId.toString(), user_id: member.userId.toString()}),
		join_timestamp: member.joinTimestamp.toISOString(),
		flags: member.flags,
	};
	if (options.self) {
		response.muted = member.muted;
		response.mute_config = member.muteConfig
			? {
					end_time: member.muteConfig.endTime?.toISOString() ?? null,
					selected_time_window: member.muteConfig.selectedTimeWindow ?? 0,
				}
			: null;
	}
	if (options.guildMember) response.member = options.guildMember;
	return response;
}

export function mapThreadMemberToRpcResponse(member: ThreadMember): ThreadMemberRpcResponse {
	return {
		id: member.threadId.toString(),
		user_id: member.userId.toString(),
		join_timestamp: member.joinTimestamp.toISOString(),
		flags: member.flags,
		muted: member.muted,
		mute_config: member.muteConfig
			? {
					end_time: member.muteConfig.endTime?.toISOString() ?? null,
					selected_time_window: member.muteConfig.selectedTimeWindow ?? 0,
				}
			: null,
	};
}

export function mapThreadToResponse(view: ThreadView, viewerMember?: ThreadMember | null): ThreadChannelResponse {
	const {channel, state, stats} = view;
	const createdAt = state.createdAt.toISOString();
	const response: ThreadChannelResponse = {
		id: channel.id.toString(),
		type: channel.type,
		guild_id: channel.guildId?.toString(),
		parent_id: channel.parentId?.toString() ?? null,
		owner_id: channel.ownerId?.toString() ?? null,
		name: channel.name ?? null,
		last_message_id: channel.lastMessageId?.toString() ?? null,
		last_pin_timestamp: channel.lastPinTimestamp?.toISOString() ?? null,
		rate_limit_per_user: channel.rateLimitPerUser ?? 0,
		flags: state.flags,
		thread_metadata: {
			archived: state.archived,
			auto_archive_duration: state.autoArchiveDuration,
			archive_timestamp: state.archiveTimestamp?.toISOString() ?? createdAt,
			locked: state.locked,
			...(state.isPrivate ? {invitable: state.invitable ?? true} : {}),
			create_timestamp: createdAt,
		},
		message_count: stats.messageCount,
		total_message_sent: stats.totalMessageSent,
		member_count: Math.min(state.memberCount, THREAD_MEMBER_COUNT_DISPLAY_CAP),
	};
	if (view.parentType !== null && THREAD_ONLY_CHANNEL_TYPES.has(view.parentType)) {
		response.applied_tags = state.appliedTags.map((tag) => tag.toString());
		response.member_ids_preview = state.memberIdsPreview.map((id) => id.toString());
	}
	if (viewerMember) {
		response.member = mapThreadMemberToResponse(viewerMember, {self: true});
	}
	return response;
}

export function mapThreadToRpcResponse(view: ThreadView): ThreadChannelResponse {
	return {
		...mapThreadToResponse(view),
		member_ids_preview: view.state.memberIdsPreview.map((id) => id.toString()),
	};
}

export function serializeThreadForAudit(view: ThreadView): Record<string, unknown> {
	const {channel, state} = view;
	return {
		name: channel.name,
		type: channel.type,
		archived: state.archived,
		locked: state.locked,
		auto_archive_duration: state.autoArchiveDuration,
		rate_limit_per_user: channel.rateLimitPerUser ?? 0,
		flags: state.flags,
		...(state.isPrivate ? {invitable: state.invitable ?? true} : {}),
		...(view.parentType !== null && THREAD_ONLY_CHANNEL_TYPES.has(view.parentType)
			? {applied_tags: state.appliedTags.map((tag) => tag.toString())}
			: {}),
	};
}
