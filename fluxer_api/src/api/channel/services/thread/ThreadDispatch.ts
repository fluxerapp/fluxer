// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {
	mapThreadMemberToRpcResponse,
	mapThreadToResponse,
	type ThreadView,
} from '@app/api/channel/services/thread/ThreadMappers';
import type {GatewayDispatchEvent} from '@app/api/constants/Gateway';
import {guildActive} from '@app/api/experiment/ChannelThreadsGate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ThreadMember} from '@app/api/models/ThreadMember';
import type {ThreadState} from '@app/api/models/ThreadState';
import {THREAD_MEMBER_COUNT_DISPLAY_CAP} from '@fluxer/constants/src/ThreadConstants';

export interface ThreadDispatchEvent {
	event: GatewayDispatchEvent;
	data: unknown;
}

function internalThreadFields(view: ThreadView) {
	return {
		_fluxer_member_ids_preview: view.state.memberIdsPreview.map((id) => id.toString()),
	};
}

export function threadCreateEvent(
	view: ThreadView,
	options: {members: Array<ThreadMember>; newlyCreated: boolean},
): ThreadDispatchEvent {
	return {
		event: 'THREAD_CREATE',
		data: {
			...mapThreadToResponse(view),
			...(options.newlyCreated ? {newly_created: true} : {}),
			...internalThreadFields(view),
			_fluxer_members: options.members.map(mapThreadMemberToRpcResponse),
		},
	};
}

export function threadUpdateEvent(view: ThreadView, unarchivedMembers?: Array<ThreadMember>): ThreadDispatchEvent {
	return {
		event: 'THREAD_UPDATE',
		data: {
			...mapThreadToResponse(view),
			...internalThreadFields(view),
			...(unarchivedMembers ? {_fluxer_members: unarchivedMembers.map(mapThreadMemberToRpcResponse)} : {}),
		},
	};
}

export function threadDeleteEvent(state: ThreadState, memberIds: Array<UserID> = []): ThreadDispatchEvent {
	return {
		event: 'THREAD_DELETE',
		data: {
			id: state.threadId.toString(),
			guild_id: state.guildId.toString(),
			parent_id: state.parentId.toString(),
			type: state.type,
			...(memberIds.length > 0 ? {_fluxer_member_ids: memberIds.map((id) => id.toString())} : {}),
		},
	};
}

export function threadMembersUpdateEvent(
	view: ThreadView,
	change: {added?: Array<ThreadMember>; removedUserIds?: Array<UserID>},
): ThreadDispatchEvent {
	const added = change.added ?? [];
	const removed = change.removedUserIds ?? [];
	return {
		event: 'THREAD_MEMBERS_UPDATE',
		data: {
			id: view.state.threadId.toString(),
			guild_id: view.state.guildId.toString(),
			member_count: Math.min(view.state.memberCount, THREAD_MEMBER_COUNT_DISPLAY_CAP),
			...(added.length > 0 ? {added_members: added.map(mapThreadMemberToRpcResponse)} : {}),
			...(removed.length > 0 ? {removed_member_ids: removed.map((id) => id.toString())} : {}),
			_fluxer_thread: {...mapThreadToResponse(view), ...internalThreadFields(view)},
		},
	};
}

export function threadMemberUpdateEvent(member: ThreadMember): ThreadDispatchEvent {
	return {
		event: 'THREAD_MEMBER_UPDATE',
		data: {
			...mapThreadMemberToRpcResponse(member),
			guild_id: member.guildId.toString(),
			_fluxer_parent_id: member.parentId.toString(),
		},
	};
}

export async function dispatchThreadEvents(
	gatewayService: IGatewayService,
	guildId: GuildID,
	events: Array<ThreadDispatchEvent>,
): Promise<void> {
	if (events.length === 0 || !guildActive(guildId)) return;
	if (events.length === 1) {
		await gatewayService.dispatchGuild({guildId, event: events[0]!.event, data: events[0]!.data});
		return;
	}
	await gatewayService.dispatchGuildMany({guildId, events});
}
