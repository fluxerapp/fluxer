// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageID, UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {
	CreateThreadMember,
	ThreadMemberAddResult,
	ThreadRepository,
} from '@app/api/channel/repositories/ThreadRepository';
import {buildBroadcastMessageData} from '@app/api/channel/services/message/MessageGatewayDispatch';
import {dispatchThreadEvents, threadMembersUpdateEvent} from '@app/api/channel/services/thread/ThreadDispatch';
import {loadThreadView} from '@app/api/channel/services/thread/ThreadViews';
import {threadMemberAutojoinTotal, threadRoleMentionCappedTotal} from '@app/api/channel/threads/ThreadMetrics';
import {THREAD_CHANNEL_TYPES} from '@app/api/experiment/ChannelThreadsGate';
import type {GatewayMentionSourceEntry, IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {Channel} from '@app/api/models/Channel';
import type {ThreadState} from '@app/api/models/ThreadState';
import {
	MAX_ROLE_MENTION_THREAD_ADDS,
	MAX_THREAD_MEMBERS,
	ServerMessageFlags,
} from '@fluxer/constants/src/ThreadConstants';
import {MaxThreadMembersError} from '@fluxer/errors/src/domains/channel/MaxThreadMembersError';

export interface ThreadMentionScopeDeps {
	channelRepository: ChannelRepository;
	gatewayService: IGatewayService;
}

export async function addThreadMembersWithinCap(
	threads: ThreadRepository,
	state: ThreadState,
	members: Array<CreateThreadMember>,
): Promise<ThreadMemberAddResult | null> {
	const room = MAX_THREAD_MEMBERS - state.memberCount;
	if (room <= 0) return null;
	try {
		return await threads.addMembers(state.threadId, members.slice(0, room));
	} catch (error) {
		if (error instanceof MaxThreadMembersError) return null;
		throw error;
	}
}

export class ThreadMentionScope {
	private roleAdds = 0;
	private roleCapped = false;

	private constructor(
		private readonly deps: ThreadMentionScopeDeps,
		private readonly channel: Channel,
		private state: ThreadState | null,
		private readonly messageId: MessageID,
	) {}

	static async load(
		deps: ThreadMentionScopeDeps,
		channel: Channel,
		messageId: MessageID,
	): Promise<ThreadMentionScope | null> {
		if (channel.guildId === null || !THREAD_CHANNEL_TYPES.has(channel.type)) return null;
		const state = await deps.channelRepository.threads.getState(channel.id);
		return new ThreadMentionScope(deps, channel, state, messageId);
	}

	async filter(entries: Array<GatewayMentionSourceEntry>): Promise<Array<GatewayMentionSourceEntry>> {
		const {state} = this;
		if (state === null || entries.length === 0) return [];
		const candidates = entries;
		const members = new Set(
			(
				await this.deps.channelRepository.threads.getMembers(
					state.threadId,
					candidates.map((entry) => entry.userId),
				)
			).map((member) => member.userId),
		);
		const canAddRoleMembers = !state.isPrivate && !state.archived;
		const kept: Array<GatewayMentionSourceEntry> = [];
		const roleAdds: Array<GatewayMentionSourceEntry> = [];
		for (const entry of candidates) {
			if (entry.direct || members.has(entry.userId)) {
				kept.push(entry);
				continue;
			}
			if (!entry.role || !canAddRoleMembers) continue;
			if (this.roleAdds + roleAdds.length >= MAX_ROLE_MENTION_THREAD_ADDS) {
				this.roleCapped = true;
				continue;
			}
			roleAdds.push(entry);
		}
		if (roleAdds.length === 0) return kept;
		const joined = await this.addRoleMembers(
			state,
			roleAdds.map((entry) => entry.userId),
		);
		for (const entry of roleAdds) {
			if (joined.has(entry.userId)) kept.push({...entry, everyone: false});
			else this.roleCapped = true;
		}
		return kept;
	}

	async finish(): Promise<void> {
		if (!this.roleCapped || this.channel.guildId === null) return;
		threadRoleMentionCappedTotal.inc();
		const message = await this.deps.channelRepository.messages.getMessage(this.channel.id, this.messageId);
		if (!message || (message.flags & ServerMessageFlags.FAILED_TO_MENTION_SOME_ROLES_IN_THREAD) !== 0) return;
		const updated = await this.deps.channelRepository.messages.upsertMessage(
			{...message.toRow(), flags: message.flags | ServerMessageFlags.FAILED_TO_MENTION_SOME_ROLES_IN_THREAD},
			message.toRow(),
		);
		await dispatchThreadEvents(this.deps.gatewayService, this.channel.guildId, [
			{
				event: 'MESSAGE_UPDATE',
				data: await buildBroadcastMessageData({channel: this.channel, message: updated}),
			},
		]);
	}

	private async addRoleMembers(state: ThreadState, userIds: Array<UserID>): Promise<Set<UserID>> {
		const {threads} = this.deps.channelRepository;
		const result = await addThreadMembersWithinCap(
			threads,
			state,
			userIds.map((userId) => ({userId, flags: 0})),
		);
		const added = result?.added ?? [];
		const joined = new Set(added.map((member) => member.userId));
		const missing = userIds.filter((userId) => !joined.has(userId));
		if (missing.length > 0) {
			for (const member of await threads.getMembers(state.threadId, missing)) joined.add(member.userId);
		}
		if (!result || added.length === 0) return joined;
		this.state = result.state;
		this.roleAdds += added.length;
		threadMemberAutojoinTotal.inc('source="role_mention"', added.length);
		const parent = await this.deps.channelRepository.channelData.findUnique(state.parentId);
		const view = parent
			? await loadThreadView(this.deps.channelRepository, this.channel, result.state, parent)
			: {channel: this.channel, state: result.state, stats: await threads.getStats(state.threadId), parentType: null};
		await dispatchThreadEvents(this.deps.gatewayService, state.guildId, [threadMembersUpdateEvent(view, {added})]);
		return joined;
	}
}
