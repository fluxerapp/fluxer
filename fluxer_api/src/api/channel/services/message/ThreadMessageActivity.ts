// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {
	dispatchThreadEvents,
	type ThreadDispatchEvent,
	threadMembersUpdateEvent,
	threadMemberUpdateEvent,
	threadUpdateEvent,
} from '@app/api/channel/services/thread/ThreadDispatch';
import type {ThreadView} from '@app/api/channel/services/thread/ThreadMappers';
import {loadThreadView} from '@app/api/channel/services/thread/ThreadViews';
import {threadMemberAutojoinTotal} from '@app/api/channel/threads/ThreadMetrics';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {getKVThreadAutoArchiveQueue} from '@app/api/middleware/ServiceSingletons';
import type {Channel} from '@app/api/models/Channel';
import type {ThreadMember} from '@app/api/models/ThreadMember';
import type {ThreadState} from '@app/api/models/ThreadState';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {addThreadMembersWithinCap, threadRecipients} from '@app/api/worker/tasks/ThreadMentionScope';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	MAX_ACTIVE_THREADS_PER_GUILD,
	MAX_THREAD_MEMBERS,
	ThreadMemberFlags,
} from '@fluxer/constants/src/ThreadConstants';
import {MaxActiveThreadsError} from '@fluxer/errors/src/domains/channel/MaxActiveThreadsError';
import {ThreadArchivedError} from '@fluxer/errors/src/domains/channel/ThreadArchivedError';
import {ThreadLockedError} from '@fluxer/errors/src/domains/channel/ThreadLockedError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';

const INTERACTED_CAS_ATTEMPTS = 3;

export class ThreadMessageActivity {
	constructor(
		private readonly channelRepository: ChannelRepository,
		private readonly gatewayService: IGatewayService,
		private readonly userRepository: UserRepository,
	) {}

	async assertCanUnarchive(state: ThreadState): Promise<void> {
		if (!state.archived) return;
		if ((await this.channelRepository.threads.countActiveThreads(state.guildId)) >= MAX_ACTIVE_THREADS_PER_GUILD) {
			throw new MaxActiveThreadsError(MAX_ACTIVE_THREADS_PER_GUILD);
		}
	}

	async unarchive(channel: Channel, state: ThreadState): Promise<ThreadState> {
		if (!state.archived) return state;
		await this.assertCanUnarchive(state);
		const transition = await this.channelRepository.threads.updateState(state.threadId, (current) =>
			current.archived ? {archived: false} : null,
		);
		if (!transition) throw new UnknownChannelError();
		if (transition.previous.archived && !transition.state.archived) {
			await getKVThreadAutoArchiveQueue().schedule(transition.state, channel.lastMessageId);
		}
		return transition.state;
	}

	private allMembers(thread: Channel): Promise<Array<ThreadMember>> {
		return this.channelRepository.threads.listMembers(thread.id, {limit: MAX_THREAD_MEMBERS});
	}

	async beforeWebhookSend(thread: Channel): Promise<void> {
		const state = await this.channelRepository.threads.getState(thread.id);
		if (!state || state.guildId !== thread.guildId) throw new UnknownChannelError();
		if (state.locked) throw new ThreadLockedError();
		const next = await this.unarchive(thread, state);
		if (next !== state) {
			await this.dispatch(thread, [threadUpdateEvent(await this.view(thread, next), await this.allMembers(thread))]);
		}
	}

	async assertWebhookCanEdit(thread: Channel): Promise<void> {
		const state = await this.channelRepository.threads.getState(thread.id);
		if (!state) throw new UnknownChannelError();
		if (state.archived) throw new ThreadArchivedError();
		if (state.locked) throw new ThreadLockedError();
	}

	async beforeUserSend(params: {
		channel: Channel;
		parent: Channel;
		state: ThreadState;
		member: ThreadMember | null;
		userId: UserID;
		isBot: boolean;
	}): Promise<void> {
		const {channel, member, userId} = params;
		const state = await this.unarchive(channel, params.state);
		const unarchived = state !== params.state;
		const events: Array<ThreadDispatchEvent> = [];
		const updated = member === null ? null : await this.markInteracted(member);
		if (unarchived) {
			events.push(threadUpdateEvent(await this.view(channel, state, params.parent), await this.allMembers(channel)));
		} else if (updated) {
			events.push(threadMemberUpdateEvent(updated));
		}
		if (member === null) {
			const result = params.isBot
				? null
				: await addThreadMembersWithinCap(this.channelRepository.threads, state, [
						{userId, flags: ThreadMemberFlags.HAS_INTERACTED},
					]);
			if (result && result.added.length > 0) {
				threadMemberAutojoinTotal.inc('source="send"');
				events.push(
					threadMembersUpdateEvent(await this.view(channel, result.state, params.parent), {added: result.added}),
				);
			}
		}
		await this.dispatch(channel, events);
	}

	private async markInteracted(loaded: ThreadMember): Promise<ThreadMember | null> {
		let member: ThreadMember | null = loaded;
		for (let attempt = 0; attempt < INTERACTED_CAS_ATTEMPTS; attempt++) {
			if (!member || (member.flags & ThreadMemberFlags.HAS_INTERACTED) !== 0) return null;
			const updated = await this.channelRepository.threads.updateMemberSettings(member, {
				flags: member.flags | ThreadMemberFlags.HAS_INTERACTED,
			});
			if (updated) return updated;
			member = await this.channelRepository.threads.getMember(loaded.threadId, loaded.userId);
		}
		return null;
	}

	async addMentionedUsers(params: {
		channel: Channel;
		parent: Channel;
		isModerator: boolean;
		authorId: UserID;
		mentionUserIds: Array<UserID>;
	}): Promise<void> {
		const {channel, parent} = params;
		const guildId = channel.guildId;
		if (guildId === null) return;
		const mentioned = [...new Set(params.mentionUserIds)].filter((userId) => userId !== params.authorId);
		if (mentioned.length === 0) return;
		const recipients = await threadRecipients(this.userRepository, guildId, mentioned);
		const candidates = mentioned.filter((userId) => recipients.has(userId));
		if (candidates.length === 0) return;
		const state = await this.channelRepository.threads.getState(channel.id);
		if (!state || state.archived) return;
		if (state.isPrivate && !params.isModerator && state.invitable === false) return;
		const existing = new Set(
			(await this.channelRepository.threads.getMembers(channel.id, candidates)).map((member) => member.userId),
		);
		const visible = await Promise.all(
			candidates
				.filter((userId) => !existing.has(userId))
				.map(async (userId) =>
					(await this.gatewayService.checkPermission({
						guildId,
						userId,
						channelId: parent.id,
						permission: Permissions.VIEW_CHANNEL,
					}))
						? userId
						: null,
				),
		);
		const toAdd = visible.filter((userId): userId is UserID => userId !== null);
		if (toAdd.length === 0) return;
		const result = await addThreadMembersWithinCap(
			this.channelRepository.threads,
			state,
			toAdd.map((userId) => ({userId, flags: 0})),
		);
		if (!result || result.added.length === 0) return;
		threadMemberAutojoinTotal.inc('source="mention"', result.added.length);
		await this.dispatch(channel, [
			threadMembersUpdateEvent(await this.view(channel, result.state, parent), {added: result.added}),
		]);
	}

	async view(channel: Channel, state: ThreadState, parent?: Channel): Promise<ThreadView> {
		const parentChannel = parent ?? (await this.channelRepository.channelData.findUnique(state.parentId));
		if (parentChannel) return loadThreadView(this.channelRepository, channel, state, parentChannel);
		return {channel, state, stats: await this.channelRepository.threads.getStats(state.threadId), parentType: null};
	}

	private async dispatch(channel: Channel, events: Array<ThreadDispatchEvent>): Promise<void> {
		if (events.length === 0 || channel.guildId === null) return;
		await dispatchThreadEvents(this.gatewayService, channel.guildId, events);
	}
}
