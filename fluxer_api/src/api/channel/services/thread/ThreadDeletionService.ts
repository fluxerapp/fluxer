// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMessageID, type UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {buildBroadcastMessageData} from '@app/api/channel/services/message/MessageGatewayDispatch';
import {MessageWriteLock} from '@app/api/channel/services/message/MessageWriteLock';
import {
	dispatchThreadEvents,
	type ThreadDispatchEvent,
	threadDeleteEvent,
} from '@app/api/channel/services/thread/ThreadDispatch';
import {serializeThreadForAudit} from '@app/api/channel/services/thread/ThreadMappers';
import type {ThreadServiceContext} from '@app/api/channel/services/thread/ThreadServiceContext';
import {loadThreadViews} from '@app/api/channel/services/thread/ThreadViews';
import type {Channel} from '@app/api/models/Channel';
import type {ThreadState} from '@app/api/models/ThreadState';
import {deleteChannelMessageSearchDocuments} from '@app/api/search/MessageSearchIndexCleanup';
import {deleteThreadSearchDocuments} from '@app/api/search/thread/ThreadSearchService';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {
	MAX_THREAD_MEMBERS,
	ServerMessageFlags,
	TEXT_THREAD_PARENT_CHANNEL_TYPES,
} from '@fluxer/constants/src/ThreadConstants';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

export async function clearThreadSourceFlag(
	channelRepository: ChannelRepository,
	cacheService: ICacheService,
	state: ThreadState,
	parent?: Channel | null,
): Promise<Array<ThreadDispatchEvent>> {
	if (!state.hasStarter) return [];
	const parentChannel = parent ?? (await channelRepository.channelData.findUnique(state.parentId));
	if (!parentChannel || !TEXT_THREAD_PARENT_CHANNEL_TYPES.has(parentChannel.type)) return [];
	const updated = await new MessageWriteLock(cacheService, channelRepository.messages).withFreshMessage(
		parentChannel.id,
		createMessageID(BigInt(state.threadId)),
		async (source) =>
			source && (source.flags & ServerMessageFlags.HAS_THREAD) !== 0
				? channelRepository.messages.upsertMessage(
						{...source.toRow(), flags: source.flags & ~ServerMessageFlags.HAS_THREAD},
						source.toRow(),
					)
				: null,
	);
	if (!updated) return [];
	return [
		{
			event: 'MESSAGE_UPDATE',
			data: {
				...(await buildBroadcastMessageData({channel: parentChannel, message: updated})),
				__thread_only_update: true,
			},
		},
	];
}

export class ThreadDeletionService {
	constructor(private readonly ctx: ThreadServiceContext) {}

	async deleteThread(params: {
		thread: Channel;
		actorId: UserID;
		auditLogReason: string | null;
		recordGuildAudit: boolean;
	}): Promise<void> {
		const {thread} = params;
		if (!thread.isThread() || thread.guildId === null) throw new InvalidChannelTypeError();
		const guildId = thread.guildId;
		const state = await this.ctx.channelRepository.threads.getState(thread.id);
		const [view] = state ? await loadThreadViews(this.ctx.channelRepository, [state]) : [];
		const members = state
			? await this.ctx.channelRepository.threads.listMembers(thread.id, {limit: MAX_THREAD_MEMBERS})
			: [];
		await deleteChannelMessageSearchDocuments(thread.id, {context: {source: 'thread_delete'}});
		await this.ctx.purgeChannelAttachments(thread);
		await this.ctx.channelRepository.threads.purgeThread(thread.id);
		await this.ctx.archiveQueue.remove(guildId, thread.id);
		await deleteThreadSearchDocuments([thread.id]);
		if (state) {
			await dispatchThreadEvents(this.ctx.gatewayService, guildId, [
				threadDeleteEvent(
					state,
					members.map((member) => member.userId),
				),
				...(await clearThreadSourceFlag(this.ctx.channelRepository, this.ctx.cacheService, state)),
			]);
		}
		if (params.recordGuildAudit) {
			await this.ctx.recordAudit({
				guildId,
				userId: params.actorId,
				action: AuditLogActionType.THREAD_DELETE,
				targetId: thread.id,
				auditLogReason: params.auditLogReason,
				changes: view ? this.ctx.guildAuditLogService.computeChanges(serializeThreadForAudit(view), null) : null,
			});
		}
	}
}
