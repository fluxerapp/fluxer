// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createChannelID, createGuildID, createUserID, type GuildID} from '@app/api/BrandedTypes';
import {
	dispatchThreadEvents,
	threadMembersUpdateEvent,
	threadUpdateEvent,
} from '@app/api/channel/services/thread/ThreadDispatch';
import {loadThreadViews} from '@app/api/channel/services/thread/ThreadViews';
import {enqueueThreadSearchSync} from '@app/api/channel/threads/ThreadJobs';
import {
	setThreadArchiveSweepLag,
	threadArchiveSweepBatch,
	threadArchiveSweepSkippedInactiveTotal,
	threadsArchivedTotal,
} from '@app/api/channel/threads/ThreadMetrics';
import {everEnabled, guildActive, isTainted} from '@app/api/experiment/ChannelThreadsGate';
import {
	KVThreadAutoArchiveQueueService,
	threadAutoArchiveDueAt,
} from '@app/api/infrastructure/KVThreadAutoArchiveQueueService';
import {Logger} from '@app/api/Logger';
import {deleteChannelMessageSearchDocuments} from '@app/api/search/MessageSearchIndexCleanup';
import {deleteThreadSearchDocuments} from '@app/api/search/thread/ThreadSearchService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {MAX_THREAD_ARCHIVES_PER_SWEEP_TICK} from '@fluxer/constants/src/ThreadConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const SnowflakeString = z.string().regex(/^\d{1,20}$/);
const RepairPayload = z.object({threadIds: z.array(SnowflakeString).min(1).max(1000)});
const ParentPayload = z.object({guildId: SnowflakeString, parentId: SnowflakeString});
const MemberPayload = z.object({guildId: SnowflakeString, userId: SnowflakeString});

const PARENT_PAGE_SIZE = 100;

function archiveQueue(): KVThreadAutoArchiveQueueService {
	const {kvClient} = getWorkerDependencies();
	return new KVThreadAutoArchiveQueueService(kvClient);
}

async function archiveDueThreads(
	queue: KVThreadAutoArchiveQueueService,
	guildId: GuildID,
	nowMs: number,
	budget: number,
): Promise<{archived: number; oldestDueMs: number | null}> {
	const {channelRepository, gatewayService} = getWorkerDependencies();
	const dueIds = await queue.getDue(guildId, nowMs, budget);
	if (dueIds.length === 0) return {archived: 0, oldestDueMs: null};
	const [states, channels] = await Promise.all([
		channelRepository.threads.getStates(dueIds),
		channelRepository.channelData.listChannels(dueIds),
	]);
	const stateById = new Map(states.map((state) => [state.threadId.toString(), state]));
	const channelById = new Map(channels.map((channel) => [channel.id.toString(), channel]));
	let archived = 0;
	let oldestDueMs: number | null = null;
	for (const threadId of dueIds) {
		const state = stateById.get(threadId.toString());
		const channel = channelById.get(threadId.toString());
		if (!state || !channel || state.archived || state.isPinned) {
			await queue.remove(guildId, threadId);
			continue;
		}
		const dueAt = threadAutoArchiveDueAt(state, channel.lastMessageId);
		if (dueAt > nowMs) {
			await queue.schedule(state, channel.lastMessageId);
			continue;
		}
		oldestDueMs = oldestDueMs === null ? dueAt : Math.min(oldestDueMs, dueAt);
		const transition = await channelRepository.threads.updateState(threadId, (current) =>
			current.archived || current.isPinned ? null : {archived: true},
		);
		await queue.remove(guildId, threadId);
		if (!transition || transition.previous.archived || !transition.state.archived) continue;
		archived++;
		threadsArchivedTotal.inc('reason="auto"');
		const [view] = await loadThreadViews(channelRepository, [transition.state]);
		if (view) await dispatchThreadEvents(gatewayService, guildId, [threadUpdateEvent(view)]);
		enqueueThreadSearchSync(threadId);
	}
	return {archived, oldestDueMs};
}

export const archiveInactiveThreads: WorkerTaskHandler = async () => {
	if (!everEnabled()) return;
	const queue = archiveQueue();
	const nowMs = Date.now();
	let budget = MAX_THREAD_ARCHIVES_PER_SWEEP_TICK;
	let oldestDueMs: number | null = null;
	for (const guildId of await queue.listGuilds()) {
		if (budget <= 0) break;
		if (!guildActive(guildId)) {
			threadArchiveSweepSkippedInactiveTotal.inc();
			continue;
		}
		const result = await archiveDueThreads(queue, guildId, nowMs, budget);
		budget -= result.archived;
		if (result.oldestDueMs !== null) {
			oldestDueMs = oldestDueMs === null ? result.oldestDueMs : Math.min(oldestDueMs, result.oldestDueMs);
		}
	}
	const archived = MAX_THREAD_ARCHIVES_PER_SWEEP_TICK - budget;
	threadArchiveSweepBatch.observe(archived);
	setThreadArchiveSweepLag(oldestDueMs === null ? 0 : (nowMs - oldestDueMs) / 1000);
	if (archived > 0) Logger.info({archived}, 'Archived inactive threads');
};

export const repairThreadIndexes: WorkerTaskHandler = async (payload) => {
	const {threadIds} = RepairPayload.parse(payload);
	const {channelRepository} = getWorkerDependencies();
	await channelRepository.threads.repairThreadIndexes(threadIds.map((id) => createChannelID(BigInt(id))));
};

export const deleteChannelThreads: WorkerTaskHandler = async (payload) => {
	const validated = ParentPayload.parse(payload);
	const guildId = createGuildID(BigInt(validated.guildId));
	const parentId = createChannelID(BigInt(validated.parentId));
	if (!everEnabled() || !(await isTainted(guildId, {fresh: true}))) return;
	const {channelRepository, channelService} = getWorkerDependencies();
	const queue = archiveQueue();
	let deleted = 0;
	let after: ChannelID | undefined;
	while (true) {
		const threadIds = await channelRepository.threads.listThreadIdsByParent(parentId, {after, limit: PARENT_PAGE_SIZE});
		if (threadIds.length === 0) break;
		after = threadIds[threadIds.length - 1];
		const channels = new Map(
			(await channelRepository.channelData.listChannels(threadIds)).map((channel) => [channel.id.toString(), channel]),
		);
		for (const threadId of threadIds) {
			const channel = channels.get(threadId.toString());
			if (channel) await channelService.attachments.purgeChannelAttachments(channel);
			await deleteChannelMessageSearchDocuments(threadId, {context: {source: 'thread_parent_delete'}});
			await channelRepository.threads.purgeThread(threadId);
			await queue.remove(guildId, threadId);
			deleted++;
		}
		await deleteThreadSearchDocuments(threadIds);
	}
	Logger.info({guildId: guildId.toString(), parentId: parentId.toString(), deleted}, 'Deleted threads of parent');
};

export const removeThreadMembershipsForGuildMember: WorkerTaskHandler = async (payload) => {
	const validated = MemberPayload.parse(payload);
	const guildId = createGuildID(BigInt(validated.guildId));
	const userId = createUserID(BigInt(validated.userId));
	if (!everEnabled() || !(await isTainted(guildId, {fresh: true}))) return;
	const {channelRepository, guildRepository, gatewayService} = getWorkerDependencies();
	if (await guildRepository.getMember(guildId, userId)) return;
	const threadIds = await channelRepository.threads.listJoinedThreadIds(userId, guildId);
	for (const threadId of threadIds) {
		const result = await channelRepository.threads.removeMembers(threadId, [userId]);
		if (!result.state || result.removed.length === 0) continue;
		const [view] = await loadThreadViews(channelRepository, [result.state]);
		if (view) {
			await dispatchThreadEvents(gatewayService, guildId, [threadMembersUpdateEvent(view, {removedUserIds: [userId]})]);
		}
	}
};
