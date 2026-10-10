// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMessageID, type UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {ThreadStatePatch} from '@app/api/channel/repositories/ThreadRepository';
import type {AuthenticatedChannel, AuthenticatedThread} from '@app/api/channel/services/AuthenticatedChannel';
import type {ChannelUtilsService} from '@app/api/channel/services/channel_data/ChannelUtilsService';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import {resolveAppliedTags} from '@app/api/channel/services/thread/ForumTagRules';
import {clearThreadSourceFlag} from '@app/api/channel/services/thread/ThreadDeletionService';
import {assertThreadAllowed} from '@app/api/channel/services/thread/ThreadDenials';
import {
	dispatchThreadEvents,
	threadDeleteEvent,
	threadUpdateEvent,
} from '@app/api/channel/services/thread/ThreadDispatch';
import {serializeThreadForAudit, type ThreadView} from '@app/api/channel/services/thread/ThreadMappers';
import {loadThreadView} from '@app/api/channel/services/thread/ThreadViews';
import {enqueueThreadSearchSync} from '@app/api/channel/threads/ThreadJobs';
import {threadsArchivedTotal} from '@app/api/channel/threads/ThreadMetrics';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getKVThreadAutoArchiveQueue} from '@app/api/middleware/ServiceSingletons';
import type {Channel} from '@app/api/models/Channel';
import {deleteChannelMessageSearchDocuments} from '@app/api/search/MessageSearchIndexCleanup';
import {deleteThreadSearchDocuments} from '@app/api/search/thread/ThreadSearchService';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	ChannelFlags,
	MAX_ACTIVE_THREADS_PER_GUILD,
	MAX_PINNED_THREADS_PER_FORUM,
	MAX_THREAD_MEMBERS,
	settableChannelFlags,
} from '@fluxer/constants/src/ThreadConstants';
import {
	canDeleteThread,
	canSetTags,
	type ThreadPatch,
	ThreadPermissionFlags,
	threadPatchRequirement,
} from '@fluxer/constants/src/ThreadPermissionUtils';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {MaxActiveThreadsError} from '@fluxer/errors/src/domains/channel/MaxActiveThreadsError';
import {MaxPinnedThreadsInForumError} from '@fluxer/errors/src/domains/channel/MaxPinnedThreadsInForumError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {ChannelUpdateThreadRequest} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';

interface ThreadModifyServiceDeps {
	channelRepository: ChannelRepository;
	gatewayService: IGatewayService;
	guildAuditLogService: GuildAuditLogService;
	rateLimitService: IRateLimitService;
	cacheService: ICacheService;
	snowflakeService: ISnowflakeService;
	messagePersistence: MessagePersistenceService;
	utils: ChannelUtilsService;
}

type ThreadUpdateData = Omit<ChannelUpdateThreadRequest, 'type'> & {
	invitable?: boolean | null;
	applied_tags?: Array<bigint>;
};

function changed<T>(value: T | undefined, current: T | null): T | undefined {
	return value === current ? undefined : value;
}

function requiresModerator(patch: ThreadPatch, thread: AuthenticatedThread): boolean {
	const {actor, state} = thread;
	if (patch.locked !== undefined && patch.locked !== state.locked && !(patch.locked && actor.isThreadOwner))
		return true;
	if (patch.rate_limit_per_user !== undefined || (patch.flags !== undefined && patch.flags !== state.flags))
		return true;
	if (patch.archived === false && state.archived && state.locked) return true;
	const ownerAction =
		patch.name !== undefined ||
		patch.auto_archive_duration !== undefined ||
		patch.invitable !== undefined ||
		(patch.archived === true && !state.archived);
	return ownerAction && !actor.isThreadOwner;
}

export class ThreadModifyService {
	constructor(private readonly deps: ThreadModifyServiceDeps) {}

	get repository(): ChannelRepository {
		return this.deps.channelRepository;
	}

	async updateThread({
		authChannel,
		userId,
		data: input,
		requestCache,
		auditLogReason,
	}: {
		authChannel: AuthenticatedChannel;
		userId: UserID;
		data: ThreadUpdateData;
		requestCache: RequestCache;
		auditLogReason: string | null;
	}): Promise<Channel> {
		const data = {
			...input,
			archived: input.archived ?? undefined,
			locked: input.locked ?? undefined,
			rate_limit_per_user: input.rate_limit_per_user === null ? 0 : input.rate_limit_per_user,
			invitable: input.invitable ?? undefined,
		};
		const thread = authChannel.thread;
		const channel = authChannel.channel;
		if (!thread || channel.guildId === null) throw new UnknownChannelError();
		const {state, parent} = thread;
		if (data.flags !== undefined && (data.flags & ~settableChannelFlags(channel.type, parent.type)) !== 0) {
			throw InputValidationError.fromCode('flags', ValidationErrorCodes.INVALID_FORMAT);
		}
		if (data.applied_tags !== undefined && !parent.isThreadOnly()) throw new InvalidChannelTypeError();
		const patch: ThreadPatch = {
			name: changed(data.name, channel.name),
			archived: data.archived,
			auto_archive_duration: changed(data.auto_archive_duration, state.autoArchiveDuration),
			locked: data.locked,
			invitable: state.isPrivate ? changed(data.invitable, state.invitable) : undefined,
			rate_limit_per_user: changed(data.rate_limit_per_user, channel.rateLimitPerUser),
			flags: data.flags,
			applied_tags: data.applied_tags,
		};
		assertThreadAllowed(threadPatchRequirement(patch, state.flags, thread.actor));
		const appliedTags =
			data.applied_tags !== undefined
				? await this.resolvePatchedTags(
						thread,
						data.applied_tags.map((id) => BigInt(id)),
					)
				: undefined;
		if (requiresModerator(patch, thread)) thread.enforceMfa(ThreadPermissionFlags.MANAGE_THREADS);
		const pinning =
			data.flags !== undefined && (data.flags & ChannelFlags.PINNED) !== 0 && !state.isPinned && data.archived !== true;
		if (pinning && !(await this.deps.channelRepository.threads.claimForumPin(parent.id, channel.id))) {
			throw new MaxPinnedThreadsInForumError(MAX_PINNED_THREADS_PER_FORUM);
		}
		if (patch.archived === false && state.archived) {
			const activeCount = await this.deps.channelRepository.threads.countActiveThreads(state.guildId);
			if (activeCount >= MAX_ACTIVE_THREADS_PER_GUILD) throw new MaxActiveThreadsError(MAX_ACTIVE_THREADS_PER_GUILD);
		}
		if (data.name !== undefined) {
			contentModerationService.scanText(data.name, {
				userId,
				guildId: channel.guildId,
				channelId: channel.id,
				messageId: null,
				surface: 'profile_field',
			});
		}
		const before = await loadThreadView(this.deps.channelRepository, channel, state, parent);
		const renamed = data.name !== undefined && data.name !== channel.name;
		const rateChanged = data.rate_limit_per_user !== undefined && data.rate_limit_per_user !== channel.rateLimitPerUser;
		const statePatch: ThreadStatePatch = {
			archived: data.archived,
			locked: data.locked,
			auto_archive_duration: data.auto_archive_duration,
			invitable: state.isPrivate ? data.invitable : undefined,
			flags: data.flags,
			applied_tags: appliedTags,
		};
		const transition = await this.deps.channelRepository.threads.updateState(channel.id, (current) =>
			data.auto_archive_duration !== undefined && data.auto_archive_duration !== current.autoArchiveDuration
				? {...statePatch, archive_timestamp: new Date()}
				: statePatch,
		);
		if (!transition) {
			if (pinning) await this.deps.channelRepository.threads.releaseForumPin(parent.id, channel.id);
			throw new UnknownChannelError();
		}
		const updatedChannel =
			renamed || rateChanged
				? await this.deps.channelRepository.channelData.upsert(
						{
							...channel.toRow(),
							name: renamed ? data.name! : channel.name,
							rate_limit_per_user: rateChanged ? data.rate_limit_per_user! : channel.rateLimitPerUser,
						},
						channel.toRow(),
					)
				: channel;
		const next = transition.state;
		if (rateChanged) await this.clearThreadSlowmode(channel);
		enqueueThreadSearchSync(channel.id);
		await this.syncArchiveQueue(updatedChannel, transition.previous.archived, next);
		const after = await loadThreadView(this.deps.channelRepository, updatedChannel, next, parent);
		const changes = this.deps.guildAuditLogService.computeChanges(
			serializeThreadForAudit(before),
			serializeThreadForAudit(after),
		);
		if (changes.length > 0) {
			const unarchivedMembers =
				transition.previous.archived && !next.archived
					? await this.deps.channelRepository.threads.listMembers(channel.id, {limit: MAX_THREAD_MEMBERS})
					: undefined;
			await dispatchThreadEvents(this.deps.gatewayService, next.guildId, [threadUpdateEvent(after, unarchivedMembers)]);
			await this.recordAudit({
				view: after,
				userId,
				action: AuditLogActionType.THREAD_UPDATE,
				auditLogReason,
				changes,
			});
		}
		if (renamed) await this.sendNameChangeMessage(updatedChannel, userId, requestCache);
		return updatedChannel;
	}

	private async resolvePatchedTags(thread: AuthenticatedThread, requested: Array<bigint>): Promise<Array<bigint>> {
		const {state, parent, actor} = thread;
		const config = await this.deps.channelRepository.threads.getParentConfig(state.guildId, parent.id);
		const available = new Map((config?.availableTags ?? []).map((tag) => [tag.id, tag]));
		const previous = new Set(state.appliedTags);
		const touchesModeratedTag = [
			...requested.filter((id) => !previous.has(id)),
			...state.appliedTags.filter((id) => !requested.includes(id)),
		].some((id) => available.get(id)?.moderated === true);
		assertThreadAllowed(canSetTags(actor, {touchesModeratedTag}));
		return resolveAppliedTags({config, requested, previous: state.appliedTags, moderator: thread.isModerator});
	}

	async deleteThread({
		authChannel,
		userId,
		requestCache,
		auditLogReason,
	}: {
		authChannel: AuthenticatedChannel;
		userId: UserID;
		requestCache: RequestCache;
		auditLogReason: string | null;
	}): Promise<void> {
		const thread = authChannel.thread;
		const channel = authChannel.channel;
		if (!thread || channel.guildId === null) throw new UnknownChannelError();
		assertThreadAllowed(canDeleteThread(thread.actor));
		thread.enforceMfa(ThreadPermissionFlags.MANAGE_THREADS);
		const view = await loadThreadView(this.deps.channelRepository, channel, thread.state, thread.parent);
		const members = await this.deps.channelRepository.threads.listMembers(channel.id, {limit: MAX_THREAD_MEMBERS});
		await this.deps.utils.purgeChannelAttachments(channel);
		await deleteChannelMessageSearchDocuments(channel.id, {context: {source: 'thread_delete'}});
		await this.deps.channelRepository.threads.purgeThread(channel.id);
		await getKVThreadAutoArchiveQueue().remove(thread.state.guildId, channel.id);
		await deleteThreadSearchDocuments([channel.id]);
		requestCache.channels.delete(channel.id);
		await dispatchThreadEvents(this.deps.gatewayService, thread.state.guildId, [
			threadDeleteEvent(
				thread.state,
				members.map((member) => member.userId),
			),
			...(await clearThreadSourceFlag(
				this.deps.channelRepository,
				this.deps.cacheService,
				thread.state,
				thread.parent,
			)),
		]);
		await this.recordAudit({
			view,
			userId,
			action: AuditLogActionType.THREAD_DELETE,
			auditLogReason,
			changes: this.deps.guildAuditLogService.computeChanges(serializeThreadForAudit(view), null),
		});
	}

	private async syncArchiveQueue(channel: Channel, wasArchived: boolean, next: ThreadView['state']): Promise<void> {
		const queue = getKVThreadAutoArchiveQueue();
		if (next.archived) {
			if (!wasArchived) threadsArchivedTotal.inc('reason="manual"');
			await queue.remove(next.guildId, next.threadId);
			return;
		}
		await queue.schedule(next, channel.lastMessageId);
	}

	private async clearThreadSlowmode(channel: Channel): Promise<void> {
		try {
			await this.deps.rateLimitService.clearLimitsByIdentifierPrefix(`slowmode:${channel.id}:`);
		} catch (error) {
			Logger.error({error, channelId: channel.id.toString()}, 'Failed to clear thread slowmode state');
		}
	}

	private async sendNameChangeMessage(channel: Channel, userId: UserID, requestCache: RequestCache): Promise<void> {
		const messageId = createMessageID(await this.deps.snowflakeService.generateForChannel(channel.id));
		const message = await this.deps.messagePersistence.createSystemMessage({
			messageId,
			channelId: channel.id,
			userId,
			type: MessageTypes.CHANNEL_NAME_CHANGE,
			content: channel.name,
			guildId: channel.guildId,
		});
		await this.deps.utils.dispatchMessageCreate({channel, message, requestCache});
	}

	private async recordAudit(params: {
		view: ThreadView;
		userId: UserID;
		action: AuditLogActionType;
		auditLogReason: string | null;
		changes: ReturnType<GuildAuditLogService['computeChanges']>;
	}): Promise<void> {
		try {
			await this.deps.guildAuditLogService
				.createBuilder(params.view.state.guildId, params.userId)
				.withAction(params.action, params.view.channel.id.toString())
				.withReason(params.auditLogReason)
				.withMetadata({type: params.view.channel.type.toString()})
				.withChanges(params.changes)
				.commit();
		} catch (error) {
			Logger.error(
				{error, guildId: params.view.state.guildId.toString(), action: params.action},
				'Failed to record thread audit log',
			);
		}
	}
}
