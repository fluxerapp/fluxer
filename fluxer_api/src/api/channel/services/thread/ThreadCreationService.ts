// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelID,
	channelIdToMessageId,
	createChannelID,
	createGuildID,
	createMessageID,
	type MessageID,
	type UserID,
} from '@app/api/BrandedTypes';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {MessageRequest} from '@app/api/channel/MessageTypes';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import {withChannelFollowLock} from '@app/api/channel/services/ChannelFollowers';
import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import {buildBroadcastMessageData} from '@app/api/channel/services/message/MessageGatewayDispatch';
import {MessageWriteLock} from '@app/api/channel/services/message/MessageWriteLock';
import {resolveAppliedTags} from '@app/api/channel/services/thread/ForumTagRules';
import {assertThreadAllowed} from '@app/api/channel/services/thread/ThreadDenials';
import {
	dispatchThreadEvents,
	type ThreadDispatchEvent,
	threadCreateEvent,
	threadDeleteEvent,
} from '@app/api/channel/services/thread/ThreadDispatch';
import {
	mapThreadToResponse,
	serializeThreadForAudit,
	type ThreadView,
} from '@app/api/channel/services/thread/ThreadMappers';
import {withThreadParentFields} from '@app/api/channel/services/thread/ThreadParentSettings';
import type {ThreadServiceContext} from '@app/api/channel/services/thread/ThreadServiceContext';
import {loadThreadView} from '@app/api/channel/services/thread/ThreadViews';
import {enqueueThreadSearchSync} from '@app/api/channel/threads/ThreadJobs';
import {threadsCreatedTotal} from '@app/api/channel/threads/ThreadMetrics';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import type {MessageRow} from '@app/api/database/types/MessageTypes';
import {ensureActiveGuildTainted, type ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import {Logger} from '@app/api/Logger';
import {createRequestCache, type RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import type {ThreadMember} from '@app/api/models/ThreadMember';
import type {ThreadState} from '@app/api/models/ThreadState';
import type {User} from '@app/api/models/User';
import {deleteThreadSearchDocuments} from '@app/api/search/thread/ThreadSearchService';
import {assertAccountNotLimited} from '@app/api/user/AccountLimit';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, MessageReferenceTypes, MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	DEFAULT_THREAD_AUTO_ARCHIVE_DURATION,
	MAX_ACTIVE_THREADS_PER_GUILD,
	publicThreadTypeFor,
	resolveTextThreadType,
	ServerMessageFlags,
	TEXT_THREAD_PARENT_CHANNEL_TYPES,
	ThreadMemberFlags,
} from '@fluxer/constants/src/ThreadConstants';
import {
	canCreateThread,
	isThreadModerator,
	type ThreadCreateKind,
	withImplicitThreadBits,
} from '@fluxer/constants/src/ThreadPermissionUtils';
import {CannotEditSystemMessageError} from '@fluxer/errors/src/domains/channel/CannotEditSystemMessageError';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {MaxActiveThreadsError} from '@fluxer/errors/src/domains/channel/MaxActiveThreadsError';
import {ThreadAlreadyCreatedForMessageError} from '@fluxer/errors/src/domains/channel/ThreadAlreadyCreatedForMessageError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {SlowmodeRateLimitError} from '@fluxer/errors/src/domains/core/SlowmodeRateLimitError';
import type {
	StartForumThreadRequest,
	StartForumThreadResponse,
} from '@fluxer/schema/src/domains/channel/ForumRequestSchemas';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';

const THREAD_CREATED_MESSAGE_RECENT_WINDOW = 5;

function slowmodeThreadKey(parentId: ChannelID, userId: UserID): string {
	return `slowmode-thread:${parentId}:${userId}`;
}

export type ForumPostInput = Omit<StartForumThreadRequest, 'message'> & {message: MessageRequest};

interface CreateThreadInput {
	name: string;
	autoArchiveDuration?: number;
	rateLimitPerUser?: number;
}

type ThreadCreator = {kind: 'user'; user: User; parentAuth: AuthenticatedChannel} | {kind: 'webhook'; ownerId: UserID};

interface NewThread {
	parent: Channel;
	creator: ThreadCreator;
	threadId: ChannelID;
	type: number;
	input: CreateThreadInput;
	invitable: boolean | null;
	hasStarter: boolean;
	appliedTags?: Array<bigint>;
}

export class ThreadCreationService {
	constructor(private readonly ctx: ThreadServiceContext) {}

	async createFromMessage(params: {
		viewer: ThreadViewer;
		user: User;
		channelId: ChannelID;
		messageId: MessageID;
		input: CreateThreadInput;
		requestCache: RequestCache;
		auditLogReason: string | null;
	}): Promise<ThreadChannelResponse> {
		const parentAuth = await this.authorizeParent(params.viewer, params.user, params.channelId, 'from_message');
		const source = await this.ctx.channelRepository.messages.getMessage(parentAuth.channel.id, params.messageId);
		if (!source) throw new UnknownMessageError();
		if (source.type !== MessageTypes.DEFAULT && source.type !== MessageTypes.REPLY) {
			throw new CannotEditSystemMessageError();
		}
		if ((source.flags & ServerMessageFlags.HAS_THREAD) !== 0) throw new ThreadAlreadyCreatedForMessageError();
		const {view, members} = await this.create({
			parent: parentAuth.channel,
			creator: {kind: 'user', user: params.user, parentAuth},
			threadId: createChannelID(BigInt(source.id)),
			type: publicThreadTypeFor(parentAuth.channel.type),
			input: params.input,
			invitable: null,
			hasStarter: true,
		});
		await this.recordCreateAudit(view, params.user.id, params.auditLogReason);
		const events: Array<ThreadDispatchEvent> = [threadCreateEvent(view, {members, newlyCreated: true})];
		const updatedSource = await new MessageWriteLock(
			this.ctx.cacheService,
			this.ctx.channelRepository.messages,
		).withFreshMessage(parentAuth.channel.id, source.id, async (current) =>
			current
				? this.ctx.channelRepository.messages.upsertMessage(
						{...current.toRow(), flags: current.flags | ServerMessageFlags.HAS_THREAD},
						current.toRow(),
					)
				: null,
		);
		if (updatedSource) {
			events.push({
				event: 'MESSAGE_UPDATE',
				data: {
					...(await buildBroadcastMessageData({channel: parentAuth.channel, message: updatedSource})),
					thread: mapThreadToResponse(view),
					__thread_only_update: true,
				},
			});
		}
		const starter = await this.buildStarterEvent(params.viewer, params.user.id, view, params.requestCache);
		if (starter) events.push(starter);
		if (!(await this.isAmongRecentMessages(parentAuth.channel.id, source.id))) {
			events.push(await this.createThreadCreatedMessage(parentAuth.channel, view, params.user.id));
		}
		await dispatchThreadEvents(this.ctx.gatewayService, view.state.guildId, events);
		return mapThreadToResponse(view);
	}

	private async isAmongRecentMessages(channelId: ChannelID, messageId: MessageID): Promise<boolean> {
		const recent = await this.ctx.channelRepository.messages.listMessages(
			channelId,
			undefined,
			THREAD_CREATED_MESSAGE_RECENT_WINDOW,
		);
		return recent.some((message) => message.id === messageId);
	}

	async authenticateParent(viewer: ThreadViewer, userId: UserID, channelId: ChannelID): Promise<AuthenticatedChannel> {
		return this.ctx.channelAuth.getChannelAuthenticated({userId, channelId, viewer});
	}

	async createTextThread(params: {
		user: User;
		parentAuth: AuthenticatedChannel;
		type: number;
		invitable?: boolean;
		input: CreateThreadInput;
		auditLogReason: string | null;
	}): Promise<ThreadChannelResponse> {
		const type = resolveTextThreadType(params.parentAuth.channel.type, params.type);
		if (type === null) throw new InvalidChannelTypeError();
		const isPrivate = type === ChannelTypes.PRIVATE_THREAD;
		const parentAuth = await this.authorizeAuthenticatedParent(
			params.parentAuth,
			params.user,
			isPrivate ? 'private' : 'public',
		);
		const threadId = createChannelID(await this.ctx.snowflakeService.generate());
		const {view, members} = await this.create({
			parent: parentAuth.channel,
			creator: {kind: 'user', user: params.user, parentAuth},
			threadId,
			type,
			input: params.input,
			invitable: isPrivate ? (params.invitable ?? true) : null,
			hasStarter: false,
		});
		await this.recordCreateAudit(view, params.user.id, params.auditLogReason);
		const events: Array<ThreadDispatchEvent> = [threadCreateEvent(view, {members, newlyCreated: true})];
		if (!isPrivate) events.push(await this.createThreadCreatedMessage(parentAuth.channel, view, params.user.id));
		await dispatchThreadEvents(this.ctx.gatewayService, view.state.guildId, events);
		return mapThreadToResponse(view);
	}

	async createForumPost(params: {
		viewer: ThreadViewer;
		user: User;
		parentAuth: AuthenticatedChannel;
		body: ForumPostInput;
		requestCache: RequestCache;
		auditLogReason: string | null;
	}): Promise<StartForumThreadResponse> {
		const {parentAuth, user, body} = params;
		assertAccountNotLimited(user);
		const {channel: parent, guild} = parentAuth;
		if (!guild || !parent.isThreadOnly()) throw new InvalidChannelTypeError();
		if (body.type !== undefined && body.type !== ChannelTypes.PUBLIC_THREAD) throw new InvalidChannelTypeError();
		const actor = await this.ctx.parentActor(parentAuth, user.id);
		assertThreadAllowed(canCreateThread(actor, 'forum_post'));
		const config = await this.ctx.channelRepository.threads.getParentConfig(parent.guildId!, parent.id);
		const appliedTags = resolveAppliedTags({
			config,
			requested: (body.applied_tags ?? []).map((id) => BigInt(id)),
			previous: [],
			moderator: isThreadModerator(withImplicitThreadBits(actor.permissions), actor),
		});
		await this.ctx.validateForumStarter({user, parentAuth, data: body.message});
		const threadId = createChannelID(await this.ctx.snowflakeService.generate());
		const {view, members} = await this.create({
			parent,
			creator: {kind: 'user', user, parentAuth},
			threadId,
			type: ChannelTypes.PUBLIC_THREAD,
			input: {
				name: body.name,
				autoArchiveDuration: body.auto_archive_duration,
				rateLimitPerUser: body.rate_limit_per_user,
			},
			invitable: null,
			hasStarter: true,
			appliedTags,
		});
		await dispatchThreadEvents(this.ctx.gatewayService, view.state.guildId, [
			threadCreateEvent(view, {members, newlyCreated: true}),
		]);
		let message: MessageResponse;
		try {
			message = await this.ctx.sendForumStarter({
				user,
				viewer: params.viewer,
				parentAuth,
				threadId,
				data: body.message,
				requestCache: params.requestCache,
			});
		} catch (error) {
			await this.discardThread(view, members, parent);
			await this.ctx.rateLimitService.resetLimit(slowmodeThreadKey(parent.id, user.id));
			throw error;
		}
		await this.recordCreateAudit(view, user.id, params.auditLogReason);
		await this.ackForumPost(params.viewer, user, parent, threadId);
		const created = await this.ctx.channelRepository.channelData.findUnique(threadId);
		return {...mapThreadToResponse(created ? {...view, channel: created} : view), message};
	}

	async createWebhookForumPost(params: {
		webhookId: UserID;
		parent: Channel;
		name: string;
		appliedTags: Array<bigint>;
		sendStarter: (thread: Channel) => Promise<Message>;
	}): Promise<Message> {
		const {parent} = params;
		if (!parent.isThreadOnly() || parent.guildId === null) throw new InvalidChannelTypeError();
		const config = await this.ctx.channelRepository.threads.getParentConfig(parent.guildId, parent.id);
		const appliedTags = resolveAppliedTags({config, requested: params.appliedTags, previous: [], moderator: false});
		const threadId = createChannelID(await this.ctx.snowflakeService.generate());
		const {view, members} = await this.create({
			parent,
			creator: {kind: 'webhook', ownerId: params.webhookId},
			threadId,
			type: ChannelTypes.PUBLIC_THREAD,
			input: {name: params.name},
			invitable: null,
			hasStarter: true,
			appliedTags,
		});
		await dispatchThreadEvents(this.ctx.gatewayService, view.state.guildId, [
			threadCreateEvent(view, {members, newlyCreated: true}),
		]);
		try {
			return await params.sendStarter(view.channel);
		} catch (error) {
			await this.discardThread(view, members, parent);
			throw error;
		}
	}

	private async discardThread(view: ThreadView, members: Array<ThreadMember>, parent: Channel): Promise<void> {
		await this.ctx.channelRepository.threads.purgeThread(view.channel.id);
		await this.ctx.channelRepository.threads.revertParentLastMessageId(
			parent.id,
			view.channel.id,
			parent.lastMessageId,
		);
		await this.ctx.archiveQueue.remove(view.state.guildId, view.channel.id);
		await deleteThreadSearchDocuments([view.channel.id]);
		const reverted = await this.ctx.channelRepository.channelData.findUnique(parent.id);
		if (reverted) {
			await dispatchChannelEvent({
				gatewayService: this.ctx.gatewayService,
				channel: reverted,
				event: 'CHANNEL_UPDATE',
				data: await withThreadParentFields(
					this.ctx.channelRepository.threads,
					reverted,
					await mapChannelToResponse({
						channel: reverted,
						currentUserId: null,
						userCacheService: this.ctx.userCacheService,
						requestCache: createRequestCache(),
					}),
				),
			});
		}
		await dispatchThreadEvents(this.ctx.gatewayService, view.state.guildId, [
			threadDeleteEvent(
				view.state,
				members.map((member) => member.userId),
			),
		]);
	}

	private async ackForumPost(viewer: ThreadViewer, user: User, parent: Channel, threadId: ChannelID): Promise<void> {
		if (user.isBot || viewer.kind !== 'user') return;
		try {
			await this.ctx.readStateService.ackMessage({
				userId: user.id,
				channelId: parent.id,
				messageId: channelIdToMessageId(threadId),
				mentionCount: 0,
				capable: viewer.capable,
				channel: {type: parent.type, guildId: parent.guildId},
			});
		} catch (error) {
			Logger.warn({error, channelId: parent.id.toString()}, 'Failed to ack forum after creating a post');
		}
	}

	private async authorizeParent(
		viewer: ThreadViewer,
		user: User,
		channelId: ChannelID,
		kind: ThreadCreateKind,
	): Promise<AuthenticatedChannel> {
		const parentAuth = await this.ctx.channelAuth.getChannelAuthenticated({userId: user.id, channelId, viewer});
		return this.authorizeAuthenticatedParent(parentAuth, user, kind);
	}

	private async authorizeAuthenticatedParent(
		parentAuth: AuthenticatedChannel,
		user: User,
		kind: ThreadCreateKind,
	): Promise<AuthenticatedChannel> {
		assertAccountNotLimited(user);
		const {channel, guild} = parentAuth;
		if (!guild || !TEXT_THREAD_PARENT_CHANNEL_TYPES.has(channel.type)) throw new InvalidChannelTypeError();
		assertThreadAllowed(canCreateThread(await this.ctx.parentActor(parentAuth, user.id), kind));
		return parentAuth;
	}

	private async enforceCreateSlowmode(parentAuth: AuthenticatedChannel, user: User): Promise<void> {
		const rateLimitPerUser = parentAuth.channel.rateLimitPerUser ?? 0;
		if (rateLimitPerUser <= 0 || user.isBot) return;
		if (await parentAuth.hasPermission(Permissions.BYPASS_SLOWMODE)) return;
		const result = await this.ctx.rateLimitService.checkLimit({
			identifier: slowmodeThreadKey(parentAuth.channel.id, user.id),
			maxAttempts: 1,
			windowMs: rateLimitPerUser * 1000,
			algorithm: 'leaky_bucket',
		});
		if (!result.allowed) {
			throw new SlowmodeRateLimitError({retryAfter: result.retryAfter, retryAfterDecimal: result.retryAfterDecimal});
		}
	}

	private async create(params: NewThread): Promise<{view: ThreadView; members: Array<ThreadMember>}> {
		const {parent, creator} = params;
		const guildId = parent.guildId!;
		const ownerId = creator.kind === 'user' ? creator.user.id : creator.ownerId;
		const {threads} = this.ctx.channelRepository;
		if ((await threads.countActiveThreads(guildId)) >= MAX_ACTIVE_THREADS_PER_GUILD) {
			throw new MaxActiveThreadsError(MAX_ACTIVE_THREADS_PER_GUILD);
		}
		if (creator.kind === 'user') await this.enforceCreateSlowmode(creator.parentAuth, creator.user);
		const state = await this.storeThread(params).catch(async (error: unknown) => {
			if (creator.kind === 'user') {
				await this.ctx.rateLimitService.resetLimit(slowmodeThreadKey(parent.id, creator.user.id));
			}
			throw error;
		});
		const channel = await this.ctx.channelRepository.channelData.findUnique(params.threadId);
		if (!channel) throw new InvalidChannelTypeError();
		const view = await loadThreadView(this.ctx.channelRepository, channel, state, parent);
		const members = creator.kind === 'user' ? await threads.getMembers(params.threadId, [ownerId]) : [];
		const kind = parent.isThreadOnly() ? 'forum_post' : params.hasStarter ? 'from_message' : 'standalone';
		threadsCreatedTotal.inc(`kind="${kind}",type="${params.type}"`);
		await this.ctx.archiveQueue.schedule(state, null);
		enqueueThreadSearchSync(params.threadId);
		return {view, members};
	}

	private async storeThread(params: NewThread): Promise<ThreadState> {
		const {parent, creator} = params;
		const guildId = parent.guildId!;
		const ownerId = creator.kind === 'user' ? creator.user.id : creator.ownerId;
		const {threads} = this.ctx.channelRepository;
		contentModerationService.scanText(params.input.name, {
			userId: ownerId,
			guildId,
			channelId: params.threadId,
			messageId: null,
			surface: 'profile_field',
		});
		const parentConfig = await threads.getParentConfig(guildId, parent.id);
		await ensureActiveGuildTainted(guildId);
		const now = new Date();
		const row: ChannelRow = {
			channel_id: params.threadId,
			guild_id: guildId,
			type: params.type,
			name: params.input.name,
			topic: null,
			icon_hash: null,
			url: null,
			parent_id: parent.id,
			position: null,
			owner_id: ownerId,
			recipient_ids: null,
			nsfw: null,
			rate_limit_per_user: params.input.rateLimitPerUser ?? parentConfig?.defaultThreadRateLimitPerUser ?? 0,
			bitrate: null,
			user_limit: null,
			voice_connection_limit: null,
			rtc_region: null,
			rtc_p2p: null,
			last_message_id: null,
			last_pin_timestamp: null,
			permission_overwrites: null,
			nicks: null,
			soft_deleted: false,
			indexed_at: now,
			version: 1,
		};
		const write = () =>
			threads.create({
				channel: row,
				parentType: parent.type,
				autoArchiveDuration: params.input.autoArchiveDuration ?? DEFAULT_THREAD_AUTO_ARCHIVE_DURATION,
				invitable: params.invitable,
				flags: 0,
				appliedTags: params.appliedTags ?? [],
				hasStarter: params.hasStarter,
				createdAt: now,
				members:
					creator.kind === 'user'
						? [{userId: creator.user.id, flags: creator.user.isBot ? 0 : ThreadMemberFlags.HAS_INTERACTED}]
						: [],
			});
		if (!TEXT_THREAD_PARENT_CHANNEL_TYPES.has(parent.type)) return write();
		return withChannelFollowLock(this.ctx.cacheService, parent.id, async () => {
			const current = await this.ctx.channelRepository.channelData.findUnique(parent.id);
			if (current?.type !== parent.type) throw new InvalidChannelTypeError();
			return write();
		});
	}

	private async recordCreateAudit(view: ThreadView, userId: UserID, auditLogReason: string | null): Promise<void> {
		await this.ctx.recordAudit({
			guildId: view.state.guildId,
			userId,
			action: AuditLogActionType.THREAD_CREATE,
			targetId: view.channel.id,
			auditLogReason,
			changes: this.ctx.guildAuditLogService.computeChanges(null, serializeThreadForAudit(view)),
		});
	}

	private async buildStarterEvent(
		viewer: ThreadViewer,
		userId: UserID,
		view: ThreadView,
		requestCache: RequestCache,
	): Promise<ThreadDispatchEvent | null> {
		const threadAuth = await this.ctx.channelAuth.getChannelAuthenticated({
			userId,
			channelId: view.channel.id,
			viewer,
			skipNsfwValidation: true,
		});
		const starter = await this.ctx.threadMessageResponses.getStarter({
			viewer,
			userId,
			authChannel: threadAuth,
			messageId: createMessageID(BigInt(view.channel.id)),
			requestCache,
		});
		if (!starter) return null;
		const referenced = starter.referenced_message;
		if (referenced?.thread) {
			const {member: _member, ...thread} = referenced.thread;
			starter.referenced_message = {...referenced, thread};
		}
		return {event: 'MESSAGE_CREATE', data: {...starter, channel_type: view.channel.type}};
	}

	private async createThreadCreatedMessage(
		parent: Channel,
		view: ThreadView,
		userId: UserID,
	): Promise<ThreadDispatchEvent> {
		const messageId = createMessageID(await this.ctx.snowflakeService.generateForChannel(parent.id));
		const row: MessageRow = {
			channel_id: parent.id,
			bucket: BucketUtils.makeBucket(messageId),
			message_id: messageId,
			author_id: userId,
			type: MessageTypes.THREAD_CREATED,
			webhook_id: null,
			webhook_name: null,
			webhook_avatar_hash: null,
			content: view.channel.name,
			edited_timestamp: null,
			pinned_timestamp: null,
			flags: 0,
			mention_everyone: false,
			mention_users: null,
			mention_roles: null,
			mention_channels: null,
			attachments: null,
			embeds: null,
			sticker_items: null,
			message_reference: {
				channel_id: view.channel.id,
				message_id: null as unknown as MessageID,
				guild_id: createGuildID(BigInt(view.state.guildId)),
				type: MessageReferenceTypes.DEFAULT,
			},
			message_snapshots: null,
			call: null,
			has_reaction: false,
			version: 1,
		};
		const message: Message = await this.ctx.channelRepository.messages.upsertMessage(row, null, {
			skipParentLastMessageId: true,
		});
		return {
			event: 'MESSAGE_CREATE',
			data: {...(await buildBroadcastMessageData({channel: parent, message})), channel_type: parent.type},
		};
	}
}
