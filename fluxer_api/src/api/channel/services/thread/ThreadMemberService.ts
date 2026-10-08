// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createMessageID, type UserID} from '@app/api/BrandedTypes';
import type {AuthenticatedChannel, AuthenticatedThread} from '@app/api/channel/services/AuthenticatedChannel';
import {dispatchMessageCreateBroadcast} from '@app/api/channel/services/message/MessageGatewayDispatch';
import {assertThreadAllowed} from '@app/api/channel/services/thread/ThreadDenials';
import {dispatchThreadEvents, threadMembersUpdateEvent} from '@app/api/channel/services/thread/ThreadDispatch';
import {mapThreadMemberToResponse} from '@app/api/channel/services/thread/ThreadMappers';
import type {ThreadServiceContext} from '@app/api/channel/services/thread/ThreadServiceContext';
import {loadThreadView} from '@app/api/channel/services/thread/ThreadViews';
import {recipientActive, type ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import {mapGuildMemberToResponse} from '@app/api/guild/GuildModel';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {ThreadMember} from '@app/api/models/ThreadMember';
import type {User} from '@app/api/models/User';
import {MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	canAddThreadMember,
	canJoinThread,
	canLeave,
	canRemoveThreadMember,
	isThreadModerator,
	ThreadPermissionFlags,
	withImplicitThreadBits,
} from '@fluxer/constants/src/ThreadPermissionUtils';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {UnknownThreadMemberError} from '@fluxer/errors/src/domains/channel/UnknownThreadMemberError';
import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';

type ThreadAuth = AuthenticatedChannel & {thread: AuthenticatedThread};

export class ThreadMemberService {
	constructor(private readonly ctx: ThreadServiceContext) {}

	async authorizeThread(viewer: ThreadViewer, userId: UserID, channelId: ChannelID): Promise<ThreadAuth> {
		const auth = await this.ctx.channelAuth.getChannelAuthenticated({
			userId,
			channelId,
			viewer,
			skipNsfwValidation: true,
		});
		if (!auth.thread) throw new InvalidChannelTypeError();
		return auth as ThreadAuth;
	}

	async join(params: {viewer: ThreadViewer; user: User; channelId: ChannelID}): Promise<void> {
		const auth = await this.authorizeThread(params.viewer, params.user.id, params.channelId);
		assertThreadAllowed(canJoinThread(auth.thread.actor));
		if (auth.thread.state.locked) auth.thread.enforceMfa(ThreadPermissionFlags.MANAGE_THREADS);
		if (auth.thread.member) return;
		await this.addMember(auth, params.user.id);
	}

	async add(params: {viewer: ThreadViewer; user: User; channelId: ChannelID; targetId: UserID}): Promise<void> {
		const auth = await this.authorizeThread(params.viewer, params.user.id, params.channelId);
		const {thread} = auth;
		const guildId = thread.state.guildId;
		const targetMember = await this.ctx.guildRepository.getMember(guildId, params.targetId);
		if (!targetMember) throw new UnknownGuildMemberError();
		const targetPermissions = await this.ctx.gatewayService.getUserPermissions({
			guildId,
			userId: params.targetId,
			channelId: thread.parent.id,
		});
		const targetIsOwner = auth.guild?.owner_id === params.targetId.toString();
		const targetUser = await this.ctx.userRepository.findUnique(params.targetId);
		const targetIsModerator = isThreadModerator(withImplicitThreadBits(targetPermissions), {
			isOwner: targetIsOwner,
			timedOut:
				!targetIsOwner &&
				targetMember.communicationDisabledUntil !== null &&
				targetMember.communicationDisabledUntil.getTime() > Date.now(),
		});
		const canViewParent =
			(targetIsOwner || (withImplicitThreadBits(targetPermissions) & Permissions.VIEW_CHANNEL) !== 0n) &&
			targetUser !== null &&
			recipientActive(guildId, params.targetId, targetUser.isBot);
		assertThreadAllowed(canAddThreadMember(thread.actor, {canViewParent, isModerator: targetIsModerator}));
		if (thread.state.locked || (thread.state.isPrivate && !(thread.state.invitable ?? true) && !targetIsModerator)) {
			thread.enforceMfa(ThreadPermissionFlags.MANAGE_THREADS);
		}
		const existing = await this.ctx.channelRepository.threads.getMember(thread.state.threadId, params.targetId);
		if (existing) return;
		const added = await this.addMember(auth, params.targetId);
		if (added && params.targetId !== params.user.id) {
			await this.sendRecipientMessage(auth, params.user.id, params.targetId, MessageTypes.RECIPIENT_ADD);
		}
	}

	async leave(params: {viewer: ThreadViewer; user: User; channelId: ChannelID}): Promise<void> {
		const auth = await this.authorizeThread(params.viewer, params.user.id, params.channelId);
		assertThreadAllowed(canLeave(auth.thread.actor));
		await this.removeMember(auth, params.user.id);
	}

	async remove(params: {viewer: ThreadViewer; user: User; channelId: ChannelID; targetId: UserID}): Promise<void> {
		if (params.targetId === params.user.id) {
			await this.leave(params);
			return;
		}
		const auth = await this.authorizeThread(params.viewer, params.user.id, params.channelId);
		const {thread} = auth;
		assertThreadAllowed(canRemoveThreadMember(thread.actor));
		if (!(thread.state.isPrivate && thread.actor.isThreadOwner)) {
			thread.enforceMfa(ThreadPermissionFlags.MANAGE_THREADS);
		}
		const existing = await this.ctx.channelRepository.threads.getMember(thread.state.threadId, params.targetId);
		if (!existing) throw new UnknownThreadMemberError();
		if (!(await this.removeMember(auth, params.targetId))) return;
		await this.sendRecipientMessage(auth, params.user.id, params.targetId, MessageTypes.RECIPIENT_REMOVE);
	}

	async list(params: {
		viewer: ThreadViewer;
		user: User;
		channelId: ChannelID;
		after?: UserID;
		limit: number;
		withMember: boolean;
		requestCache: RequestCache;
	}): Promise<Array<ThreadMemberResponse>> {
		const auth = await this.authorizeThread(params.viewer, params.user.id, params.channelId);
		const members = await this.ctx.channelRepository.threads.listMembers(auth.thread.state.threadId, {
			after: params.after,
			limit: params.limit,
		});
		return this.mapMembers(auth, members, params.user.id, params.withMember, params.requestCache);
	}

	async get(params: {
		viewer: ThreadViewer;
		user: User;
		channelId: ChannelID;
		targetId: UserID;
		withMember: boolean;
		requestCache: RequestCache;
	}): Promise<ThreadMemberResponse> {
		const auth = await this.authorizeThread(params.viewer, params.user.id, params.channelId);
		const member = await this.ctx.channelRepository.threads.getMember(auth.thread.state.threadId, params.targetId);
		if (!member) throw new UnknownThreadMemberError();
		const [response] = await this.mapMembers(auth, [member], params.user.id, params.withMember, params.requestCache);
		return response!;
	}

	private async mapMembers(
		auth: ThreadAuth,
		members: Array<ThreadMember>,
		viewerId: UserID,
		withMember: boolean,
		requestCache: RequestCache,
	): Promise<Array<ThreadMemberResponse>> {
		const guildMembers = new Map<UserID, GuildMemberResponse>();
		if (withMember) {
			const loaded = await Promise.all(
				members.map((member) => this.ctx.guildRepository.getMember(auth.thread.state.guildId, member.userId)),
			);
			for (const guildMember of loaded) {
				if (!guildMember) continue;
				guildMembers.set(
					guildMember.userId,
					await mapGuildMemberToResponse(guildMember, this.ctx.userCacheService, requestCache),
				);
			}
		}
		return members.map((member) =>
			mapThreadMemberToResponse(member, {
				self: member.userId === viewerId,
				guildMember: guildMembers.get(member.userId),
			}),
		);
	}

	private async addMember(auth: ThreadAuth, userId: UserID): Promise<boolean> {
		const threads = this.ctx.channelRepository.threads;
		const result = await threads.addMembers(auth.thread.state.threadId, [{userId, flags: 0}]);
		if (!result || result.added.length === 0) return false;
		const view = await loadThreadView(this.ctx.channelRepository, auth.channel, result.state, auth.thread.parent);
		await dispatchThreadEvents(this.ctx.gatewayService, result.state.guildId, [
			threadMembersUpdateEvent(view, {added: result.added}),
		]);
		return true;
	}

	private async removeMember(auth: ThreadAuth, userId: UserID): Promise<boolean> {
		const result = await this.ctx.channelRepository.threads.removeMembers(auth.thread.state.threadId, [userId]);
		if (result.removed.length === 0 || !result.state) return false;
		const view = await loadThreadView(this.ctx.channelRepository, auth.channel, result.state, auth.thread.parent);
		await dispatchThreadEvents(this.ctx.gatewayService, result.state.guildId, [
			threadMembersUpdateEvent(view, {removedUserIds: result.removed.map((member) => member.userId)}),
		]);
		return true;
	}

	private async sendRecipientMessage(
		auth: ThreadAuth,
		actorId: UserID,
		targetId: UserID,
		type: typeof MessageTypes.RECIPIENT_ADD | typeof MessageTypes.RECIPIENT_REMOVE,
	): Promise<void> {
		const messageId = createMessageID(await this.ctx.snowflakeService.generateForChannel(auth.channel.id));
		const message = await this.ctx.messagePersistence.createSystemMessage({
			messageId,
			channelId: auth.channel.id,
			userId: actorId,
			type,
			guildId: auth.channel.guildId,
			mentionUserIds: [targetId],
		});
		await dispatchMessageCreateBroadcast({gatewayService: this.ctx.gatewayService, channel: auth.channel, message});
	}
}
