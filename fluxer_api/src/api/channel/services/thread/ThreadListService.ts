// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import {assertThreadAllowed} from '@app/api/channel/services/thread/ThreadDenials';
import {mapThreadMemberToResponse, mapThreadToResponse} from '@app/api/channel/services/thread/ThreadMappers';
import type {ThreadServiceContext} from '@app/api/channel/services/thread/ThreadServiceContext';
import {loadThreadViews} from '@app/api/channel/services/thread/ThreadViews';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import type {ThreadState} from '@app/api/models/ThreadState';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {THREAD_PARENT_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import {
	canListArchivedThreads,
	isThreadModerator,
	withImplicitThreadBits,
} from '@fluxer/constants/src/ThreadPermissionUtils';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import type {
	ActiveThreadsResponse,
	ArchivedThreadsResponse,
} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';

export class ThreadListService {
	constructor(private readonly ctx: ThreadServiceContext) {}

	async listGuildActive(params: {userId: UserID; guildId: GuildID}): Promise<ActiveThreadsResponse> {
		const {guildId, userId} = params;
		const guild = await this.ctx.gatewayService.getGuildData({guildId, userId});
		const [states, viewableChannelIds, joinedThreadIds] = await Promise.all([
			this.ctx.channelRepository.threads.listActiveThreads(guildId),
			this.ctx.gatewayService.getViewableChannels({guildId, userId}),
			this.ctx.channelRepository.threads.listJoinedThreadIds(userId, guildId),
		]);
		const viewable = new Set(viewableChannelIds.map((id) => id.toString()));
		const joined = new Set(joinedThreadIds);
		const candidates = states.filter((state) => viewable.has(state.parentId.toString()));
		const memberships = await this.memberships(
			candidates.filter((state) => joined.has(state.threadId)),
			userId,
		);
		const privateParents = [
			...new Set(
				candidates
					.filter((state) => state.isPrivate && !memberships.has(state.threadId))
					.map((state) => state.parentId),
			),
		];
		const isOwner = guild.owner_id === userId.toString();
		const timedOut = privateParents.length > 0 && !isOwner && (await this.isTimedOut(guildId, userId));
		const moderatedParents = new Set<ChannelID>();
		await Promise.all(
			privateParents.map(async (parentId) => {
				const permissions = await this.ctx.gatewayService.getUserPermissions({guildId, userId, channelId: parentId});
				if (isThreadModerator(withImplicitThreadBits(permissions), {isOwner, timedOut})) {
					moderatedParents.add(parentId);
				}
			}),
		);
		const visible = candidates
			.filter((state) => !state.isPrivate || memberships.has(state.threadId) || moderatedParents.has(state.parentId))
			.sort((a, b) => (a.threadId > b.threadId ? -1 : a.threadId < b.threadId ? 1 : 0));
		return this.respond(visible, memberships, userId);
	}

	private async isTimedOut(guildId: GuildID, userId: UserID): Promise<boolean> {
		const until = (await this.ctx.guildRepository.getMember(guildId, userId))?.communicationDisabledUntil;
		return until != null && until.getTime() > Date.now();
	}

	async listGuildThreadsForAdmin(guildId: GuildID): Promise<ActiveThreadsResponse['threads']> {
		const threadIds = await this.ctx.channelRepository.threads.listGuildThreadIds(guildId);
		const states = await this.ctx.channelRepository.threads.getStates(threadIds);
		const views = await loadThreadViews(
			this.ctx.channelRepository,
			states.sort((a, b) => (a.threadId > b.threadId ? -1 : a.threadId < b.threadId ? 1 : 0)),
		);
		return views.map((view) => mapThreadToResponse(view));
	}

	async listPublicArchived(params: {
		viewer: ThreadViewer;
		userId: UserID;
		channelId: ChannelID;
		before?: Date;
		limit: number;
	}): Promise<ArchivedThreadsResponse> {
		const auth = await this.authorizeParent(params.viewer, params.userId, params.channelId, false);
		const page = await this.ctx.channelRepository.threads.listArchivedThreads(auth.channel.id, false, {
			before: params.before,
			limit: params.limit,
		});
		return this.respondPage(page.threads, page.hasMore, params.userId, auth);
	}

	async listPrivateArchived(params: {
		viewer: ThreadViewer;
		userId: UserID;
		channelId: ChannelID;
		before?: Date;
		limit: number;
	}): Promise<ArchivedThreadsResponse> {
		const auth = await this.authorizeParent(params.viewer, params.userId, params.channelId, true);
		const page = await this.ctx.channelRepository.threads.listArchivedThreads(auth.channel.id, true, {
			before: params.before,
			limit: params.limit,
		});
		return this.respondPage(page.threads, page.hasMore, params.userId, auth);
	}

	async listJoinedPrivateArchived(params: {
		viewer: ThreadViewer;
		userId: UserID;
		channelId: ChannelID;
		before?: ChannelID;
		limit: number;
	}): Promise<ArchivedThreadsResponse> {
		const auth = await this.authorizeParent(params.viewer, params.userId, params.channelId, false, true);
		const page = await this.ctx.channelRepository.threads.listJoinedPrivateArchivedThreads(
			params.userId,
			auth.channel.guildId!,
			auth.channel.id,
			{before: params.before, limit: params.limit},
		);
		return this.respondPage(page.threads, page.hasMore, params.userId, auth);
	}

	private async authorizeParent(
		viewer: ThreadViewer,
		userId: UserID,
		channelId: ChannelID,
		privateThreads: boolean,
		textOnly = privateThreads,
	): Promise<AuthenticatedChannel> {
		const auth = await this.ctx.channelAuth.getChannelAuthenticated({
			userId,
			channelId,
			viewer,
			skipNsfwValidation: true,
		});
		const {channel} = auth;
		const allowed = textOnly ? channel.type === ChannelTypes.GUILD_TEXT : THREAD_PARENT_CHANNEL_TYPES.has(channel.type);
		if (!auth.guild || !allowed) throw new InvalidChannelTypeError();
		assertThreadAllowed(canListArchivedThreads(await this.ctx.parentActor(auth, userId), {privateThreads}));
		return auth;
	}

	private async memberships(states: Array<ThreadState>, userId: UserID) {
		const members = await Promise.all(
			states.map((state) => this.ctx.channelRepository.threads.getMember(state.threadId, userId)),
		);
		return new Map(members.flatMap((member) => (member ? [[member.threadId, member] as const] : [])));
	}

	private async respond(
		states: Array<ThreadState>,
		memberships: Awaited<ReturnType<ThreadListService['memberships']>>,
		userId: UserID,
		parents: Array<AuthenticatedChannel['channel']> = [],
	): Promise<ActiveThreadsResponse> {
		const views = await loadThreadViews(this.ctx.channelRepository, states, parents);
		return {
			threads: views.map((view) => mapThreadToResponse(view)),
			members: views.flatMap((view) => {
				const member = memberships.get(view.state.threadId);
				return member ? [mapThreadMemberToResponse(member, {self: member.userId === userId})] : [];
			}),
		};
	}

	private async respondPage(
		states: Array<ThreadState>,
		hasMore: boolean,
		userId: UserID,
		auth: AuthenticatedChannel,
	): Promise<ArchivedThreadsResponse> {
		const memberships = await this.memberships(states, userId);
		return {...(await this.respond(states, memberships, userId, [auth.channel])), has_more: hasMore};
	}
}
