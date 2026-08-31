// SPDX-License-Identifier: AGPL-3.0-or-later

import {InviteTypes} from '@fluxer/constants/src/ChannelConstants';
import {UnknownInviteError} from '@fluxer/errors/src/domains/invite/UnknownInviteError';
import type {ChannelPartialResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildPartialResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {
	GroupDmInviteMetadataResponse,
	GroupDmInviteResponse,
	GuildInviteMetadataResponse,
	GuildInviteResponse,
} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import type {z} from 'zod';
import type {ChannelID, GuildID} from '../BrandedTypes';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {Channel} from '../models/Channel';
import type {Invite} from '../models/Invite';
import {getCachedUserPartialResponse, getCachedUserPartialResponses} from '../user/UserCacheHelpers';

interface MapInviteToGuildInviteResponseParams {
	invite: Invite;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	getChannelResponse: (channelId: ChannelID) => Promise<z.infer<typeof ChannelPartialResponse>>;
	getGuildResponse: (guildId: GuildID) => Promise<z.infer<typeof GuildPartialResponse>>;
	getGuildCounts: (guildId: GuildID) => Promise<{
		memberCount: number;
		presenceCount: number;
	}>;
	gatewayService: IGatewayService;
}

interface MapInviteToGuildInviteMetadataResponseParams {
	invite: Invite;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	getChannelResponse: (channelId: ChannelID) => Promise<z.infer<typeof ChannelPartialResponse>>;
	getGuildResponse: (guildId: GuildID) => Promise<z.infer<typeof GuildPartialResponse>>;
	getGuildCounts: (guildId: GuildID) => Promise<{
		memberCount: number;
		presenceCount: number;
	}>;
	gatewayService: IGatewayService;
}

interface MapInviteToGroupDmInviteResponseParams {
	invite: Invite;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	getChannelResponse: (channelId: ChannelID) => Promise<z.infer<typeof ChannelPartialResponse>>;
	getChannelSystem: (channelId: ChannelID) => Promise<Channel | null>;
	getChannelMemberCount: (channelId: ChannelID) => Promise<number>;
}

interface MapInviteToGroupDmInviteMetadataResponseParams {
	invite: Invite;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	getChannelResponse: (channelId: ChannelID) => Promise<z.infer<typeof ChannelPartialResponse>>;
	getChannelSystem: (channelId: ChannelID) => Promise<Channel | null>;
	getChannelMemberCount: (channelId: ChannelID) => Promise<number>;
}

export async function mapInviteToGuildInviteResponse({
	invite,
	userCacheService,
	requestCache,
	getChannelResponse,
	getGuildResponse,
	getGuildCounts,
	gatewayService,
}: MapInviteToGuildInviteResponseParams): Promise<z.infer<typeof GuildInviteResponse>> {
	if (!invite.guildId) {
		throw new UnknownInviteError();
	}
	let channelId = invite.channelId;
	if (!channelId) {
		const resolvedChannelId = await gatewayService.getFirstViewableTextChannel(invite.guildId);
		if (!resolvedChannelId) {
			throw new UnknownInviteError();
		}
		channelId = resolvedChannelId;
	}
	const [channel, guild, inviter, counts] = await Promise.all([
		getChannelResponse(channelId),
		getGuildResponse(invite.guildId),
		invite.inviterId
			? getCachedUserPartialResponse({
					userId: invite.inviterId,
					userCacheService,
					requestCache,
				})
			: null,
		getGuildCounts(invite.guildId),
	]);
	const expiresAt = invite.maxAge > 0 ? new Date(invite.createdAt.getTime() + invite.maxAge * 1000) : null;
	return {
		code: invite.code,
		type: InviteTypes.GUILD,
		guild,
		channel,
		inviter,
		member_count: counts.memberCount,
		presence_count: counts.presenceCount,
		expires_at: expiresAt?.toISOString() ?? null,
		temporary: invite.temporary,
	};
}

export async function mapInviteToGuildInviteMetadataResponse({
	invite,
	userCacheService,
	requestCache,
	getChannelResponse,
	getGuildResponse,
	getGuildCounts,
	gatewayService,
}: MapInviteToGuildInviteMetadataResponseParams): Promise<z.infer<typeof GuildInviteMetadataResponse>> {
	const baseResponse = await mapInviteToGuildInviteResponse({
		invite,
		userCacheService,
		requestCache,
		getChannelResponse,
		getGuildResponse,
		getGuildCounts,
		gatewayService,
	});
	return {
		...baseResponse,
		created_at: invite.createdAt.toISOString(),
		uses: invite.uses,
		max_uses: invite.maxUses,
		max_age: invite.maxAge,
	};
}

export async function mapInviteToGroupDmInviteResponse({
	invite,
	userCacheService,
	requestCache,
	getChannelResponse,
	getChannelSystem,
	getChannelMemberCount,
}: MapInviteToGroupDmInviteResponseParams): Promise<z.infer<typeof GroupDmInviteResponse>> {
	if (!invite.channelId) {
		throw new UnknownInviteError();
	}
	const [channel, inviter, memberCount, channelSystem] = await Promise.all([
		getChannelResponse(invite.channelId),
		invite.inviterId
			? getCachedUserPartialResponse({
					userId: invite.inviterId,
					userCacheService,
					requestCache,
				})
			: null,
		getChannelMemberCount(invite.channelId),
		getChannelSystem(invite.channelId),
	]);
	if (!channelSystem) {
		throw new UnknownInviteError();
	}
	const recipientIds = Array.from(channelSystem.recipientIds);
	const recipientPartials = await getCachedUserPartialResponses({
		userIds: recipientIds,
		userCacheService,
		requestCache,
	});
	const recipients = recipientIds.map((recipientId) => {
		const recipientPartial = recipientPartials.get(recipientId);
		if (!recipientPartial) {
			throw new UnknownInviteError();
		}
		return {username: recipientPartial.username};
	});
	const channelWithRecipients = {...channel, recipients};
	const expiresAt = invite.maxAge > 0 ? new Date(invite.createdAt.getTime() + invite.maxAge * 1000) : null;
	return {
		code: invite.code,
		type: InviteTypes.GROUP_DM,
		channel: channelWithRecipients,
		inviter,
		member_count: memberCount,
		expires_at: expiresAt?.toISOString() ?? null,
		temporary: invite.temporary,
	};
}

export async function mapInviteToGroupDmInviteMetadataResponse({
	invite,
	userCacheService,
	requestCache,
	getChannelResponse,
	getChannelSystem,
	getChannelMemberCount,
}: MapInviteToGroupDmInviteMetadataResponseParams): Promise<z.infer<typeof GroupDmInviteMetadataResponse>> {
	const baseResponse = await mapInviteToGroupDmInviteResponse({
		invite,
		userCacheService,
		requestCache,
		getChannelResponse,
		getChannelSystem,
		getChannelMemberCount,
	});
	return {
		...baseResponse,
		created_at: invite.createdAt.toISOString(),
		uses: invite.uses,
		max_uses: invite.maxUses,
	};
}
