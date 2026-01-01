/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {ChannelID, GuildID} from '~/BrandedTypes';
import {InviteTypes} from '~/Constants';
import {ChannelPartialResponse} from '~/channel/ChannelModel';
import {UnknownInviteError} from '~/Errors';
import {UnknownPackError} from '~/errors/UnknownPackError';
import {GuildPartialResponse} from '~/guild/GuildModel';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Channel, Invite} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {mapPackToSummary} from '~/pack/PackModel';
import type {PackRepository} from '~/pack/PackRepository';
import {z} from '~/Schema';
import {getCachedUserPartialResponse, getCachedUserPartialResponses} from '~/user/UserCacheHelpers';
import {UserPartialResponse} from '~/user/UserModel';

export const GuildInviteResponse = z.object({
	code: z.string(),
	type: z.literal(InviteTypes.GUILD),
	guild: GuildPartialResponse,
	channel: z.lazy(() => ChannelPartialResponse),
	inviter: z.lazy(() => UserPartialResponse).nullish(),
	member_count: z.number().int(),
	presence_count: z.number().int(),
	expires_at: z.iso.datetime().nullish(),
	temporary: z.boolean(),
});
export type GuildInviteResponse = z.infer<typeof GuildInviteResponse>;

export const GroupDmInviteResponse = z.object({
	code: z.string(),
	type: z.literal(InviteTypes.GROUP_DM),
	channel: z.lazy(() => ChannelPartialResponse),
	inviter: z.lazy(() => UserPartialResponse).nullish(),
	member_count: z.number().int(),
	expires_at: z.iso.datetime().nullish(),
	temporary: z.boolean(),
});
export type GroupDmInviteResponse = z.infer<typeof GroupDmInviteResponse>;

const PackInfoResponse = z.object({
	id: z.string(),
	name: z.string(),
	description: z.string().nullish(),
	type: z.enum(['emoji', 'sticker']),
	creator_id: z.string(),
	created_at: z.iso.datetime(),
	updated_at: z.iso.datetime(),
	creator: z.lazy(() => UserPartialResponse),
});

export const PackInviteResponse = z.object({
	code: z.string(),
	type: z.union([z.literal(InviteTypes.EMOJI_PACK), z.literal(InviteTypes.STICKER_PACK)]),
	pack: PackInfoResponse,
	inviter: z.lazy(() => UserPartialResponse).nullish(),
	expires_at: z.iso.datetime().nullish(),
	temporary: z.boolean(),
});
export type PackInviteResponse = z.infer<typeof PackInviteResponse>;

export const PackInviteMetadataResponse = z.object({
	...PackInviteResponse.shape,
	created_at: z.iso.datetime(),
	uses: z.number().int(),
	max_uses: z.number().int(),
});
export type PackInviteMetadataResponse = z.infer<typeof PackInviteMetadataResponse>;

export const GuildInviteMetadataResponse = z.object({
	...GuildInviteResponse.shape,
	created_at: z.iso.datetime(),
	uses: z.number().int(),
	max_uses: z.number().int(),
});
export type GuildInviteMetadataResponse = z.infer<typeof GuildInviteMetadataResponse>;

export const GroupDmInviteMetadataResponse = z.object({
	...GroupDmInviteResponse.shape,
	created_at: z.iso.datetime(),
	uses: z.number().int(),
	max_uses: z.number().int(),
});
export type GroupDmInviteMetadataResponse = z.infer<typeof GroupDmInviteMetadataResponse>;

interface MapInviteToGuildInviteResponseParams {
	invite: Invite;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	getChannelResponse: (channelId: ChannelID) => Promise<ChannelPartialResponse>;
	getGuildResponse: (guildId: GuildID) => Promise<GuildPartialResponse>;
	getGuildCounts: (guildId: GuildID) => Promise<{memberCount: number; presenceCount: number}>;
	gatewayService: IGatewayService;
}

interface MapInviteToGuildInviteMetadataResponseParams {
	invite: Invite;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	getChannelResponse: (channelId: ChannelID) => Promise<ChannelPartialResponse>;
	getGuildResponse: (guildId: GuildID) => Promise<GuildPartialResponse>;
	getGuildCounts: (guildId: GuildID) => Promise<{memberCount: number; presenceCount: number}>;
	gatewayService: IGatewayService;
}

interface MapInviteToGroupDmInviteResponseParams {
	invite: Invite;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	getChannelResponse: (channelId: ChannelID) => Promise<ChannelPartialResponse>;
	getChannelSystem: (channelId: ChannelID) => Promise<Channel | null>;
	getChannelMemberCount: (channelId: ChannelID) => Promise<number>;
}

interface MapInviteToGroupDmInviteMetadataResponseParams {
	invite: Invite;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	getChannelResponse: (channelId: ChannelID) => Promise<ChannelPartialResponse>;
	getChannelSystem: (channelId: ChannelID) => Promise<Channel | null>;
	getChannelMemberCount: (channelId: ChannelID) => Promise<number>;
}

export const mapInviteToGuildInviteResponse = async ({
	invite,
	userCacheService,
	requestCache,
	getChannelResponse,
	getGuildResponse,
	getGuildCounts,
	gatewayService,
}: MapInviteToGuildInviteResponseParams): Promise<GuildInviteResponse> => {
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
};

export const mapInviteToGuildInviteMetadataResponse = async ({
	invite,
	userCacheService,
	requestCache,
	getChannelResponse,
	getGuildResponse,
	getGuildCounts,
	gatewayService,
}: MapInviteToGuildInviteMetadataResponseParams): Promise<GuildInviteMetadataResponse> => {
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
	};
};

export const mapInviteToGroupDmInviteResponse = async ({
	invite,
	userCacheService,
	requestCache,
	getChannelResponse,
	getChannelSystem,
	getChannelMemberCount,
}: MapInviteToGroupDmInviteResponseParams): Promise<GroupDmInviteResponse> => {
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
};

export const mapInviteToGroupDmInviteMetadataResponse = async ({
	invite,
	userCacheService,
	requestCache,
	getChannelResponse,
	getChannelSystem,
	getChannelMemberCount,
}: MapInviteToGroupDmInviteMetadataResponseParams): Promise<GroupDmInviteMetadataResponse> => {
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
};

interface MapInviteToPackInviteResponseParams {
	invite: Invite;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	packRepository: PackRepository;
}

const buildPackInviteBase = async ({
	invite,
	userCacheService,
	requestCache,
	packRepository,
}: MapInviteToPackInviteResponseParams): Promise<PackInviteResponse> => {
	if (!invite.guildId) {
		throw new UnknownPackError();
	}

	const pack = await packRepository.getPack(invite.guildId);
	if (!pack) {
		throw new UnknownPackError();
	}

	const creator = await getCachedUserPartialResponse({
		userId: pack.creatorId,
		userCacheService,
		requestCache,
	});

	const inviter = invite.inviterId
		? await getCachedUserPartialResponse({
				userId: invite.inviterId,
				userCacheService,
				requestCache,
			})
		: null;

	const expiresAt = invite.maxAge > 0 ? new Date(invite.createdAt.getTime() + invite.maxAge * 1000) : null;

	return {
		code: invite.code,
		type: invite.type as typeof InviteTypes.EMOJI_PACK | typeof InviteTypes.STICKER_PACK,
		pack: {
			...mapPackToSummary(pack),
			creator,
		},
		inviter,
		expires_at: expiresAt?.toISOString() ?? null,
		temporary: invite.temporary,
	};
};

export const mapInviteToPackInviteResponse = async (
	params: MapInviteToPackInviteResponseParams,
): Promise<PackInviteResponse> => {
	return buildPackInviteBase(params);
};

export const mapInviteToPackInviteMetadataResponse = async (
	params: MapInviteToPackInviteResponseParams,
): Promise<PackInviteMetadataResponse> => {
	const baseResponse = await buildPackInviteBase(params);

	return {
		...baseResponse,
		created_at: params.invite.createdAt.toISOString(),
		uses: params.invite.uses,
		max_uses: params.invite.maxUses,
	};
};
