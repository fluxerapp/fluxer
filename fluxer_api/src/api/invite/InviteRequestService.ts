// SPDX-License-Identifier: AGPL-3.0-or-later

import {InviteTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelPartialResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildPartialResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {
	ChannelInviteCreateRequest,
	InviteMetadataResponseSchema,
	InviteResponseSchema,
} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import type {ChannelID, GuildID, InviteCode, UserID} from '../BrandedTypes';
import {mapChannelToPartialResponse} from '../channel/ChannelMappers';
import type {ChannelService} from '../channel/services/ChannelService';
import type {GuildService} from '../guild/services/GuildService';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {Channel} from '../models/Channel';
import type {Invite} from '../models/Invite';
import {
	mapInviteToGroupDmInviteMetadataResponse,
	mapInviteToGroupDmInviteResponse,
	mapInviteToGuildInviteMetadataResponse,
	mapInviteToGuildInviteResponse,
} from './InviteModel';
import type {InviteService} from './InviteService';

interface MappingHelpers {
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	getChannelResponse: (channelId: ChannelID) => Promise<ChannelPartialResponse>;
	getChannelSystem: (channelId: ChannelID) => Promise<Channel | null>;
	getChannelMemberCount: (channelId: ChannelID) => Promise<number>;
	getGuildResponse: (guildId: GuildID) => Promise<GuildPartialResponse>;
	getGuildCounts: (guildId: GuildID) => Promise<{
		memberCount: number;
		presenceCount: number;
	}>;
	gatewayService: IGatewayService;
}

export class InviteRequestService {
	constructor(
		private readonly inviteService: InviteService,
		private readonly channelService: ChannelService,
		private readonly guildService: GuildService,
		private readonly gatewayService: IGatewayService,
		private readonly userCacheService: UserCacheService,
	) {}

	async getInvite(params: {inviteCode: InviteCode; requestCache: RequestCache}): Promise<InviteResponseSchema> {
		const invite = await this.inviteService.getInvite(params.inviteCode);
		return this.mapInviteResponse(invite, params.requestCache);
	}

	async acceptInvite(params: {
		userId: UserID;
		inviteCode: InviteCode;
		requestCache: RequestCache;
	}): Promise<InviteResponseSchema> {
		const invite = await this.inviteService.acceptInvite(params);
		return this.mapInviteResponse(invite, params.requestCache);
	}

	async deleteInvite(params: {userId: UserID; inviteCode: InviteCode; auditLogReason?: string | null}): Promise<void> {
		const invite = await this.inviteService.getInvite(params.inviteCode);
		await this.inviteService.deleteInvite(
			{userId: params.userId, inviteCode: params.inviteCode},
			params.auditLogReason,
		);
		await this.inviteService.dispatchInviteDelete(invite);
	}

	async createChannelInvite(params: {
		inviterId: UserID;
		channelId: ChannelID;
		requestCache: RequestCache;
		data: ChannelInviteCreateRequest;
		auditLogReason?: string | null;
	}): Promise<InviteMetadataResponseSchema> {
		const {invite, isNew} = await this.inviteService.createInvite(
			{
				inviterId: params.inviterId,
				channelId: params.channelId,
				maxUses: params.data.max_uses ?? 0,
				maxAge: params.data.max_age ?? 0,
				unique: params.data.unique ?? false,
				temporary: params.data.temporary ?? false,
			},
			params.auditLogReason,
		);
		const inviteData = await this.mapInviteMetadataResponse(invite, params.requestCache);
		if (isNew) {
			await this.inviteService.dispatchInviteCreate(invite, inviteData);
		}
		return inviteData;
	}

	async listChannelInvites(params: {
		userId: UserID;
		channelId: ChannelID;
		requestCache: RequestCache;
	}): Promise<Array<InviteMetadataResponseSchema>> {
		const invites = await this.inviteService.getChannelInvitesSorted({
			userId: params.userId,
			channelId: params.channelId,
		});
		return this.mapInviteList(invites, params.requestCache);
	}

	async listGuildInvites(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<InviteMetadataResponseSchema>> {
		const invites = await this.inviteService.getGuildInvitesSorted({
			userId: params.userId,
			guildId: params.guildId,
		});
		return this.mapInviteList(invites, params.requestCache);
	}

	private createMappingHelpers(requestCache: RequestCache): MappingHelpers {
		return {
			userCacheService: this.userCacheService,
			requestCache,
			getChannelResponse: async (channelId: ChannelID) =>
				mapChannelToPartialResponse(await this.channelService.channelData.operations.getPublicChannelData(channelId)),
			getChannelSystem: async (channelId: ChannelID) =>
				await this.channelService.channelData.operations.getChannelSystem(channelId),
			getChannelMemberCount: async (channelId: ChannelID) =>
				await this.channelService.channelData.operations.getChannelMemberCount(channelId),
			getGuildResponse: async (guildId: GuildID) => await this.guildService.data.getPublicGuildData(guildId),
			getGuildCounts: async (guildId: GuildID) => await this.gatewayService.getGuildCounts(guildId),
			gatewayService: this.gatewayService,
		};
	}

	private async mapInviteResponse(invite: Invite, requestCache: RequestCache): Promise<InviteResponseSchema> {
		const helpers = this.createMappingHelpers(requestCache);
		if (invite.type === InviteTypes.GROUP_DM) {
			return mapInviteToGroupDmInviteResponse({invite, ...helpers});
		}
		return mapInviteToGuildInviteResponse({invite, ...helpers});
	}

	private async mapInviteMetadataResponse(
		invite: Invite,
		requestCache: RequestCache,
	): Promise<InviteMetadataResponseSchema> {
		const helpers = this.createMappingHelpers(requestCache);
		if (invite.type === InviteTypes.GROUP_DM) {
			return mapInviteToGroupDmInviteMetadataResponse({invite, ...helpers});
		}
		return mapInviteToGuildInviteMetadataResponse({invite, ...helpers});
	}

	private async mapInviteList(
		invites: Array<Invite>,
		requestCache: RequestCache,
	): Promise<Array<InviteMetadataResponseSchema>> {
		return Promise.all(invites.map((invite) => this.mapInviteMetadataResponse(invite, requestCache)));
	}
}
