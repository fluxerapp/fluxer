// SPDX-License-Identifier: AGPL-3.0-or-later

import {InviteTypes} from '@fluxer/constants/src/ChannelConstants';
import {UnknownPackError} from '@fluxer/errors/src/domains/pack/UnknownPackError';
import type {ChannelPartialResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildPartialResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {
	ChannelInviteCreateRequest,
	GuildInviteBundleCreateRequest,
	GuildInviteBundleMetadataResponse,
	GuildInviteBundleResponse,
	InviteMetadataResponseSchema,
	InviteResponseSchema,
	PackInviteCreateRequest,
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
import type {PackRepository} from '../pack/PackRepository';
import {
	mapInviteToGroupDmInviteMetadataResponse,
	mapInviteToGroupDmInviteResponse,
	mapInviteToGuildInviteMetadataResponse,
	mapInviteToGuildInviteResponse,
	mapInviteToPackInviteMetadataResponse,
	mapInviteToPackInviteResponse,
} from './InviteModel';
import type {InviteService} from './InviteService';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {Guild} from '../models/Guild';

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
	packRepository: PackRepository;
	gatewayService: IGatewayService;
}

export class InviteRequestService {
	constructor(
		private readonly inviteService: InviteService,
		private readonly channelService: ChannelService,
		private readonly guildService: GuildService,
		private readonly gatewayService: IGatewayService,
		private readonly packRepository: PackRepository,
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

	async listPackInvites(params: {
		userId: UserID;
		packId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<InviteMetadataResponseSchema>> {
		const invites = await this.inviteService.getPackInvitesSorted({
			userId: params.userId,
			packId: params.packId,
		});
		return this.mapInviteList(invites, params.requestCache);
	}

	async createPackInvite(params: {
		inviterId: UserID;
		packId: GuildID;
		requestCache: RequestCache;
		data: PackInviteCreateRequest;
	}): Promise<InviteMetadataResponseSchema> {
		const pack = await this.packRepository.getPack(params.packId);
		if (!pack) {
			throw new UnknownPackError();
		}
		const {invite, isNew} = await this.inviteService.createPackInvite({
			inviterId: params.inviterId,
			packId: params.packId,
			packType: pack.type,
			maxUses: params.data.max_uses ?? 0,
			maxAge: params.data.max_age ?? 0,
			unique: params.data.unique ?? false,
		});
		const inviteData = await this.mapInviteMetadataResponse(invite, params.requestCache);
		if (isNew) {
			await this.inviteService.dispatchInviteCreate(invite, inviteData);
		}
		return inviteData;
	}

	async createGuildInviteBundle(params: {
		inviterId: UserID;
		data: GuildInviteBundleCreateRequest;
		requestCache: RequestCache;
	}): Promise<GuildInviteBundleMetadataResponse> {
		const {bundle, invites} = await this.inviteService.createInviteBundle(params.inviterId, params.data);
		for (const invite of invites) {
			const inviteData = await this.mapInviteMetadataResponse(invite, params.requestCache);
			await this.inviteService.dispatchInviteCreate(invite, inviteData);
		}
		return bundle;
	}

	async acceptGuildInviteBundle(params: {
		userId: UserID;
		inviteCode: InviteCode;
		guildIds: Array<GuildID>;
		requestCache: RequestCache;
	}): Promise<GuildInviteBundleResponse> {
		const {bundle, num_expired, invitesDisabledOrFeatureTemporarilyDisabled} =
			await this.inviteService.acceptGuildInviteBundle(
				params.userId,
				params.inviteCode,
				params.guildIds,
				params.requestCache,
			);
		const guilds: Array<{
			guild: GuildPartialResponse;
			channel?: ChannelPartialResponse | null;
		}> = [];
		for (const inviteData of bundle.invites) {
			let guild: Guild;
			try {
				guild = await this.guildService.data.getGuildSystem(inviteData.guild_id);
			} catch (error) {
				if (error instanceof UnknownGuildError) {
					continue;
				}
				throw error;
			}
			const channel = await this.channelService.channelData.operations.getChannelSystem(inviteData.channel_id);
			guilds.push({
				guild: {
					id: guild.id.toString(),
					name: guild.name,
					icon: guild.iconHash,
				} as GuildPartialResponse,
				channel: channel
					? ({
							id: channel.id.toString(),
							name: channel.name,
							type: channel.type,
						} as ChannelPartialResponse)
					: undefined,
			});
		}
		const invites_disabled = [];
		for (const guildId of invitesDisabledOrFeatureTemporarilyDisabled) {
			let guild: Guild;
			try {
				guild = await this.guildService.data.getGuildSystem(guildId);
			} catch (error) {
				if (error instanceof UnknownGuildError) {
					continue;
				}
				throw error;
			}
			invites_disabled.push({
				id: guild.id.toString(),
				name: guild.name,
				icon: guild.iconHash,
			} as GuildPartialResponse);
		}
		return {
			code: bundle.code,
			guilds,
			no_invite_available_count: num_expired,
			invites_disabled,
		};
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
			packRepository: this.packRepository,
			gatewayService: this.gatewayService,
		};
	}

	private async mapInviteResponse(invite: Invite, requestCache: RequestCache): Promise<InviteResponseSchema> {
		const helpers = this.createMappingHelpers(requestCache);
		if (invite.type === InviteTypes.GROUP_DM) {
			return mapInviteToGroupDmInviteResponse({invite, ...helpers});
		}
		if (invite.type === InviteTypes.EMOJI_PACK || invite.type === InviteTypes.STICKER_PACK) {
			return mapInviteToPackInviteResponse({invite, ...helpers});
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
		if (invite.type === InviteTypes.EMOJI_PACK || invite.type === InviteTypes.STICKER_PACK) {
			return mapInviteToPackInviteMetadataResponse({invite, ...helpers});
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
