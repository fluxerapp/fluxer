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

import {
	type ChannelID,
	createInviteCode,
	type GuildID,
	type InviteCode,
	type UserID,
	vanityCodeToInviteCode,
} from '~/BrandedTypes';
import {ChannelTypes, GuildFeatures, GuildOperations, InviteTypes, MAX_GUILD_INVITES, Permissions} from '~/Constants';
import type {ChannelService} from '~/channel/services/ChannelService';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {
	FeatureTemporarilyDisabledError,
	GuildDisallowsUnclaimedAccountsError,
	InvitesDisabledError,
	MaxGuildInvitesError,
	MaxGuildMembersError,
	MissingPermissionsError,
	TemporaryInviteRequiresPresenceError,
	UnclaimedAccountRestrictedError,
	UnknownChannelError,
	UnknownInviteError,
} from '~/Errors';
import {PackAccessDeniedError} from '~/errors/PackAccessDeniedError';
import {UnknownPackError} from '~/errors/UnknownPackError';
import type {GuildAuditLogService} from '~/guild/GuildAuditLogService';
import type {GuildService} from '~/guild/services/GuildService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {
	GroupDmInviteMetadataResponse,
	GuildInviteMetadataResponse,
	PackInviteMetadataResponse,
} from '~/invite/InviteModel';
import {Logger} from '~/Logger';
import {Channel, Invite} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {PackRepository, PackType} from '~/pack/PackRepository';
import type {PackService} from '~/pack/PackService';
import type {IUserRepository} from '~/user/IUserRepository';
import * as RandomUtils from '~/utils/RandomUtils';
import type {IInviteRepository} from './IInviteRepository';

interface GetChannelInvitesParams {
	userId: UserID;
	channelId: ChannelID;
}

interface GetGuildInvitesParams {
	userId: UserID;
	guildId: GuildID;
}

interface CreateInviteParams {
	inviterId: UserID;
	channelId: ChannelID;
	maxUses: number;
	maxAge: number;
	unique: boolean;
	temporary?: boolean;
}

interface CreatePackInviteParams {
	inviterId: UserID;
	packId: GuildID;
	packType: PackType;
	maxUses: number;
	maxAge: number;
	unique: boolean;
}

interface AcceptInviteParams {
	userId: UserID;
	inviteCode: InviteCode;
	requestCache: RequestCache;
}

interface DeleteInviteParams {
	userId: UserID;
	inviteCode: InviteCode;
}

interface GetChannelInvitesSortedParams {
	userId: UserID;
	channelId: ChannelID;
}

interface GetGuildInvitesSortedParams {
	userId: UserID;
	guildId: GuildID;
}

const PACK_TYPE_TO_INVITE_TYPE: Record<PackType, number> = {
	emoji: InviteTypes.EMOJI_PACK,
	sticker: InviteTypes.STICKER_PACK,
};

export class InviteService {
	constructor(
		private inviteRepository: IInviteRepository,
		private guildService: GuildService,
		private channelService: ChannelService,
		private gatewayService: IGatewayService,
		private readonly guildAuditLogService: GuildAuditLogService,
		private userRepository: IUserRepository,
		private readonly packRepository: PackRepository,
		private readonly packService: PackService,
	) {}

	async getInvite(inviteCode: InviteCode): Promise<Invite> {
		const invite = await this.inviteRepository.findUnique(inviteCode);
		if (!invite) throw new UnknownInviteError();
		return invite;
	}

	async getChannelInvites({userId, channelId}: GetChannelInvitesParams): Promise<Array<Invite>> {
		const channel = await this.channelService.getChannel({userId, channelId});

		if (!channel.guildId) {
			if (channel.type !== ChannelTypes.GROUP_DM) throw new UnknownChannelError();

			if (channel.ownerId !== userId) {
				throw new MissingPermissionsError();
			}

			return await this.inviteRepository.listChannelInvites(channelId);
		}

		const {checkPermission, guildData} = await this.guildService.getGuildAuthenticated({
			userId,
			guildId: channel.guildId,
		});

		await checkPermission(Permissions.MANAGE_CHANNELS);

		const invites = await this.inviteRepository.listChannelInvites(channelId);
		return invites.filter((invite) => invite.code !== guildData.vanity_url_code);
	}

	async getGuildInvites({userId, guildId}: GetGuildInvitesParams): Promise<Array<Invite>> {
		const {checkPermission, guildData} = await this.guildService.getGuildAuthenticated({
			userId,
			guildId,
		});

		await checkPermission(Permissions.MANAGE_GUILD);

		const invites = await this.inviteRepository.listGuildInvites(guildId);
		return invites.filter((invite) => invite.code !== guildData.vanity_url_code);
	}

	async createInvite(
		{inviterId, channelId, maxUses, maxAge, unique, temporary = false}: CreateInviteParams,
		auditLogReason?: string | null,
	): Promise<{invite: Invite; isNew: boolean}> {
		const channel = await this.channelService.getChannel({
			userId: inviterId,
			channelId,
		});

		if (!channel.guildId) {
			if (!unique) {
				const existingInvite = await this.inviteRepository
					.listChannelInvites(channelId)
					.then((invites) =>
						invites.find(
							(invite) =>
								invite.channelId === channelId &&
								invite.inviterId === inviterId &&
								invite.maxUses === maxUses &&
								invite.maxAge === maxAge &&
								invite.temporary === temporary &&
								invite.type === InviteTypes.GROUP_DM,
						),
					);

				if (existingInvite) {
					return {invite: existingInvite, isNew: false};
				}
			}

			const newInvite = await this.inviteRepository.create({
				code: createInviteCode(RandomUtils.randomString(8)),
				type: InviteTypes.GROUP_DM,
				guild_id: null,
				channel_id: channelId,
				inviter_id: inviterId,
				uses: 0,
				max_uses: maxUses,
				max_age: maxAge,
				temporary,
			});
			return {invite: newInvite, isNew: true};
		}

		const {guildData} = await this.guildService.getGuildAuthenticated({
			userId: inviterId,
			guildId: channel.guildId,
		});

		if ((guildData.disabled_operations & GuildOperations.INSTANT_INVITES) !== 0) {
			throw new FeatureTemporarilyDisabledError();
		}

		const hasPermission = await this.gatewayService.checkPermission({
			guildId: channel.guildId,
			userId: inviterId,
			permission: Permissions.CREATE_INSTANT_INVITE,
			channelId,
		});

		if (!hasPermission) {
			throw new MissingPermissionsError();
		}

		const existingInvites = await this.inviteRepository.listGuildInvites(channel.guildId);

		if (!unique) {
			const existingInvite = existingInvites.find(
				(invite) =>
					invite.channelId === channelId &&
					invite.inviterId === inviterId &&
					invite.maxUses === maxUses &&
					invite.maxAge === maxAge &&
					invite.temporary === temporary,
			);

			if (existingInvite) {
				return {invite: existingInvite, isNew: false};
			}
		}

		if (existingInvites.length >= MAX_GUILD_INVITES) {
			throw new MaxGuildInvitesError();
		}

		const newInvite = await this.inviteRepository.create({
			code: createInviteCode(RandomUtils.randomString(8)),
			type: InviteTypes.GUILD,
			guild_id: channel.guildId,
			channel_id: channelId,
			inviter_id: inviterId,
			uses: 0,
			max_uses: maxUses,
			max_age: maxAge,
			temporary,
		});
		if (newInvite.guildId) {
			await this.logGuildInviteAction({
				invite: newInvite,
				userId: inviterId,
				action: 'create',
				auditLogReason,
			});
		}
		return {invite: newInvite, isNew: true};
	}

	async createPackInvite({
		inviterId,
		packId,
		packType,
		maxUses,
		maxAge,
		unique,
	}: CreatePackInviteParams): Promise<{invite: Invite; isNew: boolean}> {
		const pack = await this.packRepository.getPack(packId);
		if (!pack) {
			throw new UnknownPackError();
		}

		if (pack.creatorId !== inviterId) {
			throw new PackAccessDeniedError();
		}

		if (pack.type !== packType) {
			throw new PackAccessDeniedError();
		}

		const allInvites = await this.inviteRepository.listGuildInvites(packId);
		const inviteType = PACK_TYPE_TO_INVITE_TYPE[packType];

		if (!unique) {
			const existingInvite = allInvites.find(
				(invite) =>
					invite.inviterId === inviterId &&
					invite.maxUses === maxUses &&
					invite.maxAge === maxAge &&
					invite.type === inviteType,
			);
			if (existingInvite) {
				return {invite: existingInvite, isNew: false};
			}
		}

		if (allInvites.length >= MAX_GUILD_INVITES) {
			throw new MaxGuildInvitesError();
		}

		const newInvite = await this.inviteRepository.create({
			code: createInviteCode(RandomUtils.randomString(8)),
			type: inviteType,
			guild_id: packId,
			channel_id: null,
			inviter_id: inviterId,
			uses: 0,
			max_uses: maxUses,
			max_age: maxAge,
			temporary: false,
		});

		return {invite: newInvite, isNew: true};
	}

	async acceptInvite({userId, inviteCode, requestCache}: AcceptInviteParams): Promise<Invite> {
		const invite = await this.inviteRepository.findUnique(inviteCode);
		if (!invite) throw new UnknownInviteError();

		if (invite.maxUses > 0 && invite.uses >= invite.maxUses) {
			if (invite.type === InviteTypes.GUILD && invite.guildId) {
				const guild = await this.guildService.getGuildSystem(invite.guildId);
				const vanityCode = guild.vanityUrlCode ? vanityCodeToInviteCode(guild.vanityUrlCode) : null;
				if (invite.code !== vanityCode) {
					await this.inviteRepository.delete(inviteCode);
				}
			} else if (invite.type === InviteTypes.GROUP_DM) {
				await this.inviteRepository.delete(inviteCode);
			}

			throw new UnknownInviteError();
		}

		if (invite.type === InviteTypes.GROUP_DM) {
			if (!invite.channelId) throw new UnknownInviteError();

			const user = await this.userRepository.findUnique(userId);
			if (user && user.isUnclaimedAccount()) {
				throw new UnclaimedAccountRestrictedError('join group DMs');
			}

			const channel = await this.channelService.getChannelSystem(invite.channelId);
			if (!channel) throw new UnknownInviteError();

			if (channel.recipientIds.has(userId)) {
				return invite;
			}

			await this.channelService.groupDms.addRecipientViaInvite({
				channelId: invite.channelId,
				recipientId: userId,
				inviterId: invite.inviterId,
				requestCache,
			});

			const newUses = invite.uses + 1;
			await this.inviteRepository.updateInviteUses(inviteCode, newUses);
			if (invite.maxUses > 0 && newUses >= invite.maxUses) {
				await this.inviteRepository.delete(inviteCode);
			}

			return this.cloneInviteWithUses(invite, newUses);
		}

		if (invite.type === InviteTypes.EMOJI_PACK || invite.type === InviteTypes.STICKER_PACK) {
			if (!invite.guildId) throw new UnknownInviteError();
			await this.packService.installPack(userId, invite.guildId);

			const newUses = invite.uses + 1;
			await this.inviteRepository.updateInviteUses(inviteCode, newUses);
			if (invite.maxUses > 0 && newUses >= invite.maxUses) {
				await this.inviteRepository.delete(inviteCode);
			}

			return this.cloneInviteWithUses(invite, newUses);
		}

		if (!invite.guildId) throw new UnknownInviteError();

		const guild = await this.guildService.getGuildSystem(invite.guildId);

		if ((guild.disabledOperations & GuildOperations.INSTANT_INVITES) !== 0) {
			throw new FeatureTemporarilyDisabledError();
		}

		if (guild.features.has(GuildFeatures.INVITES_DISABLED)) {
			throw new InvitesDisabledError();
		}

		if (guild.features.has(GuildFeatures.DISALLOW_UNCLAIMED_ACCOUNTS)) {
			const user = await this.userRepository.findUnique(userId);
			if (user && user.isUnclaimedAccount()) {
				throw new GuildDisallowsUnclaimedAccountsError();
			}
		}

		const existingMember = await this.gatewayService.hasGuildMember({
			guildId: invite.guildId,
			userId,
		});
		if (existingMember) {
			return invite;
		}

		await this.guildService.checkUserBanStatus({userId, guildId: invite.guildId});

		const {memberCount} = await this.gatewayService.getGuildCounts(invite.guildId);
		if (memberCount >= 1000) {
			throw new MaxGuildMembersError(1000);
		}

		if (invite.temporary) {
			const hasPresence = await this.gatewayService.hasActivePresence(userId);
			if (!hasPresence) {
				throw new TemporaryInviteRequiresPresenceError();
			}
		}

		await this.guildService.addUserToGuild({
			userId,
			guildId: invite.guildId,
			sendJoinMessage: true,
			requestCache,
			isTemporary: invite.temporary,
		});

		if (invite.temporary) {
			await this.gatewayService.addTemporaryGuild({userId, guildId: invite.guildId});
		}

		const vanityCode = guild.vanityUrlCode ? vanityCodeToInviteCode(guild.vanityUrlCode) : null;
		const isVanityInvite = invite.code === vanityCode;

		const newUses = invite.uses + 1;
		await this.inviteRepository.updateInviteUses(inviteCode, newUses);

		if (!isVanityInvite && invite.maxUses > 0 && newUses >= invite.maxUses) {
			await this.inviteRepository.delete(inviteCode);
		}

		return this.cloneInviteWithUses(invite, newUses);
	}

	private cloneInviteWithUses(invite: Invite, uses: number): Invite {
		const row = invite.toRow();
		return new Invite({
			...row,
			uses,
		});
	}

	async deleteInvite({userId, inviteCode}: DeleteInviteParams, auditLogReason?: string | null): Promise<void> {
		const invite = await this.inviteRepository.findUnique(inviteCode);
		if (!invite) throw new UnknownInviteError();

		if (invite.type === InviteTypes.EMOJI_PACK || invite.type === InviteTypes.STICKER_PACK) {
			if (!invite.guildId) throw new UnknownInviteError();
			const pack = await this.packRepository.getPack(invite.guildId);
			if (!pack) {
				throw new UnknownPackError();
			}

			if (pack.creatorId !== userId) {
				throw new PackAccessDeniedError();
			}

			await this.inviteRepository.delete(inviteCode);
			return;
		}

		if (invite.type === InviteTypes.GROUP_DM) {
			if (!invite.channelId) throw new UnknownInviteError();

			const channel = await this.channelService.getChannel({
				userId,
				channelId: invite.channelId,
			});

			if (!channel.recipientIds.has(userId)) {
				throw new UnknownInviteError();
			}

			if (channel.ownerId !== userId) {
				throw new MissingPermissionsError();
			}

			await this.inviteRepository.delete(inviteCode);
			return;
		}

		if (!invite.guildId) throw new UnknownInviteError();

		const {checkPermission, guildData} = await this.guildService.getGuildAuthenticated({
			userId,
			guildId: invite.guildId,
		});

		await checkPermission(Permissions.MANAGE_GUILD);

		if (invite.code === guildData.vanity_url_code) {
			throw new UnknownInviteError();
		}

		await this.inviteRepository.delete(inviteCode);
		await this.logGuildInviteAction({
			invite,
			userId,
			action: 'delete',
			auditLogReason,
		});
	}

	async resolveVanityUrlChannel(guildId: GuildID): Promise<Channel | null> {
		const channelId = await this.gatewayService.getVanityUrlChannel(guildId);
		if (!channelId) return null;

		return await this.channelService.getChannelSystem(channelId);
	}

	async getChannelInvitesSorted({userId, channelId}: GetChannelInvitesSortedParams): Promise<Array<Invite>> {
		const invites = await this.getChannelInvites({userId, channelId});
		return invites.sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime());
	}

	async getGuildInvitesSorted({userId, guildId}: GetGuildInvitesSortedParams): Promise<Array<Invite>> {
		const invites = await this.getGuildInvites({userId, guildId});
		return invites.sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime());
	}

	async getPackInvitesSorted(params: {userId: UserID; packId: GuildID}): Promise<Array<Invite>> {
		const {userId, packId} = params;
		const pack = await this.packRepository.getPack(packId);
		if (!pack) {
			throw new UnknownPackError();
		}

		if (pack.creatorId !== userId) {
			throw new PackAccessDeniedError();
		}

		const invites = await this.inviteRepository.listGuildInvites(packId);
		const inviteType = PACK_TYPE_TO_INVITE_TYPE[pack.type];
		return invites
			.filter((invite) => invite.type === inviteType)
			.sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime());
	}

	async dispatchInviteCreate(
		invite: Invite,
		inviteData: GuildInviteMetadataResponse | GroupDmInviteMetadataResponse | PackInviteMetadataResponse,
	): Promise<void> {
		if (invite.guildId && invite.type === InviteTypes.GUILD) {
			await this.gatewayService.dispatchGuild({
				guildId: invite.guildId,
				event: 'INVITE_CREATE',
				data: inviteData,
			});
		} else if (invite.channelId) {
			const channel = await this.channelService.getChannelSystem(invite.channelId);
			if (channel) {
				for (const recipientId of channel.recipientIds) {
					await this.gatewayService.dispatchPresence({
						userId: recipientId,
						event: 'INVITE_CREATE',
						data: inviteData,
					});
				}
			}
		}
	}

	async dispatchInviteDelete(invite: Invite): Promise<void> {
		const data = {
			code: invite.code,
			channel_id: invite.channelId?.toString(),
			guild_id: invite.guildId?.toString(),
		};

		if (invite.guildId && invite.type === InviteTypes.GUILD) {
			await this.gatewayService.dispatchGuild({
				guildId: invite.guildId,
				event: 'INVITE_DELETE',
				data,
			});
		} else if (invite.channelId) {
			const channel = await this.channelService.getChannelSystem(invite.channelId);
			if (channel) {
				for (const recipientId of channel.recipientIds) {
					await this.gatewayService.dispatchPresence({
						userId: recipientId,
						event: 'INVITE_DELETE',
						data,
					});
				}
			}
		}
	}

	private async logGuildInviteAction(params: {
		invite: Invite;
		userId: UserID;
		action: 'create' | 'delete';
		auditLogReason?: string | null;
	}): Promise<void> {
		if (!params.invite.guildId) {
			return;
		}

		const metadata: Record<string, string> = {
			max_uses: params.invite.maxUses.toString(),
			max_age: params.invite.maxAge.toString(),
			temporary: params.invite.temporary ? 'true' : 'false',
		};

		if (params.invite.channelId) {
			metadata.channel_id = params.invite.channelId.toString();
		}
		if (params.invite.inviterId) {
			metadata.inviter_id = params.invite.inviterId.toString();
		}

		const snapshot = this.serializeInviteForAudit(params.invite);
		const changes =
			params.action === 'create'
				? this.guildAuditLogService.computeChanges(null, snapshot)
				: this.guildAuditLogService.computeChanges(snapshot, null);
		const builder = this.guildAuditLogService
			.createBuilder(params.invite.guildId, params.userId)
			.withReason(params.auditLogReason ?? null)
			.withMetadata(metadata)
			.withChanges(changes ?? null)
			.withAction(
				params.action === 'create' ? AuditLogActionType.INVITE_CREATE : AuditLogActionType.INVITE_DELETE,
				params.invite.code,
			);

		try {
			await builder.commit();
		} catch (error) {
			Logger.error(
				{
					error,
					guildId: params.invite.guildId.toString(),
					userId: params.userId.toString(),
					action: params.action === 'create' ? 'guild_invite_create' : 'guild_invite_delete',
					targetId: params.invite.code,
				},
				'Failed to record guild invite audit log',
			);
		}
	}

	private serializeInviteForAudit(invite: Invite): Record<string, unknown> {
		return {
			code: invite.code,
			channel_id: invite.channelId?.toString() ?? null,
			guild_id: invite.guildId?.toString() ?? null,
			inviter_id: invite.inviterId?.toString() ?? null,
			uses: invite.uses,
			max_uses: invite.maxUses,
			max_age: invite.maxAge,
			temporary: invite.temporary,
			created_at: invite.createdAt.toISOString(),
		};
	}
}
