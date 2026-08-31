// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, InviteTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures, GuildOperations, JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import {MAX_GUILD_INVITES} from '@fluxer/constants/src/LimitConstants';
import {UnclaimedAccountCannotJoinGroupDmsError} from '@fluxer/errors/src/domains/channel/UnclaimedAccountCannotJoinGroupDmsError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {MaxGuildInvitesError} from '@fluxer/errors/src/domains/guild/MaxGuildInvitesError';
import {InvitesDisabledError} from '@fluxer/errors/src/domains/invite/InvitesDisabledError';
import {TemporaryInviteRequiresPresenceError} from '@fluxer/errors/src/domains/invite/TemporaryInviteRequiresPresenceError';
import {UnknownInviteError} from '@fluxer/errors/src/domains/invite/UnknownInviteError';
import type {
	GroupDmInviteMetadataResponse,
	GuildInviteMetadataResponse,
} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import type {ApiContext} from '../ApiContext';
import type {ChannelID, GuildID, InviteCode, UserID} from '../BrandedTypes';
import {createInviteCode, vanityCodeToInviteCode} from '../BrandedTypes';
import type {ChannelService} from '../channel/services/ChannelService';
import type {GuildAuditLogService} from '../guild/GuildAuditLogService';
import type {GuildService} from '../guild/services/GuildService';
import {Logger} from '../Logger';
import type {LimitConfigService} from '../limits/LimitConfigService';
import {resolveLimitSafe} from '../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../limits/LimitMatchContextBuilder';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {Channel} from '../models/Channel';
import {Invite} from '../models/Invite';
import * as RandomUtils from '../utils/RandomUtils';
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

interface SerializedInviteForAudit {
	[key: string]: string | number | boolean | null;
	code: string;
	channel_id: string | null;
	guild_id: string | null;
	inviter_id: string | null;
	uses: number;
	max_uses: number;
	max_age: number;
	temporary: boolean;
	created_at: string;
}

interface ReusableInviteCriteria {
	channelId?: ChannelID;
	inviterId: UserID;
	maxUses: number;
	maxAge: number;
	temporary?: boolean;
	type?: number;
}

export class InviteService {
	constructor(
		private readonly apiContext: ApiContext,
		private inviteRepository: IInviteRepository,
		private guildService: GuildService,
		private channelService: ChannelService,
		private readonly guildAuditLogService: GuildAuditLogService,
		private readonly limitConfigService: LimitConfigService,
	) {}

	async getInvite(inviteCode: InviteCode): Promise<Invite> {
		const invite = await this.findInviteWithLowercaseFallback(inviteCode);
		if (invite) {
			return invite;
		}
		throw new UnknownInviteError();
	}

	async getChannelInvites({userId, channelId}: GetChannelInvitesParams): Promise<Array<Invite>> {
		const channel = await this.channelService.channelData.operations.getChannel({userId, channelId});
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
	): Promise<{
		invite: Invite;
		isNew: boolean;
	}> {
		const channel = await this.channelService.channelData.operations.getChannel({
			userId: inviterId,
			channelId,
		});
		if (!channel.guildId) {
			if (!unique) {
				const channelInvites = await this.inviteRepository.listChannelInvites(channelId);
				const existingInvite = this.findReusableInvite(channelInvites, {
					channelId,
					inviterId,
					maxUses,
					maxAge,
					temporary,
					type: InviteTypes.GROUP_DM,
				});
				if (existingInvite) {
					return {invite: existingInvite, isNew: false};
				}
			}
			const newInvite = await this.inviteRepository.create({
				code: this.createRandomInviteCode(),
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
		const hasPermission = await this.apiContext.services.gateway.checkPermission({
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
			const existingInvite = this.findReusableInvite(existingInvites, {
				channelId,
				inviterId,
				maxUses,
				maxAge,
				temporary,
			});
			if (existingInvite) {
				return {invite: existingInvite, isNew: false};
			}
		}
		const inviteLimit = this.resolveInviteLimit(guildData.features);
		if (existingInvites.length >= inviteLimit) {
			throw new MaxGuildInvitesError(inviteLimit);
		}
		const newInvite = await this.inviteRepository.create({
			code: this.createRandomInviteCode(),
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

	async acceptInvite({userId, inviteCode, requestCache}: AcceptInviteParams): Promise<Invite> {
		const invite = await this.findInviteWithLowercaseFallback(inviteCode);
		if (!invite) throw new UnknownInviteError();
		if (invite.maxUses > 0 && invite.uses >= invite.maxUses) {
			if (invite.type === InviteTypes.GUILD && invite.guildId) {
				const guild = await this.guildService.data.getGuildSystem(invite.guildId);
				const vanityCode = guild.vanityUrlCode ? vanityCodeToInviteCode(guild.vanityUrlCode) : null;
				if (invite.code !== vanityCode) {
					await this.inviteRepository.delete(invite.code);
				}
			} else if (invite.type === InviteTypes.GROUP_DM) {
				await this.inviteRepository.delete(invite.code);
			}
			throw new UnknownInviteError();
		}
		if (invite.type === InviteTypes.GROUP_DM) {
			if (!invite.channelId) throw new UnknownInviteError();
			const user = await this.apiContext.services.users.findUnique(userId);
			if (user?.isUnclaimedAccount()) {
				throw new UnclaimedAccountCannotJoinGroupDmsError();
			}
			const channel = await this.channelService.channelData.operations.getChannelSystem(invite.channelId);
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
			return this.incrementInviteUses(invite, {deleteWhenExhausted: true});
		}
		if (!invite.guildId) throw new UnknownInviteError();
		const guild = await this.guildService.data.getGuildSystem(invite.guildId);
		if ((guild.disabledOperations & GuildOperations.INSTANT_INVITES) !== 0) {
			throw new FeatureTemporarilyDisabledError();
		}
		if (guild.features.has(GuildFeatures.INVITES_DISABLED)) {
			throw new InvitesDisabledError();
		}
		const existingMember = await this.apiContext.services.gateway.hasGuildMember({
			guildId: invite.guildId,
			userId,
		});
		if (existingMember) {
			return invite;
		}
		await this.guildService.moderation.checkUserBanStatus({userId, guildId: invite.guildId});
		if (invite.temporary) {
			const hasPresence = await this.apiContext.services.gateway.hasActivePresence(userId);
			if (!hasPresence) {
				throw new TemporaryInviteRequiresPresenceError();
			}
		}
		const vanityCode = guild.vanityUrlCode ? vanityCodeToInviteCode(guild.vanityUrlCode) : null;
		const isVanityInvite = invite.code === vanityCode;
		await this.guildService.members.addUserToGuild({
			userId,
			guildId: invite.guildId,
			sendJoinMessage: true,
			requestCache,
			isTemporary: invite.temporary,
			joinSourceType: isVanityInvite ? JoinSourceTypes.VANITY_URL : JoinSourceTypes.INSTANT_INVITE,
			sourceInviteCode: isVanityInvite ? undefined : invite.code,
			inviterId: isVanityInvite ? undefined : (invite.inviterId ?? undefined),
		});
		if (invite.temporary) {
			await this.apiContext.services.gateway.addTemporaryGuild({userId, guildId: invite.guildId});
		}
		return this.incrementInviteUses(invite, {deleteWhenExhausted: !isVanityInvite});
	}

	private createRandomInviteCode(): InviteCode {
		return createInviteCode(RandomUtils.randomString(8));
	}

	private findReusableInvite(invites: Array<Invite>, criteria: ReusableInviteCriteria): Invite | undefined {
		return invites.find((invite) => {
			if (criteria.channelId !== undefined && invite.channelId !== criteria.channelId) return false;
			if (invite.inviterId !== criteria.inviterId) return false;
			if (invite.maxUses !== criteria.maxUses) return false;
			if (invite.maxAge !== criteria.maxAge) return false;
			if (criteria.temporary !== undefined && invite.temporary !== criteria.temporary) return false;
			if (criteria.type !== undefined && invite.type !== criteria.type) return false;
			return true;
		});
	}

	private async incrementInviteUses(invite: Invite, params: {deleteWhenExhausted: boolean}): Promise<Invite> {
		const newUses = invite.uses + 1;
		await this.inviteRepository.updateInviteUses(invite.code, newUses, invite);
		if (params.deleteWhenExhausted && invite.maxUses > 0 && newUses >= invite.maxUses) {
			await this.inviteRepository.delete(invite.code);
		}
		return this.cloneInviteWithUses(invite, newUses);
	}

	private async findInviteWithLowercaseFallback(inviteCode: InviteCode): Promise<Invite | null> {
		const invite = await this.inviteRepository.findUnique(inviteCode);
		if (invite) {
			return invite;
		}
		const lowercaseInviteCode = createInviteCode(inviteCode.toLowerCase());
		if (lowercaseInviteCode === inviteCode) {
			return null;
		}
		return this.inviteRepository.findUnique(lowercaseInviteCode);
	}

	private cloneInviteWithUses(invite: Invite, uses: number): Invite {
		const row = invite.toRow();
		return new Invite({
			...row,
			uses,
		});
	}

	private resolveInviteLimit(guildFeatures?: Iterable<string> | null): number {
		const limit = MAX_GUILD_INVITES;
		const ctx = createLimitMatchContext({guildFeatures});
		return resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, 'max_guild_invites', limit);
	}

	async deleteInvite({userId, inviteCode}: DeleteInviteParams, auditLogReason?: string | null): Promise<void> {
		const invite = await this.findInviteWithLowercaseFallback(inviteCode);
		if (!invite) throw new UnknownInviteError();
		if (invite.type === InviteTypes.GROUP_DM) {
			if (!invite.channelId) throw new UnknownInviteError();
			const channel = await this.channelService.channelData.operations.getChannel({
				userId,
				channelId: invite.channelId,
			});
			if (!channel.recipientIds.has(userId)) {
				throw new UnknownInviteError();
			}
			if (channel.ownerId !== userId) {
				throw new MissingPermissionsError();
			}
			await this.inviteRepository.delete(invite.code);
			return;
		}
		if (!invite.guildId) throw new UnknownInviteError();
		const {checkPermission, guildData} = await this.guildService.getGuildAuthenticated({
			userId,
			guildId: invite.guildId,
		});
		if (invite.code === guildData.vanity_url_code) {
			throw new UnknownInviteError();
		}
		const isInviteCreator = invite.inviterId === userId;
		if (!isInviteCreator) {
			await checkPermission(Permissions.MANAGE_GUILD);
		}
		await this.inviteRepository.delete(invite.code);
		await this.logGuildInviteAction({
			invite,
			userId,
			action: 'delete',
			auditLogReason,
		});
	}

	async resolveVanityUrlChannel(guildId: GuildID): Promise<Channel | null> {
		const channelId = await this.apiContext.services.gateway.getVanityUrlChannel(guildId);
		if (!channelId) return null;
		return await this.channelService.channelData.operations.getChannelSystem(channelId);
	}

	async getChannelInvitesSorted({userId, channelId}: GetChannelInvitesSortedParams): Promise<Array<Invite>> {
		const invites = await this.getChannelInvites({userId, channelId});
		return invites.sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime());
	}

	async getGuildInvitesSorted({userId, guildId}: GetGuildInvitesSortedParams): Promise<Array<Invite>> {
		const invites = await this.getGuildInvites({userId, guildId});
		return invites.sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime());
	}

	async dispatchInviteCreate(
		invite: Invite,
		inviteData: GuildInviteMetadataResponse | GroupDmInviteMetadataResponse,
	): Promise<void> {
		if (invite.guildId && invite.type === InviteTypes.GUILD) {
			await this.apiContext.services.gateway.dispatchGuild({
				guildId: invite.guildId,
				event: 'INVITE_CREATE',
				data: inviteData,
			});
		} else if (invite.channelId) {
			const channel = await this.channelService.channelData.operations.getChannelSystem(invite.channelId);
			if (channel) {
				for (const recipientId of channel.recipientIds) {
					await this.apiContext.services.gateway.dispatchPresence({
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
			await this.apiContext.services.gateway.dispatchGuild({
				guildId: invite.guildId,
				event: 'INVITE_DELETE',
				data,
			});
		} else if (invite.channelId) {
			const channel = await this.channelService.channelData.operations.getChannelSystem(invite.channelId);
			if (channel) {
				for (const recipientId of channel.recipientIds) {
					await this.apiContext.services.gateway.dispatchPresence({
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
			metadata['channel_id'] = params.invite.channelId.toString();
		}
		if (params.invite.inviterId) {
			metadata['inviter_id'] = params.invite.inviterId.toString();
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

	private serializeInviteForAudit(invite: Invite): SerializedInviteForAudit {
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
