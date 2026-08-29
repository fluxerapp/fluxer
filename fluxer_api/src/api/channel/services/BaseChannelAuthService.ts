// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {CannotSendMessagesToUserError} from '@fluxer/errors/src/domains/channel/CannotSendMessagesToUserError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {NsfwContentRequiresAgeVerificationError} from '@fluxer/errors/src/domains/moderation/NsfwContentRequiresAgeVerificationError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {ChannelID, GuildID, UserID} from '../../BrandedTypes';
import {SYSTEM_USER_ID} from '../../constants/Core';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import {createGuildMfaEnforcer} from '../../guild/services/GuildMfaEnforcement';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {Channel} from '../../models/Channel';
import type {GuildMember} from '../../models/GuildMember';
import type {User} from '../../models/User';
import type {IUserRepository} from '../../user/IUserRepository';
import {canUserAccessNsfwContent} from '../../utils/AgeUtils';
import type {IChannelRepositoryAggregate} from '../repositories/IChannelRepositoryAggregate';
import {
	type ContentWarningChannelLike,
	channelResponseToContentWarningView,
	channelToContentWarningView,
	computeEffectiveChannelNsfw,
	guildResponseToContentWarningView,
} from '../utils/EffectiveContentWarning';
import type {AuthenticatedChannel} from './AuthenticatedChannel';
import {DMPermissionValidator} from './DMPermissionValidator';
import {ensurePersonalNotesChannelExists, isPersonalNotesChannelId} from './PersonalNotesChannelRepair';

export interface ChannelAuthOptions {
	errorOnMissingGuild: 'unknown_channel' | 'missing_permissions';
	validateNsfw: boolean;
}

export abstract class BaseChannelAuthService {
	protected abstract readonly options: ChannelAuthOptions;
	protected dmPermissionValidator: DMPermissionValidator;

	constructor(
		protected channelRepository: IChannelRepositoryAggregate,
		protected userRepository: IUserRepository,
		protected guildRepository: IGuildRepositoryAggregate,
		protected gatewayService: IGatewayService,
	) {
		this.dmPermissionValidator = new DMPermissionValidator({
			userRepository: this.userRepository,
			guildRepository: this.guildRepository,
		});
	}

	async getChannelAuthenticated({
		userId,
		channelId,
		skipNsfwValidation,
	}: {
		userId: UserID;
		channelId: ChannelID;
		skipNsfwValidation?: boolean;
	}): Promise<AuthenticatedChannel> {
		if (this.isPersonalNotesChannel({userId, channelId})) {
			const channel = await ensurePersonalNotesChannelExists({
				channelRepository: this.channelRepository.channelData,
				userId,
			});
			return this.getRealPersonalNotesChannelAuth({channel, userId});
		}
		const channel = await this.channelRepository.channelData.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();
		if (!channel.guildId) {
			const recipients = await this.userRepository.listUsers(Array.from(channel.recipientIds));
			return this.getDMChannelAuth({channel, recipients, userId});
		}
		return this.getGuildChannelAuth({channel, userId, skipNsfwValidation});
	}

	isPersonalNotesChannel({userId, channelId}: {userId: UserID; channelId: ChannelID}): boolean {
		return isPersonalNotesChannelId({userId, channelId});
	}

	protected async getRealPersonalNotesChannelAuth({
		channel,
		userId,
	}: {
		channel: Channel;
		userId: UserID;
	}): Promise<AuthenticatedChannel> {
		if (!this.isPersonalNotesChannel({userId, channelId: channel.id})) {
			throw new UnknownChannelError();
		}
		if (channel.type !== ChannelTypes.DM_PERSONAL_NOTES) {
			throw new UnknownChannelError();
		}
		return {
			channel,
			guild: null,
			member: null,
			hasPermission: async () => true,
			checkPermission: async () => {},
		};
	}

	protected async getDMChannelAuth({
		channel,
		recipients,
		userId,
	}: {
		channel: Channel;
		recipients: Array<User>;
		userId: UserID;
	}): Promise<AuthenticatedChannel> {
		if (userId === SYSTEM_USER_ID) {
			return {
				channel,
				guild: null,
				member: null,
				hasPermission: async () => true,
				checkPermission: async () => {},
			};
		}
		if (channel.type === ChannelTypes.DM && channel.ownerId != null && channel.ownerId !== userId) {
			throw new UnknownChannelError();
		}
		const isRecipient = recipients.some((recipient) => recipient.id === userId);
		if (!isRecipient) throw new UnknownChannelError();
		return {
			channel,
			guild: null,
			member: null,
			hasPermission: async () => true,
			checkPermission: async () => {},
		};
	}

	async validateDMSendPermissions({channelId, userId}: {channelId: ChannelID; userId: UserID}): Promise<void> {
		const channel = await this.channelRepository.channelData.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();
		if (channel.type === ChannelTypes.GROUP_DM || channel.type === ChannelTypes.DM_PERSONAL_NOTES) {
			return;
		}
		const recipientIds = Array.from(channel.recipientIds).filter((id) => id !== userId);
		if (recipientIds.length !== 1) {
			throw new CannotSendMessagesToUserError();
		}
		await this.dmPermissionValidator.validate({senderId: userId, recipientId: recipientIds[0]});
	}

	protected async getGuildChannelAuth({
		channel,
		userId,
		skipNsfwValidation,
	}: {
		channel: Channel;
		userId: UserID;
		skipNsfwValidation?: boolean;
	}): Promise<AuthenticatedChannel> {
		const guildId = channel.guildId!;
		const [guildDataResult, guildMemberResult] = await Promise.all([
			this.fetchGuildDataOrThrow({guildId, userId}),
			this.gatewayService.getGuildMember({guildId, userId}),
		]);
		if (!guildDataResult) {
			this.throwGuildAccessError();
		}
		if (!guildMemberResult.success || !guildMemberResult.memberData) {
			this.throwGuildAccessError();
		}
		const member = await this.fillMissingMemberTimeout({
			guildId,
			userId,
			memberData: guildMemberResult.memberData!,
		});
		const enforceGuildMfa = await createGuildMfaEnforcer({
			userRepository: this.userRepository,
			guildData: guildDataResult!,
			userId,
		});
		const hasPermission = async (permission: bigint): Promise<boolean> => {
			const allowed = await this.gatewayService.checkPermission({guildId, userId, permission, channelId: channel.id});
			if (allowed) enforceGuildMfa(permission);
			return allowed;
		};
		const checkPermission = async (permission: bigint): Promise<void> => {
			const allowed = await hasPermission(permission);
			if (!allowed) throw new MissingPermissionsError();
		};
		await checkPermission(Permissions.VIEW_CHANNEL);
		const parentCategory = await this.getParentCategoryContentWarningView({
			channel,
			guild: guildDataResult!,
		});
		const requiresAgeVerification = computeEffectiveChannelNsfw(
			channelToContentWarningView(channel),
			parentCategory,
			guildResponseToContentWarningView(guildDataResult!),
		);
		if (
			this.options.validateNsfw &&
			!skipNsfwValidation &&
			(channel.type === ChannelTypes.GUILD_TEXT ||
				channel.type === ChannelTypes.GUILD_VOICE ||
				channel.type === ChannelTypes.GUILD_LINK) &&
			requiresAgeVerification
		) {
			const user = await this.userRepository.findUnique(userId);
			if (!user) throw new UnknownUserError();
			if (!canUserAccessNsfwContent(user)) {
				throw new NsfwContentRequiresAgeVerificationError();
			}
		}
		return {
			channel,
			guild: guildDataResult!,
			member,
			hasPermission,
			checkPermission,
		};
	}

	private async getParentCategoryContentWarningView({
		channel,
		guild,
	}: {
		channel: Channel;
		guild: GuildResponse;
	}): Promise<ContentWarningChannelLike | null> {
		if (!channel.parentId || channel.type === ChannelTypes.GUILD_CATEGORY) {
			return null;
		}
		const parentId = channel.parentId.toString();
		const parentFromGateway = guild.channels?.find((guildChannel) => guildChannel.id === parentId);
		if (parentFromGateway) {
			return channelResponseToContentWarningView(parentFromGateway);
		}
		const parentCategory = await this.channelRepository.channelData.findUnique(channel.parentId);
		return parentCategory ? channelToContentWarningView(parentCategory) : null;
	}

	protected throwGuildAccessError(): never {
		if (this.options.errorOnMissingGuild === 'missing_permissions') {
			throw new MissingPermissionsError();
		}
		throw new UnknownChannelError();
	}

	private async fetchGuildDataOrThrow(params: {guildId: GuildID; userId: UserID}): Promise<GuildResponse | null> {
		const {guildId, userId} = params;
		try {
			return await this.gatewayService.getGuildData({guildId, userId});
		} catch (error) {
			await this.handleGuildAccessError(error, guildId);
			return null;
		}
	}

	private async handleGuildAccessError(error: unknown, guildId: GuildID): Promise<void> {
		if (error instanceof UnknownGuildError) {
			if (await this.guildExists(guildId)) {
				throw new AccessDeniedError();
			}
			throw new UnknownGuildError();
		}
		throw error;
	}

	private async guildExists(guildId: GuildID): Promise<boolean> {
		const guild = await this.guildRepository.findUnique(guildId);
		return guild !== null;
	}

	private async fillMissingMemberTimeout({
		guildId,
		userId,
		memberData,
	}: {
		guildId: GuildID;
		userId: UserID;
		memberData: GuildMemberResponse;
	}): Promise<GuildMemberResponse> {
		if (memberData.communication_disabled_until !== undefined) {
			return memberData;
		}
		const persistedMember = await this.guildRepository.getMember(guildId, userId);
		if (!persistedMember) {
			this.throwGuildAccessError();
		}
		return {
			...memberData,
			communication_disabled_until: this.formatCommunicationDisabledUntil(persistedMember),
		};
	}

	private formatCommunicationDisabledUntil(member: GuildMember): string | null {
		return member.communicationDisabledUntil?.toISOString() ?? null;
	}
}
