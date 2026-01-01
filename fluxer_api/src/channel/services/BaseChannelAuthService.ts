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

import {type ChannelID, channelIdToUserId, type UserID, userIdToChannelId} from '~/BrandedTypes';
import {ChannelTypes, Permissions} from '~/Constants';
import {
	MissingPermissionsError,
	NsfwContentRequiresAgeVerificationError,
	UnknownChannelError,
	UnknownUserError,
} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import {Channel, type User} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import {isUserAdult} from '~/utils/AgeUtils';
import type {IChannelRepositoryAggregate} from '../repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from './AuthenticatedChannel';
import {DMPermissionValidator} from './DMPermissionValidator';

export interface ChannelAuthOptions {
	errorOnMissingGuild: 'unknown_channel' | 'missing_permissions';
	validateNsfw: boolean;
	useVirtualPersonalNotes: boolean;
}

export abstract class BaseChannelAuthService {
	protected abstract readonly options: ChannelAuthOptions;
	protected dmPermissionValidator: DMPermissionValidator;

	constructor(
		protected channelRepository: IChannelRepositoryAggregate,
		protected userRepository: IUserRepository,
		protected guildRepository: IGuildRepository,
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
	}: {
		userId: UserID;
		channelId: ChannelID;
	}): Promise<AuthenticatedChannel> {
		if (this.isPersonalNotesChannel({userId, channelId})) {
			if (this.options.useVirtualPersonalNotes) {
				return this.getVirtualPersonalNotesChannelAuth(channelId);
			}
			const channel = await this.channelRepository.channelData.findUnique(channelId);
			if (!channel) throw new UnknownChannelError();
			return this.getRealPersonalNotesChannelAuth({channel, userId});
		}

		const channel = await this.channelRepository.channelData.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();

		if (!channel.guildId) {
			const recipients = await this.userRepository.listUsers(Array.from(channel.recipientIds));
			return this.getDMChannelAuth({channel, recipients, userId});
		}

		return this.getGuildChannelAuth({channel, userId});
	}

	isPersonalNotesChannel({userId, channelId}: {userId: UserID; channelId: ChannelID}): boolean {
		return userIdToChannelId(userId) === channelId;
	}

	protected createVirtualPersonalNotesChannel(userId: UserID): Channel {
		return new Channel({
			channel_id: userIdToChannelId(userId),
			guild_id: null,
			type: ChannelTypes.DM_PERSONAL_NOTES,
			name: '',
			topic: null,
			icon_hash: null,
			url: null,
			parent_id: null,
			position: 0,
			owner_id: null,
			recipient_ids: new Set(),
			nsfw: false,
			rate_limit_per_user: 0,
			bitrate: null,
			user_limit: null,
			rtc_region: null,
			last_message_id: null,
			last_pin_timestamp: null,
			permission_overwrites: null,
			nicks: null,
			soft_deleted: false,
			indexed_at: null,
			version: 1,
		});
	}

	protected getVirtualPersonalNotesChannelAuth(channelId: ChannelID): AuthenticatedChannel {
		const channel = this.createVirtualPersonalNotesChannel(channelIdToUserId(channelId));
		return {
			channel,
			guild: null,
			member: null,
			hasPermission: async () => true,
			checkPermission: async () => {},
		};
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
		const recipients = await this.userRepository.listUsers(Array.from(channel.recipientIds));
		await this.dmPermissionValidator.validate({recipients, userId});
	}

	protected async getGuildChannelAuth({
		channel,
		userId,
	}: {
		channel: Channel;
		userId: UserID;
	}): Promise<AuthenticatedChannel> {
		const guildId = channel.guildId!;
		const [guildDataResult, guildMemberResult] = await Promise.all([
			this.gatewayService.getGuildData({guildId, userId}),
			this.gatewayService.getGuildMember({guildId, userId}),
		]);

		if (!guildDataResult) {
			this.throwGuildAccessError();
		}
		if (!guildMemberResult.success || !guildMemberResult.memberData) {
			this.throwGuildAccessError();
		}

		const hasPermission = async (permission: bigint): Promise<boolean> => {
			return await this.gatewayService.checkPermission({guildId, userId, permission, channelId: channel.id});
		};

		const checkPermission = async (permission: bigint): Promise<void> => {
			const allowed = await hasPermission(permission);
			if (!allowed) throw new MissingPermissionsError();
		};

		await checkPermission(Permissions.VIEW_CHANNEL);

		if (this.options.validateNsfw && channel.type === ChannelTypes.GUILD_TEXT && channel.isNsfw) {
			const user = await this.userRepository.findUnique(userId);
			if (!user) throw new UnknownUserError();

			if (!isUserAdult(user.dateOfBirth)) {
				throw new NsfwContentRequiresAgeVerificationError();
			}
		}

		return {
			channel,
			guild: guildDataResult!,
			member: guildMemberResult.memberData!,
			hasPermission,
			checkPermission,
		};
	}

	protected throwGuildAccessError(): never {
		if (this.options.errorOnMissingGuild === 'missing_permissions') {
			throw new MissingPermissionsError();
		}
		throw new UnknownChannelError();
	}
}
