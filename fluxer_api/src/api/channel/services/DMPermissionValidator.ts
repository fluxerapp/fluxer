// SPDX-License-Identifier: AGPL-3.0-or-later

import {RelationshipTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {CannotSendMessagesToUserError} from '@fluxer/errors/src/domains/channel/CannotSendMessagesToUserError';
import {UnclaimedAccountCannotSendDirectMessagesError} from '@fluxer/errors/src/domains/channel/UnclaimedAccountCannotSendDirectMessagesError';
import type {UserID} from '../../BrandedTypes';
import {SYSTEM_USER_ID} from '../../constants/Core';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {Guild} from '../../models/Guild';
import type {User} from '../../models/User';
import type {UserSettings} from '../../models/UserSettings';
import type {IUserRepository} from '../../user/IUserRepository';
import {isBugHunterBotUser} from '../../user/UserHelpers';
import {checkGuildVerificationWithGuildModel} from '../../utils/GuildVerificationUtils';
import {getMutualGuildsForDmAccess} from '../../utils/MutualGuildDmAccess';

interface DMPermissionValidatorDeps {
	userRepository: IUserRepository;
	guildRepository: IGuildRepositoryAggregate;
}

export class DMPermissionValidator {
	constructor(private deps: DMPermissionValidatorDeps) {}

	async validate({senderId, recipientId}: {senderId: UserID; recipientId: UserID}): Promise<void> {
		if (senderId === SYSTEM_USER_ID) {
			return;
		}
		const senderUser = await this.deps.userRepository.findUnique(senderId);
		if (!senderUser) {
			throw new CannotSendMessagesToUserError();
		}
		if (senderUser.isUnclaimedAccount()) {
			throw new UnclaimedAccountCannotSendDirectMessagesError();
		}
		const targetUser = await this.deps.userRepository.findUnique(recipientId);
		if (!targetUser) {
			throw new CannotSendMessagesToUserError();
		}
		if (isBugHunterBotUser(senderUser)) {
			return;
		}
		const isShadowbannedSpammer = !senderUser.isBot && (senderUser.flags & UserFlags.SPAMMER) === UserFlags.SPAMMER;
		const [senderBlockedTarget, targetBlockedSender, areFriends, targetSettings, senderSettings] = await Promise.all([
			this.deps.userRepository.getRelationship(senderId, recipientId, RelationshipTypes.BLOCKED),
			this.deps.userRepository.getRelationship(recipientId, senderId, RelationshipTypes.BLOCKED),
			this.deps.userRepository.getRelationship(senderId, recipientId, RelationshipTypes.FRIEND),
			this.deps.userRepository.findSettings(recipientId),
			this.deps.userRepository.findSettings(senderId),
		]);
		if (senderBlockedTarget || targetBlockedSender) {
			throw new CannotSendMessagesToUserError();
		}
		if (isShadowbannedSpammer) {
			return;
		}
		if (areFriends) {
			return;
		}
		if (targetUser.flags & UserFlags.APP_STORE_REVIEWER) {
			throw new CannotSendMessagesToUserError();
		}
		if (this.hasStaffDmAccess({senderUser, targetUser, senderSettings, targetSettings, senderId, recipientId})) {
			return;
		}
		const [senderGuilds, targetGuilds] = await Promise.all([
			this.deps.guildRepository.listUserGuilds(senderId),
			this.deps.guildRepository.listUserGuilds(recipientId),
		]);
		const mutualGuilds = getMutualGuildsForDmAccess({userGuilds: senderGuilds, targetGuilds});
		if (mutualGuilds.length === 0) {
			throw new CannotSendMessagesToUserError();
		}
		const targetHasRestrictions = hasDmRestrictions(targetSettings, senderUser.isBot);
		const senderHasRestrictions = hasDmRestrictions(senderSettings, targetUser.isBot);
		if (!targetHasRestrictions && !senderHasRestrictions) {
			return;
		}
		if (targetHasRestrictions) {
			if (!targetSettings) {
				throw new CannotSendMessagesToUserError();
			}
			await this.validateGuildRestrictions({
				settings: targetSettings,
				usesBotRestrictions: senderUser.isBot,
				userToVerify: senderUser,
				userToVerifyId: senderId,
				mutualGuilds,
			});
		}
		if (senderHasRestrictions) {
			if (!senderSettings) {
				throw new CannotSendMessagesToUserError();
			}
			await this.validateGuildRestrictions({
				settings: senderSettings,
				usesBotRestrictions: targetUser.isBot,
				userToVerify: targetUser,
				userToVerifyId: recipientId,
				mutualGuilds,
			});
		}
	}

	private async validateGuildRestrictions({
		settings,
		usesBotRestrictions,
		userToVerify,
		userToVerifyId,
		mutualGuilds,
	}: {
		settings: UserSettings;
		usesBotRestrictions: boolean;
		userToVerify: User;
		userToVerifyId: UserID;
		mutualGuilds: Array<Guild>;
	}): Promise<void> {
		const restrictedGuildIds = usesBotRestrictions ? settings.botRestrictedGuilds : settings.restrictedGuilds;
		const eligibleGuilds = mutualGuilds.filter((guild) => !restrictedGuildIds.has(guild.id));
		if (eligibleGuilds.length === 0) {
			throw new CannotSendMessagesToUserError();
		}
		for (const guild of eligibleGuilds) {
			const member = await this.deps.guildRepository.getMember(guild.id, userToVerifyId);
			if (!member) {
				continue;
			}
			try {
				checkGuildVerificationWithGuildModel({user: userToVerify, guild, member});
				return;
			} catch {}
		}
		throw new CannotSendMessagesToUserError();
	}

	private hasStaffDmAccess(params: {
		senderUser: User;
		targetUser: User;
		senderSettings: UserSettings | null;
		targetSettings: UserSettings | null;
		senderId: UserID;
		recipientId: UserID;
	}): boolean {
		const senderIsStaff = (params.senderUser.flags & UserFlags.STAFF) === UserFlags.STAFF;
		if (senderIsStaff && params.senderSettings?.staffDmAccessUserIds.has(params.recipientId)) {
			return true;
		}
		const targetIsStaff = (params.targetUser.flags & UserFlags.STAFF) === UserFlags.STAFF;
		if (targetIsStaff && params.targetSettings?.staffDmAccessUserIds.has(params.senderId)) {
			return true;
		}
		return false;
	}
}

function hasDmRestrictions(settings: UserSettings | null, isBot: boolean): boolean {
	if (!settings) {
		return false;
	}
	const defaultRestricted = isBot ? settings.botDefaultGuildsRestricted : settings.defaultGuildsRestricted;
	const restrictedGuilds = isBot ? settings.botRestrictedGuilds : settings.restrictedGuilds;
	return defaultRestricted || restrictedGuilds.size > 0;
}
