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

import type {UserID} from '~/BrandedTypes';
import {RelationshipTypes, UserFlags} from '~/Constants';
import {CannotSendMessagesToUserError, UnclaimedAccountRestrictedError} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {User} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import {checkGuildVerificationWithGuildModel} from '~/utils/GuildVerificationUtils';

interface DMPermissionValidatorDeps {
	userRepository: IUserRepository;
	guildRepository: IGuildRepository;
}

export class DMPermissionValidator {
	constructor(private deps: DMPermissionValidatorDeps) {}

	async validate({recipients, userId}: {recipients: Array<User>; userId: UserID}): Promise<void> {
		const senderUser = await this.deps.userRepository.findUnique(userId);
		if (senderUser && !senderUser.passwordHash && !senderUser.isBot) {
			throw new UnclaimedAccountRestrictedError('send direct messages');
		}

		const targetUser = recipients.find((recipient) => recipient.id !== userId);
		if (!targetUser) return;

		if (!targetUser.passwordHash && !targetUser.isBot) {
			throw new UnclaimedAccountRestrictedError('receive direct messages');
		}

		const senderBlockedTarget = await this.deps.userRepository.getRelationship(
			userId,
			targetUser.id,
			RelationshipTypes.BLOCKED,
		);
		if (senderBlockedTarget) {
			throw new CannotSendMessagesToUserError();
		}

		const targetBlockedSender = await this.deps.userRepository.getRelationship(
			targetUser.id,
			userId,
			RelationshipTypes.BLOCKED,
		);
		if (targetBlockedSender) {
			throw new CannotSendMessagesToUserError();
		}

		const friendship = await this.deps.userRepository.getRelationship(userId, targetUser.id, RelationshipTypes.FRIEND);
		if (friendship) return;

		if (targetUser.flags & UserFlags.APP_STORE_REVIEWER) {
			throw new CannotSendMessagesToUserError();
		}

		const targetSettings = await this.deps.userRepository.findSettings(targetUser.id);
		if (!targetSettings) return;

		const dmRestrictionsEnabled = targetSettings.defaultGuildsRestricted || targetSettings.restrictedGuilds.size > 0;
		if (!dmRestrictionsEnabled) {
			return;
		}

		const [userGuilds, targetGuilds] = await Promise.all([
			this.deps.guildRepository.listUserGuilds(userId),
			this.deps.guildRepository.listUserGuilds(targetUser.id),
		]);

		if (!senderUser) {
			throw new CannotSendMessagesToUserError();
		}

		const userGuildIds = new Set(userGuilds.map((guild) => guild.id));
		const mutualGuilds = targetGuilds.filter((guild) => userGuildIds.has(guild.id));

		if (mutualGuilds.length === 0) {
			throw new CannotSendMessagesToUserError();
		}

		const restrictedGuildIds = targetSettings.restrictedGuilds;

		let hasValidMutualGuild = false;
		for (const guild of mutualGuilds) {
			if (restrictedGuildIds.has(guild.id)) {
				continue;
			}

			const member = await this.deps.guildRepository.getMember(guild.id, userId);
			if (!member) {
				continue;
			}

			try {
				checkGuildVerificationWithGuildModel({user: senderUser, guild, member});
				hasValidMutualGuild = true;
				break;
			} catch {}
		}

		if (!hasValidMutualGuild) {
			throw new CannotSendMessagesToUserError();
		}
	}
}
