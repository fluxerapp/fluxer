// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildMFALevel} from '@fluxer/constants/src/GuildConstants';
import {MfaNotEnabledError} from '@fluxer/errors/src/domains/auth/MfaNotEnabledError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {UserID} from '../../BrandedTypes';
import type {IUserRepository} from '../../user/IUserRepository';

const ELEVATED_MFA_PERMISSIONS =
	Permissions.KICK_MEMBERS |
	Permissions.BAN_MEMBERS |
	Permissions.ADMINISTRATOR |
	Permissions.MANAGE_CHANNELS |
	Permissions.MANAGE_GUILD |
	Permissions.MANAGE_MESSAGES |
	Permissions.MANAGE_ROLES |
	Permissions.MANAGE_WEBHOOKS |
	Permissions.MODERATE_MEMBERS;

export async function createGuildMfaEnforcer(params: {
	userRepository: IUserRepository;
	guildData: Pick<GuildResponse, 'mfa_level' | 'owner_id'>;
	userId: UserID;
}): Promise<(permission: bigint) => void> {
	const {userRepository, guildData, userId} = params;
	const requiresGuildMfa = guildData.mfa_level === GuildMFALevel.ELEVATED && guildData.owner_id !== userId.toString();
	let actorLacksMfa = false;
	if (requiresGuildMfa) {
		const actor = await userRepository.findUnique(userId);
		actorLacksMfa = !actor || actor.authenticatorTypes.size === 0;
	}
	return (permission: bigint) => {
		if (requiresGuildMfa && actorLacksMfa && (permission & ELEVATED_MFA_PERMISSIONS) !== 0n) {
			throw new MfaNotEnabledError();
		}
	};
}
