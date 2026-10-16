// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac} from 'node:crypto';
import {Config} from '@app/api/Config';
import type {User} from '@app/api/models/User';
import {isTemporarilyBanned} from '@app/api/user/UserHelpers';
import {generateSeededUsername} from '@app/api/utils/UsernameGenerator';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {NON_SELF_HOSTED_RESERVED_DISCRIMINATORS} from '@fluxer/constants/src/DiscriminatorConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

type ProfileStanding = Pick<
	User,
	'flags' | 'isSystem' | 'tempBannedUntil' | 'pendingDeletionAt' | 'deletionReasonCode'
>;

const NON_ENFORCEMENT_DELETION_REASONS: ReadonlySet<number> = new Set([
	DeletionReasons.USER_REQUESTED,
	DeletionReasons.OTHER,
	DeletionReasons.INACTIVITY,
]);

export function isEnforcementDeletionReason(code: number | null): boolean {
	return code != null && !NON_ENFORCEMENT_DELETION_REASONS.has(code);
}

function isPendingEnforcementDeletion(user: Pick<User, 'pendingDeletionAt' | 'deletionReasonCode'>): boolean {
	return user.pendingDeletionAt != null && isEnforcementDeletionReason(user.deletionReasonCode);
}

export function isUnderEnforcement(user: Omit<ProfileStanding, 'isSystem'>, now = Date.now()): boolean {
	return (
		(user.flags & UserFlags.SPAMMER) !== 0n || isTemporarilyBanned(user, now) || isPendingEnforcementDeletion(user)
	);
}

export function isProfileHidden(user: ProfileStanding, now = Date.now()): boolean {
	if (user.isSystem) return false;
	return (user.flags & UserFlags.PROFILE_HIDDEN) !== 0n || isUnderEnforcement(user, now);
}

const MAX_DISCRIMINATOR = 9999;

interface ProfilePseudonym {
	username: string;
	discriminator: string;
}

function pseudonymDiscriminator(seed: Buffer): number {
	let value = (seed.readUInt32BE(seed.byteLength - 4) % MAX_DISCRIMINATOR) + 1;
	while (NON_SELF_HOSTED_RESERVED_DISCRIMINATORS.has(value)) {
		value = (value % MAX_DISCRIMINATOR) + 1;
	}
	return value;
}

export function profilePseudonym(
	userId: string | bigint,
	secret: string = Config.auth.profilePseudonymSecret,
): ProfilePseudonym {
	const seed = createHmac('sha256', secret).update(userId.toString()).digest();
	return {
		username: generateSeededUsername(seed),
		discriminator: pseudonymDiscriminator(seed).toString().padStart(4, '0'),
	};
}

export function hiddenUserPartial(partial: UserPartialResponse): UserPartialResponse {
	const pseudonym = profilePseudonym(partial.id);
	return {
		...partial,
		username: pseudonym.username,
		discriminator: pseudonym.discriminator,
		global_name: null,
		avatar: null,
		avatar_color: null,
	};
}

export function isHiddenPartial(
	partial: Pick<UserPartialResponse, 'id' | 'username' | 'discriminator' | 'global_name' | 'avatar'>,
): boolean {
	if (partial.global_name != null || partial.avatar != null) return false;
	const pseudonym = profilePseudonym(partial.id);
	return partial.username === pseudonym.username && partial.discriminator === pseudonym.discriminator;
}

export function hiddenGuildMember(member: GuildMemberResponse): GuildMemberResponse {
	return {...member, nick: null, avatar: null, banner: null, accent_color: null};
}
