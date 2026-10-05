// SPDX-License-Identifier: AGPL-3.0-or-later

import type {User} from '@app/api/models/User';
import {isTemporarilyBanned} from '@app/api/user/UserHelpers';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {
	HIDDEN_USER_DISCRIMINATOR,
	HIDDEN_USER_USERNAME,
	PublicUserFlags,
	UserFlags,
} from '@fluxer/constants/src/UserConstants';
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

export function hiddenUserPartial(partial: UserPartialResponse): UserPartialResponse {
	return {
		...partial,
		username: HIDDEN_USER_USERNAME,
		discriminator: HIDDEN_USER_DISCRIMINATOR.toString().padStart(4, '0'),
		global_name: null,
		avatar: null,
		avatar_color: null,
		flags: partial.flags | PublicUserFlags.PROFILE_HIDDEN,
	};
}

export function isHiddenPartial(partial: Pick<UserPartialResponse, 'flags'>): boolean {
	return (partial.flags & PublicUserFlags.PROFILE_HIDDEN) !== 0;
}

export function hiddenGuildMember(member: GuildMemberResponse): GuildMemberResponse {
	return {...member, nick: null, avatar: null, banner: null, accent_color: null};
}
