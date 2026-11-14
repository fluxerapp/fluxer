// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRoleIDSet, createUserID, type RoleID, type UserID} from '@app/api/BrandedTypes';
import type {Guild} from '@app/api/models/Guild';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {User} from '@app/api/models/User';
import {assertTierAllows, NO_TIER_CONTEXT} from '@app/api/user/AccountTier';
import {TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';
import {
	GuildFeatures,
	GuildVerificationLevel,
	getEffectiveGuildVerificationLevel,
} from '@fluxer/constants/src/GuildConstants';
import {
	GuildVerificationWaitReason,
	getGuildVerificationWait,
	isExemptFromGuildVerification,
} from '@fluxer/constants/src/GuildVerificationRules';
import {AccountTooNewForGuildError} from '@fluxer/errors/src/domains/guild/AccountTooNewForGuildError';
import {
	GuildVerificationRequiredError,
	GuildVerificationRequiredReasons,
} from '@fluxer/errors/src/domains/guild/GuildVerificationRequiredError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

interface VerificationParams {
	user: User;
	ownerId: UserID;
	verificationLevel: number;
	memberJoinedAt?: Date | string | null;
	memberRoles?: Set<RoleID>;
}

function checkGuildVerification(params: VerificationParams): void {
	const {user, ownerId, verificationLevel, memberJoinedAt, memberRoles} = params;
	const exempt = isExemptFromGuildVerification({
		verificationLevel,
		isOwner: user.id === ownerId,
		isBot: user.isBot,
		hasRoles: (memberRoles?.size ?? 0) > 0,
	});
	if (exempt) return;
	assertTierAllows(user, TierGatedActions.GUILD_VERIFICATION, NO_TIER_CONTEXT);
	const wait = getGuildVerificationWait({
		verificationLevel,
		accountCreatedAt: snowflakeToDate(BigInt(user.id)).getTime(),
		memberJoinedAt: memberJoinedAt ? new Date(memberJoinedAt).getTime() : null,
		now: Date.now(),
	});
	if (wait?.reason === GuildVerificationWaitReason.ACCOUNT_TOO_NEW) throw new AccountTooNewForGuildError();
	if (wait?.reason === GuildVerificationWaitReason.NOT_MEMBER_LONG_ENOUGH) {
		throw new GuildVerificationRequiredError(GuildVerificationRequiredReasons.MEMBERSHIP_AGE);
	}
}

export function checkGuildVerificationWithGuildModel({
	user,
	guild,
	member,
}: {
	user: User;
	guild: Guild;
	member: GuildMember;
}): void {
	checkGuildVerification({
		user,
		ownerId: guild.ownerId,
		verificationLevel: getEffectiveGuildVerificationLevel(
			guild.verificationLevel ?? GuildVerificationLevel.NONE,
			guild.features.has(GuildFeatures.DISCOVERABLE),
		),
		memberJoinedAt: member.joinedAt,
		memberRoles: member.roleIds,
	});
}

export function checkGuildVerificationWithResponse({
	user,
	guild,
	member,
}: {
	user: User;
	guild: GuildResponse;
	member: GuildMemberResponse;
}): void {
	if (!guild.owner_id) {
		throw new Error('Guild owner_id is missing - cannot perform verification');
	}
	const ownerIdBigInt = typeof guild.owner_id === 'bigint' ? guild.owner_id : BigInt(guild.owner_id);
	checkGuildVerification({
		user,
		ownerId: createUserID(ownerIdBigInt),
		verificationLevel: getEffectiveGuildVerificationLevel(
			guild.verification_level ?? GuildVerificationLevel.NONE,
			(guild.features ?? []).includes(GuildFeatures.DISCOVERABLE),
		),
		memberJoinedAt: member.joined_at,
		memberRoles: createRoleIDSet(new Set(member.roles.map((roleId) => BigInt(roleId)))),
	});
}
