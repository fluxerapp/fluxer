// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRoleIDSet, createUserID, type RoleID, type UserID} from '@app/api/BrandedTypes';
import type {Guild} from '@app/api/models/Guild';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {User} from '@app/api/models/User';
import {
	GuildFeatures,
	GuildVerificationLevel,
	getEffectiveGuildVerificationLevel,
} from '@fluxer/constants/src/GuildConstants';
import {GuildEmailVerificationRequiredError} from '@fluxer/errors/src/domains/auth/EmailVerificationRequiredError';
import {GuildPhoneVerificationRequiredError} from '@fluxer/errors/src/domains/auth/GuildPhoneVerificationRequiredError';
import {AccountTooNewForGuildError} from '@fluxer/errors/src/domains/guild/AccountTooNewForGuildError';
import {GuildVerificationRequiredError} from '@fluxer/errors/src/domains/guild/GuildVerificationRequiredError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {ms} from 'itty-time';

interface VerificationParams {
	user: User;
	ownerId: UserID;
	verificationLevel: number;
	memberJoinedAt?: Date | string | null;
	memberRoles?: Set<RoleID>;
}

function checkGuildVerification(params: VerificationParams): void {
	const {user, ownerId, verificationLevel, memberJoinedAt, memberRoles} = params;
	if (user.id === ownerId) {
		return;
	}
	if (verificationLevel === GuildVerificationLevel.NONE) {
		return;
	}
	if (user.isBot) {
		return;
	}
	if (memberRoles && memberRoles.size > 0) {
		return;
	}
	if (verificationLevel === GuildVerificationLevel.VERY_HIGH) {
		if (!user.hasVerifiedPhone) {
			throw new GuildPhoneVerificationRequiredError();
		}
		return;
	}
	if (!user.email) {
		throw new GuildVerificationRequiredError('You need to claim your account to send messages in this guild.');
	}
	if (verificationLevel >= GuildVerificationLevel.LOW) {
		if (!user.emailVerified) {
			throw new GuildEmailVerificationRequiredError();
		}
	}
	if (verificationLevel >= GuildVerificationLevel.MEDIUM) {
		const createdAt = snowflakeToDate(BigInt(user.id)).getTime();
		const accountAge = Date.now() - createdAt;
		if (accountAge < ms('5 minutes')) {
			throw new AccountTooNewForGuildError();
		}
	}
	if (verificationLevel >= GuildVerificationLevel.HIGH) {
		if (memberJoinedAt) {
			const joinedAtTime =
				typeof memberJoinedAt === 'string' ? new Date(memberJoinedAt).getTime() : memberJoinedAt.getTime();
			const membershipDuration = Date.now() - joinedAtTime;
			if (membershipDuration < ms('10 minutes')) {
				throw new GuildVerificationRequiredError(
					"You haven't been a member of this guild long enough to send messages.",
				);
			}
		}
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
