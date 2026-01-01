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

import {createRoleIDSet, createUserID, type RoleID, type UserID} from '~/BrandedTypes';
import {GuildVerificationLevel} from '~/Constants';
import {GuildVerificationRequiredError} from '~/Errors';
import type {GuildMemberResponse, GuildResponse} from '~/guild/GuildModel';
import type {Guild, GuildMember, User} from '~/Models';
import {extractTimestamp} from '~/utils/SnowflakeUtils';

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

	if (!user.email) {
		throw new GuildVerificationRequiredError('You need to claim your account to send messages in this guild.');
	}

	if (verificationLevel >= GuildVerificationLevel.LOW) {
		if (!user.emailVerified) {
			throw new GuildVerificationRequiredError('You need to verify your email to send messages in this guild.');
		}
	}

	if (verificationLevel >= GuildVerificationLevel.MEDIUM) {
		const createdAt = extractTimestamp(BigInt(user.id));
		const accountAge = Date.now() - createdAt;
		const FIVE_MINUTES_MS = 5 * 60 * 1000;
		if (accountAge < FIVE_MINUTES_MS) {
			throw new GuildVerificationRequiredError('Your account is too new to send messages in this guild.');
		}
	}

	if (verificationLevel >= GuildVerificationLevel.HIGH) {
		if (memberJoinedAt) {
			const joinedAtTime =
				typeof memberJoinedAt === 'string' ? new Date(memberJoinedAt).getTime() : memberJoinedAt.getTime();
			const membershipDuration = Date.now() - joinedAtTime;
			const TEN_MINUTES_MS = 10 * 60 * 1000;
			if (membershipDuration < TEN_MINUTES_MS) {
				throw new GuildVerificationRequiredError(
					"You haven't been a member of this guild long enough to send messages.",
				);
			}
		}
	}

	if (verificationLevel >= GuildVerificationLevel.VERY_HIGH) {
		if (!user.phone) {
			throw new GuildVerificationRequiredError('You need to add a phone number to send messages in this guild.');
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
		verificationLevel: guild.verificationLevel ?? GuildVerificationLevel.NONE,
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
	const ownerIdSource = guild.owner_id ?? user.id;
	const ownerIdBigInt = typeof ownerIdSource === 'bigint' ? ownerIdSource : BigInt(ownerIdSource);

	checkGuildVerification({
		user,
		ownerId: createUserID(ownerIdBigInt),
		verificationLevel: guild.verification_level ?? GuildVerificationLevel.NONE,
		memberJoinedAt: member.joined_at,
		memberRoles: createRoleIDSet(new Set(member.roles.map((roleId) => BigInt(roleId)))),
	});
}
