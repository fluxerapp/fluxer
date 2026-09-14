// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {promisify} from 'node:util';
import type {ApiContext} from '@app/api/ApiContext';
import type {UserID} from '@app/api/BrandedTypes';
import type {User} from '@app/api/models/User';
import * as AgeUtils from '@app/api/utils/AgeUtils';
import * as RandomUtils from '@app/api/utils/RandomUtils';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {BotUserAuthEndpointAccessDeniedError} from '@fluxer/errors/src/domains/auth/BotUserAuthEndpointAccessDeniedError';
import {AccountPermanentlySuspendedError} from '@fluxer/errors/src/domains/user/AccountPermanentlySuspendedError';
import {AccountTemporarilySuspendedError} from '@fluxer/errors/src/domains/user/AccountTemporarilySuspendedError';

const randomBytesAsync = promisify(crypto.randomBytes);
const ALPHANUMERIC_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

interface ValidateAgeParams {
	dateOfBirth: string;
	minAge: number;
}

interface AccountBanStatus {
	isPermanentlyBanned: boolean;
	isTempBanned: boolean;
	tempBanExpired: boolean;
}

function base62Encode(buffer: Uint8Array): string {
	let num = BigInt(`0x${Buffer.from(buffer).toString('hex')}`);
	const base = BigInt(ALPHANUMERIC_CHARS.length);
	let encoded = '';
	while (num > 0) {
		const remainder = num % base;
		encoded = ALPHANUMERIC_CHARS[Number(remainder)] + encoded;
		num = num / base;
	}
	return encoded;
}

export async function generateSecureToken(_ctx: ApiContext, length = 64): Promise<string> {
	return RandomUtils.randomString(length);
}

export async function generateAuthToken(_ctx: ApiContext): Promise<string> {
	const bytes = await randomBytesAsync(27);
	let token = base62Encode(new Uint8Array(bytes));
	if (token.length < 36) {
		token += RandomUtils.randomString(36 - token.length);
	}
	if (token.length > 36) {
		token = token.slice(0, 36);
	}
	return `flx_${token}`;
}

export function generateBackupCodes(_ctx: ApiContext): Array<string> {
	return Array.from({length: 10}, () => {
		return `${RandomUtils.randomString(4).toLowerCase()}-${RandomUtils.randomString(4).toLowerCase()}`;
	});
}

export function getTokenIdHash(_ctx: ApiContext, token: string): Uint8Array {
	return new Uint8Array(crypto.createHash('sha256').update(token).digest());
}

export function validateAge(_ctx: ApiContext, {dateOfBirth, minAge}: ValidateAgeParams): boolean {
	const birthDate = new Date(dateOfBirth);
	const age = AgeUtils.calculateAge({
		year: birthDate.getFullYear(),
		month: birthDate.getMonth() + 1,
		day: birthDate.getDate(),
	});
	return age >= minAge;
}

export function assertNonBotUser(_ctx: ApiContext, user: User): void {
	if (user.isBot) {
		throw new BotUserAuthEndpointAccessDeniedError();
	}
}

export async function authorizeIpByToken(
	ctx: ApiContext,
	token: string,
): Promise<{
	userId: UserID;
	email: string;
} | null> {
	const {users} = ctx.services;
	return users.authorizeIpByToken(token);
}

function checkAccountBanStatus(_ctx: ApiContext, user: User): AccountBanStatus {
	const isPermanentlyBanned = !!(user.flags & UserFlags.DELETED);
	const hasTempBan = !!(user.flags & UserFlags.DISABLED && user.tempBannedUntil);
	const tempBanExpired = hasTempBan && user.tempBannedUntil! <= new Date();
	return {
		isPermanentlyBanned,
		isTempBanned: hasTempBan && !tempBanExpired,
		tempBanExpired,
	};
}

export async function handleBanStatus(ctx: ApiContext, user: User): Promise<User> {
	const {users} = ctx.services;
	if (user.deletionStartedAt) throw new AccountPermanentlySuspendedError();
	const banStatus = checkAccountBanStatus(ctx, user);
	if (banStatus.isPermanentlyBanned) {
		throw new AccountPermanentlySuspendedError();
	}
	if (banStatus.isTempBanned) {
		throw new AccountTemporarilySuspendedError();
	}
	if (banStatus.tempBanExpired) {
		return users.patchUpsert(
			user.id,
			{
				flags: user.flags & ~UserFlags.DISABLED,
				temp_banned_until: null,
			},
			user.toRow(),
		);
	}
	return user;
}
