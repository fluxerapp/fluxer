// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {promisify} from 'node:util';
import type {ApiContext} from '@app/api/ApiContext';
import {createNoticeLookupDeps, loadAccountNotice} from '@app/api/enforcement/EnforcementAccountNotice';
import {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import {createNetworkBlockError} from '@app/api/enforcement/NetworkBlockError';
import {clearExpiredSuspension} from '@app/api/enforcement/SuspensionState';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {followOperatorPassWallStanding} from '@app/api/operator_pass/OperatorPassWallStanding';
import {isAccountClosed, isSignInRefused} from '@app/api/user/UserHelpers';
import * as AgeUtils from '@app/api/utils/AgeUtils';
import * as RandomUtils from '@app/api/utils/RandomUtils';
import {DeletionReasons} from '@fluxer/constants/src/Core';
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

export async function generateAuthToken(): Promise<string> {
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

export function generateBackupCodes(): Array<string> {
	return Array.from({length: 10}, () => {
		return `${RandomUtils.randomString(4).toLowerCase()}-${RandomUtils.randomString(4).toLowerCase()}`;
	});
}

export function getTokenIdHash(_ctx: ApiContext, token: string): Uint8Array {
	return new Uint8Array(crypto.createHash('sha256').update(token).digest());
}

export function validateAge({dateOfBirth, minAge}: ValidateAgeParams): boolean {
	const birthDate = new Date(dateOfBirth);
	const age = AgeUtils.calculateAge({
		year: birthDate.getUTCFullYear(),
		month: birthDate.getUTCMonth() + 1,
		day: birthDate.getUTCDate(),
	});
	return age >= minAge;
}

export function assertNonBotUser(_ctx: ApiContext, user: User): void {
	if (user.isBot) {
		throw new BotUserAuthEndpointAccessDeniedError();
	}
}

export function assertNetworkAllowed(ctx: ApiContext): void {
	const block = ctx.request.blockedNetworkSignIn;
	if (block) throw createNetworkBlockError(block, ctx.request.locale);
}

export function assertMfaTicketAllowed(ctx: ApiContext, user: User): void {
	if (!isSignInRefused(user)) assertNetworkAllowed(ctx);
}

const HOLDER_OR_HOUSEKEEPING_DELETION_REASONS: ReadonlySet<number> = new Set([
	DeletionReasons.USER_REQUESTED,
	DeletionReasons.INACTIVITY,
]);

function isHolderOrHousekeepingDeletion(user: User): boolean {
	if (user.enforcementDecisionId !== null) return false;
	return (
		(user.flags & UserFlags.SELF_DELETED) !== 0n ||
		(user.deletionReasonCode !== null && HOLDER_OR_HOUSEKEEPING_DELETION_REASONS.has(user.deletionReasonCode))
	);
}

async function refuseRestrictedSignIn(ctx: ApiContext, user: User): Promise<never> {
	if (isAccountClosed(user) && isHolderOrHousekeepingDeletion(user)) throw new AccountPermanentlySuspendedError(null);
	const notice = await loadAccountNotice(createNoticeLookupDeps(), user, ctx.request.locale);
	throw isAccountClosed(user) || notice.permanent
		? new AccountPermanentlySuspendedError(notice)
		: new AccountTemporarilySuspendedError(notice);
}

export async function assertSignInAllowed(ctx: ApiContext, user: User): Promise<User> {
	if (isSignInRefused(user)) await refuseRestrictedSignIn(ctx, user);
	assertNetworkAllowed(ctx);
	const expiry = await clearExpiredSuspension(
		{users: ctx.services.users, repository: new EnforcementDecisionRepository()},
		user,
	);
	return expiry?.cleared ?? user;
}

export async function reactivateOnSignIn(
	ctx: ApiContext,
	user: User,
	deletionQueue: Pick<KVAccountDeletionQueueService, 'removeFromQueue'>,
): Promise<User> {
	const {users} = ctx.services;
	let currentUser = user;
	if ((currentUser.flags & UserFlags.DISABLED) !== 0n && !currentUser.tempBannedUntil) {
		currentUser = (await users.updateFlags(currentUser.id, (flags) => flags & ~UserFlags.DISABLED)) ?? currentUser;
		Logger.info({userId: currentUser.id}, 'Auto-undisabled user on sign-in');
	}
	if ((currentUser.flags & UserFlags.SELF_DELETED) !== 0n) {
		currentUser = await users.updateDeletionSchedule(currentUser, {
			flags: currentUser.flags & ~UserFlags.SELF_DELETED,
			pending_deletion_at: null,
			deletion_reason_code: null,
			deletion_public_reason: null,
			deletion_audit_log_reason: null,
		});
		await deletionQueue.removeFromQueue(currentUser.id);
		Logger.info({userId: currentUser.id}, 'Auto-cancelled deletion on sign-in');
	}
	await followOperatorPassWallStanding(user, currentUser);
	return currentUser;
}
