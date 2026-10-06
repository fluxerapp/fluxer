// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {selectActiveStoreSubscription} from '@app/api/store_billing/StoreBillingMappers';
import type {StoreBillingRepository} from '@app/api/store_billing/StoreBillingRepository';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {clearPerksSanitizedFlag} from '@app/api/user/UserHelpers';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';

const USER_WRITE_ATTEMPTS = 3;

interface StoreEntitlementWriterDeps {
	repository: StoreBillingRepository;
	userRepository: IUserRepository;
	userCacheService: UserCacheService;
	gatewayService: IGatewayService;
}

function sameTime(left: Date | null | undefined, right: Date | null | undefined): boolean {
	return (left?.getTime() ?? null) === (right?.getTime() ?? null);
}

export function isStripeSubscriptionActive(user: User, now: Date): boolean {
	return (
		Boolean(user.stripeSubscriptionId) &&
		user.premiumBillingCycle !== null &&
		user.premiumUntil !== null &&
		user.premiumUntil > now
	);
}

function setIfChanged<K extends keyof UserRow>(
	patch: Partial<UserRow>,
	key: K,
	current: UserRow[K],
	next: UserRow[K],
): void {
	if (current instanceof Date || next instanceof Date) {
		if (!sameTime(current as Date | null, next as Date | null)) {
			patch[key] = next;
		}
		return;
	}
	if ((current ?? null) !== (next ?? null)) {
		patch[key] = next;
	}
}

function buildGrantPatch(user: User, row: StorePurchaseRow, now: Date): Partial<UserRow> {
	const patch: Partial<UserRow> = {};
	const expiresAt = row.expires_at;
	if (!expiresAt) {
		return patch;
	}
	const oldUntil = user.premiumUntil;
	if (isStripeSubscriptionActive(user, now)) {
		if (oldUntil && oldUntil.getTime() >= expiresAt.getTime()) {
			return patch;
		}
		setIfChanged(patch, 'premium_type', user.premiumType, UserPremiumTypes.SUBSCRIPTION);
		setIfChanged(patch, 'premium_until', oldUntil, expiresAt);
		setIfChanged(patch, 'has_ever_purchased', user.hasEverPurchased, true);
		return patch;
	}
	const inStoreGrace =
		oldUntil !== null &&
		user.premiumGraceEndsAt !== null &&
		user.premiumGraceEndsAt.getTime() > oldUntil.getTime() &&
		user.premiumGraceEndsAt.getTime() > now.getTime();
	const anchorMs = inStoreGrace ? oldUntil.getTime() : Math.max(now.getTime(), oldUntil?.getTime() ?? 0);
	const giftEnd = user.premiumGiftExtensionEndsAt;
	const shiftMs = expiresAt.getTime() - anchorMs;
	const shiftedGiftEnd =
		giftEnd && shiftMs > 0 && giftEnd.getTime() > anchorMs ? new Date(giftEnd.getTime() + shiftMs) : giftEnd;
	const startedAt = row.started_at ?? now;
	const premiumSince = user.premiumSince && user.premiumSince <= startedAt ? user.premiumSince : startedAt;
	setIfChanged(patch, 'premium_type', user.premiumType, UserPremiumTypes.SUBSCRIPTION);
	setIfChanged(patch, 'premium_since', user.premiumSince, premiumSince);
	setIfChanged(patch, 'premium_until', oldUntil, expiresAt);
	setIfChanged(patch, 'premium_gift_extension_ends_at', giftEnd, shiftedGiftEnd);
	setIfChanged(
		patch,
		'premium_grace_ends_at',
		user.premiumGraceEndsAt,
		row.state === 'grace' ? (row.grace_ends_at ?? null) : null,
	);
	setIfChanged(patch, 'premium_will_cancel', user.premiumWillCancel, row.auto_renew !== true);
	setIfChanged(patch, 'premium_billing_cycle', user.premiumBillingCycle, null);
	setIfChanged(patch, 'has_ever_purchased', user.hasEverPurchased, true);
	setIfChanged(patch, 'premium_flags', user.premiumFlags, clearPerksSanitizedFlag(user.premiumFlags));
	return patch;
}

function buildCutPatch(
	user: User,
	rows: Array<StorePurchaseRow>,
	departedRows: Array<StorePurchaseRow>,
	now: Date,
): Partial<UserRow> {
	const patch: Partial<UserRow> = {};
	const premiumUntil = user.premiumUntil;
	if (user.premiumType !== UserPremiumTypes.SUBSCRIPTION || !premiumUntil || isStripeSubscriptionActive(user, now)) {
		return patch;
	}
	const candidates = [
		...rows.filter((row) => row.kind === 'subscription' && !row.entitled),
		...departedRows.filter((row) => row.kind === 'subscription'),
	];
	const untilMs = premiumUntil.getTime();
	const owner = candidates.find((row) =>
		[row.expires_at, row.grace_ends_at, row.revoked_at].some((value) => value?.getTime() === untilMs),
	);
	if (!owner) {
		return patch;
	}
	let cutMs = Math.min(untilMs, now.getTime());
	if (owner.revoked_at) {
		cutMs = Math.min(untilMs, owner.revoked_at.getTime());
	}
	const cutAt = new Date(cutMs);
	setIfChanged(patch, 'premium_until', premiumUntil, cutAt);
	setIfChanged(patch, 'premium_grace_ends_at', user.premiumGraceEndsAt, cutAt);
	const giftEnd = user.premiumGiftExtensionEndsAt;
	if (giftEnd && giftEnd.getTime() > untilMs && cutMs < untilMs) {
		setIfChanged(patch, 'premium_gift_extension_ends_at', giftEnd, new Date(giftEnd.getTime() - (untilMs - cutMs)));
	}
	return patch;
}

export class StoreEntitlementWriter {
	constructor(private readonly deps: StoreEntitlementWriterDeps) {}

	async applyEntitlement(userId: UserID, departedRows: Array<StorePurchaseRow>): Promise<void> {
		if (Config.instance.selfHosted) {
			return;
		}
		for (let attempt = 1; attempt <= USER_WRITE_ATTEMPTS; attempt++) {
			try {
				await this.writeEntitlement(userId, departedRows);
				return;
			} catch (error) {
				if (attempt === USER_WRITE_ATTEMPTS) {
					throw error;
				}
				Logger.warn({error, userId: userId.toString(), attempt}, 'Retrying a store entitlement write');
			}
		}
	}

	async patchUserAndDispatch(user: User, patch: Partial<UserRow>): Promise<User> {
		const updatedUser = await this.deps.userRepository.patchUpsert(user.id, patch, user.toRow());
		this.deps.userCacheService.setUserPartialResponseFromUserInBackground(updatedUser);
		await this.deps.gatewayService.dispatchPresence({
			userId: updatedUser.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(updatedUser),
		});
		return updatedUser;
	}

	private async writeEntitlement(userId: UserID, departedRows: Array<StorePurchaseRow>): Promise<void> {
		const user = await this.deps.userRepository.findUnique(userId);
		if (
			!user ||
			user.isBot ||
			user.premiumType === UserPremiumTypes.LIFETIME ||
			(user.flags & UserFlags.DELETED) !== 0n
		) {
			return;
		}
		const rows = await this.deps.repository.listPurchasesForUser(userId);
		const now = new Date();
		const best = selectActiveStoreSubscription(rows, now);
		const patch = best ? buildGrantPatch(user, best, now) : buildCutPatch(user, rows, departedRows, now);
		if (Object.keys(patch).length === 0) {
			return;
		}
		await this.patchUserAndDispatch(user, patch);
		Logger.debug({userId: userId.toString(), fields: Object.keys(patch)}, 'Applied store entitlement to user');
	}
}
