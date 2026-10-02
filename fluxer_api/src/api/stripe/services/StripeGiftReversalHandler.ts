// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {PremiumStateReconciliationQueueService} from '@app/api/infrastructure/PremiumStateReconciliationQueueService';
import {Logger} from '@app/api/Logger';
import {addGiftCodeDuration, type GiftCode} from '@app/api/models/GiftCode';
import type {User} from '@app/api/models/User';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {clearPerksSanitizedFlag} from '@app/api/user/UserHelpers';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';

interface RemainingGiftEntitlement {
	hasLifetimeGift: boolean;
	giftExtensionEndsAt: Date | null;
}

export class StripeGiftReversalHandler {
	constructor(
		private userRepository: IUserRepository,
		private gatewayService: IGatewayService,
		private premiumStateReconciliationQueueService: PremiumStateReconciliationQueueService,
		private storeEntitlementService: StoreEntitlementService | null = null,
	) {}

	async handleGiftPremiumReversal(
		giftCode: GiftCode,
		context: {
			reason: string;
			chargeId?: string;
		},
	): Promise<void> {
		const redeemerId = giftCode.redeemedByUserId;
		if (!redeemerId) {
			return;
		}
		const redeemer = await this.userRepository.findUnique(redeemerId);
		if (!redeemer) {
			Logger.warn({giftCode: giftCode.code, redeemerId}, 'Gift redeemer not found for premium reversal');
			return;
		}
		if (redeemer.stripeSubscriptionId || redeemer.stripeCustomerId) {
			const redeemedGifts = await this.userRepository.findGiftCodesByRedeemer(redeemer.id);
			const currentGiftEnd = redeemer.premiumGiftExtensionEndsAt;
			let newGiftEnd: Date | null;
			let needsAdjustment: boolean;
			let reapplyIfMarked = true;
			if (await this.storeEntitlementService?.getActiveStoreEntitlement(redeemer.id)) {
				const reduced = this.reduceStackedGiftExtension(redeemer, giftCode, redeemedGifts, new Date());
				newGiftEnd = reduced.giftExtensionEndsAt;
				needsAdjustment = reduced.changed;
				reapplyIfMarked = false;
			} else {
				newGiftEnd = this.computeRemainingGiftEntitlement(redeemedGifts, giftCode.code).giftExtensionEndsAt;
				needsAdjustment =
					(newGiftEnd?.getTime() ?? 0) !== (currentGiftEnd?.getTime() ?? 0) &&
					(currentGiftEnd == null || newGiftEnd == null || currentGiftEnd.getTime() > newGiftEnd.getTime());
			}
			if (
				needsAdjustment &&
				(await this.commitReversal(redeemer, giftCode, {premium_gift_extension_ends_at: newGiftEnd}, {reapplyIfMarked}))
			) {
				Logger.info(
					{
						giftCode: giftCode.code,
						redeemerId: redeemer.id,
						chargeId: context.chargeId,
						reason: context.reason,
						adjustedGiftEnd: newGiftEnd?.toISOString() ?? null,
						previousGiftEnd: currentGiftEnd?.toISOString() ?? null,
					},
					'Reduced gift extension after gift reversal for user with Stripe identity',
				);
			}
			await this.enqueuePremiumStateReconciliation(redeemer.id, {
				reason: context.reason,
				subscriptionId: redeemer.stripeSubscriptionId ?? undefined,
			});
			Logger.info(
				{
					giftCode: giftCode.code,
					redeemerId: redeemer.id,
					chargeId: context.chargeId,
					reason: context.reason,
				},
				'Enqueued reconciliation after gift reversal for user with Stripe identity',
			);
			return;
		}
		if (await this.storeEntitlementService?.getActiveStoreEntitlement(redeemer.id)) {
			await this.reverseGiftForStoreSubscriber(redeemer, giftCode, context);
			return;
		}
		const redeemedGifts = await this.userRepository.findGiftCodesByRedeemer(redeemer.id);
		const hasGiftInRedeemerIndex = redeemedGifts.some((entry) => entry.code === giftCode.code);
		if (!hasGiftInRedeemerIndex) {
			Logger.warn(
				{
					giftCode: giftCode.code,
					redeemerId: redeemer.id,
					redeemedGiftCount: redeemedGifts.length,
					reason: context.reason,
				},
				'Skipped direct gift premium revocation because redeemer gift history is incomplete',
			);
			return;
		}
		const remainingEntitlement = this.computeRemainingGiftEntitlement(redeemedGifts, giftCode.code);
		if (remainingEntitlement.hasLifetimeGift) {
			Logger.info(
				{
					giftCode: giftCode.code,
					redeemerId: redeemer.id,
					reason: context.reason,
				},
				'Skipped direct gift premium revocation because user has another redeemed lifetime gift',
			);
			return;
		}
		const entitlementUntil = remainingEntitlement.giftExtensionEndsAt;
		if (entitlementUntil && entitlementUntil.getTime() > Date.now()) {
			const patch: Partial<UserRow> = {};
			if (redeemer.premiumType !== UserPremiumTypes.SUBSCRIPTION) {
				patch.premium_type = UserPremiumTypes.SUBSCRIPTION;
			}
			if (
				!redeemer.premiumGiftExtensionEndsAt ||
				redeemer.premiumGiftExtensionEndsAt.getTime() !== entitlementUntil.getTime()
			) {
				patch.premium_gift_extension_ends_at = entitlementUntil;
			}
			if (redeemer.premiumWillCancel !== false) {
				patch.premium_will_cancel = false;
			}
			if (Object.keys(patch).length > 0) {
				await this.commitReversal(redeemer, giftCode, patch, {reapplyIfMarked: true});
			}
			Logger.info(
				{
					giftCode: giftCode.code,
					redeemerId: redeemer.id,
					chargeId: context.chargeId,
					reason: context.reason,
					entitlementUntil: entitlementUntil.toISOString(),
				},
				'Adjusted gift extension after gift premium reversal',
			);
			return;
		}
		if (redeemer.premiumType === UserPremiumTypes.LIFETIME) {
			Logger.warn(
				{
					giftCode: giftCode.code,
					redeemerId: redeemer.id,
					reason: context.reason,
				},
				'Skipped lifetime premium revocation for gift reversal to preserve explicit lifetime overrides',
			);
			return;
		}
		await this.commitReversal(
			redeemer,
			giftCode,
			{
				premium_type: UserPremiumTypes.NONE,
				premium_until: null,
				premium_gift_extension_ends_at: null,
			},
			{reapplyIfMarked: true},
		);
		Logger.debug(
			{
				giftCode: giftCode.code,
				redeemerId: redeemer.id,
				chargeId: context.chargeId,
				reason: context.reason,
			},
			'Premium revoked after gift premium reversal',
		);
	}

	private async reverseGiftForStoreSubscriber(
		redeemer: User,
		giftCode: GiftCode,
		context: {
			reason: string;
			chargeId?: string;
		},
	): Promise<void> {
		const currentGiftEnd = redeemer.premiumGiftExtensionEndsAt;
		const newGiftEnd = await this.reverseStackedGift(redeemer, giftCode);
		await this.storeEntitlementService?.applyStoreEntitlementToUser(redeemer.id);
		Logger.info(
			{
				giftCode: giftCode.code,
				redeemerId: redeemer.id,
				chargeId: context.chargeId,
				reason: context.reason,
				adjustedGiftEnd: newGiftEnd?.toISOString() ?? null,
				previousGiftEnd: currentGiftEnd?.toISOString() ?? null,
			},
			'Reduced gift extension after gift reversal for user with a store subscription',
		);
	}

	async reverseStackedGift(redeemer: User, giftCode: GiftCode): Promise<Date | null> {
		const redeemedGifts = await this.userRepository.findGiftCodesByRedeemer(redeemer.id);
		const reduced = this.reduceStackedGiftExtension(redeemer, giftCode, redeemedGifts, new Date());
		if (
			!reduced.changed ||
			!(await this.commitReversal(
				redeemer,
				giftCode,
				{premium_gift_extension_ends_at: reduced.giftExtensionEndsAt},
				{reapplyIfMarked: false},
			))
		) {
			return redeemer.premiumGiftExtensionEndsAt;
		}
		return reduced.giftExtensionEndsAt;
	}

	private reduceStackedGiftExtension(
		redeemer: User,
		giftCode: GiftCode,
		redeemedGifts: Array<GiftCode>,
		now: Date,
	): {changed: boolean; giftExtensionEndsAt: Date | null} {
		const currentGiftEnd = redeemer.premiumGiftExtensionEndsAt;
		const remaining = this.computeRemainingGiftEntitlement(redeemedGifts, giftCode.code);
		if (!currentGiftEnd || remaining.hasLifetimeGift) {
			return {changed: false, giftExtensionEndsAt: currentGiftEnd};
		}
		const extendedEnd = addGiftCodeDuration(currentGiftEnd, giftCode.durationType, giftCode.durationQuantity);
		if (!extendedEnd) {
			const recomputed = remaining.giftExtensionEndsAt;
			const changed = !recomputed || recomputed.getTime() < currentGiftEnd.getTime();
			return {changed, giftExtensionEndsAt: changed ? recomputed : currentGiftEnd};
		}
		const reducedMs = currentGiftEnd.getTime() - (extendedEnd.getTime() - currentGiftEnd.getTime());
		const floorMs = Math.max(now.getTime(), redeemer.premiumUntil?.getTime() ?? 0);
		return {changed: true, giftExtensionEndsAt: reducedMs <= floorMs ? null : new Date(reducedMs)};
	}

	async restoreReversedGift(giftCode: GiftCode): Promise<void> {
		const redeemerId = giftCode.redeemedByUserId;
		if (!redeemerId) {
			return;
		}
		const seconds = (await this.userRepository.findGiftCode(giftCode.code))?.premiumReversedSeconds ?? null;
		if (seconds === null || !(await this.userRepository.clearGiftPremiumReversed(giftCode.code, seconds))) {
			return;
		}
		const user = seconds > 0 ? await this.userRepository.findUnique(redeemerId) : null;
		if (!user) {
			return;
		}
		const now = new Date();
		const anchorMs = Math.max(
			now.getTime(),
			user.premiumUntil?.getTime() ?? 0,
			user.premiumGiftExtensionEndsAt?.getTime() ?? 0,
		);
		const patch: Partial<UserRow> = {
			premium_gift_extension_ends_at: new Date(anchorMs + seconds * 1000),
			premium_grace_ends_at: null,
		};
		if ((user.premiumType ?? 0) <= 0) {
			patch.premium_type = UserPremiumTypes.SUBSCRIPTION;
			patch.premium_flags = clearPerksSanitizedFlag(user.premiumFlags);
			patch.premium_since = user.premiumSince ?? now;
		}
		let updatedUser: User;
		try {
			updatedUser = await this.userRepository.patchUpsert(redeemerId, patch, user.toRow());
		} catch (error) {
			try {
				await this.userRepository.markGiftPremiumReversed(giftCode, seconds);
			} catch (markError) {
				Logger.error({giftCode: giftCode.code, redeemerId, markError}, 'Failed to restore gift reversal marker');
			}
			throw error;
		}
		await this.dispatchUser(updatedUser);
	}

	private async commitReversal(
		redeemer: User,
		giftCode: GiftCode,
		patch: Partial<UserRow>,
		{reapplyIfMarked}: {reapplyIfMarked: boolean},
	): Promise<boolean> {
		const seconds = this.computeRemovedSeconds(redeemer, patch, Date.now());
		if (!(await this.userRepository.markGiftPremiumReversed(giftCode, seconds))) {
			if (!reapplyIfMarked) {
				Logger.info(
					{giftCode: giftCode.code, redeemerId: redeemer.id},
					'Skipped a gift premium reversal that was already applied',
				);
				return false;
			}
			await this.dispatchUser(await this.userRepository.patchUpsert(redeemer.id, patch, redeemer.toRow()));
			return true;
		}
		let updatedUser: User;
		try {
			updatedUser = await this.userRepository.patchUpsert(redeemer.id, patch, redeemer.toRow());
		} catch (error) {
			try {
				await this.userRepository.clearGiftPremiumReversed(giftCode.code, seconds);
			} catch (clearError) {
				Logger.error(
					{giftCode: giftCode.code, redeemerId: redeemer.id, clearError},
					'Failed to release gift reversal marker',
				);
			}
			throw error;
		}
		await this.dispatchUser(updatedUser);
		return true;
	}

	private computeRemovedSeconds(redeemer: User, patch: Partial<UserRow>, nowMs: number): number {
		const nextUntil = 'premium_until' in patch ? patch.premium_until : redeemer.premiumUntil;
		const nextGiftEnd =
			'premium_gift_extension_ends_at' in patch
				? patch.premium_gift_extension_ends_at
				: redeemer.premiumGiftExtensionEndsAt;
		const beforeMs = Math.max(
			redeemer.premiumUntil?.getTime() ?? 0,
			redeemer.premiumGiftExtensionEndsAt?.getTime() ?? 0,
		);
		const afterMs = Math.max(nowMs, nextUntil?.getTime() ?? 0, nextGiftEnd?.getTime() ?? 0);
		return Math.max(0, Math.ceil((beforeMs - afterMs) / 1000));
	}

	computeRemainingGiftEntitlement(redeemedGifts: Array<GiftCode>, excludedCode: string): RemainingGiftEntitlement {
		const sortedGifts = redeemedGifts
			.filter(
				(giftCode) =>
					giftCode.code !== excludedCode &&
					giftCode.redeemedAt != null &&
					giftCode.premiumReversedSeconds === null &&
					giftCode.revokedAt === null,
			)
			.sort((left, right) => {
				const leftRedeemedAt = left.redeemedAt?.getTime() ?? 0;
				const rightRedeemedAt = right.redeemedAt?.getTime() ?? 0;
				if (leftRedeemedAt !== rightRedeemedAt) {
					return leftRedeemedAt - rightRedeemedAt;
				}
				return left.code.localeCompare(right.code);
			});
		let hasLifetimeGift = false;
		let giftExtensionEndsAt: Date | null = null;
		for (const giftCode of sortedGifts) {
			const redeemedAt = giftCode.redeemedAt;
			if (!redeemedAt) {
				continue;
			}
			const startAt =
				giftExtensionEndsAt && giftExtensionEndsAt.getTime() > redeemedAt.getTime()
					? giftExtensionEndsAt
					: new Date(redeemedAt.getTime());
			const entitlementUntil = addGiftCodeDuration(startAt, giftCode.durationType, giftCode.durationQuantity);
			if (!entitlementUntil) {
				hasLifetimeGift = true;
				continue;
			}
			giftExtensionEndsAt = entitlementUntil;
		}
		return {
			hasLifetimeGift,
			giftExtensionEndsAt,
		};
	}

	private async dispatchUser(user: User): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(user),
		});
	}

	private async enqueuePremiumStateReconciliation(
		userId: UserID,
		context: {
			reason: string;
			subscriptionId?: string;
		},
	): Promise<void> {
		try {
			await this.premiumStateReconciliationQueueService.enqueueUser(userId);
		} catch (error) {
			Logger.warn(
				{
					error,
					userId: userId.toString(),
					reason: context.reason,
					subscriptionId: context.subscriptionId,
				},
				'Failed to enqueue premium reconciliation from Stripe webhook',
			);
		}
	}
}
