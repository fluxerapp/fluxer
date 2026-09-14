// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {PremiumStateReconciliationQueueService} from '@app/api/infrastructure/PremiumStateReconciliationQueueService';
import {Logger} from '@app/api/Logger';
import {addGiftCodeDuration, type GiftCode} from '@app/api/models/GiftCode';
import type {User} from '@app/api/models/User';
import type {StripePremiumService} from '@app/api/stripe/services/StripePremiumService';
import type {IUserRepository} from '@app/api/user/IUserRepository';
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
		private premiumService: StripePremiumService,
		private premiumStateReconciliationQueueService: PremiumStateReconciliationQueueService,
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
			const remainingEntitlement = this.computeRemainingGiftEntitlement(redeemedGifts, giftCode.code);
			const currentGiftEnd = redeemer.premiumGiftExtensionEndsAt;
			const newGiftEnd = remainingEntitlement.giftExtensionEndsAt;
			const needsAdjustment =
				(newGiftEnd?.getTime() ?? 0) !== (currentGiftEnd?.getTime() ?? 0) &&
				(currentGiftEnd == null || newGiftEnd == null || currentGiftEnd.getTime() > newGiftEnd.getTime());
			if (needsAdjustment) {
				const patch: Partial<UserRow> = {premium_gift_extension_ends_at: newGiftEnd};
				const updatedUser = await this.userRepository.patchUpsert(redeemer.id, patch, redeemer.toRow());
				await this.dispatchUser(updatedUser);
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
				const updatedUser = await this.userRepository.patchUpsert(redeemer.id, patch, redeemer.toRow());
				await this.dispatchUser(updatedUser);
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
		await this.premiumService.revokePremium(redeemer.id);
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

	computeRemainingGiftEntitlement(redeemedGifts: Array<GiftCode>, excludedCode: string): RemainingGiftEntitlement {
		const sortedGifts = redeemedGifts
			.filter((giftCode) => giftCode.code !== excludedCode && giftCode.redeemedAt != null)
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
