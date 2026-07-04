// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type {UserID} from '../../BrandedTypes';
import {createGuildID, createRoleID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {SYSTEM_USER_ID} from '../../constants/Core';
import type {GiftCodeDurationType} from '../../database/types/PaymentTypes';
import type {UserRow} from '../../database/types/UserTypes';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {GuildService} from '../../guild/services/GuildService';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {Logger} from '../../Logger';
import {createRequestCache} from '../../middleware/RequestCacheMiddleware';
import {addGiftCodeDuration} from '../../models/GiftCode';
import type {User} from '../../models/User';
import type {IUserRepository} from '../../user/IUserRepository';
import {createPremiumClearPatch} from '../../user/UserHelpers';
import {mapUserToPrivateResponse} from '../../user/UserMappers';

export class StripePremiumService {
	constructor(
		private userRepository: IUserRepository,
		private gatewayService: IGatewayService,
		private guildRepository: IGuildRepositoryAggregate,
		private guildService: GuildService,
	) {}

	async setPremiumFromSubscriptionPeriod(
		userId: UserID,
		premiumType: 1 | 2,
		periodEnd: Date,
		billingCycle: string | null = null,
		hasEverPurchased: boolean = false,
		premiumSinceAnchor: Date | null = null,
	): Promise<void> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new StripeError('User not found for premium grant');
		}
		const now = new Date();
		let visionarySequence: number | null = user.premiumLifetimeSequence;
		if (premiumType === UserPremiumTypes.LIFETIME && !visionarySequence) {
			visionarySequence = await this.allocateVisionarySequence(userId);
		}
		const shiftMs = user.premiumUntil ? periodEnd.getTime() - user.premiumUntil.getTime() : 0;
		const adjustedGiftEnd =
			user.premiumGiftExtensionEndsAt && shiftMs > 0
				? new Date(user.premiumGiftExtensionEndsAt.getTime() + shiftMs)
				: user.premiumGiftExtensionEndsAt;
		const updatedUser = await this.userRepository.patchUpsert(
			userId,
			{
				premium_type: premiumType,
				premium_since: this.resolvePremiumSince(user.premiumSince, premiumSinceAnchor, now),
				premium_until: periodEnd,
				premium_gift_extension_ends_at: adjustedGiftEnd,
				premium_lifetime_sequence: visionarySequence,
				has_ever_purchased: hasEverPurchased,
				premium_will_cancel: false,
				premium_billing_cycle: billingCycle,
				premium_grace_ends_at: null,
			},
			user.toRow(),
		);
		await this.dispatchUser(updatedUser);
		Logger.debug(
			{userId, premiumType, periodEnd, shiftMs, adjustedGiftEnd, billingCycle},
			'Premium set from subscription period',
		);
	}

	async setPremiumLifetime(
		userId: UserID,
		hasEverPurchased: boolean = false,
		premiumSinceAnchor: Date | null = null,
	): Promise<void> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new StripeError('User not found for premium grant');
		}
		const now = new Date();
		const visionarySequence = user.premiumLifetimeSequence ?? (await this.allocateVisionarySequence(userId));
		const updatedUser = await this.userRepository.patchUpsert(
			userId,
			{
				premium_type: UserPremiumTypes.LIFETIME,
				premium_since: this.resolvePremiumSince(user.premiumSince, premiumSinceAnchor, now),
				premium_until: null,
				premium_lifetime_sequence: visionarySequence,
				has_ever_purchased: hasEverPurchased,
				premium_will_cancel: false,
				premium_billing_cycle: null,
				premium_grace_ends_at: null,
			},
			user.toRow(),
		);
		await this.dispatchUser(updatedUser);
		Logger.debug({userId, visionarySequence}, 'Lifetime premium granted');
	}

	async extendPremiumByGift(
		userId: UserID,
		premiumType: 1 | 2,
		durationType: GiftCodeDurationType,
		durationQuantity: number,
		hasEverPurchased: boolean = false,
	): Promise<void> {
		if (premiumType === UserPremiumTypes.LIFETIME) {
			await this.setPremiumLifetime(userId, hasEverPurchased);
			return;
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new StripeError('User not found for premium grant');
		}
		if (durationQuantity <= 0) {
			return;
		}
		const now = new Date();
		const newGiftEnd = this.resolveGiftExtensionEnd(user, now, durationType, durationQuantity);
		const patch: Partial<UserRow> = {
			premium_gift_extension_ends_at: newGiftEnd,
			premium_grace_ends_at: null,
		};
		if ((user.premiumType ?? 0) <= 0) {
			patch.premium_type = premiumType;
			patch.premium_since = this.resolvePremiumSince(user.premiumSince, null, now);
		}
		if (hasEverPurchased && !user.hasEverPurchased) {
			patch.has_ever_purchased = true;
		}
		const updatedUser = await this.userRepository.patchUpsert(userId, patch, user.toRow());
		await this.dispatchUser(updatedUser);
		Logger.debug({userId, premiumType, durationType, durationQuantity, newGiftEnd}, 'Premium extended by gift');
	}

	async grantPremiumFromGiftWithDuration(
		userId: UserID,
		premiumType: 1 | 2,
		durationType: GiftCodeDurationType,
		durationQuantity: number,
		visionarySequenceNumber: number,
	): Promise<void> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new StripeError('User not found for gift premium grant');
		}
		const now = new Date();
		if (premiumType === UserPremiumTypes.LIFETIME) {
			await this.addToVisionariesGuild(userId);
			const updatedUser = await this.userRepository.patchUpsert(
				userId,
				{
					premium_type: premiumType,
					premium_since: user.premiumSince || now,
					premium_until: null,
					premium_lifetime_sequence: visionarySequenceNumber,
					premium_will_cancel: false,
					premium_grace_ends_at: null,
				},
				user.toRow(),
			);
			await this.dispatchUser(updatedUser);
			Logger.debug({userId, lifetimeSequence: visionarySequenceNumber}, 'Lifetime premium granted from gift');
			return;
		}
		if (durationQuantity <= 0) {
			return;
		}
		const newGiftEnd = this.resolveGiftExtensionEnd(user, now, durationType, durationQuantity);
		const updatedUser = await this.userRepository.patchUpsert(
			userId,
			{
				premium_type: (user.premiumType ?? 0) > 0 ? user.premiumType : premiumType,
				premium_since: user.premiumSince || now,
				premium_gift_extension_ends_at: newGiftEnd,
				premium_lifetime_sequence: user.premiumLifetimeSequence,
				premium_will_cancel: false,
				premium_grace_ends_at: null,
			},
			user.toRow(),
		);
		await this.dispatchUser(updatedUser);
		Logger.debug(
			{userId, premiumType, durationType, durationQuantity, newGiftEnd},
			'Premium extended by gift redemption',
		);
	}

	private async allocateVisionarySequence(userId: UserID): Promise<number> {
		const allSlots = await this.userRepository.listVisionarySlots();
		const myReservedSlot = allSlots
			.slice()
			.sort((a, b) => a.slotIndex - b.slotIndex)
			.find((slot) => slot.userId === userId);
		if (myReservedSlot) {
			await this.addToVisionariesGuild(userId);
			return myReservedSlot.slotIndex;
		}
		const unreservedSlot = allSlots
			.slice()
			.sort((a, b) => a.slotIndex - b.slotIndex)
			.find((slot) => !slot.isReserved());
		let newSequence: number;
		if (!unreservedSlot) {
			const maxSlotIndex = allSlots.length > 0 ? Math.max(...allSlots.map((s) => s.slotIndex)) : -1;
			newSequence = maxSlotIndex + 1;
			await this.userRepository.expandVisionarySlots(1);
			await this.userRepository.reserveVisionarySlot(newSequence, userId);
			Logger.warn(
				{userId, newSlotIndex: newSequence, totalSlots: allSlots.length + 1},
				'Auto-expanded visionary slots due to payment completion',
			);
		} else {
			newSequence = unreservedSlot.slotIndex;
			await this.userRepository.reserveVisionarySlot(unreservedSlot.slotIndex, userId);
		}
		await this.addToVisionariesGuild(userId);
		return newSequence;
	}

	async endGracePeriod(userId: UserID): Promise<boolean> {
		const user = await this.userRepository.findUniqueAssert(userId);
		if (user.premiumGraceEndsAt == null) {
			return false;
		}
		if (user.premiumType === UserPremiumTypes.LIFETIME) {
			return false;
		}
		const updatedUser = await this.userRepository.patchUpsert(userId, createPremiumClearPatch(), user.toRow());
		await this.dispatchUser(updatedUser);
		Logger.debug({userId}, 'Premium grace period ended early');
		return true;
	}

	async revokePremium(userId: UserID): Promise<void> {
		const user = await this.userRepository.findUniqueAssert(userId);
		const updatedUser = await this.userRepository.patchUpsert(
			userId,
			{
				premium_type: UserPremiumTypes.NONE,
				premium_until: null,
				premium_gift_extension_ends_at: null,
			},
			user.toRow(),
		);
		await this.dispatchUser(updatedUser);
	}

	async rejoinVisionariesGuild(userId: UserID): Promise<void> {
		await this.assertHasVisionaryCommunityAccess(userId);
		await this.addToVisionariesGuild(userId);
	}

	private async assertHasVisionaryCommunityAccess(userId: UserID): Promise<void> {
		const user = await this.userRepository.findUniqueAssert(userId);
		if (user.premiumType !== UserPremiumTypes.LIFETIME) {
			throw new MissingAccessError();
		}
	}

	private resolvePremiumSince(currentPremiumSince: Date | null, premiumSinceAnchor: Date | null, fallback: Date): Date {
		if (premiumSinceAnchor && (!currentPremiumSince || currentPremiumSince > premiumSinceAnchor)) {
			return premiumSinceAnchor;
		}
		return currentPremiumSince ?? fallback;
	}

	private resolveGiftExtensionEnd(
		user: User,
		now: Date,
		durationType: GiftCodeDurationType,
		durationQuantity: number,
	): Date | null {
		const anchorMs = Math.max(
			now.getTime(),
			user.premiumUntil?.getTime() ?? 0,
			user.premiumGiftExtensionEndsAt?.getTime() ?? 0,
		);
		return addGiftCodeDuration(new Date(anchorMs), durationType, durationQuantity);
	}

	private async addToVisionariesGuild(userId: UserID): Promise<void> {
		if (!Config.instance.visionariesGuildId) {
			throw new StripeError('Visionaries guild id not configured');
		}
		if (!Config.instance.visionariesGuildVisionaryRoleId) {
			throw new StripeError('Visionaries guild visionary role id not configured');
		}
		const visionariesGuildId = createGuildID(BigInt(Config.instance.visionariesGuildId));
		const visionaryRoleId = createRoleID(BigInt(Config.instance.visionariesGuildVisionaryRoleId));
		const requestCache = createRequestCache();
		const existingMember = await this.guildRepository.getMember(visionariesGuildId, userId);
		if (!existingMember) {
			await this.guildService.members.addUserToGuild({
				userId,
				guildId: visionariesGuildId,
				sendJoinMessage: true,
				skipBanCheck: true,
				requestCache,
			});
			Logger.debug({userId, guildId: visionariesGuildId}, 'Added visionary user to visionaries guild');
		}

		try {
			await this.guildService.members.systemAddMemberRole({
				targetId: userId,
				guildId: visionariesGuildId,
				roleId: visionaryRoleId,
				initiatorId: SYSTEM_USER_ID,
				requestCache,
			});
		} catch (error) {
			Logger.error(
				{userId, guildId: visionariesGuildId, roleId: visionaryRoleId, error},
				'Failed to add visionary role to a rejoining visionary.',
			);
		}
	}

	private async dispatchUser(user: User): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(user),
		});
	}
}
