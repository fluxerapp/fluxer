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

import {createGuildID, type UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {UserPremiumTypes} from '~/Constants';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {GuildService} from '~/guild/services/GuildService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import {Logger} from '~/Logger';
import type {User} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import {mapUserToPrivateResponse} from '~/user/UserModel';
import {addMonthsClamp} from '../StripeUtils';

export class StripePremiumService {
	constructor(
		private userRepository: IUserRepository,
		private gatewayService: IGatewayService,
		private guildRepository: IGuildRepository,
		private guildService: GuildService,
	) {}

	async grantPremium(
		userId: UserID,
		premiumType: 1 | 2,
		durationMonths: number,
		billingCycle: string | null = null,
		hasEverPurchased: boolean = false,
	): Promise<void> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			return;
		}

		const now = new Date();
		let premiumUntil: Date | null = null;
		let visionarySequence: number | null = user.premiumLifetimeSequence;

		if (durationMonths > 0) {
			const currentPremiumUntil = user.premiumUntil && user.premiumUntil > now ? user.premiumUntil : now;
			premiumUntil = addMonthsClamp(currentPremiumUntil, durationMonths);
		}

		if (premiumType === UserPremiumTypes.LIFETIME && !visionarySequence) {
			const allSlots = await this.userRepository.listVisionarySlots();

			const myReservedSlot = allSlots
				.slice()
				.sort((a, b) => a.slotIndex - b.slotIndex)
				.find((slot) => slot.userId === userId);

			if (myReservedSlot) {
				visionarySequence = myReservedSlot.slotIndex;
			} else {
				const unreservedSlot = allSlots
					.slice()
					.sort((a, b) => a.slotIndex - b.slotIndex)
					.find((slot) => !slot.isReserved());

				if (!unreservedSlot) {
					const maxSlotIndex = allSlots.length > 0 ? Math.max(...allSlots.map((s) => s.slotIndex)) : -1;
					const newSlotIndex = maxSlotIndex + 1;

					await this.userRepository.expandVisionarySlots(1);
					visionarySequence = newSlotIndex;
					await this.userRepository.reserveVisionarySlot(newSlotIndex, userId);

					Logger.warn(
						{userId, newSlotIndex, totalSlots: allSlots.length + 1},
						'Auto-expanded visionary slots due to payment completion',
					);
				} else {
					visionarySequence = unreservedSlot.slotIndex;
					await this.userRepository.reserveVisionarySlot(unreservedSlot.slotIndex, userId);
				}
			}

			await this.addToVisionariesGuild(userId);
		}

		const updatedUser = await this.userRepository.patchUpsert(userId, {
			premium_type: premiumType,
			premium_since: user.premiumSince || now,
			premium_until: premiumUntil,
			premium_lifetime_sequence: visionarySequence,
			has_ever_purchased: hasEverPurchased,
			premium_will_cancel: false,
			premium_billing_cycle: billingCycle,
		});

		if (updatedUser) {
			await this.dispatchUser(updatedUser);
		}

		Logger.debug({userId, premiumType, durationMonths, visionarySequence, billingCycle}, 'Premium granted to user');
	}

	async grantPremiumFromGift(
		userId: UserID,
		premiumType: 1 | 2,
		durationMonths: number,
		visionarySequenceNumber: number,
	): Promise<void> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			return;
		}

		const now = new Date();
		let premiumUntil: Date | null = null;

		if (durationMonths > 0) {
			const currentPremiumUntil = user.premiumUntil && user.premiumUntil > now ? user.premiumUntil : now;
			premiumUntil = addMonthsClamp(currentPremiumUntil, durationMonths);
		}

		if (premiumType === UserPremiumTypes.LIFETIME) {
			await this.addToVisionariesGuild(userId);
		}

		const updatedUser = await this.userRepository.patchUpsert(userId, {
			premium_type: premiumType,
			premium_since: user.premiumSince || now,
			premium_until: premiumUntil,
			premium_lifetime_sequence:
				premiumType === UserPremiumTypes.LIFETIME ? visionarySequenceNumber : user.premiumLifetimeSequence,
			premium_will_cancel: false,
		});

		if (updatedUser) {
			await this.dispatchUser(updatedUser);
		}

		Logger.debug(
			{userId, premiumType, durationMonths, lifetimeSequence: visionarySequenceNumber},
			'Premium granted to user from gift',
		);
	}

	async revokePremium(userId: UserID): Promise<void> {
		const updatedUser = await this.userRepository.patchUpsert(userId, {
			premium_type: UserPremiumTypes.NONE,
			premium_until: null,
		});

		if (updatedUser) {
			await this.dispatchUser(updatedUser);
		}
	}

	async getVisionarySlots(): Promise<{total: number; remaining: number}> {
		const allSlots = await this.userRepository.listVisionarySlots();
		const usedSlots = allSlots.filter((s) => s.isReserved());
		return {total: allSlots.length, remaining: allSlots.length - usedSlots.length};
	}

	async rejoinVisionariesGuild(userId: UserID): Promise<void> {
		await this.addToVisionariesGuild(userId);
	}

	async rejoinOperatorsGuild(userId: UserID): Promise<void> {
		await this.addToOperatorsGuild(userId);
	}

	private async addToVisionariesGuild(userId: UserID): Promise<void> {
		if (!Config.instance.visionariesGuildId) {
			return;
		}

		try {
			const visionariesGuildId = createGuildID(BigInt(Config.instance.visionariesGuildId));
			const existingMember = await this.guildRepository.getMember(visionariesGuildId, userId);

			if (!existingMember) {
				await this.guildService.addUserToGuild({
					userId,
					guildId: visionariesGuildId,
					sendJoinMessage: true,
					skipBanCheck: true,
					requestCache: {
						userPartials: new Map(),
						clear: () => {},
					},
				});
				Logger.debug({userId, guildId: visionariesGuildId}, 'Added visionary user to visionaries guild');
			}
		} catch (error) {
			Logger.error(
				{error, userId, guildId: Config.instance.visionariesGuildId},
				'Failed to add user to visionaries guild',
			);
		}
	}

	private async addToOperatorsGuild(userId: UserID): Promise<void> {
		if (!Config.instance.operatorsGuildId) {
			return;
		}

		try {
			const operatorsGuildId = createGuildID(BigInt(Config.instance.operatorsGuildId));
			const existingMember = await this.guildRepository.getMember(operatorsGuildId, userId);

			if (!existingMember) {
				await this.guildService.addUserToGuild({
					userId,
					guildId: operatorsGuildId,
					sendJoinMessage: true,
					skipBanCheck: true,
					requestCache: {
						userPartials: new Map(),
						clear: () => {},
					},
				});
				Logger.debug({userId, guildId: operatorsGuildId}, 'Added operator user to operators guild');
			}
		} catch (error) {
			Logger.error({error, userId, guildId: Config.instance.operatorsGuildId}, 'Failed to add user to operators guild');
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
