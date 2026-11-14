// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {GiftCodeDurationType, GiftCodeRow} from '@app/api/database/types/PaymentTypes';
import {
	createGiftCodeRow,
	type GiftCode,
	generateUniqueGiftCode,
	mapGiftCodeDurationToMonths,
} from '@app/api/models/GiftCode';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {StripeGiftRedemptionInProgressError} from '@fluxer/errors/src/domains/payment/StripeGiftRedemptionInProgressError';
import {UnknownGiftCodeError} from '@fluxer/errors/src/domains/payment/UnknownGiftCodeError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {seconds} from 'itty-time';

const MAX_CODES_PER_REQUEST = 100;
const DURATION_TYPES = new Set<GiftCodeDurationType>(['days', 'weeks', 'months', 'years']);

interface GenerateGiftCodesOptions {
	count: number;
	durationType: GiftCodeDurationType | 'lifetime';
	durationQuantity: number;
	createdByUserId: UserID;
}

export class AdminCodeGenerationService {
	constructor(
		private readonly userRepository: UserRepository,
		private readonly cacheService: ICacheService,
	) {}

	async generateGiftCodes(options: GenerateGiftCodesOptions): Promise<Array<string>> {
		const {count, createdByUserId} = this.validateOptions(options);
		const lifetime = options.durationType === 'lifetime';
		const durationType = options.durationType === 'lifetime' ? 'months' : options.durationType;
		const durationQuantity = lifetime ? 0 : options.durationQuantity;
		const durationMonths = mapGiftCodeDurationToMonths(durationType, durationQuantity);
		const codes: Array<string> = [];
		for (let i = 0; i < count; i += 1) {
			const code = await generateUniqueGiftCode((code) => this.userRepository.findGiftCode(code));
			const giftCodeRow: GiftCodeRow = createGiftCodeRow({
				code: code,
				durationMonths: durationMonths,
				durationType: durationType,
				durationQuantity: durationQuantity,
				createdAt: new Date(),
				createdByUserId: createdByUserId,
			});
			await this.userRepository.createGiftCode(giftCodeRow);
			codes.push(code);
		}
		return codes;
	}

	async revokeGiftCode(code: string): Promise<GiftCode> {
		const lockKey = `gift_redeem_lock:${code}`;
		const lockToken = await this.cacheService.acquireLock(lockKey, seconds('2 minutes'));
		if (!lockToken) {
			throw new StripeGiftRedemptionInProgressError();
		}
		try {
			const giftCode = await this.userRepository.findGiftCode(code);
			if (!giftCode || giftCode.revokedAt) {
				throw new UnknownGiftCodeError();
			}
			if (giftCode.redeemedByUserId) {
				throw InputValidationError.create(
					'code',
					`This gift code was already redeemed by account ${giftCode.redeemedByUserId}. Use Remove premium on that account instead.`,
				);
			}
			if (
				giftCode.stripePaymentIntentId ||
				giftCode.checkoutSessionId ||
				giftCode.visionarySequenceNumber !== null ||
				(!Config.instance.selfHosted && giftCode.createdByUserId !== SYSTEM_USER_ID)
			) {
				throw InputValidationError.create(
					'code',
					'This gift code was bought by a customer. Refund the purchase instead, which revokes the code.',
				);
			}
			await this.userRepository.revokeGiftCode(code);
			return giftCode;
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken);
		}
	}

	private validateOptions(options: GenerateGiftCodesOptions): GenerateGiftCodesOptions {
		if (!Number.isInteger(options.count) || options.count < 1 || options.count > MAX_CODES_PER_REQUEST) {
			throw new Error(`Gift code count must be between 1 and ${MAX_CODES_PER_REQUEST}`);
		}
		if (options.durationType === 'lifetime') {
			return options;
		}
		if (!DURATION_TYPES.has(options.durationType)) {
			throw new Error(`Unsupported gift code duration type: ${options.durationType}`);
		}
		if (!Number.isInteger(options.durationQuantity) || options.durationQuantity < 1) {
			throw new Error('Gift code duration quantity must be a positive integer');
		}
		return options;
	}
}
