// SPDX-License-Identifier: AGPL-3.0-or-later

import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {GiftCodeDurationType, GiftCodeRow} from '@app/api/database/types/PaymentTypes';
import {mapGiftCodeDurationToMonths} from '@app/api/models/GiftCode';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import * as RandomUtils from '@app/api/utils/RandomUtils';

const CODE_LENGTH = 32;
const MAX_GENERATION_ATTEMPTS = 100;
const MAX_CODES_PER_REQUEST = 100;
const DURATION_TYPES = new Set<GiftCodeDurationType>(['days', 'weeks', 'months', 'years']);

interface GenerateGiftCodesOptions {
	count: number;
	durationType: GiftCodeDurationType;
	durationQuantity: number;
}

export class AdminCodeGenerationService {
	constructor(private readonly userRepository: IUserRepository) {}

	async generateGiftCodes(options: GenerateGiftCodesOptions): Promise<Array<string>> {
		const {count, durationType, durationQuantity} = this.validateOptions(options);
		const durationMonths = mapGiftCodeDurationToMonths(durationType, durationQuantity);
		const codes: Array<string> = [];
		for (let i = 0; i < count; i += 1) {
			const code = await this.generateUniqueGiftCode();
			const giftCodeRow: GiftCodeRow = {
				code,
				duration_months: durationMonths,
				duration_type: durationType,
				duration_quantity: durationQuantity,
				created_at: new Date(),
				created_by_user_id: SYSTEM_USER_ID,
				redeemed_at: null,
				redeemed_by_user_id: null,
				stripe_payment_intent_id: null,
				visionary_sequence_number: null,
				checkout_session_id: null,
				version: 1,
			};
			await this.userRepository.createGiftCode(giftCodeRow);
			codes.push(code);
		}
		return codes;
	}

	private validateOptions(options: GenerateGiftCodesOptions): GenerateGiftCodesOptions {
		if (!Number.isInteger(options.count) || options.count < 1 || options.count > MAX_CODES_PER_REQUEST) {
			throw new Error(`Gift code count must be between 1 and ${MAX_CODES_PER_REQUEST}`);
		}
		if (!DURATION_TYPES.has(options.durationType)) {
			throw new Error(`Unsupported gift code duration type: ${options.durationType}`);
		}
		if (!Number.isInteger(options.durationQuantity) || options.durationQuantity < 1) {
			throw new Error('Gift code duration quantity must be a positive integer');
		}
		return options;
	}

	private async generateUniqueGiftCode(): Promise<string> {
		for (let attempt = 0; attempt < MAX_GENERATION_ATTEMPTS; attempt++) {
			const candidate = RandomUtils.randomString(CODE_LENGTH);
			const exists = await this.userRepository.findGiftCode(candidate);
			if (!exists) {
				return candidate;
			}
		}
		throw new Error(
			`Failed to generate unique gift code after ${MAX_GENERATION_ATTEMPTS} attempts. ` +
				'This may indicate a high collision rate or database issues.',
		);
	}
}
