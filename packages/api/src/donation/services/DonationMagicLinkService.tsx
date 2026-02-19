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

import {randomBytes} from 'node:crypto';
import {Config} from '@fluxer/api/src/Config';
import type {IDonationRepository} from '@fluxer/api/src/donation/IDonationRepository';
import {DonorMagicLinkToken} from '@fluxer/api/src/donation/models/DonorMagicLinkToken';
import type {IEmailDnsValidationService} from '@fluxer/api/src/infrastructure/IEmailDnsValidationService';
import {Logger} from '@fluxer/api/src/Logger';
import {DONATION_MAGIC_LINK_EXPIRY_MS} from '@fluxer/constants/src/DonationConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {IEmailService} from '@fluxer/email/src/IEmailService';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {DonationMagicLinkExpiredError} from '@fluxer/errors/src/domains/donation/DonationMagicLinkExpiredError';
import {DonationMagicLinkInvalidError} from '@fluxer/errors/src/domains/donation/DonationMagicLinkInvalidError';
import {DonationMagicLinkUsedError} from '@fluxer/errors/src/domains/donation/DonationMagicLinkUsedError';

export class DonationMagicLinkService {
	constructor(
		private donationRepository: IDonationRepository,
		private emailService: IEmailService,
		private emailDnsValidationService: IEmailDnsValidationService,
	) {}

	async sendMagicLink(email: string): Promise<void> {
		const hasValidDns = await this.emailDnsValidationService.hasValidDnsRecords(email);
		if (!hasValidDns) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
		}

		const donor = await this.donationRepository.findDonorByEmail(email);
		if (!donor) {
			Logger.info({email}, 'Donation magic link requested for unknown donor');
			return;
		}

		await this.donationRepository.invalidateTokensForEmail(email);

		const token = randomBytes(32).toString('hex');
		const expiresAt = new Date(Date.now() + DONATION_MAGIC_LINK_EXPIRY_MS);

		const tokenModel = new DonorMagicLinkToken({
			token_: token,
			donor_email: email,
			expires_at: expiresAt,
			used_at: null,
		});
		await this.donationRepository.createMagicLinkToken(tokenModel);

		const manageUrl = `${Config.endpoints.apiPublic}/donations/manage?token=${token}`;
		await this.emailService.sendDonationMagicLink(email, token, manageUrl, expiresAt, null);

		Logger.debug({email}, 'Donation magic link sent');
	}

	async validateToken(token: string): Promise<{email: string; stripeCustomerId: string | null}> {
		const tokenModel = await this.donationRepository.findMagicLinkToken(token);

		if (!tokenModel) {
			throw new DonationMagicLinkInvalidError();
		}

		if (tokenModel.isExpired()) {
			throw new DonationMagicLinkExpiredError();
		}

		if (tokenModel.isUsed()) {
			throw new DonationMagicLinkUsedError();
		}

		await this.donationRepository.markMagicLinkTokenUsed(token, new Date());

		const donor = await this.donationRepository.findDonorByEmail(tokenModel.donorEmail);

		Logger.debug({email: tokenModel.donorEmail}, 'Donation magic link validated');

		return {
			email: tokenModel.donorEmail,
			stripeCustomerId: donor?.stripeCustomerId ?? null,
		};
	}
}
