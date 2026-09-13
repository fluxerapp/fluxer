// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import {Config} from '@app/api/Config';
import type {IDonationRepository} from '@app/api/donation/IDonationRepository';
import {DonorMagicLinkToken} from '@app/api/donation/models/DonorMagicLinkToken';
import type {IEmailDnsValidationService} from '@app/api/infrastructure/IEmailDnsValidationService';
import {Logger} from '@app/api/Logger';
import {DONATION_MAGIC_LINK_EXPIRY_MS} from '@fluxer/constants/src/DonationConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {DonationMagicLinkExpiredError} from '@fluxer/errors/src/domains/donation/DonationMagicLinkExpiredError';
import {DonationMagicLinkInvalidError} from '@fluxer/errors/src/domains/donation/DonationMagicLinkInvalidError';
import {DonationMagicLinkUsedError} from '@fluxer/errors/src/domains/donation/DonationMagicLinkUsedError';
import type {IEmailService} from '@pkgs/email/src/IEmailService';

export class DonationMagicLinkService {
	constructor(
		private donationRepository: IDonationRepository,
		private emailService: IEmailService,
		private emailDnsValidationService: IEmailDnsValidationService,
	) {}

	async sendMagicLink(email: string, locale: string | null = null): Promise<void> {
		const hasValidDns = await this.emailDnsValidationService.hasValidDnsRecords(email);
		if (!hasValidDns) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_DOMAIN_CANNOT_RECEIVE_MAIL);
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
		await this.emailService.sendDonationMagicLink(email, token, manageUrl, expiresAt, locale);
		Logger.debug({email}, 'Donation magic link sent');
	}

	async validateToken(token: string): Promise<{
		email: string;
		stripeCustomerId: string | null;
	}> {
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
