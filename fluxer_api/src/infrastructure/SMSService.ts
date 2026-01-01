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

import twilio from 'twilio';
import {Config} from '~/Config';
import {InvalidPhoneNumberError} from '~/Errors';
import type {ISMSService} from '~/infrastructure/ISMSService';
import {Logger} from '~/Logger';

const TWILIO_INVALID_PHONE_ERROR_CODE = 21211;

interface TwilioErrorLike {
	code?: number;
	status?: number;
	message?: string;
}

const isInvalidTwilioPhoneError = (error: unknown): error is TwilioErrorLike => {
	if (typeof error !== 'object' || error === null) {
		return false;
	}
	return (error as TwilioErrorLike).code === TWILIO_INVALID_PHONE_ERROR_CODE;
};

export class SMSService implements ISMSService {
	private twilioClient: ReturnType<typeof twilio> | null = null;

	constructor() {
		if (Config.sms.enabled && Config.sms.accountSid && Config.sms.authToken && Config.sms.verifyServiceSid) {
			this.twilioClient = twilio(Config.sms.accountSid, Config.sms.authToken);
		}
	}

	async startVerification(phone: string): Promise<void> {
		if (!Config.sms.enabled || !Config.sms.verifyServiceSid) {
			return;
		}

		if (!this.twilioClient) {
			Logger.error('[SMSService] Twilio client not initialized');
			throw new Error('Twilio Verify service not properly configured');
		}

		try {
			await this.twilioClient.verify.v2
				.services(Config.sms.verifyServiceSid)
				.verifications.create({to: phone, channel: 'sms'});
		} catch (error) {
			if (isInvalidTwilioPhoneError(error)) {
				Logger.warn({error}, `[SMSService] Twilio rejected phone ${phone.slice(0, 6)}*** as invalid`);
				throw new InvalidPhoneNumberError();
			}

			Logger.error({error}, '[SMSService] Failed to start verification via Twilio Verify');
			throw error;
		}
	}

	async checkVerification(phone: string, code: string): Promise<boolean> {
		if (!Config.sms.enabled || !Config.sms.verifyServiceSid) {
			return true;
		}

		if (!this.twilioClient) {
			Logger.error('[SMSService] Twilio client not initialized');
			throw new Error('Twilio Verify service not properly configured');
		}

		try {
			const verificationCheck = await this.twilioClient.verify.v2
				.services(Config.sms.verifyServiceSid)
				.verificationChecks.create({to: phone, code});

			return verificationCheck.status === 'approved';
		} catch (error) {
			Logger.error({error}, '[SMSService] Failed to check verification via Twilio Verify');
			return false;
		}
	}
}
