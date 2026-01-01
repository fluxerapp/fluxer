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

import {Config} from '~/Config';
import {FLUXER_USER_AGENT} from '~/Constants';
import type {ICaptchaService, VerifyCaptchaParams} from '~/infrastructure/ICaptchaService';
import {Logger} from '~/Logger';

interface HCaptchaVerifyResponse {
	success: boolean;
	challenge_ts?: string;
	hostname?: string;
	credit?: boolean;
	'error-codes'?: Array<string>;
	score?: number;
	score_reason?: Array<string>;
}

export class CaptchaService implements ICaptchaService {
	private readonly secretKey: string;
	private readonly verifyUrl = 'https://api.hcaptcha.com/siteverify';

	constructor() {
		if (!Config.captcha.hcaptcha?.secretKey) {
			throw new Error('HCAPTCHA_SECRET_KEY is required when CAPTCHA_ENABLED is true');
		}
		this.secretKey = Config.captcha.hcaptcha.secretKey;
	}

	async verify({token, remoteIp}: VerifyCaptchaParams): Promise<boolean> {
		try {
			const params = new URLSearchParams();
			params.append('secret', this.secretKey);
			params.append('response', token);
			if (remoteIp) {
				params.append('remoteip', remoteIp);
			}

			const response = await fetch(this.verifyUrl, {
				method: 'POST',
				headers: {
					'Content-Type': 'application/x-www-form-urlencoded',
					'User-Agent': FLUXER_USER_AGENT,
				},
				body: params.toString(),
			});

			if (!response.ok) {
				Logger.error({status: response.status}, 'hCaptcha verify request failed');
				return false;
			}

			const data = (await response.json()) as HCaptchaVerifyResponse;

			if (!data.success) {
				Logger.warn({errorCodes: data['error-codes']}, 'hCaptcha verification failed');
				return false;
			}

			Logger.debug({hostname: data.hostname}, 'hCaptcha verification successful');
			return true;
		} catch (error) {
			Logger.error({error}, 'Error verifying hCaptcha token');
			return false;
		}
	}
}
