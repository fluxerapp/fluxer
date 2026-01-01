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

interface TurnstileVerifyResponse {
	success: boolean;
	challenge_ts?: string;
	hostname?: string;
	'error-codes'?: Array<string>;
	action?: string;
	cdata?: string;
}

export class TurnstileService implements ICaptchaService {
	private readonly secretKey: string;
	private readonly verifyUrl = 'https://challenges.cloudflare.com/turnstile/v0/siteverify';

	constructor() {
		if (!Config.captcha.turnstile?.secretKey) {
			throw new Error('TURNSTILE_SECRET_KEY is required when using Turnstile captcha');
		}
		this.secretKey = Config.captcha.turnstile.secretKey;
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
				Logger.error({status: response.status}, 'Turnstile verify request failed');
				return false;
			}

			const data = (await response.json()) as TurnstileVerifyResponse;

			if (!data.success) {
				Logger.warn({errorCodes: data['error-codes']}, 'Turnstile verification failed');
				return false;
			}

			Logger.debug({hostname: data.hostname}, 'Turnstile verification successful');
			return true;
		} catch (error) {
			Logger.error({error}, 'Error verifying Turnstile token');
			return false;
		}
	}
}
