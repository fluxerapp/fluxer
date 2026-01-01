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

import {createMiddleware} from 'hono/factory';
import type {HonoEnv} from '~/App';
import {Config} from '~/Config';
import {CaptchaVerificationRequiredError, InvalidCaptchaError} from '~/Errors';
import {CaptchaService} from '~/infrastructure/CaptchaService';
import type {ICaptchaService} from '~/infrastructure/ICaptchaService';
import {TestCaptchaService} from '~/infrastructure/TestCaptchaService';
import {TurnstileService} from '~/infrastructure/TurnstileService';
import {extractClientIp} from '~/utils/IpUtils';

const useTestCaptcha = Config.dev.testModeEnabled;
const testCaptchaService = new TestCaptchaService();
let hcaptchaService: ICaptchaService | null = null;
let turnstileService: ICaptchaService | null = null;

if (!useTestCaptcha && Config.captcha.enabled) {
	if (Config.captcha.hcaptcha?.secretKey) {
		hcaptchaService = new CaptchaService();
	}
	if (Config.captcha.turnstile?.secretKey) {
		turnstileService = new TurnstileService();
	}
	if (!hcaptchaService && !turnstileService) {
		throw new Error(
			'CAPTCHA_ENABLED=true but no captcha service has been configured. ' +
				'Please supply HCAPTCHA_SECRET_KEY or TURNSTILE_SECRET_KEY (or disable captcha).',
		);
	}
}

export const CaptchaMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	if (!Config.captcha.enabled) {
		await next();
		return;
	}

	const token = ctx.req.header('x-captcha-token');
	const captchaType = ctx.req.header('x-captcha-type');

	if (!token) {
		throw new CaptchaVerificationRequiredError();
	}

	let captchaService: ICaptchaService;
	if (useTestCaptcha) {
		captchaService = testCaptchaService;
	} else if (captchaType === 'turnstile' && turnstileService) {
		captchaService = turnstileService;
	} else if (captchaType === 'hcaptcha' && hcaptchaService) {
		captchaService = hcaptchaService;
	} else {
		if (Config.captcha.provider === 'turnstile' && turnstileService) {
			captchaService = turnstileService;
		} else if (Config.captcha.provider === 'hcaptcha' && hcaptchaService) {
			captchaService = hcaptchaService;
		} else {
			const fallbackService = turnstileService || hcaptchaService;
			if (!fallbackService) {
				throw new Error(
					`Captcha service not available (provider=${Config.captcha.provider}, ` +
						`turnstile=${Boolean(turnstileService)}, hcaptcha=${Boolean(hcaptchaService)})`,
				);
			}
			captchaService = fallbackService;
		}
	}

	const isValid = await captchaService.verify({
		token,
		remoteIp: extractClientIp(ctx.req.raw) ?? undefined,
	});

	if (!isValid) {
		throw new InvalidCaptchaError();
	}

	await next();
});
