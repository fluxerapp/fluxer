// SPDX-License-Identifier: AGPL-3.0-or-later

import {type AltchaChallenge, readAltchaChallenge, solveAltchaChallenge} from '@app/features/auth/altcha/AltchaSolver';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import type {RestResponse} from '@app/features/platform/types/TransportTypes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {replyCode} from '@app/features/platform/utils/ResponseInspection';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Headers} from '@fluxer/constants/src/Headers';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const STILL_WORKING_DESCRIPTOR = msg({
	message: 'Still working on it…',
	comment:
		'Small toast shown only when an automatic background check before sign-in, sign-up or a similar request takes more than a couple of seconds. No user action needed. Keep it short and calm.',
});

const logger = new Logger('CaptchaInterceptor');

const MAX_SOLVES = 2;
const SLOW_SOLVE_HINT_DELAY_MS = 2000;
const SLOW_SOLVE_HINT_TIMEOUT_MS = 60_000;

function readCaptchaChallenge(status: number, body: unknown): AltchaChallenge | null {
	if (status !== 400) return null;
	const code = replyCode(body);
	if (code !== 'CAPTCHA_REQUIRED' && code !== 'INVALID_CAPTCHA') return null;
	return readAltchaChallenge(body);
}

class CaptchaInterceptor {
	private i18n: I18n | null = null;

	constructor() {
		http.installHooks({
			intercept: this.intercept.bind(this),
		});
	}

	setI18n(i18n: I18n) {
		this.i18n = i18n;
	}

	private intercept(
		reply: RestResponse,
		retry: (extraHeaders: Record<string, string>) => Promise<RestResponse>,
	): Promise<RestResponse | undefined> | undefined {
		const challenge = readCaptchaChallenge(reply.status, reply.body);
		if (!challenge) return undefined;
		return this.solveAndRetry(challenge, retry);
	}

	private async solveAndRetry(
		initialChallenge: AltchaChallenge,
		retry: (extraHeaders: Record<string, string>) => Promise<RestResponse>,
	): Promise<RestResponse | undefined> {
		let challenge = initialChallenge;
		let failed: () => RestResponse | undefined = () => undefined;
		for (let solves = 1; ; solves++) {
			const token = await this.solveWithHint(challenge);
			if (!token) return failed();
			try {
				const next = await retry({[Headers.X_CAPTCHA_TOKEN]: token});
				const again = solves < MAX_SOLVES ? readCaptchaChallenge(next.status, next.body) : null;
				if (!again) return next;
				challenge = again;
				failed = () => next;
			} catch (error) {
				const again =
					solves < MAX_SOLVES && error instanceof HttpError ? readCaptchaChallenge(error.status, error.body) : null;
				if (!again) throw error;
				challenge = again;
				failed = () => {
					throw error;
				};
			}
		}
	}

	private async solveWithHint(challenge: AltchaChallenge): Promise<string | null> {
		let toastId: string | null = null;
		const timer = setTimeout(() => {
			toastId = ToastCommands.createToast({
				type: 'info',
				children: this.i18n?._(STILL_WORKING_DESCRIPTOR),
				timeout: SLOW_SOLVE_HINT_TIMEOUT_MS,
			});
		}, SLOW_SOLVE_HINT_DELAY_MS);
		try {
			return await solveAltchaChallenge(challenge);
		} catch (error) {
			logger.error('ALTCHA solve failed:', error);
			return null;
		} finally {
			clearTimeout(timer);
			if (toastId !== null) ToastCommands.destroyToast(toastId);
		}
	}
}

export default new CaptchaInterceptor();
