// SPDX-License-Identifier: AGPL-3.0-or-later

import {type AltchaChallenge, readAltchaChallenge} from '@app/features/auth/altcha/AltchaSolver';
import {CaptchaModal, type CaptchaType} from '@app/features/auth/components/modals/CaptchaModal';
import {http} from '@app/features/platform/transport/RestTransport';
import type {RestResponse} from '@app/features/platform/types/TransportTypes';
import {replyCode, replyMessage} from '@app/features/platform/utils/ResponseInspection';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {action, makeObservable, observable} from 'mobx';
import {observer} from 'mobx-react-lite';

const CAPTCHA_VERIFICATION_FAILED_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Captcha verification failed. Try again.',
	comment: 'Toast error shown when captcha verification fails inside the auth captcha interceptor.',
});

export interface CaptchaResult {
	token: string;
	type: CaptchaType;
}

class CaptchaState {
	error: string | null = null;
	isVerifying = false;

	constructor() {
		makeObservable(this, {
			error: observable,
			isVerifying: observable,
			setError: action,
			setIsVerifying: action,
			reset: action,
		});
	}

	setError(error: string | null) {
		this.error = error;
	}

	setIsVerifying(isVerifying: boolean) {
		this.isVerifying = isVerifying;
	}

	reset() {
		this.error = null;
		this.isVerifying = false;
	}
}

class CaptchaInterceptorState {
	private state = new CaptchaState();
	private pendingPromise: {resolve: (result: CaptchaResult) => void; reject: (error: Error) => void} | null = null;
	private i18n: I18n | null = null;

	setI18n(i18n: I18n) {
		this.i18n = i18n;
	}

	constructor() {
		http.installHooks({
			intercept: this.intercept.bind(this),
		});
	}

	private isCaptchaError(reply: RestResponse): boolean {
		const code = replyCode(reply.body);
		return code === 'CAPTCHA_REQUIRED' || code === 'INVALID_CAPTCHA';
	}

	private showCaptchaModal(altchaChallenge: AltchaChallenge | null): Promise<CaptchaResult> {
		if (this.pendingPromise) {
			this.pendingPromise.reject(new Error('Captcha cancelled'));
			this.pendingPromise = null;
		}
		this.state.reset();
		return new Promise((resolve, reject) => {
			this.pendingPromise = {resolve, reject};
			const handleVerify = (token: string, captchaType: CaptchaType) => {
				const result = {token, type: captchaType};
				this.state.setIsVerifying(true);
				if (this.pendingPromise) {
					this.pendingPromise.resolve(result);
					this.pendingPromise = null;
				}
			};
			const handleCancel = () => {
				this.state.reset();
				if (this.pendingPromise) {
					this.pendingPromise.reject(new Error('Captcha cancelled'));
					this.pendingPromise = null;
				}
				ModalCommands.pop();
			};
			const CaptchaModalWrapper = observer(() => (
				<CaptchaModal
					altchaChallenge={altchaChallenge}
					onVerify={handleVerify}
					onCancel={handleCancel}
					error={this.state.error}
					isVerifying={this.state.isVerifying}
					closeOnVerify={false}
					data-flx="auth.captcha-interceptor.captcha-modal-wrapper.captcha-modal"
				/>
			));
			ModalCommands.push(
				modal(() => <CaptchaModalWrapper data-flx="auth.captcha-interceptor.captcha-modal-wrapper" />),
			);
		});
	}

	private intercept(
		reply: RestResponse,
		retry: (extraHeaders: Record<string, string>) => Promise<RestResponse>,
		reject: (error: Error) => void,
	): boolean | Promise<RestResponse> | undefined {
		if (reply.status === 400 && this.isCaptchaError(reply)) {
			const i18n = this.i18n!;
			const errorMessage = replyMessage(reply.body) || i18n._(CAPTCHA_VERIFICATION_FAILED_PLEASE_TRY_AGAIN_DESCRIPTOR);
			this.state.setError(errorMessage);
			this.state.setIsVerifying(false);
			const promise = this.showCaptchaModal(readAltchaChallenge(reply.body))
				.then((captchaResult) => {
					this.state.setError(null);
					this.state.setIsVerifying(false);
					ModalCommands.pop();
					return retry({
						'X-Captcha-Token': captchaResult.token,
						'X-Captcha-Type': captchaResult.type,
					});
				})
				.catch((error) => {
					this.state.reset();
					ModalCommands.pop();
					reject(error);
					throw error;
				});
			return promise;
		}
		return undefined;
	}
}

export default new CaptchaInterceptorState();
