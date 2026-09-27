// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import type {AltchaChallenge} from '@app/features/auth/altcha/AltchaSolver';
import {AltchaVerification} from '@app/features/auth/components/AltchaVerification';
import styles from '@app/features/auth/components/modals/CaptchaModal.module.css';
import {TurnstileWidget} from '@app/features/auth/components/TurnstileWidget';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import HCaptcha from '@hcaptcha/react-hcaptcha';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';

const VERIFY_YOU_RE_HUMAN_DESCRIPTOR = msg({
	message: "Verify you're human",
	comment: 'Short label in the authentication captcha modal. Keep the tone plain and specific.',
});
const logger = new Logger('CaptchaModal');

export type CaptchaType = 'turnstile' | 'hcaptcha' | 'altcha';

interface HCaptchaComponentProps {
	sitekey: string;
	onVerify?: (token: string) => void;
	onExpire?: () => void;
	onError?: (error: string) => void;
	theme?: 'light' | 'dark';
	ref?: React.Ref<HCaptcha>;
}

const HCaptchaComponent = HCaptcha as React.ComponentType<HCaptchaComponentProps>;

interface CaptchaModalProps {
	onVerify: (token: string, captchaType: CaptchaType) => void;
	onCancel?: () => void;
	preferredType?: CaptchaType;
	altchaChallenge?: AltchaChallenge | null;
	error?: string | null;
	isVerifying?: boolean;
	closeOnVerify?: boolean;
}

export const CaptchaModal = observer(
	({
		onVerify,
		onCancel,
		preferredType,
		altchaChallenge,
		error,
		isVerifying,
		closeOnVerify = true,
	}: CaptchaModalProps) => {
		const {i18n} = useLingui();
		const hcaptchaRef = useRef<HCaptcha>(null);
		const [captchaType, setCaptchaType] = useState<CaptchaType>(() => {
			if (altchaChallenge) return 'altcha';
			if (preferredType) return preferredType;
			if (RuntimeConfig.captchaProvider === 'turnstile' && RuntimeConfig.turnstileSiteKey) {
				return 'turnstile';
			}
			if (RuntimeConfig.captchaProvider === 'hcaptcha' && RuntimeConfig.hcaptchaSiteKey) {
				return 'hcaptcha';
			}
			return RuntimeConfig.turnstileSiteKey ? 'turnstile' : 'hcaptcha';
		});
		useEffect(() => {
			if (captchaType === 'hcaptcha') {
				const timer = setTimeout(() => {
					hcaptchaRef.current?.resetCaptcha();
				}, 100);
				return () => clearTimeout(timer);
			}
			return;
		}, [captchaType]);
		useEffect(() => {
			if (error) {
				if (captchaType === 'hcaptcha') {
					hcaptchaRef.current?.resetCaptcha();
				}
			}
		}, [error, captchaType]);
		const handleVerify = useCallback(
			(token: string) => {
				onVerify(token, captchaType);
				if (closeOnVerify) {
					ModalCommands.pop();
				}
			},
			[onVerify, captchaType, closeOnVerify],
		);
		const handleCancel = useCallback(() => {
			onCancel?.();
			ModalCommands.pop();
		}, [onCancel]);
		const handleExpire = useCallback(() => {
			if (captchaType === 'hcaptcha') {
				hcaptchaRef.current?.resetCaptcha();
			}
		}, [captchaType]);
		const handleError = useCallback(
			(error: string) => {
				logger.error(`${captchaType} error:`, error);
			},
			[captchaType],
		);
		const handleSwitchToHCaptcha = useCallback(() => {
			setCaptchaType('hcaptcha');
		}, []);
		const handleSwitchToTurnstile = useCallback(() => {
			setCaptchaType('turnstile');
		}, []);
		const showSwitchButton =
			(captchaType === 'turnstile' && RuntimeConfig.hcaptchaSiteKey) ||
			(captchaType === 'hcaptcha' && RuntimeConfig.turnstileSiteKey);
		return (
			<Modal.Root size="small" centered onClose={handleCancel} data-flx="auth.captcha-modal.modal-root">
				<Modal.Header
					title={i18n._(VERIFY_YOU_RE_HUMAN_DESCRIPTOR)}
					onClose={handleCancel}
					data-flx="auth.captcha-modal.modal-header"
				/>
				<Modal.Content data-flx="auth.captcha-modal.modal-content">
					<Modal.ContentLayout className={styles.container} data-flx="auth.captcha-modal.container">
						<Modal.Description data-flx="auth.captcha-modal.description">
							<Trans>We need to make sure you're not a bot. Complete the verification below.</Trans>
						</Modal.Description>
						{error && (
							<div className={styles.errorBox} data-flx="auth.captcha-modal.error-box">
								<p className={styles.errorText} data-flx="auth.captcha-modal.error-text">
									{error}
								</p>
							</div>
						)}
						<div className={styles.captchaContainer} data-flx="auth.captcha-modal.captcha-container">
							{captchaType === 'altcha' && altchaChallenge ? (
								<AltchaVerification
									challenge={altchaChallenge}
									onVerify={handleVerify}
									data-flx="auth.captcha-modal.altcha-verification"
								/>
							) : captchaType === 'turnstile' ? (
								<TurnstileWidget
									sitekey={RuntimeConfig.turnstileSiteKey ?? ''}
									onVerify={handleVerify}
									onExpire={handleExpire}
									onError={handleError}
									theme="dark"
									data-flx="auth.captcha-modal.turnstile-widget"
								/>
							) : (
								<HCaptchaComponent
									ref={hcaptchaRef}
									sitekey={RuntimeConfig.hcaptchaSiteKey ?? ''}
									onVerify={handleVerify}
									onExpire={handleExpire}
									onError={handleError}
									theme="dark"
									data-flx="auth.captcha-modal.h-captcha-component"
								/>
							)}
						</div>
						{showSwitchButton && (
							<div className={styles.switchContainer} data-flx="auth.captcha-modal.switch-container">
								<button
									type="button"
									onClick={captchaType === 'turnstile' ? handleSwitchToHCaptcha : handleSwitchToTurnstile}
									className={styles.switchButton}
									disabled={isVerifying}
									data-flx="auth.captcha-modal.switch-button.switch-to-h-captcha"
								>
									{captchaType === 'turnstile' ? (
										<Trans>Having issues? Try hCaptcha instead</Trans>
									) : (
										<Trans>Try Turnstile instead</Trans>
									)}
								</button>
							</div>
						)}
					</Modal.ContentLayout>
				</Modal.Content>
			</Modal.Root>
		);
	},
);
