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

import HCaptcha from '@hcaptcha/react-hcaptcha';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {TurnstileWidget} from '~/components/captcha/TurnstileWidget';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import styles from './CaptchaModal.module.css';

export type CaptchaType = 'turnstile' | 'hcaptcha';

interface CaptchaModalProps {
	onVerify: (token: string, captchaType: CaptchaType) => void;
	onCancel?: () => void;
	preferredType?: CaptchaType;
	error?: string | null;
	isVerifying?: boolean;
	closeOnVerify?: boolean;
}

export const CaptchaModal = observer(
	({onVerify, onCancel, preferredType, error, isVerifying, closeOnVerify = true}: CaptchaModalProps) => {
		const {t} = useLingui();
		const hcaptchaRef = useRef<HCaptcha>(null);
		const [captchaType, setCaptchaType] = useState<CaptchaType>(() => {
			if (preferredType) return preferredType;

			if (RuntimeConfigStore.captchaProvider === 'turnstile' && RuntimeConfigStore.turnstileSiteKey) {
				return 'turnstile';
			}
			if (RuntimeConfigStore.captchaProvider === 'hcaptcha' && RuntimeConfigStore.hcaptchaSiteKey) {
				return 'hcaptcha';
			}

			return RuntimeConfigStore.turnstileSiteKey ? 'turnstile' : 'hcaptcha';
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
					ModalActionCreators.pop();
				}
			},
			[onVerify, captchaType, closeOnVerify],
		);

		const handleCancel = useCallback(() => {
			onCancel?.();
			ModalActionCreators.pop();
		}, [onCancel]);

		const handleExpire = useCallback(() => {
			if (captchaType === 'hcaptcha') {
				hcaptchaRef.current?.resetCaptcha();
			}
		}, [captchaType]);

		const handleError = useCallback(
			(error: string) => {
				console.error(`${captchaType} error:`, error);
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
			(captchaType === 'turnstile' && RuntimeConfigStore.hcaptchaSiteKey) ||
			(captchaType === 'hcaptcha' && RuntimeConfigStore.turnstileSiteKey);

		return (
			<Modal.Root size="small" centered onClose={handleCancel}>
				<Modal.Header title={t`Verify You're Human`} onClose={handleCancel} />
				<Modal.Content>
					<div className={styles.container}>
						<p className={styles.description}>
							<Trans>We need to make sure you're not a bot. Please complete the verification below.</Trans>
						</p>

						{error && (
							<div className={styles.errorBox}>
								<p className={styles.errorText}>{error}</p>
							</div>
						)}

						<div className={styles.captchaContainer}>
							{captchaType === 'turnstile' ? (
								<TurnstileWidget
									sitekey={RuntimeConfigStore.turnstileSiteKey ?? ''}
									onVerify={handleVerify}
									onExpire={handleExpire}
									onError={handleError}
									theme="dark"
								/>
							) : (
								<HCaptcha
									ref={hcaptchaRef}
									sitekey={RuntimeConfigStore.hcaptchaSiteKey ?? ''}
									onVerify={handleVerify}
									onExpire={handleExpire}
									onError={handleError}
									theme="dark"
								/>
							)}
						</div>

						{showSwitchButton && (
							<div className={styles.switchContainer}>
								<button
									type="button"
									onClick={captchaType === 'turnstile' ? handleSwitchToHCaptcha : handleSwitchToTurnstile}
									className={styles.switchButton}
									disabled={isVerifying}
								>
									{captchaType === 'turnstile' ? (
										<Trans>Having issues? Try hCaptcha instead</Trans>
									) : (
										<Trans>Try Turnstile instead</Trans>
									)}
								</button>
							</div>
						)}
					</div>
				</Modal.Content>
				<Modal.Footer>
					<Button variant="secondary" onClick={handleCancel} disabled={isVerifying}>
						<Trans>Cancel</Trans>
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
