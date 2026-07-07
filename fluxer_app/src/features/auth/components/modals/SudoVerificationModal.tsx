// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import styles from '@app/features/auth/components/modals/SudoVerificationModal.module.css';
import {SSO_SUDO_COMPLETE_MESSAGE, startSsoSudo} from '@app/features/auth/state/SsoSudoFlow';
import SudoPrompt, {SudoVerificationMethod} from '@app/features/auth/state/SudoPrompt';
import * as WebAuthnUtils from '@app/features/auth/utils/WebAuthnUtils';
import {PASSWORD_DESCRIPTOR, VERIFY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useRef, useState} from 'react';
import {useForm} from 'react-hook-form';

const PASSKEYS_REQUIRE_A_SIGNED_MACOS_BUNDLE_WITH_A_DESCRIPTOR = msg({
	message:
		'Passkeys require a signed macOS bundle with a valid application identifier. Install the signed desktop client and retry.',
	comment:
		'Sudo (re-auth) modal body shown on unsigned macOS desktop bundles where passkeys cannot work. Direct the user to install the signed client.',
});
const COULDN_T_VERIFY_WITH_SSO_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: "Couldn't verify with SSO. Try again.",
	comment: 'Sudo (re-auth) modal toast error shown when SSO verification fails. Keep plain.',
});
const COULDN_T_VERIFY_WITH_PASSKEY_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: "Couldn't verify with passkey. Try again.",
	comment: 'Sudo (re-auth) modal toast error shown when passkey verification fails. Keep plain.',
});
const ENTER_YOUR_PASSWORD_DESCRIPTOR = msg({
	message: 'Enter your password.',
	comment: 'Body text in the authentication sudo verification modal. Keep the tone plain and specific.',
});
const ENTER_THE_CODE_FROM_YOUR_AUTHENTICATOR_APP_DESCRIPTOR = msg({
	message: 'Enter the code from your authenticator app.',
	comment: 'Body text in the authentication sudo verification modal. Keep the tone plain and specific.',
});
const VERIFY_IDENTITY_FORM_DESCRIPTOR = msg({
	message: 'Verify identity form',
	comment: 'Accessible form label in the authentication sudo verification modal. Keep the tone plain and specific.',
});
const VERIFY_IT_S_YOU_DESCRIPTOR = msg({
	message: "Verify it's you",
	comment: 'Short label in the authentication sudo verification modal. Keep the tone plain and specific.',
});
const AUTHENTICATOR_CODE_DESCRIPTOR = msg({
	message: 'Authenticator code',
	comment: 'Short label in the authentication sudo verification modal. Keep the tone plain and specific.',
});
const MESSAGE_6_DIGIT_CODE_DESCRIPTOR = msg({
	message: '6-digit code',
	comment: 'Short label in the authentication sudo verification modal. Keep the tone plain and specific.',
});
const logger = new Logger('SudoVerificationModal');

interface FormInputs {
	password: string;
	totp: string;
}

const isMacAppIdentifierError = (error: unknown): boolean => {
	const message = error instanceof Error ? error.message : '';
	return message.toLowerCase().includes('application identifier');
};
const SudoVerificationModal: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {availableMethods, isVerifying, verificationError, rawError, lastUsedMfaMethod} = SudoPrompt;
	const form = useForm<FormInputs>({defaultValues: {password: '', totp: ''}});
	const [webAuthnInFlight, setWebAuthnInFlight] = useState(false);
	const [webAuthnError, setWebAuthnError] = useState<string | null>(null);
	const [ssoInFlight, setSsoInFlight] = useState(false);
	const [ssoError, setSsoError] = useState<string | null>(null);
	const autoTriggeredRef = useRef(false);
	const popupTimerRef = useRef<number | null>(null);
	const messageListenerRef = useRef<((event: MessageEvent) => void) | null>(null);
	const showPasskey = availableMethods.webauthn;
	const showTotp = availableMethods.totp;
	const showPassword = availableMethods.password;
	const showSso = availableMethods.sso;
	const noMethodsAvailable = !showPasskey && !showTotp && !showPassword && !showSso;
	useEffect(() => {
		return () => {
			if (popupTimerRef.current) {
				window.clearInterval(popupTimerRef.current);
			}
			if (messageListenerRef.current) {
				window.removeEventListener('message', messageListenerRef.current);
			}
		};
	}, []);
	useEffect(() => {
		form.reset({password: '', totp: ''});
		setWebAuthnError(null);
		setWebAuthnInFlight(false);
		setSsoError(null);
		setSsoInFlight(false);
		autoTriggeredRef.current = false;
	}, [form]);
	useEffect(() => {
		if (!verificationError && !rawError) return;
		const fallback: keyof FormInputs = showTotp ? 'totp' : showPassword ? 'password' : 'password';
		if (rawError) {
			FormUtils.handleError(i18n, form, rawError, fallback);
		} else if (verificationError) {
			form.setError(fallback, {type: 'server', message: verificationError});
		}
		setWebAuthnInFlight(false);
		setSsoInFlight(false);
	}, [form, verificationError, rawError, i18n, showPassword, showTotp]);
	const handleSso = async () => {
		if (ssoInFlight || isVerifying) return;
		setSsoError(null);
		form.clearErrors();
		setSsoInFlight(true);
		try {
			const redirectTo = `${window.location.pathname}${window.location.search}${window.location.hash}`;
			const result = await startSsoSudo({redirectTo});
			const popup = window.open(result.authorizationUrl, 'fluxer-sso-sudo', 'popup,width=520,height=720');
			if (!popup) {
				window.location.assign(result.authorizationUrl);
				return;
			}
			if (popupTimerRef.current) {
				window.clearInterval(popupTimerRef.current);
				popupTimerRef.current = null;
			}
			if (messageListenerRef.current) {
				window.removeEventListener('message', messageListenerRef.current);
				messageListenerRef.current = null;
			}
			const handleMessage = (event: MessageEvent) => {
				if (event.origin !== window.location.origin) return;
				const data = event.data as {type?: unknown; state?: unknown; sudoToken?: unknown};
				if (data.type !== SSO_SUDO_COMPLETE_MESSAGE || data.state !== result.state) return;
				if (popupTimerRef.current) {
					window.clearInterval(popupTimerRef.current);
					popupTimerRef.current = null;
				}
				window.removeEventListener('message', handleMessage);
				messageListenerRef.current = null;
				if (typeof data.sudoToken !== 'string' || !data.sudoToken) {
					setSsoInFlight(false);
					setSsoError(i18n._(COULDN_T_VERIFY_WITH_SSO_PLEASE_TRY_AGAIN_DESCRIPTOR));
					return;
				}
				SudoPrompt.completeWithSsoToken(data.sudoToken);
			};
			messageListenerRef.current = handleMessage;
			window.addEventListener('message', handleMessage);
			popupTimerRef.current = window.setInterval(() => {
				if (!popup.closed) return;
				if (popupTimerRef.current) {
					window.clearInterval(popupTimerRef.current);
					popupTimerRef.current = null;
				}
				window.removeEventListener('message', handleMessage);
				messageListenerRef.current = null;
				setSsoInFlight(false);
			}, 500);
		} catch (err) {
			logger.error('SSO verification failed', err);
			if (popupTimerRef.current) {
				window.clearInterval(popupTimerRef.current);
				popupTimerRef.current = null;
			}
			if (messageListenerRef.current) {
				window.removeEventListener('message', messageListenerRef.current);
				messageListenerRef.current = null;
			}
			setSsoInFlight(false);
			setSsoError(i18n._(COULDN_T_VERIFY_WITH_SSO_PLEASE_TRY_AGAIN_DESCRIPTOR));
		}
	};
	const handleWebAuthn = async () => {
		if (webAuthnInFlight || isVerifying || ssoInFlight) return;
		setWebAuthnError(null);
		form.clearErrors();
		setWebAuthnInFlight(true);
		try {
			await WebAuthnUtils.assertWebAuthnSupported();
			const optionsResponse = await http.post<{challenge: string}>(Endpoints.SUDO_WEBAUTHN_OPTIONS);
			const credential = await WebAuthnUtils.performAuthentication(optionsResponse.body);
			SudoPrompt.submit({
				mfa_method: SudoVerificationMethod.WEBAUTHN,
				webauthn_challenge: optionsResponse.body.challenge,
				webauthn_response: credential,
			});
		} catch (err) {
			logger.error('WebAuthn verification failed', err);
			setWebAuthnInFlight(false);
			if (isMacAppIdentifierError(err)) {
				setWebAuthnError(i18n._(PASSKEYS_REQUIRE_A_SIGNED_MACOS_BUNDLE_WITH_A_DESCRIPTOR));
				return;
			}
			setWebAuthnError(i18n._(COULDN_T_VERIFY_WITH_PASSKEY_PLEASE_TRY_AGAIN_DESCRIPTOR));
		}
	};
	useEffect(() => {
		if (autoTriggeredRef.current) return;
		if (showSso) return;
		if (!showPasskey || showPassword || showTotp) return;
		if (lastUsedMfaMethod && lastUsedMfaMethod !== 'webauthn') return;
		autoTriggeredRef.current = true;
		void handleWebAuthn();
	}, [showPasskey, showPassword, showTotp, showSso]);
	const handleClose = () => {
		SudoPrompt.reject(new DOMException('User cancelled verification', 'AbortError'));
	};
	const onSubmit = (values: FormInputs) => {
		form.clearErrors();
		if (showTotp && values.totp) {
			SudoPrompt.submit({mfa_method: SudoVerificationMethod.TOTP, mfa_code: values.totp});
			return;
		}
		if (showPassword && values.password) {
			SudoPrompt.submit({password: values.password});
			return;
		}
		if (showPasskey && !showTotp && !showPassword) {
			void handleWebAuthn();
			return;
		}
		const target: keyof FormInputs = showTotp ? 'totp' : 'password';
		form.setError(target, {
			type: 'manual',
			message:
				target === 'password'
					? i18n._(ENTER_YOUR_PASSWORD_DESCRIPTOR)
					: i18n._(ENTER_THE_CODE_FROM_YOUR_AUTHENTICATOR_APP_DESCRIPTOR),
		});
	};
	return (
		<Modal.Root size="small" centered onClose={handleClose} data-flx="auth.sudo-verification-modal.modal-root">
			<Form
				form={form}
				onSubmit={onSubmit}
				aria-label={i18n._(VERIFY_IDENTITY_FORM_DESCRIPTOR)}
				data-flx="auth.sudo-verification-modal.form.submit"
			>
				<Modal.Header
					title={i18n._(VERIFY_IT_S_YOU_DESCRIPTOR)}
					onClose={handleClose}
					data-flx="auth.sudo-verification-modal.modal-header"
				/>
				<Modal.Content data-flx="auth.sudo-verification-modal.modal-content">
					<Modal.ContentLayout data-flx="auth.sudo-verification-modal.container">
						<Modal.Description data-flx="auth.sudo-verification-modal.description">
							<Trans>For your security, please confirm it's you to continue.</Trans>
						</Modal.Description>

						{noMethodsAvailable ? (
							<div className={styles.unavailable} role="alert" data-flx="auth.sudo-verification-modal.unavailable">
								<p className={styles.unavailableTitle} data-flx="auth.sudo-verification-modal.unavailable-title">
									<Trans>No verification methods available</Trans>
								</p>
								<Modal.Description
									className={styles.unavailableBody}
									data-flx="auth.sudo-verification-modal.unavailable-body"
								>
									<Trans>
										Your account requires multi-factor authentication, but no supported methods are set up on this
										device. Sign in on a device with your authenticator app or security key, or contact support.
									</Trans>
								</Modal.Description>
							</div>
						) : (
							<>
								{showTotp && (
									<Input
										id="totp"
										data-flx="auth.sudo-verification-modal.input"
										{...form.register('totp')}
										label={i18n._(AUTHENTICATOR_CODE_DESCRIPTOR)}
										placeholder={i18n._(MESSAGE_6_DIGIT_CODE_DESCRIPTOR)}
										type="text"
										autoComplete="one-time-code"
										autoCapitalize="none"
										autoCorrect="off"
										enterKeyHint="done"
										inputMode="numeric"
										spellCheck={false}
										autoFocus
										error={form.formState.errors.totp?.message}
									/>
								)}

								{showSso && (
									<>
										<Button
											type="button"
											onClick={handleSso}
											submitting={ssoInFlight}
											disabled={isVerifying || webAuthnInFlight || ssoInFlight}
											fitContainer
											autoFocus
											data-flx="auth.sudo-verification-modal.button.sso"
										>
											<Trans>Continue with SSO</Trans>
										</Button>
										{ssoError && (
											<p className={styles.formError} role="alert" data-flx="auth.sudo-verification-modal.sso-error">
												{ssoError}
											</p>
										)}
									</>
								)}

								{showPasskey && (
									<>
										{webAuthnInFlight ? (
											<div
												className={styles.passkeyVerifying}
												role="status"
												data-flx="auth.sudo-verification-modal.passkey-verifying"
											>
												<Spinner data-flx="auth.sudo-verification-modal.spinner" />
												<span data-flx="auth.sudo-verification-modal.span">
													<Trans>Waiting for passkey…</Trans>
												</span>
											</div>
										) : (
											<Button
												type="button"
												onClick={handleWebAuthn}
												disabled={isVerifying}
												fitContainer
												autoFocus={!showTotp && !showPassword}
												variant={showTotp || showPassword ? 'secondary' : 'primary'}
												data-flx="auth.sudo-verification-modal.button.web-authn"
											>
												<Trans>Continue with passkey</Trans>
											</Button>
										)}
										{webAuthnError && (
											<p className={styles.formError} role="alert" data-flx="auth.sudo-verification-modal.form-error">
												{webAuthnError}
											</p>
										)}
									</>
								)}

								{showPassword && (
									<Input
										data-flx="auth.sudo-verification-modal.input.password"
										{...form.register('password')}
										label={i18n._(PASSWORD_DESCRIPTOR)}
										type="password"
										autoFocus={!showPasskey && !showTotp}
										error={form.formState.errors.password?.message}
									/>
								)}
							</>
						)}
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="auth.sudo-verification-modal.modal-footer">
					<Button
						type="button"
						variant="secondary"
						onClick={handleClose}
						disabled={isVerifying}
						data-flx="auth.sudo-verification-modal.button.close"
					>
						<Trans>Cancel</Trans>
					</Button>
					{!noMethodsAvailable && (showTotp || showPassword) && (
						<Button
							type="submit"
							submitting={isVerifying}
							disabled={isVerifying || webAuthnInFlight || ssoInFlight}
							data-flx="auth.sudo-verification-modal.button.submit"
						>
							{i18n._(VERIFY_DESCRIPTOR)}
						</Button>
					)}
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});

export default SudoVerificationModal;
