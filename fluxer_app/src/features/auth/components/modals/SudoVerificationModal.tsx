// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import styles from '@app/features/auth/components/modals/SudoVerificationModal.module.css';
import {
	isPasskeyCeremonyDismissed,
	PasskeyBridgeSudoLink,
	runPasskeyBridgeNativeSudo,
} from '@app/features/auth/passkey_migration/PasskeyLegacyCeremony';
import PasskeyMigration from '@app/features/auth/passkey_migration/PasskeyMigration';
import {isPasskeyMigrationOrigin} from '@app/features/auth/passkey_migration/PasskeyMigrationOrigin';
import AccountManager from '@app/features/auth/state/AccountManager';
import Sudo from '@app/features/auth/state/AuthSudo';
import SudoPrompt, {SUDO_MODAL_KEY, SudoVerificationMethod} from '@app/features/auth/state/SudoPrompt';
import * as WebAuthnUtils from '@app/features/auth/utils/WebAuthnUtils';
import {
	COULDN_T_VERIFY_WITH_PASSKEY_DESCRIPTOR,
	PASSWORD_DESCRIPTOR,
	VERIFY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {http} from '@app/features/platform/transport/RestTransport';
import {Platform} from '@app/features/platform/types/Platform';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import buttonStyles from '@app/features/ui/button/Button.module.css';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import WebAuthnCredentials from '@app/features/user/state/WebAuthnCredentials';
import * as FormUtils from '@app/lib/forms';
import {PASSKEY_MIGRATION_RP_ID} from '@fluxer/constants/src/PasskeyConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
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
const ENTER_YOUR_PASSWORD_DESCRIPTOR = msg({
	message: 'Enter your password.',
	comment: 'Body text in the authentication sudo verification modal. Keep the tone plain and specific.',
});
const ENTER_THE_CODE_FROM_YOUR_AUTHENTICATOR_APP_DESCRIPTOR = msg({
	message: 'Enter the code from your authenticator app.',
	comment: 'Body text in the authentication sudo verification modal. Keep the tone plain and specific.',
});
const ENTER_ONE_OF_YOUR_BACKUP_CODES_DESCRIPTOR = msg({
	message: 'Enter one of your backup codes.',
	comment:
		'Body text in the authentication sudo verification modal, shown when the only code the account can enter is a backup code. Keep the tone plain and specific.',
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
const BACKUP_CODE_DESCRIPTOR = msg({
	message: 'Backup code',
	comment:
		'Label and placeholder for the code field in the authentication sudo verification modal when the only code the account can use is a backup code.',
});
const VERIFICATION_FAILED_DESCRIPTOR = msg({message: 'Verification failed'});
const FINISH_IN_THE_NEW_TAB_DESCRIPTOR = msg({
	message: 'Finish in the new tab. If you closed it, press Continue with passkey again.',
	comment:
		'Sudo (re-auth) modal hint shown after the passkey button opened a new tab to confirm the passkey. "Continue with passkey" is the button label. Keep plain.',
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
const holdsPasskeyFor = (matches: (rpId: string) => boolean): boolean =>
	WebAuthnCredentials.credentials.some((credential) => matches(credential.rp_id));
const holdsMigratedPasskey = (): boolean => holdsPasskeyFor((rpId) => rpId === PASSKEY_MIGRATION_RP_ID);
const holdsLegacyPasskey = (): boolean => holdsPasskeyFor((rpId) => rpId !== PASSKEY_MIGRATION_RP_ID);
const SudoVerificationModal: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {availableMethods, isVerifying, verificationFailed, rawError, lastUsedMfaMethod} = SudoPrompt;
	const form = useForm<FormInputs>({defaultValues: {password: '', totp: ''}});
	const [webAuthnInFlight, setWebAuthnInFlight] = useState(false);
	const [webAuthnError, setWebAuthnError] = useState<string | null>(null);
	const autoTriggeredRef = useRef(false);
	const [legacyLinkUrl, setLegacyLinkUrl] = useState<string | null>(null);
	const [legacyLinkActive, setLegacyLinkActive] = useState(false);
	const [legacyLinkFollowed, setLegacyLinkFollowed] = useState(false);
	const legacyLinkRef = useRef<PasskeyBridgeSudoLink | null>(null);
	const preferLegacyRef = useRef(false);
	const openRef = useRef(true);
	const userIdAtOpenRef = useRef(AccountManager.currentUserId);
	const showPasskey = availableMethods.webauthn;
	const showTotp = availableMethods.totp;
	const backupCodeOnly = !showTotp && availableMethods.backupCodes;
	const showCode = showTotp || availableMethods.backupCodes;
	const showPassword = availableMethods.password;
	const noMethodsAvailable = !showPasskey && !showCode && !showPassword;
	useEffect(() => {
		form.reset({password: '', totp: ''});
		setWebAuthnError(null);
		setWebAuthnInFlight(false);
		autoTriggeredRef.current = false;
	}, [form]);
	useEffect(() => {
		if (!verificationFailed && !rawError) return;
		const fallback: keyof FormInputs = showCode ? 'totp' : 'password';
		if (rawError) {
			FormUtils.handleError(i18n, form, rawError, fallback);
		} else if (verificationFailed) {
			form.setError(fallback, {type: 'server', message: i18n._(VERIFICATION_FAILED_DESCRIPTOR)});
		}
		setWebAuthnInFlight(false);
	}, [form, verificationFailed, rawError, i18n, i18n.locale, showPassword, showCode]);
	const finishLegacySudo = (sudoToken: string) => {
		if (!openRef.current || AccountManager.currentUserId !== userIdAtOpenRef.current) return;
		Sudo.setToken(sudoToken);
		PasskeyMigration.checkAfterSudo(SUDO_MODAL_KEY);
		SudoPrompt.submit({});
	};
	const startLegacyLink = () => {
		if (legacyLinkRef.current === null) {
			legacyLinkRef.current = new PasskeyBridgeSudoLink({
				onLink: (url) => {
					setLegacyLinkUrl(url);
					if (url === null) {
						setLegacyLinkFollowed(false);
					}
				},
				onCompleted: finishLegacySudo,
				onError: (error) => {
					logger.error('WebAuthn verification in a new tab failed', error);
					setWebAuthnError(i18n._(COULDN_T_VERIFY_WITH_PASSKEY_DESCRIPTOR));
				},
			});
		}
		setLegacyLinkActive(true);
		void legacyLinkRef.current.start();
	};
	useEffect(() => {
		if (showPasskey && isPasskeyMigrationOrigin() && !Platform.isElectron && !holdsMigratedPasskey()) {
			startLegacyLink();
		}
		return () => {
			openRef.current = false;
			legacyLinkRef.current?.dispose();
		};
	}, []);
	const handleWebAuthn = async () => {
		if (webAuthnInFlight || isVerifying) return;
		setWebAuthnError(null);
		form.clearErrors();
		setWebAuthnInFlight(true);
		const migrationOrigin = isPasskeyMigrationOrigin();
		const runsLegacyNatively =
			migrationOrigin && Platform.isElectron && (preferLegacyRef.current || !holdsMigratedPasskey());
		try {
			if (runsLegacyNatively) {
				preferLegacyRef.current = false;
				const result = await runPasskeyBridgeNativeSudo();
				if (result.status === 'completed') {
					finishLegacySudo(result.sudo_token);
					return;
				}
				setWebAuthnInFlight(false);
				return;
			}
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
			if (migrationOrigin && !runsLegacyNatively && isPasskeyCeremonyDismissed(err) && holdsLegacyPasskey()) {
				if (!Platform.isElectron) {
					startLegacyLink();
					return;
				}
				preferLegacyRef.current = true;
			}
			if (isMacAppIdentifierError(err)) {
				setWebAuthnError(i18n._(PASSKEYS_REQUIRE_A_SIGNED_MACOS_BUNDLE_WITH_A_DESCRIPTOR));
				return;
			}
			setWebAuthnError(i18n._(COULDN_T_VERIFY_WITH_PASSKEY_DESCRIPTOR));
		}
	};
	useEffect(() => {
		if (autoTriggeredRef.current || legacyLinkRef.current !== null) return;
		if (!showPasskey || showPassword || showCode) return;
		if (lastUsedMfaMethod && lastUsedMfaMethod !== 'webauthn') return;
		autoTriggeredRef.current = true;
		void handleWebAuthn();
	}, [showPasskey, showPassword, showCode]);
	const handleClose = () => {
		SudoPrompt.reject(new DOMException('User cancelled verification', 'AbortError'));
	};
	const onSubmit = (values: FormInputs) => {
		form.clearErrors();
		if (showCode && values.totp) {
			SudoPrompt.submit({mfa_method: SudoVerificationMethod.TOTP, mfa_code: values.totp});
			return;
		}
		if (showPassword && values.password) {
			SudoPrompt.submit({password: values.password});
			return;
		}
		if (showPasskey && !showCode && !showPassword) {
			void handleWebAuthn();
			return;
		}
		const target: keyof FormInputs = showCode ? 'totp' : 'password';
		form.setError(target, {
			type: 'manual',
			message:
				target === 'password'
					? i18n._(ENTER_YOUR_PASSWORD_DESCRIPTOR)
					: backupCodeOnly
						? i18n._(ENTER_ONE_OF_YOUR_BACKUP_CODES_DESCRIPTOR)
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
								{showCode && (
									<Input
										id="totp"
										data-flx="auth.sudo-verification-modal.input"
										{...form.register('totp')}
										label={i18n._(backupCodeOnly ? BACKUP_CODE_DESCRIPTOR : AUTHENTICATOR_CODE_DESCRIPTOR)}
										placeholder={i18n._(backupCodeOnly ? BACKUP_CODE_DESCRIPTOR : MESSAGE_6_DIGIT_CODE_DESCRIPTOR)}
										type="text"
										autoComplete="one-time-code"
										autoCapitalize="none"
										autoCorrect="off"
										enterKeyHint="done"
										inputMode={backupCodeOnly ? 'text' : 'numeric'}
										spellCheck={false}
										autoFocus
										error={form.formState.errors.totp?.message}
									/>
								)}

								{showPasskey && (
									<>
										{legacyLinkActive && legacyLinkUrl !== null ? (
											<FocusRing offset={-2} data-flx="auth.sudo-verification-modal.focus-ring.legacy-link">
												<a
													href={legacyLinkUrl}
													target="_blank"
													rel="noopener noreferrer"
													onClick={() => {
														setWebAuthnError(null);
														setLegacyLinkFollowed(true);
													}}
													className={clsx(
														buttonStyles.button,
														buttonStyles[showCode || showPassword ? 'secondary' : 'primary'],
														buttonStyles.fitContainer,
													)}
													data-flx="auth.sudo-verification-modal.link.web-authn"
												>
													<Trans>Continue with passkey</Trans>
												</a>
											</FocusRing>
										) : legacyLinkActive ? (
											<Button
												type="button"
												onClick={startLegacyLink}
												submitting={webAuthnError === null}
												disabled={isVerifying}
												fitContainer
												variant={showCode || showPassword ? 'secondary' : 'primary'}
												data-flx="auth.sudo-verification-modal.button.web-authn-starting"
											>
												<Trans>Continue with passkey</Trans>
											</Button>
										) : webAuthnInFlight ? (
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
												autoFocus={!showCode && !showPassword}
												variant={showCode || showPassword ? 'secondary' : 'primary'}
												data-flx="auth.sudo-verification-modal.button.web-authn"
											>
												<Trans>Continue with passkey</Trans>
											</Button>
										)}
										{legacyLinkActive && legacyLinkFollowed && !webAuthnError && (
											<p className={styles.passkeyHint} data-flx="auth.sudo-verification-modal.passkey-hint">
												{i18n._(FINISH_IN_THE_NEW_TAB_DESCRIPTOR)}
											</p>
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
										autoFocus={!showPasskey && !showCode}
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
					{!noMethodsAvailable && (showCode || showPassword) && (
						<Button
							type="submit"
							submitting={isVerifying}
							disabled={isVerifying || webAuthnInFlight}
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
