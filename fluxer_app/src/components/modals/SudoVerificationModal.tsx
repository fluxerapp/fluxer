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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {useForm} from 'react-hook-form';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {Spinner} from '~/components/uikit/Spinner';
import {Endpoints} from '~/Endpoints';
import HttpClient from '~/lib/HttpClient';
import SudoPromptStore, {SudoVerificationMethod, type SudoVerificationPayload} from '~/stores/SudoPromptStore';
import * as FormUtils from '~/utils/FormUtils';
import * as WebAuthnUtils from '~/utils/WebAuthnUtils';
import styles from './SudoVerificationModal.module.css';

interface FormInputs {
	password: string;
	totpCode: string;
	smsCode: string;
}

enum SmsStatus {
	IDLE = 'idle',
	SENDING = 'sending',
	SENT = 'sent',
}

const isMacAppIdentifierError = (error: unknown): boolean => {
	const message =
		error instanceof Error
			? error.message
			: typeof error === 'object' && error !== null && 'message' in error
				? String((error as {message?: unknown}).message ?? '')
				: '';
	return message.toLowerCase().includes('application identifier');
};

const getMethodAvailable = (
	method: SudoVerificationMethod,
	available: {password: boolean; totp: boolean; sms: boolean; webauthn: boolean},
): boolean => {
	switch (method) {
		case SudoVerificationMethod.PASSWORD:
			return available.password;
		case SudoVerificationMethod.TOTP:
			return available.totp;
		case SudoVerificationMethod.SMS:
			return available.sms;
		case SudoVerificationMethod.WEBAUTHN:
			return available.webauthn;
		default:
			return false;
	}
};

const getDefaultMethod = (
	available: {password: boolean; totp: boolean; sms: boolean; webauthn: boolean},
	lastUsed: SudoVerificationPayload['mfa_method'] | null,
): SudoVerificationMethod | null => {
	if (lastUsed && getMethodAvailable(lastUsed as SudoVerificationMethod, available)) {
		return lastUsed as SudoVerificationMethod;
	}

	const preference = [
		SudoVerificationMethod.WEBAUTHN,
		SudoVerificationMethod.TOTP,
		SudoVerificationMethod.SMS,
		SudoVerificationMethod.PASSWORD,
	];

	return preference.find((method) => getMethodAvailable(method, available)) ?? null;
};

const getDefaultFieldForMethod = (method: SudoVerificationMethod | null): keyof FormInputs => {
	switch (method) {
		case SudoVerificationMethod.TOTP:
			return 'totpCode';
		case SudoVerificationMethod.SMS:
			return 'smsCode';
		case SudoVerificationMethod.WEBAUTHN:
			return 'password';
		default:
			return 'password';
	}
};

const SudoVerificationModal: React.FC = observer(() => {
	const {t, i18n} = useLingui();
	const {isOpen, availableMethods, isLoadingMethods, isVerifying, verificationError, rawError, lastUsedMfaMethod} =
		SudoPromptStore;

	const form = useForm<FormInputs>({
		defaultValues: {password: '', totpCode: '', smsCode: ''},
	});

	const [selectedMethod, setSelectedMethod] = React.useState<SudoVerificationMethod | null>(null);
	const [smsStatus, setSmsStatus] = React.useState<SmsStatus>(SmsStatus.IDLE);
	const [webAuthnPayload, setWebAuthnPayload] = React.useState<{challenge: string; response: unknown} | null>(null);

	React.useEffect(() => {
		if (!isOpen) return;

		setSelectedMethod(null);
		setSmsStatus(SmsStatus.IDLE);
		setWebAuthnPayload(null);
		form.reset({password: '', totpCode: '', smsCode: ''});

		SudoPromptStore.loadAvailableMethods().catch(() => {});
	}, [isOpen, form]);

	React.useEffect(() => {
		if (isLoadingMethods || !isOpen) return;

		const currentMethodAvailable = selectedMethod !== null && getMethodAvailable(selectedMethod, availableMethods);
		if (currentMethodAvailable) return;

		const next = getDefaultMethod(availableMethods, lastUsedMfaMethod);
		if (next) {
			setSelectedMethod(next);
		}
	}, [isLoadingMethods, isOpen, availableMethods, selectedMethod, lastUsedMfaMethod]);

	React.useEffect(() => {
		if (!verificationError && !rawError) return;

		const fallbackField = getDefaultFieldForMethod(selectedMethod);
		if (rawError) {
			FormUtils.handleError(i18n, form, rawError as any, fallbackField);
		} else if (verificationError) {
			form.setError(fallbackField, {type: 'server', message: verificationError});
		}
	}, [form, verificationError, rawError, selectedMethod, i18n]);

	React.useEffect(() => {
		if (selectedMethod !== SudoVerificationMethod.WEBAUTHN) return;
		if (!webAuthnPayload || isVerifying) return;

		SudoPromptStore.submit({
			mfa_method: SudoVerificationMethod.WEBAUTHN,
			webauthn_challenge: webAuthnPayload.challenge,
			webauthn_response: webAuthnPayload.response,
		});
	}, [selectedMethod, webAuthnPayload, isVerifying]);

	const methodButtons = (Object.values(SudoVerificationMethod) as Array<SudoVerificationMethod>).filter((method) =>
		getMethodAvailable(method, availableMethods),
	);

	const methodLabels = React.useMemo<Record<SudoVerificationMethod, string>>(
		() => ({
			[SudoVerificationMethod.PASSWORD]: t`Password`,
			[SudoVerificationMethod.TOTP]: t`Authenticator app`,
			[SudoVerificationMethod.SMS]: t`SMS code`,
			[SudoVerificationMethod.WEBAUTHN]: t`Security key / Passkey`,
		}),
		[t],
	);

	const setFieldError = (message: string) => {
		const fallbackField = getDefaultFieldForMethod(selectedMethod);
		form.setError(fallbackField, {type: 'manual', message});
	};

	const handleSendSms = async () => {
		setSmsStatus(SmsStatus.SENDING);
		form.clearErrors();

		try {
			await HttpClient.post({url: Endpoints.SUDO_SMS_SEND});
			setSmsStatus(SmsStatus.SENT);
		} catch (err) {
			console.error('Failed to send SMS code', err);
			setSmsStatus(SmsStatus.IDLE);
			setFieldError(t`Failed to send SMS code. Please try again.`);
		}
	};

	const handleWebAuthn = async () => {
		form.clearErrors();

		try {
			await WebAuthnUtils.assertWebAuthnSupported();
		} catch (error) {
			console.error('WebAuthn unavailable', error);
			setFieldError(t`Passkeys are not available on this device.`);
			return;
		}

		try {
			const optionsResponse = await HttpClient.post<{challenge: string}>({url: Endpoints.SUDO_WEBAUTHN_OPTIONS});
			const credential = await WebAuthnUtils.performAuthentication(optionsResponse.body);
			setWebAuthnPayload({challenge: optionsResponse.body.challenge, response: credential});
		} catch (err) {
			console.error('WebAuthn verification failed', err);
			if (isMacAppIdentifierError(err)) {
				setFieldError(
					t`Passkeys require a signed macOS bundle with a valid application identifier. Please install the signed desktop client and retry.`,
				);
				return;
			}

			setFieldError(t`Security key verification failed. Please try again.`);
		}
	};

	const handleClose = () => {
		SudoPromptStore.reject(new DOMException('User cancelled verification', 'AbortError'));
	};

	const renderMethodInput = () => {
		switch (selectedMethod) {
			case SudoVerificationMethod.PASSWORD:
				return (
					<Input
						{...form.register('password')}
						label={t`Password`}
						type="password"
						autoFocus
						required
						error={form.formState.errors.password?.message}
					/>
				);
			case SudoVerificationMethod.TOTP:
				return (
					<Input
						{...form.register('totpCode')}
						label={t`Authenticator code`}
						autoFocus
						autoComplete="one-time-code"
						required
						error={form.formState.errors.totpCode?.message}
					/>
				);
			case SudoVerificationMethod.SMS:
				return (
					<div className={styles.smsSection}>
						<Button
							type="button"
							onClick={handleSendSms}
							disabled={smsStatus === SmsStatus.SENDING}
							variant={smsStatus === SmsStatus.SENT ? 'secondary' : 'primary'}
							fitContainer
						>
							{smsStatus === SmsStatus.SENT ? <Trans>Code sent</Trans> : <Trans>Send SMS code</Trans>}
						</Button>
						{smsStatus === SmsStatus.SENT && (
							<Input
								{...form.register('smsCode')}
								label={t`SMS code`}
								autoFocus
								autoComplete="one-time-code"
								required
								error={form.formState.errors.smsCode?.message}
							/>
						)}
					</div>
				);
			case SudoVerificationMethod.WEBAUTHN:
				return webAuthnPayload ? (
					<div className={styles.webauthnReady}>
						<Spinner />
						<span className={styles.srOnly}>
							<Trans>Verifying...</Trans>
						</span>
					</div>
				) : (
					<Button type="button" onClick={handleWebAuthn} fitContainer>
						<Trans>Use security key</Trans>
					</Button>
				);
			default:
				return null;
		}
	};

	const onSubmit = (values: FormInputs) => {
		form.clearErrors();

		if (!selectedMethod) {
			setFieldError(t`Select a verification method.`);
			return;
		}

		let payload: SudoVerificationPayload | null = null;

		switch (selectedMethod) {
			case SudoVerificationMethod.PASSWORD:
				if (!values.password) {
					setFieldError(t`Enter your password.`);
					return;
				}
				payload = {password: values.password};
				break;
			case SudoVerificationMethod.TOTP:
				if (!values.totpCode) {
					setFieldError(t`Enter the code from your authenticator app.`);
					return;
				}
				payload = {mfa_method: SudoVerificationMethod.TOTP, mfa_code: values.totpCode};
				break;
			case SudoVerificationMethod.SMS:
				if (smsStatus !== SmsStatus.SENT) {
					setFieldError(t`Send an SMS code first.`);
					return;
				}
				if (!values.smsCode) {
					setFieldError(t`Enter the SMS code you received.`);
					return;
				}
				payload = {mfa_method: SudoVerificationMethod.SMS, mfa_code: values.smsCode};
				break;
			case SudoVerificationMethod.WEBAUTHN:
				if (!webAuthnPayload) {
					setFieldError(t`Complete the security key prompt first.`);
					return;
				}
				payload = {
					mfa_method: SudoVerificationMethod.WEBAUTHN,
					webauthn_challenge: webAuthnPayload.challenge,
					webauthn_response: webAuthnPayload.response,
				};
				break;
			default:
				payload = null;
		}

		if (payload) {
			SudoPromptStore.submit(payload);
		}
	};

	const shouldShowSubmit =
		selectedMethod !== SudoVerificationMethod.WEBAUTHN &&
		!(selectedMethod === SudoVerificationMethod.SMS && smsStatus !== SmsStatus.SENT);

	return (
		<Modal.Root size="small" centered onClose={handleClose}>
			<Form form={form} onSubmit={onSubmit} aria-label={t`Verify identity form`}>
				<Modal.Header title={t`Verify your identity`} onClose={handleClose} />
				<Modal.Content>
					<div className={styles.container}>
						<p className={styles.description}>
							<Trans>This action requires verification to continue.</Trans>
						</p>

						{isLoadingMethods ? (
							<div className={styles.loading}>
								<Spinner />
								<span className={styles.srOnly}>
									<Trans>Loading verification methods...</Trans>
								</span>
							</div>
						) : (
							<>
								{methodButtons.length > 1 && (
									<div className={styles.methodSelector}>
										{methodButtons.map((method) => (
											<Button
												key={method}
												type="button"
												variant={selectedMethod === method ? 'primary' : 'secondary'}
												onClick={() => {
													setSelectedMethod(method);
													form.clearErrors();
													if (method !== SudoVerificationMethod.SMS) {
														setSmsStatus(SmsStatus.IDLE);
													}
												}}
												compact
											>
												{methodLabels[method]}
											</Button>
										))}
									</div>
								)}
								{renderMethodInput()}
							</>
						)}
					</div>
				</Modal.Content>
				<Modal.Footer>
					<Button type="button" variant="secondary" onClick={handleClose} disabled={isVerifying}>
						<Trans>Cancel</Trans>
					</Button>
					{shouldShowSubmit && (
						<Button type="submit" submitting={isVerifying} disabled={isVerifying}>
							<Trans>Continue</Trans>
						</Button>
					)}
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});

export default SudoVerificationModal;
