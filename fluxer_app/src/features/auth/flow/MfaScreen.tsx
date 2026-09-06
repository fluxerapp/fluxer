// SPDX-License-Identifier: AGPL-3.0-or-later

import {MFA_CODE_DIGIT_COUNT} from '@app/features/app/config/I18nDisplayConstants';
import FormField from '@app/features/auth/flow/AuthFormField';
import styles from '@app/features/auth/flow/MfaScreen.module.css';
import {useAuthCardPresentation} from '@app/features/auth/flow/useAuthCardPresentation';
import {useMfaController} from '@app/features/auth/hooks/useLoginFlow';
import type {LoginSuccessPayload, MfaChallenge} from '@app/features/auth/state/AuthFlow';
import {
	BACK_TO_SIGN_IN_DESCRIPTOR,
	SIGN_IN_DESCRIPTOR,
	TWO_FACTOR_AUTHENTICATION_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';

const CODE_DESCRIPTOR = msg({
	message: 'Authenticator code',
	comment: 'Label for the MFA login one-time password field from an authenticator app or backup code.',
});
const AUTHENTICATION_CODE_PLACEHOLDER_DESCRIPTOR = msg({
	message: '6-digit code',
	comment: 'Placeholder for the MFA login one-time password field.',
});
const SECURITY_KEY_OR_PASSKEY_DESCRIPTOR = msg({
	message: 'Security key / passkey',
	comment: 'MFA action button for authenticating with a hardware security key or passkey.',
});
const MFA_CODE_INSTRUCTIONS_DESCRIPTOR = msg({
	message: 'Enter the {digitCount}-digit code from your authenticator app or a backup code.',
	comment: 'MFA code entry instructions. digitCount is the expected authenticator-code length.',
});
const TRY_SECURITY_KEY_INSTEAD_DESCRIPTOR = msg({
	message: 'Try security key / passkey instead',
	comment: 'Secondary MFA action that switches from code entry to passkey or security-key authentication.',
});

interface MfaScreenProps {
	challenge: MfaChallenge;
	inviteCode?: string;
	onSuccess: (payload: LoginSuccessPayload) => Promise<void> | void;
	onCancel: () => void;
}

const MfaScreen = ({challenge, inviteCode, onSuccess, onCancel}: MfaScreenProps) => {
	const {i18n} = useLingui();
	const {form, isLoading, fieldErrors, handleWebAuthn, isWebAuthnLoading, supports} = useMfaController({
		ticket: challenge.ticket,
		methods: {totp: challenge.totp, webauthn: challenge.webauthn},
		inviteCode,
		onLoginSuccess: onSuccess,
	});
	useAuthCardPresentation({showLogoSide: false, variant: 'compact'});
	const showCodeForm = supports.totp;
	const showWebAuthn = supports.webauthn;
	return (
		<div className={styles.container} data-flx="auth.flow.mfa-screen.container--2">
			<h1 className={styles.title} data-flx="auth.flow.mfa-screen.title">
				{i18n._(TWO_FACTOR_AUTHENTICATION_DESCRIPTOR)}
			</h1>
			{showCodeForm && (
				<p className={styles.description} data-flx="auth.flow.mfa-screen.description">
					{i18n._(MFA_CODE_INSTRUCTIONS_DESCRIPTOR, {digitCount: MFA_CODE_DIGIT_COUNT})}
				</p>
			)}
			{showCodeForm && (
				<form
					className={styles.form}
					onSubmit={form.handleSubmit}
					autoComplete="on"
					name="mfa"
					data-flx="auth.flow.mfa-screen.form.submit"
				>
					<FormField
						id="totp"
						name="totp"
						type="text"
						autoComplete="one-time-code"
						autoCapitalize="none"
						autoCorrect="off"
						enterKeyHint="done"
						inputMode="text"
						spellCheck={false}
						autoFocus
						data-step-focus="true"
						required
						placeholder={i18n._(AUTHENTICATION_CODE_PLACEHOLDER_DESCRIPTOR)}
						label={i18n._(CODE_DESCRIPTOR)}
						value={form.getValue('code')}
						onChange={(value) => form.setValue('code', value)}
						error={form.getError('code') || fieldErrors?.get('code') || fieldErrors?.get('ticket')}
						data-flx="auth.flow.mfa-screen.form-field.set-value.text"
					/>
					<Button
						type="submit"
						fitContainer
						disabled={isLoading || form.isSubmitting}
						data-flx="auth.flow.mfa-screen.button.submit"
					>
						{i18n._(SIGN_IN_DESCRIPTOR)}
					</Button>
				</form>
			)}
			{showWebAuthn && (
				<div className={styles.webauthnSection} data-flx="auth.flow.mfa-screen.webauthn-section">
					<Button
						type="button"
						fitContainer
						variant={showCodeForm ? 'secondary' : 'primary'}
						onClick={handleWebAuthn}
						disabled={isWebAuthnLoading}
						autoFocus={!showCodeForm}
						data-step-focus={showCodeForm ? undefined : 'true'}
						data-flx="auth.flow.mfa-screen.button.web-authn"
					>
						{i18n._(showCodeForm ? TRY_SECURITY_KEY_INSTEAD_DESCRIPTOR : SECURITY_KEY_OR_PASSKEY_DESCRIPTOR)}
					</Button>
				</div>
			)}
			<div className={styles.footerButtons} data-flx="auth.flow.mfa-screen.footer-buttons">
				<Button
					type="button"
					variant="secondary"
					onClick={onCancel}
					className={styles.footerButton}
					data-flx="auth.flow.mfa-screen.footer-button.cancel"
				>
					{i18n._(BACK_TO_SIGN_IN_DESCRIPTOR)}
				</Button>
			</div>
		</div>
	);
};

export default MfaScreen;
