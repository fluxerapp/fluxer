// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {openRecoveryKitModal} from '@app/features/auth/components/modals/RecoveryKitModal';
import styles from '@app/features/auth/components/pages/RecoverAccountPage.module.css';
import FormField from '@app/features/auth/flow/AuthFormField';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import MfaScreen from '@app/features/auth/flow/MfaScreen';
import {useAuthForm} from '@app/features/auth/hooks/useAuthForm';
import {type LoginSuccessPayload, type MfaChallenge, recoverAccount} from '@app/features/auth/state/AuthFlow';
import {BACK_TO_SIGN_IN_DESCRIPTOR, USERNAME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {rememberRecoveryKit} from '@app/features/user/commands/RecoveryKitCommands';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {formatRecoveryKeyInput} from '@fluxer/constants/src/RecoveryKeyUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useId, useState} from 'react';

const RECOVER_ACCOUNT_DESCRIPTOR = msg({
	message: 'Recover your account',
	comment: 'Title of the page where someone resets their password with a recovery kit.',
});
const RECOVERY_KEY_DESCRIPTOR = msg({
	message: 'Recovery key',
	comment: 'Label for the recovery key field on the account recovery page.',
});
const NEW_PASSWORD_DESCRIPTOR = msg({
	message: 'New password',
	comment: 'Label for the new password field on the account recovery page.',
});
const CONFIRM_NEW_PASSWORD_DESCRIPTOR = msg({
	message: 'Confirm new password',
	comment: 'Label for the new password confirmation field on the account recovery page.',
});
const PASSWORDS_DO_NOT_MATCH_DESCRIPTOR = msg({
	message: 'Passwords do not match',
	comment: 'Account recovery page error when the new password and its confirmation differ.',
});

const API_RENDERED_FIELDS = new Set(['login', 'recovery_key', 'password']);

interface RecoveryFragment {
	username: string;
	key: string;
}

function readRecoveryFragment(): RecoveryFragment {
	const params = new URLSearchParams(window.location.hash.replace(/^#/, ''));
	return {username: params.get('username') ?? '', key: params.get('key') ?? ''};
}

function clearRecoveryFragment(): void {
	if (!window.location.hash) return;
	window.history.replaceState(window.history.state, '', `${window.location.pathname}${window.location.search}`);
}

function resolveBannerError(
	error: string | null,
	fieldErrors: ReadonlyMap<string, string> | null | undefined,
): string | null {
	if (error != null) {
		return error;
	}
	if (fieldErrors == null) {
		return null;
	}
	for (const [fieldName, message] of fieldErrors) {
		if (!API_RENDERED_FIELDS.has(fieldName)) {
			return message;
		}
	}
	return null;
}

function usernameFromLogin(login: string): {username: string; discriminator?: string} {
	const [username, discriminator] = login.split('#');
	return discriminator ? {username, discriminator} : {username};
}

interface PendingRecoveryKit {
	recoveryKey: string;
	createdAt: string;
	login: string;
}

function showRecoveredKit(payload: LoginSuccessPayload, kit: PendingRecoveryKit): void {
	const account = payload.userData
		? {username: payload.userData.username, discriminator: payload.userData.discriminator}
		: usernameFromLogin(kit.login);
	openRecoveryKitModal({
		recoveryKey: kit.recoveryKey,
		createdAt: kit.createdAt,
		username: account.username,
		discriminator: account.discriminator,
		reason: 'recovered',
	});
	rememberRecoveryKit(payload.userId, kit.createdAt);
}

const RecoverAccountPage = observer(function RecoverAccountPage() {
	const {i18n} = useLingui();
	const loginId = useId();
	const keyId = useId();
	const passwordId = useId();
	const confirmPasswordId = useId();
	useFluxerDocumentTitle(i18n._(RECOVER_ACCOUNT_DESCRIPTOR));
	const [fragment] = useState(readRecoveryFragment);
	const [mfaChallenge, setMfaChallenge] = useState<MfaChallenge | null>(null);
	const [pendingKit, setPendingKit] = useState<PendingRecoveryKit | null>(null);
	useEffect(() => {
		clearRecoveryFragment();
	}, []);
	const {form, isLoading, error, fieldErrors} = useAuthForm({
		initialValues: {
			login: fragment.username,
			recovery_key: formatRecoveryKeyInput(fragment.key),
			password: '',
			confirm_password: '',
		},
		onSubmit: async (values) => {
			if (values.password !== values.confirm_password) {
				form.setError('confirm_password', i18n._(PASSWORDS_DO_NOT_MATCH_DESCRIPTOR));
				return false;
			}
			const login = values.login.trim();
			const result = await recoverAccount({login, recoveryKey: values.recovery_key, password: values.password});
			if (result.type === 'mfa') {
				setPendingKit({...result.kit, login});
				setMfaChallenge(result.challenge);
				return false;
			}
			showRecoveredKit(result.payload, {...result.kit, login});
			await AuthenticationCommands.completeLogin(result.payload);
			return undefined;
		},
		firstFieldName: 'login',
	});
	const handleMfaSuccess = useCallback(
		async (payload: LoginSuccessPayload) => {
			if (pendingKit) {
				showRecoveredKit(payload, pendingKit);
			}
			await AuthenticationCommands.completeLogin(payload);
		},
		[pendingKit],
	);
	const handleMfaCancel = useCallback(() => {
		setMfaChallenge(null);
		setPendingKit(null);
	}, []);
	if (mfaChallenge) {
		return (
			<MfaScreen
				challenge={mfaChallenge}
				onSuccess={handleMfaSuccess}
				onCancel={handleMfaCancel}
				data-flx="auth.recover-account-page.mfa-screen"
			/>
		);
	}
	const bannerError = resolveBannerError(error, fieldErrors);
	return (
		<>
			<h1 className={styles.title} data-flx="auth.recover-account-page.title">
				{i18n._(RECOVER_ACCOUNT_DESCRIPTOR)}
			</h1>
			<p className={styles.description} data-flx="auth.recover-account-page.description">
				<Trans>Enter your username and the recovery key from your recovery kit, then choose a new password.</Trans>
			</p>
			{bannerError ? (
				<div className={styles.formError} role="alert" data-flx="auth.recover-account-page.form-error">
					{bannerError}
				</div>
			) : null}
			<form className={styles.form} onSubmit={form.handleSubmit} data-flx="auth.recover-account-page.form.submit">
				<FormField
					id={loginId}
					name="login"
					type="text"
					autoComplete="username"
					autoCapitalize="none"
					autoCorrect="off"
					spellCheck={false}
					required
					label={i18n._(USERNAME_DESCRIPTOR)}
					value={form.getValue('login')}
					onChange={(value) => form.setValue('login', value)}
					error={form.getError('login') || fieldErrors?.get('login')}
					data-flx="auth.recover-account-page.form-field.set-value.login"
				/>
				<FormField
					id={keyId}
					name="recovery_key"
					type="text"
					autoComplete="off"
					autoCapitalize="characters"
					autoCorrect="off"
					spellCheck={false}
					required
					className={styles.keyInput}
					placeholder="XXXX-XXXX-XXXX-XXXX-XXXX-XXXX-XXXX-XXXX"
					label={i18n._(RECOVERY_KEY_DESCRIPTOR)}
					value={form.getValue('recovery_key')}
					onChange={(value) => form.setValue('recovery_key', value)}
					onBlur={() => form.setValue('recovery_key', formatRecoveryKeyInput(form.getValue('recovery_key')))}
					onPaste={(event) => {
						const input = event.currentTarget;
						const current = input.value;
						const start = input.selectionStart ?? current.length;
						const end = input.selectionEnd ?? current.length;
						const pasted = event.clipboardData.getData('text');
						event.preventDefault();
						form.setValue(
							'recovery_key',
							formatRecoveryKeyInput(`${current.slice(0, start)}${pasted}${current.slice(end)}`),
						);
					}}
					error={form.getError('recovery_key') || fieldErrors?.get('recovery_key')}
					data-flx="auth.recover-account-page.form-field.set-value.recovery-key"
				/>
				<FormField
					id={passwordId}
					name="password"
					type="password"
					autoComplete="new-password"
					required
					label={i18n._(NEW_PASSWORD_DESCRIPTOR)}
					value={form.getValue('password')}
					onChange={(value) => form.setValue('password', value)}
					error={form.getError('password') || fieldErrors?.get('password')}
					data-flx="auth.recover-account-page.form-field.set-value.password"
				/>
				<FormField
					id={confirmPasswordId}
					name="confirm_password"
					type="password"
					autoComplete="new-password"
					required
					label={i18n._(CONFIRM_NEW_PASSWORD_DESCRIPTOR)}
					value={form.getValue('confirm_password')}
					onChange={(value) => form.setValue('confirm_password', value)}
					error={form.getError('confirm_password')}
					data-flx="auth.recover-account-page.form-field.set-value.confirm-password"
				/>
				<Button
					type="submit"
					fitContainer
					disabled={isLoading || form.isSubmitting}
					data-flx="auth.recover-account-page.button.submit"
				>
					<Trans>Reset password</Trans>
				</Button>
			</form>
			<div className={styles.footer} data-flx="auth.recover-account-page.footer">
				<p className={styles.hint} data-flx="auth.recover-account-page.no-kit-hint">
					<Trans>No recovery kit? Ask an admin of this instance for a password reset link.</Trans>
				</p>
				<AuthRouterLink to={Routes.LOGIN} className={styles.link} data-flx="auth.recover-account-page.link">
					{i18n._(BACK_TO_SIGN_IN_DESCRIPTOR)}
				</AuthRouterLink>
			</div>
		</>
	);
});

export default RecoverAccountPage;
