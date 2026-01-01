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
import {useId} from 'react';
import FormField from '~/components/auth/FormField';
import {Button} from '~/components/uikit/Button/Button';
import {type LoginSuccessPayload, type MfaChallenge, useMfaController} from '~/hooks/useLoginFlow';
import styles from './MfaScreen.module.css';

interface MfaScreenProps {
	challenge: MfaChallenge;
	inviteCode?: string;
	onSuccess: (payload: LoginSuccessPayload) => Promise<void> | void;
	onCancel: () => void;
}

const MfaScreen = ({challenge, inviteCode, onSuccess, onCancel}: MfaScreenProps) => {
	const {t} = useLingui();
	const codeId = useId();

	const {
		form,
		isLoading,
		fieldErrors,
		selectedMethod,
		setSelectedMethod,
		smsSent,
		handleSendSms,
		handleWebAuthn,
		isWebAuthnLoading,
		supports,
	} = useMfaController({
		ticket: challenge.ticket,
		methods: {sms: challenge.sms, totp: challenge.totp, webauthn: challenge.webauthn},
		inviteCode,
		onLoginSuccess: onSuccess,
	});

	if (!selectedMethod && (supports.sms || supports.webauthn || supports.totp)) {
		return (
			<div className={styles.container}>
				<h1 className={styles.title}>
					<Trans>Two-factor authentication</Trans>
				</h1>
				<p className={styles.description}>
					<Trans>Choose a verification method</Trans>
				</p>
				<div className={styles.buttons}>
					{supports.totp && (
						<Button type="button" fitContainer onClick={() => setSelectedMethod('totp')}>
							<Trans>Authenticator App</Trans>
						</Button>
					)}
					{supports.sms && (
						<Button type="button" fitContainer variant="secondary" onClick={() => setSelectedMethod('sms')}>
							<Trans>SMS Code</Trans>
						</Button>
					)}
					{supports.webauthn && (
						<Button
							type="button"
							fitContainer
							variant="secondary"
							onClick={handleWebAuthn}
							disabled={isWebAuthnLoading}
						>
							<Trans>Security Key / Passkey</Trans>
						</Button>
					)}
				</div>
				<div className={styles.footer}>
					<Button type="button" variant="secondary" onClick={onCancel} className={styles.footerButton}>
						<Trans>Back to login</Trans>
					</Button>
				</div>
			</div>
		);
	}

	return (
		<div className={styles.container}>
			<h1 className={styles.title}>
				<Trans>Two-factor authentication</Trans>
			</h1>
			<p className={styles.description}>
				{selectedMethod === 'sms' ? (
					<Trans>Enter the 6-digit code sent to your phone.</Trans>
				) : (
					<Trans>Enter the 6-digit code from your authenticator app or one of your backup codes.</Trans>
				)}
			</p>
			{selectedMethod === 'sms' && !smsSent && supports.sms && (
				<div className={styles.smsSection}>
					<Button type="button" fitContainer onClick={handleSendSms}>
						<Trans>Send SMS Code</Trans>
					</Button>
				</div>
			)}
			{supports.webauthn && (
				<div className={styles.webauthnSection}>
					<Button type="button" fitContainer variant="secondary" onClick={handleWebAuthn} disabled={isWebAuthnLoading}>
						<Trans>Try security key / passkey instead</Trans>
					</Button>
				</div>
			)}
			<form className={styles.form} onSubmit={form.handleSubmit}>
				<FormField
					id={codeId}
					name="code"
					type="text"
					autoComplete="one-time-code"
					required
					label={t`Code`}
					value={form.getValue('code')}
					onChange={(value) => form.setValue('code', value)}
					error={form.getError('code') || fieldErrors?.code}
				/>
				<Button type="submit" fitContainer disabled={isLoading || form.isSubmitting}>
					<Trans>Log in</Trans>
				</Button>
			</form>
			<div className={styles.footerButtons}>
				{(supports.sms || supports.webauthn || supports.totp) && (
					<Button
						type="button"
						variant="secondary"
						onClick={() => setSelectedMethod(null)}
						className={styles.footerButton}
					>
						<Trans>Try another method</Trans>
					</Button>
				)}
				<Button type="button" variant="secondary" onClick={onCancel} className={styles.footerButton}>
					<Trans>Back to login</Trans>
				</Button>
			</div>
		</div>
	);
};

export default MfaScreen;
