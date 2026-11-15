// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	FORGOT_YOUR_PASSWORD_DESCRIPTOR,
	USE_A_DIFFERENT_EMAIL_DESCRIPTOR,
} from '@app/features/auth/AuthMessageDescriptors';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import FormField from '@app/features/auth/flow/AuthFormField';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import type {AuthIdentifierFirstStepProps} from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep';
import styles from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep.module.css';
import type {IdentifierStep} from '@app/features/auth/flow/identifier_first/IdentifierFirstState';
import {QuietButton} from '@app/features/auth/flow/QuietButton';
import {
	type RegistrationLegalConsentConfig,
	RegistrationLegalNotice,
} from '@app/features/auth/flow/RegistrationLegalNotice';
import {loginWithPassword, toLoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {
	CREATE_ACCOUNT_DESCRIPTOR,
	PASSWORD_DESCRIPTOR,
	SIGN_IN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {InstanceHTTPTarget} from '@app/features/platform/transport/InstanceHTTP';
import {Button} from '@app/features/ui/button/Button';
import {useLingui} from '@lingui/react/macro';
import {type ReactNode, type RefObject, useState} from 'react';

interface AuthIdentifierPasswordStepProps
	extends Pick<
		AuthIdentifierFirstStepProps,
		'runtimeSnapshot' | 'inviteCode' | 'onSignedIn' | 'onRequireMfa' | 'onRequireIpAuthorization'
	> {
	readonly step: Extract<IdentifierStep, {kind: 'password' | 'create_password'}>;
	readonly target: InstanceHTTPTarget;
	readonly registrationUrlCode: string | null;
	readonly heading: ReactNode;
	readonly consentConfig: RegistrationLegalConsentConfig;
	readonly fieldId: string;
	readonly fieldRef: RefObject<HTMLInputElement | null>;
	readonly error: string | null;
	readonly submitting: boolean;
	readonly run: (work: () => Promise<void>) => Promise<void>;
	readonly onPendingApproval: () => void;
	readonly onBack: () => void;
}

export function AuthIdentifierPasswordStep({
	runtimeSnapshot,
	inviteCode,
	onSignedIn,
	onRequireMfa,
	onRequireIpAuthorization,
	step,
	target,
	registrationUrlCode,
	heading,
	consentConfig,
	fieldId,
	fieldRef,
	error,
	submitting,
	run,
	onPendingApproval,
	onBack,
}: AuthIdentifierPasswordStepProps) {
	const {i18n} = useLingui();
	const [password, setPassword] = useState('');
	const creating = step.kind === 'create_password';
	const handlePasswordSubmit = () =>
		run(async () => {
			if (step.kind !== 'password') return;
			const result = await loginWithPassword({
				email: step.email,
				password,
				inviteCode: inviteCode ?? undefined,
				runtimeSnapshot,
			});
			if (result.type === 'mfa') {
				onRequireMfa(result.challenge);
				return;
			}
			if (result.type === 'ip_authorization') {
				onRequireIpAuthorization(result.challenge);
				return;
			}
			if (result.type === 'success') {
				await onSignedIn(result.payload, null);
			}
		});

	const handleCreateWithPassword = () =>
		run(async () => {
			if (step.kind !== 'create_password') return;
			const response = await AuthenticationCommands.register(
				{
					email: step.email,
					password,
					consent: true,
					invite_code: inviteCode ?? undefined,
					registration_url_code: registrationUrlCode ?? undefined,
				},
				target,
			);
			if (AuthenticationCommands.isRegistrationPendingApprovalResponse(response)) {
				onPendingApproval();
				return;
			}
			await onSignedIn(toLoginSuccessPayload(response), null);
		});

	const submitPassword = creating ? handleCreateWithPassword : handlePasswordSubmit;

	return (
		<flx-auth-identifier-first className="flx-element" data-flx="auth.flow.identifier-first.password-step">
			{heading}
			<form
				className={styles.form}
				onSubmit={(event) => {
					event.preventDefault();
					void submitPassword();
				}}
				autoComplete="on"
				name="login"
				data-flx="auth.flow.identifier-first.password-form"
			>
				<input
					type="email"
					name="email"
					autoComplete="username"
					value={step.email}
					readOnly
					hidden
					data-flx="auth.flow.identifier-first.hidden-email"
				/>
				<FormField
					id={`${fieldId}-password`}
					name="password"
					type="password"
					autoComplete={creating ? 'new-password' : 'current-password'}
					enterKeyHint="done"
					autoFocus
					data-step-focus="true"
					required
					label={i18n._(PASSWORD_DESCRIPTOR)}
					value={password}
					onChange={setPassword}
					error={error ?? undefined}
					inputRef={fieldRef}
					data-flx="auth.flow.identifier-first.form-field.password"
				/>
				{creating ? (
					<RegistrationLegalNotice
						config={consentConfig}
						data-flx="auth.flow.identifier-first.auth-identifier-first-step.registration-legal-notice--2"
					/>
				) : null}
				<Button
					type="submit"
					fitContainer
					submitting={submitting}
					disabled={password.length === 0}
					data-flx="auth.flow.identifier-first.button.password-submit"
				>
					{i18n._(creating ? CREATE_ACCOUNT_DESCRIPTOR : SIGN_IN_DESCRIPTOR)}
				</Button>
			</form>
			<div className={styles.quietActions} data-flx="auth.flow.identifier-first.password-actions">
				{!creating && runtimeSnapshot.features.emails_enabled ? (
					<AuthRouterLink
						to="/forgot"
						search={{email: step.email}}
						className={styles.textButton}
						data-flx="auth.flow.identifier-first.link.forgot"
					>
						{i18n._(FORGOT_YOUR_PASSWORD_DESCRIPTOR)}
					</AuthRouterLink>
				) : null}
				<QuietButton
					onClick={onBack}
					data-flx="auth.flow.identifier-first.auth-identifier-first-step.quiet-button.go-to--2"
				>
					{i18n._(USE_A_DIFFERENT_EMAIL_DESCRIPTOR)}
				</QuietButton>
			</div>
		</flx-auth-identifier-first>
	);
}
