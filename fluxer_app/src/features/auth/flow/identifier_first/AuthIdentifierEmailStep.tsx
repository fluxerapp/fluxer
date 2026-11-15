// SPDX-License-Identifier: AGPL-3.0-or-later

import {SIGN_IN_WITH_A_PASSKEY_DESCRIPTOR} from '@app/features/auth/AuthMessageDescriptors';
import * as AccountEntryCommands from '@app/features/auth/commands/AccountEntryCommands';
import loginStyles from '@app/features/auth/components/pages/LoginPage.module.css';
import FormField from '@app/features/auth/flow/AuthFormField';
import type {CodeDeliveryRequest} from '@app/features/auth/flow/identifier_first/AuthIdentifierCodeStep';
import type {AuthIdentifierFirstStepProps} from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep';
import styles from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep.module.css';
import {
	decideAfterIdentify,
	type IdentifierStep,
	needsPasswordStep,
} from '@app/features/auth/flow/identifier_first/IdentifierFirstState';
import {QuietButton} from '@app/features/auth/flow/QuietButton';
import {isAgeBlocked} from '@app/features/auth/state/AccountEntryStorage';
import {CONTINUE_DESCRIPTOR, EMAIL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import type {ReactNode, RefObject} from 'react';

const CONTINUE_WITHOUT_EMAIL_DESCRIPTOR = msg({
	message: 'Continue without email',
	comment: 'Quiet text button on the sign-in screen that creates an account without an email address.',
});
const REGISTRATION_CLOSED_DESCRIPTOR = msg({
	message: "No account uses this email, and this instance isn't taking new accounts right now.",
	comment: 'Error under the email field when the address is new and the instance does not allow sign-ups.',
});

interface AuthIdentifierEmailStepProps
	extends Pick<
		AuthIdentifierFirstStepProps,
		| 'runtimeSnapshot'
		| 'showTitle'
		| 'extraTopContent'
		| 'notice'
		| 'passkeyAvailable'
		| 'passkeyDisabled'
		| 'onPasskeyLogin'
		| 'alternativeActions'
	> {
	readonly heading: ReactNode;
	readonly email: string;
	readonly setEmail: (value: string) => void;
	readonly error: string | null;
	readonly setError: (value: string | null) => void;
	readonly submitting: boolean;
	readonly fieldId: string;
	readonly fieldRef: RefObject<HTMLInputElement | null>;
	readonly emailCodes: boolean;
	readonly instantAccounts: boolean;
	readonly registrationUrlCode: string | null;
	readonly run: (work: () => Promise<void>) => Promise<void>;
	readonly goTo: (step: IdentifierStep) => void;
	readonly sendCode: (request: CodeDeliveryRequest) => Promise<void>;
	readonly onAgeBlocked: () => void;
}

export function AuthIdentifierEmailStep({
	runtimeSnapshot,
	showTitle,
	extraTopContent,
	notice,
	passkeyAvailable,
	passkeyDisabled,
	onPasskeyLogin,
	alternativeActions,
	heading,
	email,
	setEmail,
	error,
	setError,
	submitting,
	fieldId,
	fieldRef,
	emailCodes,
	instantAccounts,
	registrationUrlCode,
	run,
	goTo,
	sendCode,
	onAgeBlocked,
}: AuthIdentifierEmailStepProps) {
	const {i18n} = useLingui();
	const handleEmailSubmit = () =>
		run(async () => {
			const address = email.trim();
			const response = await AccountEntryCommands.identify(address, runtimeSnapshot);
			const decision = decideAfterIdentify(response, emailCodes, registrationUrlCode !== null);
			if (decision.kind === 'registration_closed') {
				setError(i18n._(REGISTRATION_CLOSED_DESCRIPTOR));
				return;
			}
			if (decision.kind === 'password') {
				goTo({kind: 'password', email: address, ticket: response.ticket});
				return;
			}
			if (isAgeBlocked() && response.account === 'new') {
				onAgeBlocked();
				return;
			}
			if (decision.kind === 'create_password') {
				goTo({kind: 'create_password', email: address});
				return;
			}
			try {
				await sendCode({email: address, ticket: response.ticket, existingAccount: response.account === 'existing'});
			} catch (caught) {
				if (!needsPasswordStep(caught)) throw caught;
				goTo({kind: 'password', email: address, ticket: response.ticket});
			}
		});

	return (
		<flx-auth-identifier-first className="flx-element" data-flx="auth.flow.identifier-first.email-step">
			{extraTopContent}
			{heading}
			{notice === null ? null : (
				<div className={loginStyles.loginNotice} role="alert" dir="auto" data-flx="auth.flow.identifier-first.notice">
					{notice}
				</div>
			)}
			<form
				className={clsx(styles.form, !showTitle && styles.formAfterHeader)}
				onSubmit={(event) => {
					event.preventDefault();
					void handleEmailSubmit();
				}}
				autoComplete="on"
				name="login"
				data-flx="auth.flow.identifier-first.email-form"
			>
				<FormField
					id={`${fieldId}-email`}
					name="email"
					type="email"
					autoComplete="username webauthn"
					autoCapitalize="none"
					autoCorrect="off"
					spellCheck={false}
					enterKeyHint="go"
					autoFocus
					data-step-focus="true"
					required
					label={i18n._(EMAIL_DESCRIPTOR)}
					value={email}
					onChange={(value) => {
						setError(null);
						setEmail(value);
					}}
					error={error ?? undefined}
					inputRef={fieldRef}
					data-flx="auth.flow.identifier-first.form-field.email"
				/>
				<Button
					type="submit"
					fitContainer
					submitting={submitting}
					disabled={email.trim().length === 0}
					data-flx="auth.flow.identifier-first.button.continue"
				>
					{i18n._(CONTINUE_DESCRIPTOR)}
				</Button>
			</form>
			<div className={styles.quietActions} data-flx="auth.flow.identifier-first.email-actions">
				{instantAccounts ? (
					<QuietButton
						onClick={() => {
							if (isAgeBlocked()) {
								onAgeBlocked();
								return;
							}
							goTo({kind: 'name'});
						}}
						data-flx="auth.flow.identifier-first.auth-identifier-first-step.quiet-button.go-to--4"
					>
						{i18n._(CONTINUE_WITHOUT_EMAIL_DESCRIPTOR)}
					</QuietButton>
				) : null}
				{passkeyAvailable ? (
					<QuietButton
						onClick={onPasskeyLogin}
						disabled={passkeyDisabled}
						data-flx="auth.flow.identifier-first.auth-identifier-first-step.quiet-button.passkey-login--2"
					>
						{i18n._(SIGN_IN_WITH_A_PASSKEY_DESCRIPTOR)}
					</QuietButton>
				) : null}
			</div>
			{alternativeActions === null ? null : (
				<div className={styles.alternatives} data-flx="auth.flow.identifier-first.alternatives">
					{alternativeActions}
				</div>
			)}
		</flx-auth-identifier-first>
	);
}
