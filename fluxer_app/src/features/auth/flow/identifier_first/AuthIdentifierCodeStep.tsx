// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	NEW_CODE_SENT_DESCRIPTOR,
	SIGN_IN_WITH_A_PASSKEY_DESCRIPTOR,
	USE_A_DIFFERENT_EMAIL_DESCRIPTOR,
} from '@app/features/auth/AuthMessageDescriptors';
import type {AccountEntryResult} from '@app/features/auth/commands/AccountEntryCommands';
import * as AccountEntryCommands from '@app/features/auth/commands/AccountEntryCommands';
import {EmailCodeField} from '@app/features/auth/flow/EmailCodeField';
import type {AuthIdentifierFirstStepProps} from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep';
import styles from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep.module.css';
import {isProfileRejection} from '@app/features/auth/flow/identifier_first/AuthIdentifierProfileStep';
import {type IdentifierStep, isCompleteEmailCode} from '@app/features/auth/flow/identifier_first/IdentifierFirstState';
import {QuietButton} from '@app/features/auth/flow/QuietButton';
import {
	type RegistrationLegalConsentConfig,
	RegistrationLegalNotice,
} from '@app/features/auth/flow/RegistrationLegalNotice';
import {CREATE_ACCOUNT_DESCRIPTOR, SIGN_IN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {makeAutoObservable} from 'mobx';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';

const SEND_NEW_CODE_DESCRIPTOR = msg({
	message: 'Send a new code',
	comment: 'Quiet text button that emails a fresh sign-in code.',
});
const SEND_NEW_CODE_IN_DESCRIPTOR = msg({
	message: 'Send a new code in {seconds}s',
	comment: 'Disabled text button while a new sign-in code cannot be sent yet. seconds counts down.',
});

export type CodeDeliveryRequest = Pick<Extract<IdentifierStep, {kind: 'code'}>, 'email' | 'ticket' | 'existingAccount'>;

export class IdentifierCodeDraft {
	code = '';
	status: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setCode(value: string): void {
		this.code = value;
	}

	setStatus(value: string | null): void {
		this.status = value;
	}
}

interface AuthIdentifierCodeStepProps
	extends Pick<
		AuthIdentifierFirstStepProps,
		'runtimeSnapshot' | 'inviteCode' | 'passkeyAvailable' | 'passkeyDisabled' | 'onPasskeyLogin'
	> {
	readonly step: Extract<IdentifierStep, {kind: 'code'}>;
	readonly draft: IdentifierCodeDraft;
	readonly registrationUrlCode: string | null;
	readonly heading: ReactNode;
	readonly consentConfig: RegistrationLegalConsentConfig;
	readonly error: string | null;
	readonly submitting: boolean;
	readonly resendSeconds: number;
	readonly run: (work: () => Promise<void>) => Promise<void>;
	readonly finishEntry: (result: AccountEntryResult) => Promise<void>;
	readonly sendCode: (request: CodeDeliveryRequest) => Promise<void>;
	readonly profile: AccountEntryCommands.NewAccountProfile | null;
	readonly onProfileRejected: (error: unknown) => void;
	readonly onBack: () => void;
}

export const AuthIdentifierCodeStep = observer(function AuthIdentifierCodeStep({
	runtimeSnapshot,
	inviteCode,
	passkeyAvailable,
	passkeyDisabled,
	onPasskeyLogin,
	step,
	draft,
	registrationUrlCode,
	heading,
	consentConfig,
	error,
	submitting,
	resendSeconds,
	run,
	finishEntry,
	sendCode,
	profile,
	onProfileRejected,
	onBack,
}: AuthIdentifierCodeStepProps) {
	const {i18n} = useLingui();
	const {code, status, setCode, setStatus} = draft;
	const signingUp = step.purpose === 'sign_up';
	const handleCodeSubmit = (value: string) => {
		if (!isCompleteEmailCode(value)) return;
		const {ticket, purpose} = step;
		const signUpProfile = purpose === 'sign_up' ? profile : null;
		void run(async () => {
			let result: AccountEntryResult;
			try {
				result = await AccountEntryCommands.verifyEmailCode({
					ticket,
					code: value,
					consent: purpose === 'sign_up',
					inviteCode,
					registrationUrlCode,
					profile: signUpProfile,
					snapshot: runtimeSnapshot,
				});
			} catch (caught) {
				if (signUpProfile === null || !isProfileRejection(caught)) throw caught;
				onProfileRejected(caught);
				return;
			}
			await finishEntry(result);
		});
	};

	const handleResend = () => {
		const {email: address, ticket, existingAccount} = step;
		void run(async () => {
			await sendCode({email: address, ticket, existingAccount});
			setStatus(i18n._(NEW_CODE_SENT_DESCRIPTOR));
		});
	};

	return (
		<flx-auth-identifier-first className="flx-element" data-flx="auth.flow.identifier-first.code-step">
			{heading}
			<form
				className={styles.form}
				onSubmit={(event) => {
					event.preventDefault();
					handleCodeSubmit(code);
				}}
				data-flx="auth.flow.identifier-first.code-form"
			>
				<EmailCodeField
					value={code}
					error={error}
					disabled={submitting}
					onChange={setCode}
					onComplete={signingUp ? undefined : handleCodeSubmit}
					data-flx="auth.flow.identifier-first.email-code-field"
				/>
				{status === null ? null : (
					<p className={styles.hint} role="status" data-flx="auth.flow.identifier-first.status">
						{status}
					</p>
				)}
				{signingUp ? (
					<RegistrationLegalNotice
						config={consentConfig}
						data-flx="auth.flow.identifier-first.auth-identifier-first-step.registration-legal-notice"
					/>
				) : null}
				<Button
					type="submit"
					fitContainer
					submitting={submitting}
					disabled={!isCompleteEmailCode(code)}
					data-flx="auth.flow.identifier-first.button.verify-code"
				>
					{i18n._(signingUp ? CREATE_ACCOUNT_DESCRIPTOR : SIGN_IN_DESCRIPTOR)}
				</Button>
			</form>
			<div className={styles.quietActions} data-flx="auth.flow.identifier-first.code-actions">
				<QuietButton
					onClick={handleResend}
					disabled={submitting || resendSeconds > 0}
					data-flx="auth.flow.identifier-first.auth-identifier-first-step.quiet-button.resend"
				>
					{resendSeconds > 0
						? i18n._(SEND_NEW_CODE_IN_DESCRIPTOR, {seconds: resendSeconds})
						: i18n._(SEND_NEW_CODE_DESCRIPTOR)}
				</QuietButton>
				{step.existingAccount && passkeyAvailable ? (
					<QuietButton
						onClick={onPasskeyLogin}
						disabled={passkeyDisabled}
						data-flx="auth.flow.identifier-first.auth-identifier-first-step.quiet-button.passkey-login"
					>
						{i18n._(SIGN_IN_WITH_A_PASSKEY_DESCRIPTOR)}
					</QuietButton>
				) : null}
				<QuietButton
					onClick={onBack}
					data-flx="auth.flow.identifier-first.auth-identifier-first-step.quiet-button.go-to"
				>
					{i18n._(USE_A_DIFFERENT_EMAIL_DESCRIPTOR)}
				</QuietButton>
			</div>
		</flx-auth-identifier-first>
	);
});
