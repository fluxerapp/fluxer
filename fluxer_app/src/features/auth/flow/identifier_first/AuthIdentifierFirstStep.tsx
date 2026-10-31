// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import type {AccountEntryResult} from '@app/features/auth/commands/AccountEntryCommands';
import * as AccountEntryCommands from '@app/features/auth/commands/AccountEntryCommands';
import {AgeBlockedNotice} from '@app/features/auth/components/AgeBlockedNotice';
import loginStyles from '@app/features/auth/components/pages/LoginPage.module.css';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {NO_INSTANT_ACCOUNT_ERRORS} from '@app/features/auth/flow/InstantAccountForm';
import {
	AuthIdentifierCodeStep,
	type CodeDeliveryRequest,
	IdentifierCodeDraft,
} from '@app/features/auth/flow/identifier_first/AuthIdentifierCodeStep';
import {AuthIdentifierEmailStep} from '@app/features/auth/flow/identifier_first/AuthIdentifierEmailStep';
import styles from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep.module.css';
import {
	AuthIdentifierNameStep,
	IdentifierNameDraft,
} from '@app/features/auth/flow/identifier_first/AuthIdentifierNameStep';
import {AuthIdentifierPasswordStep} from '@app/features/auth/flow/identifier_first/AuthIdentifierPasswordStep';
import {
	AuthIdentifierProfileStep,
	IdentifierProfileDraft,
	PROFILE_STEP_DESCRIPTION_DESCRIPTOR,
	PROFILE_STEP_HEADING_DESCRIPTOR,
} from '@app/features/auth/flow/identifier_first/AuthIdentifierProfileStep';
import {
	type IdentifierStep,
	offersInstantAccount,
	parseIsoTimestamp,
} from '@app/features/auth/flow/identifier_first/IdentifierFirstState';
import {usePasskeyAutofill} from '@app/features/auth/flow/identifier_first/usePasskeyAutofill';
import {QuietButton} from '@app/features/auth/flow/QuietButton';
import {getRegistrationLegalConsentConfig} from '@app/features/auth/flow/RegistrationLegalNotice';
import {getAuthErrorMessage} from '@app/features/auth/hooks/useAuthForm';
import {useResendCooldown} from '@app/features/auth/hooks/useResendCooldown';
import {isAgeBlocked, takeAgeBlockedAccountRemoved} from '@app/features/auth/state/AccountEntryStorage';
import type {IpAuthorizationChallenge, LoginSuccessPayload, MfaChallenge} from '@app/features/auth/state/AuthFlow';
import {isolateBidi} from '@app/features/i18n/utils/BidiIsolate';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {MS_PER_SECOND} from '@fluxer/date_utils/src/DateConstants';
import type {GuildEntryResult} from '@fluxer/schema/src/domains/user/AccountSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {type ReactNode, useCallback, useEffect, useId, useMemo, useRef, useState} from 'react';

const EMAIL_CODE_RESEND_FALLBACK_MS = 30 * MS_PER_SECOND;

const SIGN_IN_OR_CREATE_DESCRIPTOR = msg({
	message: 'Sign in or create an account',
	comment: 'Heading of the single sign-in screen. The same screen signs people in and creates new accounts.',
});
const CHECK_YOUR_EMAIL_DESCRIPTOR = msg({
	message: 'Check your email',
	comment: 'Heading of the sign-in step where people type the code we emailed them.',
});
export const CREATE_YOUR_ACCOUNT_DESCRIPTOR = msg({
	message: 'Create your account',
	comment:
		'Heading of the sign-up step where a new person confirms their email with a code, sets a password, or finishes an account started on the website.',
});
export const SIGN_IN_TO_EXISTING_ACCOUNT_DESCRIPTOR = msg({
	message: 'Sign in to an existing account',
	comment: 'Quiet text button under the age notice that goes to sign-in for an account that already exists.',
});
const CODE_SENT_TO_DESCRIPTOR = msg({
	message: 'Enter the code we sent to {email}.',
	comment: 'Text under the heading of the email code step. email is the address the code went to.',
});

const ENTER_YOUR_PASSWORD_DESCRIPTOR = msg({
	message: 'Enter your password',
	comment: 'Heading of the password sign-in step.',
});
const WHAT_SHOULD_WE_CALL_YOU_DESCRIPTOR = msg({
	message: 'What should we call you?',
	comment: 'Heading of the sign-up step without an email. The person types the name other people will see.',
});
const INSTANT_ACCOUNT_HINT_DESCRIPTOR = msg({
	message: 'You can add an email later to sign in on other devices.',
	comment: 'Text under the heading of the sign-up step without an email.',
});
export const PENDING_APPROVAL_DESCRIPTOR = msg({
	message: 'Your account request is pending approval. You can sign in after an admin approves it.',
	comment: 'Notice after creating an account on an instance where admins approve new accounts.',
});

type EntryOutcome = (payload: LoginSuccessPayload, entry: GuildEntryResult | null) => Promise<void>;

export interface AuthIdentifierFirstStepProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
	readonly title: ReactNode | null;
	readonly showTitle: boolean;
	readonly extraTopContent: ReactNode;
	readonly notice: string | null;
	readonly initialEmail: string | null;
	readonly inviteCode: string | null;
	readonly passkeyAvailable: boolean;
	readonly passkeyDisabled: boolean;
	readonly onPasskeyLogin: () => void;
	readonly alternativeActions: ReactNode;
	readonly onSignedIn: EntryOutcome;
	readonly onRequireMfa: (challenge: MfaChallenge) => void;
	readonly onRequireIpAuthorization: (challenge: IpAuthorizationChallenge) => void;
}

export function AuthIdentifierFirstStep({
	runtimeSnapshot,
	title,
	showTitle,
	extraTopContent,
	notice,
	initialEmail,
	inviteCode,
	passkeyAvailable,
	passkeyDisabled,
	onPasskeyLogin,
	alternativeActions,
	onSignedIn,
	onRequireMfa,
	onRequireIpAuthorization,
}: AuthIdentifierFirstStepProps) {
	const {i18n} = useLingui();
	const location = useLocation();
	const fieldId = useId();
	const registrationUrlCode = useMemo(
		() => new URLSearchParams(location.search).get('registration_url')?.trim() || null,
		[location.search],
	);
	const emailCodes = runtimeSnapshot.features.email_codes === true;
	const instantAccounts = offersInstantAccount(runtimeSnapshot.features, registrationUrlCode);
	const consentConfig = useMemo(() => getRegistrationLegalConsentConfig(runtimeSnapshot), [runtimeSnapshot]);
	const target = useMemo(() => instanceTargetFromSnapshot(runtimeSnapshot), [runtimeSnapshot]);
	const [step, setStep] = useState<IdentifierStep>({kind: 'email'});
	const [email, setEmail] = useState(initialEmail ?? '');
	const [codeDraft] = useState(() => new IdentifierCodeDraft());
	const [error, setError] = useState<string | null>(null);
	const [nameDraft] = useState(() => new IdentifierNameDraft());
	const [profileDraft] = useState(() => new IdentifierProfileDraft());
	const [submitting, setSubmitting] = useState(false);
	const [pendingApproval, setPendingApproval] = useState(false);
	const [ageBlocked, setAgeBlocked] = useState(() => isAgeBlocked());
	const [ageBlockedAccountRemoved] = useState(() => takeAgeBlockedAccountRemoved());
	const resendAvailableAt = step.kind === 'code' ? step.resendAvailableAt : null;
	const resendSeconds = useResendCooldown(resendAvailableAt);
	const fieldRef = useRef<HTMLInputElement | null>(null);

	useEffect(() => {
		if (error === null || (step.kind !== 'email' && step.kind !== 'password' && step.kind !== 'create_password')) {
			return;
		}
		fieldRef.current?.focus();
		fieldRef.current?.select();
	}, [error, step.kind]);

	usePasskeyAutofill(step.kind === 'email' && passkeyAvailable, runtimeSnapshot, inviteCode, (payload) =>
		onSignedIn(payload, null),
	);

	const goTo = useCallback(
		(next: IdentifierStep) => {
			setError(null);
			nameDraft.setErrors(NO_INSTANT_ACCOUNT_ERRORS);
			codeDraft.setStatus(null);
			codeDraft.setCode('');
			if (next.kind === 'email') profileDraft.setConfirmed(false);
			setStep(next);
		},
		[codeDraft, nameDraft, profileDraft],
	);

	const run = useCallback(
		async (work: () => Promise<void>) => {
			if (submitting) return;
			setSubmitting(true);
			setError(null);
			try {
				await work();
			} catch (caught) {
				setError(getAuthErrorMessage(caught, i18n));
			} finally {
				setSubmitting(false);
			}
		},
		[i18n, submitting],
	);

	const finishEntry = useCallback(
		async (result: AccountEntryResult) => {
			if (result.type === 'mfa') {
				onRequireMfa(result.challenge);
				return;
			}
			if (result.type === 'pending_approval') {
				setPendingApproval(true);
				return;
			}
			await onSignedIn(result.payload, result.entry);
		},
		[onRequireMfa, onSignedIn],
	);

	const sendCode = useCallback(
		async ({email: address, ticket, existingAccount}: CodeDeliveryRequest) => {
			const sent = await AccountEntryCommands.sendEmailCode(ticket, registrationUrlCode, runtimeSnapshot);
			const now = Date.now();
			goTo({
				kind: sent.purpose === 'sign_up' && !profileDraft.confirmed ? 'profile' : 'code',
				email: address,
				ticket,
				purpose: sent.purpose,
				existingAccount,
				resendAvailableAt: parseIsoTimestamp(sent.resend_available_at, now + EMAIL_CODE_RESEND_FALLBACK_MS),
			});
		},
		[goTo, profileDraft, registrationUrlCode, runtimeSnapshot],
	);

	const renderHeading = (heading: ReactNode, description: ReactNode = null) => (
		<>
			<h1 className={loginStyles.title} dir="auto" data-flx="auth.flow.identifier-first.heading">
				{heading}
			</h1>
			{description === null ? null : (
				<p className={styles.description} dir="auto" data-flx="auth.flow.identifier-first.description">
					{description}
				</p>
			)}
		</>
	);

	if (ageBlocked) {
		return (
			<>
				<AgeBlockedNotice accountDeleted={ageBlockedAccountRemoved} data-flx="auth.flow.identifier-first.age-blocked" />
				<div className={styles.quietActions} data-flx="auth.flow.identifier-first.age-blocked-actions">
					<QuietButton
						onClick={() => {
							setAgeBlocked(false);
							goTo({kind: 'email'});
						}}
						data-flx="auth.flow.identifier-first.age-blocked.sign-in-existing"
					>
						{i18n._(SIGN_IN_TO_EXISTING_ACCOUNT_DESCRIPTOR)}
					</QuietButton>
				</div>
			</>
		);
	}

	if (pendingApproval) {
		return (
			<flx-auth-identifier-first className="flx-element" data-flx="auth.flow.identifier-first.pending">
				{renderHeading(i18n._(CREATE_YOUR_ACCOUNT_DESCRIPTOR))}
				<div className={sharedStyles.consentLine} role="status" data-flx="auth.flow.identifier-first.pending-text">
					{i18n._(PENDING_APPROVAL_DESCRIPTOR)}
				</div>
			</flx-auth-identifier-first>
		);
	}

	if (step.kind === 'profile') {
		return (
			<AuthIdentifierProfileStep
				draft={profileDraft}
				runtimeSnapshot={runtimeSnapshot}
				email={step.email}
				heading={renderHeading(i18n._(PROFILE_STEP_HEADING_DESCRIPTOR), i18n._(PROFILE_STEP_DESCRIPTION_DESCRIPTOR))}
				onContinue={() => {
					setError(null);
					setStep({...step, kind: 'code'});
				}}
				onBack={() => goTo({kind: 'email'})}
				data-flx="auth.flow.identifier-first.profile-step-component"
			/>
		);
	}

	if (step.kind === 'code') {
		const sentTo = i18n._(CODE_SENT_TO_DESCRIPTOR, {email: isolateBidi(step.email)});
		const signingUp = step.purpose === 'sign_up';
		return (
			<AuthIdentifierCodeStep
				step={step}
				draft={codeDraft}
				runtimeSnapshot={runtimeSnapshot}
				inviteCode={inviteCode}
				registrationUrlCode={registrationUrlCode}
				heading={renderHeading(
					i18n._(signingUp ? CREATE_YOUR_ACCOUNT_DESCRIPTOR : CHECK_YOUR_EMAIL_DESCRIPTOR),
					sentTo,
				)}
				consentConfig={consentConfig}
				error={error}
				submitting={submitting}
				resendSeconds={resendSeconds}
				run={run}
				finishEntry={finishEntry}
				sendCode={sendCode}
				profile={profileDraft.confirmed ? profileDraft.profile : null}
				onProfileRejected={(caught) => {
					profileDraft.rejectWith(caught, i18n);
					setStep({...step, kind: 'profile'});
				}}
				passkeyAvailable={passkeyAvailable}
				passkeyDisabled={passkeyDisabled}
				onPasskeyLogin={onPasskeyLogin}
				onBack={() => goTo({kind: 'email'})}
				data-flx="auth.flow.identifier-first.code-step-component"
			/>
		);
	}

	if (step.kind === 'password' || step.kind === 'create_password') {
		const creating = step.kind === 'create_password';
		return (
			<AuthIdentifierPasswordStep
				step={step}
				runtimeSnapshot={runtimeSnapshot}
				target={target}
				inviteCode={inviteCode}
				registrationUrlCode={registrationUrlCode}
				heading={renderHeading(
					i18n._(creating ? CREATE_YOUR_ACCOUNT_DESCRIPTOR : ENTER_YOUR_PASSWORD_DESCRIPTOR),
					step.email,
				)}
				consentConfig={consentConfig}
				fieldId={fieldId}
				fieldRef={fieldRef}
				error={error}
				submitting={submitting}
				run={run}
				onSignedIn={onSignedIn}
				onRequireMfa={onRequireMfa}
				onRequireIpAuthorization={onRequireIpAuthorization}
				onPendingApproval={() => setPendingApproval(true)}
				onBack={() => goTo({kind: 'email'})}
				data-flx="auth.flow.identifier-first.password-step-component"
			/>
		);
	}

	if (step.kind === 'name') {
		return (
			<AuthIdentifierNameStep
				draft={nameDraft}
				runtimeSnapshot={runtimeSnapshot}
				inviteCode={inviteCode}
				registrationUrlCode={registrationUrlCode}
				heading={renderHeading(i18n._(WHAT_SHOULD_WE_CALL_YOU_DESCRIPTOR), i18n._(INSTANT_ACCOUNT_HINT_DESCRIPTOR))}
				consentConfig={consentConfig}
				submitting={submitting}
				setSubmitting={setSubmitting}
				finishEntry={finishEntry}
				onBack={() => goTo({kind: 'email'})}
				data-flx="auth.flow.identifier-first.name-step-component"
			/>
		);
	}

	return (
		<AuthIdentifierEmailStep
			runtimeSnapshot={runtimeSnapshot}
			showTitle={showTitle}
			extraTopContent={extraTopContent}
			heading={showTitle ? renderHeading(title ?? i18n._(SIGN_IN_OR_CREATE_DESCRIPTOR)) : null}
			notice={notice}
			passkeyAvailable={passkeyAvailable}
			passkeyDisabled={passkeyDisabled}
			onPasskeyLogin={onPasskeyLogin}
			alternativeActions={alternativeActions}
			email={email}
			setEmail={setEmail}
			error={error}
			setError={setError}
			submitting={submitting}
			fieldId={fieldId}
			fieldRef={fieldRef}
			emailCodes={emailCodes}
			instantAccounts={instantAccounts}
			registrationUrlCode={registrationUrlCode}
			run={run}
			goTo={goTo}
			sendCode={sendCode}
			onAgeBlocked={() => setAgeBlocked(true)}
			data-flx="auth.flow.identifier-first.email-step-component"
		/>
	);
}
