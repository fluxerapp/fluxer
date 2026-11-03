// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AgeBlockedNotice} from '@app/features/auth/components/AgeBlockedNotice';
import loginStyles from '@app/features/auth/components/pages/LoginPage.module.css';
import {
	type AccountHandoffOutcome,
	accountLabel,
	finishAccountHandoffArrival,
	redeemCapturedAccountHandoff,
} from '@app/features/auth/flow/AccountHandoffRedeem';
import {AuthCardContainer} from '@app/features/auth/flow/AuthCardContainer';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {
	InstantAccountForm,
	instantAccountErrors,
	NO_INSTANT_ACCOUNT_ERRORS,
} from '@app/features/auth/flow/InstantAccountForm';
import {
	CREATE_YOUR_ACCOUNT_DESCRIPTOR,
	SIGN_IN_TO_EXISTING_ACCOUNT_DESCRIPTOR,
} from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep';
import styles from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep.module.css';
import {getRegistrationLegalConsentConfig} from '@app/features/auth/flow/RegistrationLegalNotice';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {getAuthErrorMessage} from '@app/features/auth/hooks/useAuthForm';
import {isAgeBlocked} from '@app/features/auth/state/AccountEntryStorage';
import {clearCapturedAccountHandoff, readCapturedAccountHandoff} from '@app/features/auth/state/AccountHandoffCapture';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {openGuildWhenAvailable} from '@app/features/guild/commands/GuildVisitCommands';
import {isolateBidi} from '@app/features/i18n/utils/BidiIsolate';
import {CONTINUE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button, ButtonVariant} from '@app/features/ui/button/Button';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {LinkBreakIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useMemo, useState} from 'react';

const logger = new Logger('StartPage');

const SETTING_UP_DESCRIPTOR = msg({
	message: 'Setting up your account',
	comment: 'Browser tab title while the app signs in to an account made on the website.',
});
const LINK_EXPIRED_TITLE_DESCRIPTOR = msg({
	message: 'This link has expired',
	comment: 'Heading when the link from the website that signs in a new account was already used or is too old.',
});
const LINK_EXPIRED_TEXT_DESCRIPTOR = msg({
	message: 'Sign in or create an account to continue.',
	comment: 'Text under the expired link heading.',
});
const NEW_ACCOUNT_READY_DESCRIPTOR = msg({
	message: 'Your new account is ready',
	comment: 'Heading when someone made a new account on the website while already signed in to another account.',
});
const NEW_ACCOUNT_CHOICE_DESCRIPTOR = msg({
	message: "You're signed in as {current}. Do you want to switch to {created}?",
	comment: 'Text under the new account heading. current and created are usernames.',
});
const SWITCH_TO_DESCRIPTOR = msg({
	message: 'Switch to {username}',
	comment: 'Button that switches to the account just made on the website. username is its name.',
});
const STAY_AS_DESCRIPTOR = msg({
	message: 'Stay as {username}',
	comment: 'Button that keeps the account already signed in. username is its name.',
});

type StartArrival = Exclude<AccountHandoffOutcome, {readonly kind: 'signed_in'}>;

type StartState =
	| {readonly kind: 'details'}
	| {readonly kind: 'expired'}
	| {readonly kind: 'failed'; readonly message: string}
	| {readonly kind: 'age_blocked'}
	| Extract<AccountHandoffOutcome, {readonly kind: 'choose'}>;

function startStateFor(outcome: StartArrival, i18n: I18n): StartState {
	switch (outcome.kind) {
		case 'retry':
			return {kind: 'details'};
		case 'failed':
			return {kind: 'failed', message: getAuthErrorMessage(outcome.error, i18n)};
		default:
			return outcome;
	}
}

const StartPageContent = observer(function StartPageContent({
	runtimeSnapshot,
	arrival,
}: {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
	readonly arrival: StartArrival | null;
}) {
	const {i18n} = useLingui();
	const consentConfig = useMemo(() => getRegistrationLegalConsentConfig(runtimeSnapshot), [runtimeSnapshot]);
	const [initialName] = useState(() => readCapturedAccountHandoff()?.name ?? '');
	const [state, setState] = useState<StartState>(() => {
		if (arrival !== null) return startStateFor(arrival, i18n);
		if (readCapturedAccountHandoff() === null) return {kind: 'expired'};
		if (isAgeBlocked()) {
			clearCapturedAccountHandoff();
			return {kind: 'age_blocked'};
		}
		return {kind: 'details'};
	});
	const [submitting, setSubmitting] = useState(false);
	const [errors, setErrors] = useState(() =>
		arrival?.kind === 'retry' ? instantAccountErrors(arrival.error, i18n) : NO_INSTANT_ACCOUNT_ERRORS,
	);
	const [switching, setSwitching] = useState(false);

	const redeem = async (globalName: string) => {
		setSubmitting(true);
		setErrors(NO_INSTANT_ACCOUNT_ERRORS);
		const outcome = await redeemCapturedAccountHandoff(globalName, runtimeSnapshot);
		setSubmitting(false);
		if (outcome.kind === 'signed_in') return;
		if (outcome.kind === 'retry') {
			setErrors(instantAccountErrors(outcome.error, i18n));
			return;
		}
		setState(startStateFor(outcome, i18n));
	};

	if (state.kind === 'age_blocked') {
		return (
			<>
				<AgeBlockedNotice data-flx="auth.start-page.age-blocked" />
				<div className={styles.quietActions} data-flx="auth.start-page.age-blocked-actions">
					<AuthRouterLink
						to={Routes.LOGIN}
						className={styles.textButton}
						data-flx="auth.start-page.link.sign-in-existing"
					>
						{i18n._(SIGN_IN_TO_EXISTING_ACCOUNT_DESCRIPTOR)}
					</AuthRouterLink>
				</div>
			</>
		);
	}
	if (state.kind === 'expired' || state.kind === 'failed') {
		return (
			<AuthErrorState
				icon={LinkBreakIcon}
				title={state.kind === 'failed' ? state.message : i18n._(LINK_EXPIRED_TITLE_DESCRIPTOR)}
				text={i18n._(LINK_EXPIRED_TEXT_DESCRIPTOR)}
				action={
					<AuthRouterLink to={Routes.LOGIN} className={styles.textButton} data-flx="auth.start-page.link.login">
						{i18n._(CONTINUE_DESCRIPTOR)}
					</AuthRouterLink>
				}
				data-flx="auth.start-page.expired"
			/>
		);
	}
	if (state.kind === 'details') {
		const details = (
			<flx-auth-start-page className="flx-element" data-flx="auth.start-page.details">
				<h1 className={loginStyles.title} dir="auto" data-flx="auth.start-page.details-title">
					{i18n._(CREATE_YOUR_ACCOUNT_DESCRIPTOR)}
				</h1>
				<InstantAccountForm
					initialName={initialName}
					consentConfig={consentConfig}
					submitting={submitting}
					errors={errors}
					onEdit={() => setErrors(NO_INSTANT_ACCOUNT_ERRORS)}
					onSubmit={(globalName) => void redeem(globalName)}
					data-flx="auth.start-page.details-form"
				/>
			</flx-auth-start-page>
		);
		if (!isMobileExperienceEnabled()) return details;
		return (
			<AuthCardContainer variant={AuthCardVariant.COMPACT} data-flx="auth.start-page.details-mobile-card">
				{details}
			</AuthCardContainer>
		);
	}
	const created = accountLabel(state.payload.userData, state.payload.userId);
	const handleSwitch = async () => {
		setSwitching(true);
		try {
			await AuthenticationCommands.completeLogin({...state.payload, runtimeSnapshot}, {redirectPath: Routes.ME});
			finishAccountHandoffArrival();
			if (state.entry !== null) void openGuildWhenAvailable(state.entry.guild_id);
		} catch (error) {
			logger.warn('Switching to the new account failed', error);
			setState({kind: 'failed', message: getAuthErrorMessage(error, i18n)});
		} finally {
			setSwitching(false);
		}
	};
	const handleStay = async () => {
		setSwitching(true);
		try {
			await SessionManager.addStoredAccount({
				userId: state.payload.userId,
				token: state.payload.token,
				userData: state.payload.userData,
				instance: runtimeSnapshot,
			});
		} catch (error) {
			logger.warn('Keeping the new account in the account switcher failed', error);
		} finally {
			setSwitching(false);
		}
		finishAccountHandoffArrival();
	};
	const choice = (
		<flx-auth-start-page className="flx-element" data-flx="auth.start-page.choose">
			<h1 className={loginStyles.title} dir="auto" data-flx="auth.start-page.title">
				{i18n._(NEW_ACCOUNT_READY_DESCRIPTOR)}
			</h1>
			<p className={styles.description} dir="auto" data-flx="auth.start-page.description">
				{i18n._(NEW_ACCOUNT_CHOICE_DESCRIPTOR, {current: isolateBidi(state.current), created: isolateBidi(created)})}
			</p>
			<div className={styles.form} data-flx="auth.start-page.actions">
				<Button fitContainer submitting={switching} onClick={handleSwitch} data-flx="auth.start-page.button.switch">
					{i18n._(SWITCH_TO_DESCRIPTOR, {username: isolateBidi(created)})}
				</Button>
				<Button
					fitContainer
					variant={ButtonVariant.SECONDARY}
					disabled={switching}
					onClick={() => void handleStay()}
					data-flx="auth.start-page.button.stay"
				>
					{i18n._(STAY_AS_DESCRIPTOR, {username: isolateBidi(state.current)})}
				</Button>
			</div>
		</flx-auth-start-page>
	);
	if (!isMobileExperienceEnabled()) return choice;
	return (
		<AuthCardContainer variant={AuthCardVariant.COMPACT} data-flx="auth.start-page.mobile-card">
			{choice}
		</AuthCardContainer>
	);
});

const StartPage = observer(function StartPage({arrival}: {readonly arrival: StartArrival | null}) {
	const {i18n} = useLingui();
	useFluxerDocumentTitle(i18n._(SETTING_UP_DESCRIPTOR));
	useAuthPresentation({variant: AuthCardVariant.COMPACT});
	return (
		<AuthRuntimeTargetGate data-flx="auth.start-page.runtime-target-gate">
			{(runtimeSnapshot) => (
				<StartPageContent runtimeSnapshot={runtimeSnapshot} arrival={arrival} data-flx="auth.start-page.content" />
			)}
		</AuthRuntimeTargetGate>
	);
});

export default StartPage;
