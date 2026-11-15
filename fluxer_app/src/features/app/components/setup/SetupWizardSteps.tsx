// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import {SetupWelcomeGate} from '@app/features/app/components/setup/SetupWelcomeGate';
import {Button} from '@app/features/ui/button/Button';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {LanguageSelector} from '@app/features/user/components/modals/tabs/LanguageTab';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {
	type AccountIdentityLockReason,
	AccountIdentityLockReasons,
	type AccountIdentityMode,
	AccountIdentityModes,
} from '@fluxer/constants/src/AccountIdentityConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const SETUP_LANGUAGE_MENU_MAX_HEIGHT = 220;

export type RegistrationMode = 'open' | 'approval' | 'closed';

const WELCOME_HEADING_DESCRIPTOR = msg({
	message: 'Welcome',
	comment: 'Hidden heading of the first setup wizard screen, read by screen readers while the greeting rotates.',
});
const WELCOME_BODY_DESCRIPTOR = msg({
	message:
		"This sets up your instance, the copy of Fluxer you installed, and creates your account on it. It takes about five minutes. You can change most choices later in the admin panel, and a screen tells you when you can't.",
	comment:
		'Body of the first setup wizard screen. An instance is one installation of Fluxer run by the person reading this.',
});
const LANGUAGE_LABEL_DESCRIPTOR = msg({
	message: 'Language',
	comment: 'Label for the language selector on the first setup wizard screen.',
});

const SIGN_IN_LOCKED_EMAIL_TITLE_DESCRIPTOR = msg({
	message: 'People sign in with their email address',
	comment: 'Setup wizard title when the instance is fixed to email sign-in.',
});
const SIGN_IN_LOCKED_USERNAME_TITLE_DESCRIPTOR = msg({
	message: 'People sign in with a username',
	comment: 'Setup wizard title when the instance is fixed to username sign-in.',
});
const LOCK_ACCOUNTS_EXIST_DESCRIPTOR = msg({
	message:
		"This instance already has accounts, so how people sign in can't change anymore. To use a different sign-in method, start again with an empty database.",
	comment: 'Setup wizard explanation of why the sign-in method is fixed: accounts already exist.',
});
const SIGN_IN_TO_FINISH_TITLE_DESCRIPTOR = msg({
	message: 'Sign in to finish setup',
	comment: 'Setup wizard title shown to someone signed out after the administrator account was created.',
});
const SIGN_IN_TO_FINISH_BODY_DESCRIPTOR = msg({
	message: 'This instance already has an administrator. Sign in with that account to finish setting it up.',
	comment: 'Setup wizard body shown to someone signed out after the administrator account was created.',
});
const LOCK_SETUP_CONFIGURED_DESCRIPTOR = msg({
	message: "Setup was marked as finished in this instance's configuration, so how people sign in can't change anymore.",
	comment: 'Setup wizard explanation of why the sign-in method is fixed: the configuration says setup is done.',
});
const LOCK_EXISTING_INSTANCE_DESCRIPTOR = msg({
	message: 'This instance was upgraded from a version of Fluxer that used email sign-in, so it stays on email.',
	comment: 'Setup wizard explanation of why the sign-in method is fixed: the instance is older than username sign-in.',
});
const LOCK_UNKNOWN_DESCRIPTOR = msg({
	message: "How people sign in on this instance can't change anymore.",
	comment: 'Setup wizard explanation when the sign-in method is fixed for a reason the app does not know.',
});

export const REGISTRATION_CLOSED_NAME_DESCRIPTOR = msg({
	message: 'Only people you invite',
	comment: 'Registration mode option where only people with a registration link can sign up.',
});

export const REGISTRATION_APPROVAL_NAME_DESCRIPTOR = msg({
	message: 'Anyone, after you approve them',
	comment: 'Registration mode option where new accounts wait for an admin to approve them.',
});

export const REGISTRATION_OPEN_NAME_DESCRIPTOR = msg({
	message: 'Anyone',
	comment: 'Registration mode option where anyone can sign up.',
});

export const PUSH_YES_NAME_DESCRIPTOR = msg({
	message: 'Yes, use the push relay',
	comment: 'Setup wizard option that accepts the push relay privacy notice.',
});

export const PUSH_NO_NAME_DESCRIPTOR = msg({
	message: 'Not now',
	comment: 'Setup wizard option that leaves the push relay off.',
});

const DONE_TITLE_DESCRIPTOR = msg({
	message: '{name} is ready',
	comment: 'Title of the last setup wizard screen. name is the instance name.',
});
const DONE_OPEN_DESCRIPTOR = msg({
	message: 'People can sign up at {origin}.',
	comment: 'Last setup wizard screen when anyone can sign up. origin is the instance address.',
});
const DONE_APPROVAL_DESCRIPTOR = msg({
	message: 'New accounts will wait for your approval in the admin panel.',
	comment: 'Last setup wizard screen when new accounts need approval.',
});
const DONE_CLOSED_DESCRIPTOR = msg({
	message: 'To let people in, make a registration link in the admin panel.',
	comment: 'Last setup wizard screen when only invited people can sign up.',
});
const DONE_MORE_DESCRIPTOR = msg({
	message:
		'In the admin panel you can also set up a captcha, Bluesky sign-in, links to your terms and privacy policy, and more.',
	comment: 'Last setup wizard screen, listing what else the admin panel has. Bluesky is a product name.',
});
const DONE_OPEN_APP_DESCRIPTOR = msg({
	message: 'Open {name}',
	comment: 'Button on the last setup wizard screen that opens the app. name is the instance name.',
});
const DONE_OPEN_ADMIN_DESCRIPTOR = msg({
	message: 'Open the admin panel',
	comment: 'Button on the last setup wizard screen that opens the admin panel in a new tab.',
});

interface StepHeaderProps {
	title: string;
	body?: string;
}

export const StepHeader = ({title, body}: StepHeaderProps) => (
	<div className={styles.stepHeader} data-flx="app.self-hosted-setup-wizard-gate.step-header">
		<h1
			className={styles.title}
			tabIndex={-1}
			data-step-focus="true"
			data-flx="app.self-hosted-setup-wizard-gate.title"
		>
			{title}
		</h1>
		{body && (
			<p className={styles.body} data-flx="app.self-hosted-setup-wizard-gate.body">
				{body}
			</p>
		)}
	</div>
);

export const Note = ({children}: {children: React.ReactNode}) => (
	<p className={styles.note} data-flx="app.self-hosted-setup-wizard-gate.note">
		{children}
	</p>
);

export const WelcomeStep = observer(() => {
	const {i18n} = useLingui();
	const currentLocale = LocaleUtils.getCurrentOrDetectedLocale();
	return (
		<section className={styles.welcomeStep} data-flx="app.self-hosted-setup-wizard-gate.welcome-step">
			<h1
				className={styles.visuallyHidden}
				tabIndex={-1}
				data-step-focus="true"
				data-flx="app.self-hosted-setup-wizard-gate.welcome-heading"
			>
				{i18n._(WELCOME_HEADING_DESCRIPTOR)}
			</h1>
			<div
				aria-hidden="true"
				className={styles.welcomeHeroWrap}
				data-flx="app.self-hosted-setup-wizard-gate.welcome-hero-wrap"
			>
				<SetupWelcomeGate
					localeCode={currentLocale}
					data-flx="app.setup.setup-wizard-steps.welcome-step.setup-welcome-gate"
				/>
			</div>
			<p className={styles.welcomeBody} data-flx="app.self-hosted-setup-wizard-gate.welcome-body">
				{i18n._(WELCOME_BODY_DESCRIPTOR)}
			</p>
			<div className={styles.localeBlock} data-flx="app.self-hosted-setup-wizard-gate.locale-block">
				<div className={styles.fieldLabel} data-flx="app.self-hosted-setup-wizard-gate.locale-label">
					{i18n._(LANGUAGE_LABEL_DESCRIPTOR)}
				</div>
				<LanguageSelector
					value={currentLocale}
					onChange={LocaleUtils.setLocalLocale}
					openMenuOnFocus={false}
					maxMenuHeight={SETUP_LANGUAGE_MENU_MAX_HEIGHT}
					menuPlacement="bottom"
					className={styles.localeSelector}
					data-flx="app.setup.setup-wizard-steps.welcome-step.locale-selector"
				/>
			</div>
		</section>
	);
});

function lockReasonDescriptor(reason: AccountIdentityLockReason | null | undefined) {
	switch (reason) {
		case AccountIdentityLockReasons.ACCOUNTS_EXIST:
			return LOCK_ACCOUNTS_EXIST_DESCRIPTOR;
		case AccountIdentityLockReasons.SETUP_CONFIGURED:
			return LOCK_SETUP_CONFIGURED_DESCRIPTOR;
		case AccountIdentityLockReasons.EXISTING_INSTANCE:
			return LOCK_EXISTING_INSTANCE_DESCRIPTOR;
		default:
			return LOCK_UNKNOWN_DESCRIPTOR;
	}
}

export const SignInLockedStep = observer(
	({mode, reason}: {mode: AccountIdentityMode; reason: AccountIdentityLockReason | null | undefined}) => {
		const {i18n} = useLingui();
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.sign-in-locked-step">
				<StepHeader
					title={i18n._(
						mode === AccountIdentityModes.USERNAME
							? SIGN_IN_LOCKED_USERNAME_TITLE_DESCRIPTOR
							: SIGN_IN_LOCKED_EMAIL_TITLE_DESCRIPTOR,
					)}
					body={i18n._(lockReasonDescriptor(reason))}
					data-flx="app.setup.setup-wizard-steps.sign-in-locked-step.step-header"
				/>
			</section>
		);
	},
);

export const SignInToFinishStep = observer(() => {
	const {i18n} = useLingui();
	return (
		<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.sign-in-to-finish-step">
			<StepHeader
				title={i18n._(SIGN_IN_TO_FINISH_TITLE_DESCRIPTOR)}
				body={i18n._(SIGN_IN_TO_FINISH_BODY_DESCRIPTOR)}
				data-flx="app.setup.setup-wizard-steps.sign-in-to-finish-step.step-header"
			/>
		</section>
	);
});

export const DoneStep = observer(
	({
		name,
		registrationMode,
		origin,
		adminUrl,
		onOpen,
	}: {
		name: string;
		registrationMode: RegistrationMode;
		origin: string;
		adminUrl: string | null;
		onOpen: () => void;
	}) => {
		const {i18n} = useLingui();
		let registrationLine: string;
		switch (registrationMode) {
			case 'open':
				registrationLine = i18n._(DONE_OPEN_DESCRIPTOR, {origin});
				break;
			case 'approval':
				registrationLine = i18n._(DONE_APPROVAL_DESCRIPTOR);
				break;
			default:
				registrationLine = i18n._(DONE_CLOSED_DESCRIPTOR);
				break;
		}
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.done-step">
				<CheckCircleIcon
					size={48}
					weight="fill"
					className={styles.doneIcon}
					aria-hidden="true"
					data-flx="app.setup.setup-wizard-steps.done-step.done-icon"
				/>
				<StepHeader
					title={i18n._(DONE_TITLE_DESCRIPTOR, {name})}
					body={registrationLine}
					data-flx="app.setup.setup-wizard-steps.done-step.step-header"
				/>
				<p className={styles.body} data-flx="app.self-hosted-setup-wizard-gate.done-more">
					{i18n._(DONE_MORE_DESCRIPTOR)}
				</p>
				<div className={styles.doneActions} data-flx="app.self-hosted-setup-wizard-gate.done-actions">
					<Button onClick={onOpen} data-flx="app.self-hosted-setup-wizard-gate.done-open">
						{i18n._(DONE_OPEN_APP_DESCRIPTOR, {name})}
					</Button>
					{adminUrl && (
						<Button
							variant="secondary"
							onClick={() => window.open(adminUrl, '_blank', 'noopener,noreferrer')}
							data-flx="app.self-hosted-setup-wizard-gate.done-admin"
						>
							{i18n._(DONE_OPEN_ADMIN_DESCRIPTOR)}
						</Button>
					)}
				</div>
			</section>
		);
	},
);

export const TextButton = ({
	children,
	onClick,
	disabled,
	ariaLabel,
}: {
	children: React.ReactNode;
	onClick: () => void;
	disabled?: boolean;
	ariaLabel?: string;
}) => (
	<FocusRing offset={-2} data-flx="app.self-hosted-setup-wizard-gate.text-button-focus-ring">
		<button
			type="button"
			className={styles.textButton}
			disabled={disabled}
			aria-label={ariaLabel}
			onClick={onClick}
			data-flx="app.self-hosted-setup-wizard-gate.text-button"
		>
			{children}
		</button>
	</FocusRing>
);

export const StepError = ({error}: {error: string | null}) =>
	error ? (
		<p className={styles.alert} role="alert" data-flx="app.self-hosted-setup-wizard-gate.step-error">
			{error}
		</p>
	) : null;

export const SUMMARY_DESCRIPTORS = {
	signIn: msg({message: 'Sign-in', comment: 'Review row label for how people sign in.'}),
	account: msg({message: 'Your account', comment: 'Review row label for the administrator account.'}),
	email: msg({message: 'Email', comment: 'Review row label for the email service.'}),
	name: msg({message: 'Name', comment: 'Review row label for the instance name.'}),
	look: msg({message: 'Look', comment: 'Review row label for the theme color and images.'}),
	communities: msg({message: 'Communities', comment: 'Review row label for one community or many.'}),
	directMessages: msg({message: 'Direct messages', comment: 'Review row label for direct messages.'}),
	personalNotes: msg({message: 'Personal notes', comment: 'Review row label for personal notes.'}),
	registration: msg({message: 'Who can create an account', comment: 'Review row label for the registration mode.'}),
	push: msg({message: 'Phone notifications', comment: 'Review row label for the push relay.'}),
	gif: msg({message: 'GIF search', comment: 'Review row label for GIF search.'}),
	youtube: msg({message: 'YouTube previews', comment: 'Review row label for YouTube link previews.'}),
	attachments: msg({message: 'Old attachments', comment: 'Review row label for automatic attachment expiry.'}),
	usernames: msg({message: 'Usernames', comment: 'Review value when people sign in with a username.'}),
	emailAddressesUnique: msg({
		message: 'Email addresses, every username is unique',
		comment: 'Review value for email sign-in where each username belongs to one person.',
	}),
	emailAddressesShared: msg({
		message: 'Email addresses, usernames get a number',
		comment: 'Review value for email sign-in where usernames get a random number.',
	}),
	emailLater: msg({message: 'Set up later', comment: 'Review value when the email service will be added later.'}),
	emailAlreadySet: msg({message: 'Already set up', comment: 'Review value when an email service was already set up.'}),
	emailFrom: msg({
		message: '{host}, sending as {from}',
		comment: 'Review value for the email service. host is the SMTP server and from is the sender address.',
	}),
	singleCommunity: msg({
		message: 'One community, {name}',
		comment: 'Review value for single community mode. name is the community name.',
	}),
	manyCommunities: msg({message: 'Many communities', comment: 'Review value when people can create communities.'}),
	allowed: msg({message: 'Allowed', comment: 'Review value when a feature is allowed.'}),
	off: msg({message: 'Off', comment: 'Review value when a feature is turned off.'}),
	on: msg({message: 'On', comment: 'Review value when a feature is turned on.'}),
	skipped: msg({message: 'Not set up', comment: 'Review value when an optional API key was left empty.'}),
	deleteOld: msg({message: 'Delete old attachments', comment: 'Review value when old attachments are deleted.'}),
	keepAll: msg({message: 'Keep everything', comment: 'Review value when attachments are kept.'}),
	defaultColor: msg({message: 'Default color', comment: 'Review value when the theme color is the default.'}),
	registrationClosed: REGISTRATION_CLOSED_NAME_DESCRIPTOR,
	registrationApproval: REGISTRATION_APPROVAL_NAME_DESCRIPTOR,
	registrationOpen: REGISTRATION_OPEN_NAME_DESCRIPTOR,
	pushOn: PUSH_YES_NAME_DESCRIPTOR,
	pushOff: PUSH_NO_NAME_DESCRIPTOR,
};
