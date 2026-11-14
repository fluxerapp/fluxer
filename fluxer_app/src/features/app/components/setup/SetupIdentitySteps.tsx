// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import type {SetupIdentityDraft} from '@app/features/app/components/setup/SetupIdentityDraft';
import {setupTokenViolation} from '@app/features/app/components/setup/SetupTokenStep';
import {setSetupAccountIdentity} from '@app/features/app/components/setup/SetupWizardClient';
import type {EmailDeliveryAnswer} from '@app/features/app/components/setup/SetupWizardStateMachine';
import {Note, StepError, StepHeader} from '@app/features/app/components/setup/SetupWizardSteps';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import * as FormUtils from '@app/lib/forms';
import {
	type AccountIdentityMode,
	AccountIdentityModes,
	type TagStyle,
	TagStyles,
} from '@fluxer/constants/src/AccountIdentityConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {RefObject} from 'react';

const logger = new Logger('SelfHostedSetupWizardGate');

const SIGN_IN_TITLE_DESCRIPTOR = msg({
	message: 'How will people sign in?',
	comment: 'Setup wizard title for choosing whether people sign in with a username or an email address.',
});
const SIGN_IN_USERNAME_NAME_DESCRIPTOR = msg({
	message: 'With a username',
	comment: 'Setup wizard option where people sign in with a username and a password.',
});
const SIGN_IN_USERNAME_DESC_DESCRIPTOR = msg({
	message:
		'People choose a username and a password, and nobody needs an email address. If someone forgets their password, they use the recovery kit they saved, or an admin makes them a reset link.',
	comment: 'Description of the username sign-in option in the setup wizard.',
});
const SIGN_IN_EMAIL_NAME_DESCRIPTOR = msg({
	message: 'With an email address',
	comment: 'Setup wizard option where people sign in with an email address.',
});
const SIGN_IN_EMAIL_DESC_DESCRIPTOR = msg({
	message:
		'People sign up with their email address. To reset forgotten passwords by email, your instance needs an email service with SMTP details, which you can add in a few steps.',
	comment: 'Description of the email sign-in option in the setup wizard. SMTP must not be translated.',
});
const SIGN_IN_FIXED_NOTE_DESCRIPTOR = msg({
	message: "You can't change this after you create your account.",
	comment: 'Note under a setup wizard choice that becomes permanent once the first account exists.',
});

const SHARED_USERNAMES_TITLE_DESCRIPTOR = msg({
	message: 'Can two people have the same username?',
	comment: 'Setup wizard title for choosing whether usernames are unique on an email sign-in instance.',
});
const SHARED_USERNAMES_NO_NAME_DESCRIPTOR = msg({
	message: 'No, every username is unique',
	comment: 'Setup wizard option where each username belongs to one person.',
});
const SHARED_USERNAMES_NO_DESC_DESCRIPTOR = msg({
	message: 'Each username belongs to one person. If a name is taken, people pick another one.',
	comment: 'Description of the unique usernames option in the setup wizard.',
});
const SHARED_USERNAMES_YES_NAME_DESCRIPTOR = msg({
	message: 'Yes, with a number after it',
	comment: 'Setup wizard option where usernames get a random number so several people can share a name.',
});
const SHARED_USERNAMES_YES_DESC_DESCRIPTOR = msg({
	message: 'Fluxer adds a random number to each username, like alex#4821, so many people can use the same name.',
	comment: 'Description of the shared usernames option in the setup wizard. Keep the example alex#4821 as it is.',
});

const EMAIL_DELIVERY_TITLE_DESCRIPTOR = msg({
	message: 'Can your instance send email?',
	comment: 'Setup wizard title asking whether the person has an email service ready for the instance.',
});
const EMAIL_DELIVERY_NOW_NAME_DESCRIPTOR = msg({
	message: 'Yes, I have SMTP details',
	comment: 'Setup wizard option for someone who has SMTP details for an email service. SMTP must not be translated.',
});
const EMAIL_DELIVERY_NOW_DESC_DESCRIPTOR = msg({
	message: "You'll enter them right after you create your account.",
	comment: 'Description of the option to connect an email service during setup.',
});
const EMAIL_DELIVERY_LATER_NAME_DESCRIPTOR = msg({
	message: 'Not yet',
	comment: 'Setup wizard option for someone who has no email service ready.',
});
const EMAIL_DELIVERY_LATER_DESC_DESCRIPTOR = msg({
	message:
		'People can still sign up and sign in. Until you add an email service in the admin panel, nobody can reset a forgotten password, including you.',
	comment: 'Description of the option to set up email later in the setup wizard.',
});

interface IdentityStepActions {
	setupToken: string;
	onAdvance: () => void;
	onError: (message: string) => void;
	onSetupTokenRejected: () => void;
}

interface IdentityStepForm extends IdentityStepActions {
	formRef: RefObject<HTMLFormElement | null>;
	onSubmit: (submit: () => Promise<void> | void) => Promise<void>;
}

async function saveIdentity({
	mode,
	style,
	setupToken,
	i18n,
	onAdvance,
	onError,
	onSetupTokenRejected,
}: IdentityStepActions & {mode: AccountIdentityMode; style: TagStyle; i18n: I18n}): Promise<void> {
	try {
		const saved = await setSetupAccountIdentity(mode, style, setupToken.trim() || undefined);
		await RuntimeConfig.refreshDiscovery();
		RuntimeConfig.applyAccountIdentity(saved.mode, saved.tag_style);
		onAdvance();
	} catch (error) {
		if (setupTokenViolation(error)) {
			onSetupTokenRejected();
			return;
		}
		if (failureCode(error) === APIErrorCodes.ACCOUNT_IDENTITY_LOCKED) {
			await RuntimeConfig.refreshDiscovery().catch((refreshError) => {
				logger.warn('Could not refresh discovery after the sign-in method locked', refreshError);
			});
			return;
		}
		logger.error('Failed to save the sign-in method', error);
		onError(FormUtils.extractErrorMessage(i18n, error));
	}
}

export const SignInMethodStep = observer(
	({
		draft,
		disabled,
		error,
		onClearError,
		formRef,
		setupToken,
		onSubmit,
		onAdvance,
		onError,
		onSetupTokenRejected,
	}: IdentityStepForm & {
		draft: SetupIdentityDraft;
		disabled: boolean;
		error: string | null;
		onClearError: () => void;
	}) => {
		const {i18n} = useLingui();
		const mode = draft.signInMethod;
		const onChange = (mode: AccountIdentityMode) => {
			draft.setSignInMethod(mode);
			onClearError();
		};
		const options: ReadonlyArray<RadioOption<AccountIdentityMode>> = [
			{
				value: AccountIdentityModes.USERNAME,
				name: i18n._(SIGN_IN_USERNAME_NAME_DESCRIPTOR),
				desc: i18n._(SIGN_IN_USERNAME_DESC_DESCRIPTOR),
			},
			{
				value: AccountIdentityModes.EMAIL,
				name: i18n._(SIGN_IN_EMAIL_NAME_DESCRIPTOR),
				desc: i18n._(SIGN_IN_EMAIL_DESC_DESCRIPTOR),
			},
		];
		const submit = () => {
			if (mode === AccountIdentityModes.USERNAME) {
				return saveIdentity({
					mode: AccountIdentityModes.USERNAME,
					style: TagStyles.NONE,
					setupToken,
					i18n,
					onAdvance,
					onError,
					onSetupTokenRejected,
				});
			}
			onAdvance();
			return;
		};
		return (
			<form
				ref={formRef}
				noValidate
				onSubmit={(event) => {
					event.preventDefault();
					void onSubmit(submit);
				}}
				className={styles.step}
				data-flx="app.self-hosted-setup-wizard-gate.sign-in-method-step"
			>
				<StepHeader
					title={i18n._(SIGN_IN_TITLE_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.sign-in-method-step.step-header"
				/>
				<RadioGroup
					options={options}
					value={mode}
					onChange={onChange}
					disabled={disabled}
					aria-label={i18n._(SIGN_IN_TITLE_DESCRIPTOR)}
					data-flx="app.self-hosted-setup-wizard-gate.sign-in-method-radio-group"
				/>
				<Note data-flx="app.setup.setup-wizard-steps.sign-in-method-step.note">
					{i18n._(SIGN_IN_FIXED_NOTE_DESCRIPTOR)}
				</Note>
				<StepError error={error} data-flx="app.setup.setup-wizard-steps.sign-in-method-step.step-error" />
			</form>
		);
	},
);

export const SharedUsernamesStep = observer(
	({
		draft,
		disabled,
		error,
		formRef,
		setupToken,
		onSubmit,
		onAdvance,
		onError,
		onSetupTokenRejected,
	}: IdentityStepForm & {draft: SetupIdentityDraft; disabled: boolean; error: string | null}) => {
		const {i18n} = useLingui();
		const {tagStyle, setTagStyle: onChange} = draft;
		const options: ReadonlyArray<RadioOption<TagStyle>> = [
			{
				value: TagStyles.NONE,
				name: i18n._(SHARED_USERNAMES_NO_NAME_DESCRIPTOR),
				desc: i18n._(SHARED_USERNAMES_NO_DESC_DESCRIPTOR),
			},
			{
				value: TagStyles.RANDOM,
				name: i18n._(SHARED_USERNAMES_YES_NAME_DESCRIPTOR),
				desc: i18n._(SHARED_USERNAMES_YES_DESC_DESCRIPTOR),
			},
		];
		const submit = () =>
			saveIdentity({
				mode: AccountIdentityModes.EMAIL,
				style: tagStyle,
				setupToken,
				i18n,
				onAdvance,
				onError,
				onSetupTokenRejected,
			});
		return (
			<form
				ref={formRef}
				noValidate
				onSubmit={(event) => {
					event.preventDefault();
					void onSubmit(submit);
				}}
				className={styles.step}
				data-flx="app.self-hosted-setup-wizard-gate.shared-usernames-step"
			>
				<StepHeader
					title={i18n._(SHARED_USERNAMES_TITLE_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.shared-usernames-step.step-header"
				/>
				<RadioGroup
					options={options}
					value={tagStyle}
					onChange={onChange}
					disabled={disabled}
					aria-label={i18n._(SHARED_USERNAMES_TITLE_DESCRIPTOR)}
					data-flx="app.self-hosted-setup-wizard-gate.shared-usernames-radio-group"
				/>
				<Note data-flx="app.setup.setup-wizard-steps.shared-usernames-step.note">
					{i18n._(SIGN_IN_FIXED_NOTE_DESCRIPTOR)}
				</Note>
				<StepError error={error} data-flx="app.setup.setup-wizard-steps.shared-usernames-step.step-error" />
			</form>
		);
	},
);

export const EmailDeliveryStep = observer(({draft}: {draft: SetupIdentityDraft}) => {
	const {i18n} = useLingui();
	const {emailAnswer: answer, setEmailAnswer: onChange} = draft;
	const options: ReadonlyArray<RadioOption<EmailDeliveryAnswer>> = [
		{
			value: 'now',
			name: i18n._(EMAIL_DELIVERY_NOW_NAME_DESCRIPTOR),
			desc: i18n._(EMAIL_DELIVERY_NOW_DESC_DESCRIPTOR),
		},
		{
			value: 'later',
			name: i18n._(EMAIL_DELIVERY_LATER_NAME_DESCRIPTOR),
			desc: i18n._(EMAIL_DELIVERY_LATER_DESC_DESCRIPTOR),
		},
	];
	return (
		<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.email-delivery-step">
			<StepHeader
				title={i18n._(EMAIL_DELIVERY_TITLE_DESCRIPTOR)}
				data-flx="app.setup.setup-wizard-steps.email-delivery-step.step-header"
			/>
			<RadioGroup
				options={options}
				value={answer}
				onChange={onChange}
				aria-label={i18n._(EMAIL_DELIVERY_TITLE_DESCRIPTOR)}
				data-flx="app.self-hosted-setup-wizard-gate.email-delivery-radio-group"
			/>
		</section>
	);
});
