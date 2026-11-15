// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {usesUniqueUsernames} from '@app/features/app/utils/AccountIdentityFeatures';
import {USE_A_DIFFERENT_EMAIL_DESCRIPTOR} from '@app/features/auth/AuthMessageDescriptors';
import type {NewAccountProfile} from '@app/features/auth/commands/AccountEntryCommands';
import FormField from '@app/features/auth/flow/AuthFormField';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {
	MAX_USERNAME_LENGTH,
	ONLY_LETTERS_NUMBERS_AND_UNDERSCORES_DESCRIPTOR,
	USERNAME_AVAILABLE_DESCRIPTOR,
	USERNAME_MUST_BE_CHARACTERS_OR_LESS_DESCRIPTOR,
	USERNAME_PATTERN,
	USERNAME_TAKEN_DESCRIPTOR,
} from '@app/features/auth/flow/AuthRegisterFormCore';
import {
	DISPLAY_NAME_DESCRIPTOR,
	NAME_LENGTH_DESCRIPTOR,
	NAME_NOT_ALLOWED_DESCRIPTOR,
} from '@app/features/auth/flow/InstantAccountForm';
import styles from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep.module.css';
import {QuietButton} from '@app/features/auth/flow/QuietButton';
import {type MissingField, SubmitTooltip, shouldDisableSubmit} from '@app/features/auth/flow/SubmitTooltip';
import {getAuthErrorMessage} from '@app/features/auth/hooks/useAuthForm';
import {useUsernameAvailability} from '@app/features/auth/hooks/useUsernameAvailability';
import {isolateBidi} from '@app/features/i18n/utils/BidiIsolate';
import {USERNAME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {failureCode, failureValidationErrors} from '@app/features/platform/utils/ResponseInspection';
import {Button} from '@app/features/ui/button/Button';
import {useUsernameSuggestions} from '@app/features/user/hooks/useUsernameSuggestions';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {makeAutoObservable} from 'mobx';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useEffect, useId, useMemo} from 'react';

export const PROFILE_STEP_HEADING_DESCRIPTOR = msg({
	message: "Welcome! Let's set up your profile",
	comment:
		'Heading of the sign-up step where a new person picks their display name and username, before their account is created.',
});
export const PROFILE_STEP_DESCRIPTION_DESCRIPTOR = msg({
	message: 'This is how other people will see you.',
	comment: 'Text under the heading of the sign-up step where a new person picks their display name and username.',
});
const USERNAME_TAG_HINT_DESCRIPTOR = msg({
	message: 'A number is added after your username, like {example}. You can change both later.',
	comment:
		'Hint under the username field at sign-up on instances where usernames get a number tag. example is the username the person typed followed by a sample tag, such as anna#1234.',
});
const USERNAME_UNIQUE_HINT_DESCRIPTOR = msg({
	message: 'Pick a username nobody else has. You can change it later.',
	comment: 'Hint under the username field at sign-up on instances where every username is unique.',
});
const SIGN_IN_WITH_CODE_NOTE_DESCRIPTOR = msg({
	message: 'You sign in with a code we email to {email}. You can set a password later in Settings, under Account.',
	comment:
		'Note at the bottom of the sign-up profile step. Tells a new person how they sign in next time and where to add a password. email is their address.',
});
const CONTINUE_DESCRIPTOR = msg({
	message: 'Continue',
	comment: 'Button on the sign-up profile step that goes on to the email code.',
});
const SAMPLE_USERNAME_DESCRIPTOR = msg({
	message: 'name',
	comment: 'Placeholder username used in the tag example on the sign-up profile step before the person types one.',
});

const SAMPLE_TAG = '#1234';

interface ProfileErrors {
	readonly globalName: string | null;
	readonly username: string | null;
	readonly other: string | null;
}

const NO_PROFILE_ERRORS: ProfileErrors = {globalName: null, username: null, other: null};

export class IdentifierProfileDraft {
	globalName = '';
	username = '';
	usernameEdited = false;
	confirmed = false;
	errors: ProfileErrors = NO_PROFILE_ERRORS;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get profile(): NewAccountProfile {
		return {globalName: this.globalName.trim(), username: this.username.trim()};
	}

	setGlobalName(value: string): void {
		this.globalName = value;
		this.errors = {...this.errors, globalName: null, other: null};
	}

	setUsername(value: string): void {
		this.username = value;
		this.usernameEdited = true;
		this.errors = {...this.errors, username: null, other: null};
	}

	suggestUsername(value: string): void {
		if (!this.usernameEdited) this.username = value;
	}

	setConfirmed(value: boolean): void {
		this.confirmed = value;
		if (value) this.errors = NO_PROFILE_ERRORS;
	}

	rejectWith(error: unknown, i18n: I18n): void {
		this.confirmed = false;
		this.errors = profileErrors(error, i18n);
	}
}

export function isProfileRejection(error: unknown): boolean {
	const faults = failureValidationErrors(error) ?? [];
	return (
		faults.some((fault) => fault.path === 'username' || fault.path === 'global_name') ||
		failureCode(error) === APIErrorCodes.CONTENT_BLOCKED
	);
}

function profileErrors(error: unknown, i18n: I18n): ProfileErrors {
	const faults = failureValidationErrors(error) ?? [];
	const nameFault = faults.find((fault) => fault.path === 'global_name');
	const usernameFault = faults.find((fault) => fault.path === 'username');
	if (nameFault === undefined && usernameFault === undefined) {
		if (failureCode(error) === APIErrorCodes.CONTENT_BLOCKED) {
			return {...NO_PROFILE_ERRORS, username: i18n._(NAME_NOT_ALLOWED_DESCRIPTOR)};
		}
		return {...NO_PROFILE_ERRORS, other: getAuthErrorMessage(error, i18n)};
	}
	let globalName: string | null = null;
	if (nameFault !== undefined) {
		const tooLong = nameFault.code === ValidationErrorCodes.GLOBAL_NAME_LENGTH_INVALID;
		globalName = i18n._(tooLong ? NAME_LENGTH_DESCRIPTOR : NAME_NOT_ALLOWED_DESCRIPTOR);
	}
	let username: string | null = null;
	if (usernameFault !== undefined) {
		username =
			usernameFault.code === ValidationErrorCodes.USERNAME_ALREADY_TAKEN
				? i18n._(USERNAME_TAKEN_DESCRIPTOR)
				: usernameFault.message || i18n._(NAME_NOT_ALLOWED_DESCRIPTOR);
	}
	return {globalName, username, other: null};
}

interface AuthIdentifierProfileStepProps {
	readonly draft: IdentifierProfileDraft;
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
	readonly email: string;
	readonly heading: ReactNode;
	readonly onContinue: () => void;
	readonly onBack: () => void;
	readonly 'data-flx'?: string;
}

export const AuthIdentifierProfileStep = observer(function AuthIdentifierProfileStep({
	draft,
	runtimeSnapshot,
	email,
	heading,
	onContinue,
	onBack,
	'data-flx': dataFlx = 'auth.flow.identifier-first.profile-step',
}: AuthIdentifierProfileStepProps) {
	const {i18n} = useLingui();
	const fieldId = useId();
	const target = useMemo(() => instanceTargetFromSnapshot(runtimeSnapshot), [runtimeSnapshot]);
	const uniqueUsernames = usesUniqueUsernames(runtimeSnapshot.features);
	const globalName = draft.globalName.trim();
	const username = draft.username.trim();
	const usernameTooLong = username.length > MAX_USERNAME_LENGTH;
	const usernameFormatValid = !usernameTooLong && USERNAME_PATTERN.test(username);
	const {suggestions} = useUsernameSuggestions({
		globalName,
		username: draft.usernameEdited ? draft.username : '',
		target,
	});
	const firstSuggestion = suggestions[0];
	useEffect(() => {
		if (firstSuggestion !== undefined) draft.suggestUsername(firstSuggestion);
	}, [draft, firstSuggestion]);
	const availability = useUsernameAvailability(target, username, uniqueUsernames && usernameFormatValid);

	let usernameHelp: {readonly error: boolean; readonly message: string};
	if (draft.errors.username !== null) {
		usernameHelp = {error: true, message: draft.errors.username};
	} else if (usernameTooLong) {
		usernameHelp = {
			error: true,
			message: i18n._(USERNAME_MUST_BE_CHARACTERS_OR_LESS_DESCRIPTOR, {maxUsernameLength: MAX_USERNAME_LENGTH}),
		};
	} else if (username.length > 0 && !usernameFormatValid) {
		usernameHelp = {error: true, message: i18n._(ONLY_LETTERS_NUMBERS_AND_UNDERSCORES_DESCRIPTOR)};
	} else if (uniqueUsernames && availability === 'taken') {
		usernameHelp = {error: true, message: i18n._(USERNAME_TAKEN_DESCRIPTOR)};
	} else if (uniqueUsernames && availability === 'available') {
		usernameHelp = {error: false, message: i18n._(USERNAME_AVAILABLE_DESCRIPTOR)};
	} else if (uniqueUsernames) {
		usernameHelp = {error: false, message: i18n._(USERNAME_UNIQUE_HINT_DESCRIPTOR)};
	} else {
		const example = `${username.length > 0 ? username : i18n._(SAMPLE_USERNAME_DESCRIPTOR)}${SAMPLE_TAG}`;
		usernameHelp = {error: false, message: i18n._(USERNAME_TAG_HINT_DESCRIPTOR, {example: isolateBidi(example)})};
	}

	const missingFields: Array<MissingField> = [];
	if (globalName.length === 0) missingFields.push({key: 'global_name', label: i18n._(DISPLAY_NAME_DESCRIPTOR)});
	if (username.length === 0) missingFields.push({key: 'username', label: i18n._(USERNAME_DESCRIPTOR)});
	const blocked =
		shouldDisableSubmit(missingFields) ||
		!usernameFormatValid ||
		(uniqueUsernames && availability === 'taken') ||
		draft.errors.username !== null ||
		draft.errors.globalName !== null;

	return (
		<flx-auth-identifier-first className="flx-element" data-flx={dataFlx}>
			{heading}
			<form
				className={styles.form}
				onSubmit={(event) => {
					event.preventDefault();
					if (blocked) return;
					draft.setConfirmed(true);
					onContinue();
				}}
				data-flx={`${dataFlx}.form`}
			>
				<FormField
					id={`${fieldId}-global-name`}
					name="global_name"
					type="text"
					autoComplete="nickname"
					autoCapitalize="words"
					spellCheck={false}
					autoFocus
					data-step-focus="true"
					required
					dir="auto"
					label={i18n._(DISPLAY_NAME_DESCRIPTOR)}
					value={draft.globalName}
					onChange={draft.setGlobalName}
					error={draft.errors.globalName ?? undefined}
					data-flx={`${dataFlx}.global-name`}
				/>
				<div data-flx={`${dataFlx}.username-group`}>
					<FormField
						id={`${fieldId}-username`}
						name="username"
						type="text"
						autoComplete="username"
						autoCapitalize="none"
						autoCorrect="off"
						spellCheck={false}
						required
						label={i18n._(USERNAME_DESCRIPTOR)}
						value={draft.username}
						onChange={draft.setUsername}
						aria-describedby={`${fieldId}-username-help`}
						data-flx={`${dataFlx}.username`}
					/>
					<span
						id={`${fieldId}-username-help`}
						className={usernameHelp.error ? sharedStyles.usernameError : sharedStyles.usernameHint}
						aria-live="polite"
						dir="auto"
						data-flx={`${dataFlx}.username-help`}
					>
						{usernameHelp.message}
					</span>
				</div>
				<p className={styles.hint} dir="auto" data-flx={`${dataFlx}.sign-in-note`}>
					{i18n._(SIGN_IN_WITH_CODE_NOTE_DESCRIPTOR, {email: isolateBidi(email)})}
				</p>
				{draft.errors.other === null ? null : (
					<p className={clsx(styles.hint, styles.hintError)} role="alert" dir="auto" data-flx={`${dataFlx}.error`}>
						{draft.errors.other}
					</p>
				)}
				<SubmitTooltip missingFields={missingFields} data-flx={`${dataFlx}.submit-tooltip`}>
					<Button type="submit" fitContainer disabled={blocked} data-flx={`${dataFlx}.button.continue`}>
						{i18n._(CONTINUE_DESCRIPTOR)}
					</Button>
				</SubmitTooltip>
			</form>
			<div className={styles.quietActions} data-flx={`${dataFlx}.actions`}>
				<QuietButton onClick={onBack} data-flx={`${dataFlx}.quiet-button.back`}>
					{i18n._(USE_A_DIFFERENT_EMAIL_DESCRIPTOR)}
				</QuietButton>
			</div>
		</flx-auth-identifier-first>
	);
});
