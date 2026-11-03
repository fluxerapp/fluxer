// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import {
	Note,
	PUSH_NO_NAME_DESCRIPTOR,
	PUSH_YES_NAME_DESCRIPTOR,
	REGISTRATION_APPROVAL_NAME_DESCRIPTOR,
	REGISTRATION_CLOSED_NAME_DESCRIPTOR,
	REGISTRATION_OPEN_NAME_DESCRIPTOR,
	type RegistrationMode,
	StepHeader,
} from '@app/features/app/components/setup/SetupWizardSteps';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {
	createContext,
	type Dispatch,
	type ReactNode,
	type RefObject,
	type SetStateAction,
	useCallback,
	useContext,
	useMemo,
	useRef,
	useState,
} from 'react';

const PUSH_RELAY_NOTICE_URL = 'https://fluxer.com/push-relay';
const MIN_COMMUNITY_NAME_LENGTH = 1;
const MAX_COMMUNITY_NAME_LENGTH = 100;

const COMMUNITIES_TITLE_DESCRIPTOR = msg({
	message: 'Is this instance for one community or many?',
	comment: 'Setup wizard title for choosing between many communities and a single community.',
});
const COMMUNITIES_MANY_NAME_DESCRIPTOR = msg({
	message: 'Many communities',
	comment: 'Setup wizard option where people can create and join many communities.',
});
const COMMUNITIES_MANY_DESC_DESCRIPTOR = msg({
	message: 'People can create their own communities and join as many as they like.',
	comment: 'Description of the many communities option in the setup wizard.',
});
const COMMUNITIES_ONE_NAME_DESCRIPTOR = msg({
	message: 'One community',
	comment: 'Setup wizard option where the whole instance is one community.',
});
const COMMUNITIES_ONE_DESC_DESCRIPTOR = msg({
	message:
		'Everyone who signs up joins one community that you own, and nobody can create others. This suits a club, a team or a company.',
	comment: 'Description of the single community option in the setup wizard.',
});
const COMMUNITY_NAME_LABEL_DESCRIPTOR = msg({
	message: 'Community name',
	comment: 'Label for the name of the single community in the setup wizard.',
});
const COMMUNITY_NAME_ERROR_DESCRIPTOR = msg({
	message: 'Enter a community name between 1 and 100 characters.',
	comment: 'Error under the community name field in the setup wizard.',
});
const COMMUNITIES_NOTE_DESCRIPTOR = msg({
	message: 'You can switch between one community and many communities later in the admin panel.',
	comment: 'Note under the community choice in the setup wizard, shown when one community is chosen.',
});

const DMS_TITLE_DESCRIPTOR = msg({
	message: 'Can people send each other direct messages?',
	comment: 'Setup wizard title for choosing whether direct messages are allowed.',
});
const DMS_YES_DESC_DESCRIPTOR = msg({
	message: 'People can send friend requests and direct messages.',
	comment: 'Description of allowing direct messages in the setup wizard.',
});
const DMS_NO_DESC_DESCRIPTOR = msg({
	message: 'People can only talk in communities.',
	comment: 'Description of turning off direct messages in the setup wizard.',
});
const DMS_NOTE_DESCRIPTOR = msg({
	message: 'You can turn direct messages and friend requests back on later in the admin panel.',
	comment: 'Warning shown when direct messages are turned off in the setup wizard.',
});
const PERSONAL_NOTES_TITLE_DESCRIPTOR = msg({
	message: 'Should people have personal notes?',
	comment: 'Setup wizard title for choosing whether people can use personal notes.',
});
const PERSONAL_NOTES_YES_DESC_DESCRIPTOR = msg({
	message: 'Each person gets a private channel where they can write messages to themselves.',
	comment: 'Description of turning on personal notes in the setup wizard.',
});
const PERSONAL_NOTES_NO_DESC_DESCRIPTOR = msg({
	message: 'Nobody can open or write personal notes.',
	comment: 'Description of leaving personal notes off in the setup wizard.',
});
const PERSONAL_NOTES_NOTE_DESCRIPTOR = msg({
	message: 'You can turn personal notes on later in the admin panel.',
	comment: 'Note shown when personal notes are left off in the setup wizard.',
});
const YES_DESCRIPTOR = msg({
	message: 'Yes',
	comment: 'Answer to a yes or no question in the setup wizard.',
});
const NO_DESCRIPTOR = msg({
	message: 'No',
	comment: 'Answer to a yes or no question in the setup wizard.',
});

const REGISTRATION_TITLE_DESCRIPTOR = msg({
	message: 'Who can create an account?',
	comment: 'Setup wizard title for the registration mode.',
});
const REGISTRATION_CLOSED_DESC_DESCRIPTOR = msg({
	message: 'Nobody can sign up on their own. You make registration links in the admin panel and send them to people.',
	comment: 'Description of the invite-only registration mode in the setup wizard.',
});
const REGISTRATION_APPROVAL_DESC_DESCRIPTOR = msg({
	message: 'People can sign up, and each new account waits until an admin approves it in the admin panel.',
	comment: 'Description of the approval registration mode in the setup wizard.',
});
const REGISTRATION_OPEN_DESC_DESCRIPTOR = msg({
	message: 'Anyone who can open your instance can sign up.',
	comment: 'Description of the open registration mode in the setup wizard.',
});
const REGISTRATION_OPEN_NOTE_DESCRIPTOR = msg({
	message: 'Spam accounts can sign up too. You can turn on a captcha in the admin panel.',
	comment: 'Note shown when open registration is chosen in the setup wizard.',
});

const PUSH_TITLE_DESCRIPTOR = msg({
	message: 'Send notifications to the Fluxer mobile apps?',
	comment: 'Setup wizard title for the push relay consent.',
});
const PUSH_BODY_DESCRIPTOR = msg({
	message:
		'The Fluxer apps for iOS and Android get notifications through a push relay that Fluxer runs, which passes them to Apple and Google. To use it, you accept the push relay privacy notice.',
	comment: 'Setup wizard body explaining the push relay.',
});
const PUSH_NOTICE_LINK_DESCRIPTOR = msg({
	message: 'Read the push relay privacy notice',
	comment: 'Link to the push relay privacy notice in the setup wizard.',
});
const PUSH_NO_DESC_DESCRIPTOR = msg({
	message:
		"People using the Fluxer mobile apps won't get notifications from your instance. You can turn this on later in the admin panel.",
	comment: 'Description of leaving the push relay off in the setup wizard.',
});

export interface FinishDraft {
	registrationMode: RegistrationMode;
	singleCommunityEnabled: boolean;
	singleCommunityName: string;
	directMessagesDisabled: boolean;
	personalNotesEnabled: boolean;
	pushRelayConsentAccepted: boolean;
}

interface PolicyDraftState {
	draft: FinishDraft;
	setDraft: Dispatch<SetStateAction<FinishDraft>>;
	updateDraft: (patch: Partial<FinishDraft>) => void;
}

const PolicyDraftContext = createContext<PolicyDraftState | null>(null);

export function SetupPolicyDraftProvider({children}: {children: ReactNode}) {
	const [draft, setDraft] = useState<FinishDraft>({
		registrationMode: 'closed',
		singleCommunityEnabled: false,
		singleCommunityName: '',
		directMessagesDisabled: false,
		personalNotesEnabled: false,
		pushRelayConsentAccepted: false,
	});
	const updateDraft = useCallback((patch: Partial<FinishDraft>) => {
		setDraft((current) => ({...current, ...patch}));
	}, []);
	const value = useMemo(() => ({draft, setDraft, updateDraft}), [draft, updateDraft]);
	return <PolicyDraftContext.Provider value={value}>{children}</PolicyDraftContext.Provider>;
}

export function useSetupPolicyDraft(): PolicyDraftState {
	const state = useContext(PolicyDraftContext);
	if (state === null) throw new Error('Setup policy draft provider is missing');
	return state;
}

function isValidCommunityName(value: string): boolean {
	const length = value.trim().length;
	return length >= MIN_COMMUNITY_NAME_LENGTH && length <= MAX_COMMUNITY_NAME_LENGTH;
}

export const CommunitiesStep = observer(
	({
		formRef,
		showErrors,
		onShowErrors,
		onSubmit,
		onAdvance,
	}: {
		formRef: RefObject<HTMLFormElement | null>;
		showErrors: boolean;
		onShowErrors: (show: boolean) => void;
		onSubmit: (submit: () => void) => Promise<void>;
		onAdvance: () => void;
	}) => {
		const {i18n} = useLingui();
		const {draft, updateDraft} = useSetupPolicyDraft();
		const singleCommunity = draft.singleCommunityEnabled;
		const communityName = draft.singleCommunityName;
		const nameInputRef = useRef<HTMLInputElement | null>(null);
		const continueWithCommunity = () => {
			onShowErrors(true);
			if (!draft.singleCommunityEnabled || isValidCommunityName(draft.singleCommunityName)) {
				onAdvance();
			} else {
				nameInputRef.current?.focus();
			}
		};
		const options: ReadonlyArray<RadioOption<'many' | 'one'>> = [
			{
				value: 'many',
				name: i18n._(COMMUNITIES_MANY_NAME_DESCRIPTOR),
				desc: i18n._(COMMUNITIES_MANY_DESC_DESCRIPTOR),
			},
			{value: 'one', name: i18n._(COMMUNITIES_ONE_NAME_DESCRIPTOR), desc: i18n._(COMMUNITIES_ONE_DESC_DESCRIPTOR)},
		];
		return (
			<form
				ref={formRef}
				noValidate
				onSubmit={(event) => {
					event.preventDefault();
					void onSubmit(continueWithCommunity);
				}}
				className={styles.step}
				data-flx="app.self-hosted-setup-wizard-gate.communities-step"
			>
				<StepHeader
					title={i18n._(COMMUNITIES_TITLE_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.communities-step.step-header"
				/>
				<RadioGroup
					options={options}
					value={singleCommunity ? 'one' : 'many'}
					onChange={(value) => updateDraft({singleCommunityEnabled: value === 'one'})}
					aria-label={i18n._(COMMUNITIES_TITLE_DESCRIPTOR)}
					data-flx="app.self-hosted-setup-wizard-gate.communities-radio-group"
				/>
				{singleCommunity && (
					<Input
						ref={nameInputRef}
						label={i18n._(COMMUNITY_NAME_LABEL_DESCRIPTOR)}
						value={communityName}
						maxLength={MAX_COMMUNITY_NAME_LENGTH}
						autoComplete="off"
						error={
							showErrors && !isValidCommunityName(communityName) ? i18n._(COMMUNITY_NAME_ERROR_DESCRIPTOR) : undefined
						}
						onChange={(event) => updateDraft({singleCommunityName: event.target.value})}
						data-flx="app.self-hosted-setup-wizard-gate.community-name-input"
					/>
				)}
				{singleCommunity && (
					<Note data-flx="app.setup.setup-wizard-steps.communities-step.note">
						{i18n._(COMMUNITIES_NOTE_DESCRIPTOR)}
					</Note>
				)}
			</form>
		);
	},
);

export const DirectMessagesStep = observer(() => {
	const {i18n} = useLingui();
	const {
		draft: {directMessagesDisabled: disabled},
		updateDraft,
	} = useSetupPolicyDraft();
	const options: ReadonlyArray<RadioOption<'yes' | 'no'>> = [
		{value: 'yes', name: i18n._(YES_DESCRIPTOR), desc: i18n._(DMS_YES_DESC_DESCRIPTOR)},
		{value: 'no', name: i18n._(NO_DESCRIPTOR), desc: i18n._(DMS_NO_DESC_DESCRIPTOR)},
	];
	return (
		<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.direct-messages-step">
			<StepHeader
				title={i18n._(DMS_TITLE_DESCRIPTOR)}
				data-flx="app.setup.setup-wizard-steps.direct-messages-step.step-header"
			/>
			<RadioGroup
				options={options}
				value={disabled ? 'no' : 'yes'}
				onChange={(value) => updateDraft({directMessagesDisabled: value === 'no'})}
				aria-label={i18n._(DMS_TITLE_DESCRIPTOR)}
				data-flx="app.self-hosted-setup-wizard-gate.direct-messages-radio-group"
			/>
			{disabled && (
				<Note data-flx="app.setup.setup-wizard-steps.direct-messages-step.note">{i18n._(DMS_NOTE_DESCRIPTOR)}</Note>
			)}
		</section>
	);
});

export const PersonalNotesStep = observer(() => {
	const {i18n} = useLingui();
	const {
		draft: {personalNotesEnabled: enabled},
		updateDraft,
	} = useSetupPolicyDraft();
	const options: ReadonlyArray<RadioOption<'yes' | 'no'>> = [
		{value: 'yes', name: i18n._(YES_DESCRIPTOR), desc: i18n._(PERSONAL_NOTES_YES_DESC_DESCRIPTOR)},
		{value: 'no', name: i18n._(NO_DESCRIPTOR), desc: i18n._(PERSONAL_NOTES_NO_DESC_DESCRIPTOR)},
	];
	return (
		<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.personal-notes-step">
			<StepHeader
				title={i18n._(PERSONAL_NOTES_TITLE_DESCRIPTOR)}
				data-flx="app.setup.setup-wizard-steps.personal-notes-step.step-header"
			/>
			<RadioGroup
				options={options}
				value={enabled ? 'yes' : 'no'}
				onChange={(value) => updateDraft({personalNotesEnabled: value === 'yes'})}
				aria-label={i18n._(PERSONAL_NOTES_TITLE_DESCRIPTOR)}
				data-flx="app.self-hosted-setup-wizard-gate.personal-notes-radio-group"
			/>
			{!enabled && (
				<Note data-flx="app.setup.setup-wizard-steps.personal-notes-step.note">
					{i18n._(PERSONAL_NOTES_NOTE_DESCRIPTOR)}
				</Note>
			)}
		</section>
	);
});

export const RegistrationStep = observer(() => {
	const {i18n} = useLingui();
	const {
		draft: {registrationMode: mode},
		updateDraft,
	} = useSetupPolicyDraft();
	const options: ReadonlyArray<RadioOption<RegistrationMode>> = [
		{
			value: 'closed',
			name: i18n._(REGISTRATION_CLOSED_NAME_DESCRIPTOR),
			desc: i18n._(REGISTRATION_CLOSED_DESC_DESCRIPTOR),
		},
		{
			value: 'approval',
			name: i18n._(REGISTRATION_APPROVAL_NAME_DESCRIPTOR),
			desc: i18n._(REGISTRATION_APPROVAL_DESC_DESCRIPTOR),
		},
		{value: 'open', name: i18n._(REGISTRATION_OPEN_NAME_DESCRIPTOR), desc: i18n._(REGISTRATION_OPEN_DESC_DESCRIPTOR)},
	];
	return (
		<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.registration-step">
			<StepHeader
				title={i18n._(REGISTRATION_TITLE_DESCRIPTOR)}
				data-flx="app.setup.setup-wizard-steps.registration-step.step-header"
			/>
			<RadioGroup
				options={options}
				value={mode}
				onChange={(registrationMode) => updateDraft({registrationMode})}
				aria-label={i18n._(REGISTRATION_TITLE_DESCRIPTOR)}
				data-flx="app.self-hosted-setup-wizard-gate.registration-radio-group"
			/>
			{mode === 'open' && (
				<Note data-flx="app.setup.setup-wizard-steps.registration-step.note">
					{i18n._(REGISTRATION_OPEN_NOTE_DESCRIPTOR)}
				</Note>
			)}
		</section>
	);
});

export const PushRelayStep = observer(() => {
	const {i18n} = useLingui();
	const {
		draft: {pushRelayConsentAccepted: accepted},
		updateDraft,
	} = useSetupPolicyDraft();
	const options: ReadonlyArray<RadioOption<'yes' | 'no'>> = [
		{value: 'yes', name: i18n._(PUSH_YES_NAME_DESCRIPTOR)},
		{value: 'no', name: i18n._(PUSH_NO_NAME_DESCRIPTOR), desc: i18n._(PUSH_NO_DESC_DESCRIPTOR)},
	];
	return (
		<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.push-relay-step">
			<StepHeader
				title={i18n._(PUSH_TITLE_DESCRIPTOR)}
				body={i18n._(PUSH_BODY_DESCRIPTOR)}
				data-flx="app.setup.setup-wizard-steps.push-relay-step.step-header"
			/>
			<FocusRing data-flx="app.self-hosted-setup-wizard-gate.push-relay-notice-focus-ring">
				<a
					className={styles.link}
					href={PUSH_RELAY_NOTICE_URL}
					target="_blank"
					rel="noreferrer"
					data-flx="app.self-hosted-setup-wizard-gate.push-relay-notice-link"
				>
					{i18n._(PUSH_NOTICE_LINK_DESCRIPTOR)}
				</a>
			</FocusRing>
			<RadioGroup
				options={options}
				value={accepted ? 'yes' : 'no'}
				onChange={(value) => updateDraft({pushRelayConsentAccepted: value === 'yes'})}
				aria-label={i18n._(PUSH_TITLE_DESCRIPTOR)}
				data-flx="app.self-hosted-setup-wizard-gate.push-relay-radio-group"
			/>
		</section>
	);
});
