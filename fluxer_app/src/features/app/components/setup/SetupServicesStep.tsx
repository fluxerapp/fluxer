// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import type {updateInstanceConfig} from '@app/features/app/components/setup/SetupWizardClient';
import {StepError, StepHeader} from '@app/features/app/components/setup/SetupWizardSteps';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {type RefObject, useState} from 'react';

const SERVICES_TITLE_DESCRIPTOR = msg({
	message: 'GIF search and YouTube previews',
	comment: 'Setup wizard title for the GIF search and YouTube preview API keys.',
});
const SERVICES_BODY_DESCRIPTOR = msg({
	message:
		'Each one needs an API key from its provider. Leave them empty to skip, and add them later in the admin panel.',
	comment: 'Setup wizard body for the optional API keys.',
});
const KLIPY_KEY_LABEL_DESCRIPTOR = msg({
	message: 'Klipy API key',
	comment: 'Label for the Klipy API key field. Klipy is a product name and must not be translated.',
});
const KLIPY_KEY_HELP_DESCRIPTOR = msg({
	message: 'Turns on GIF search.',
	comment: 'Help under the Klipy API key field in the setup wizard.',
});
const YOUTUBE_KEY_LABEL_DESCRIPTOR = msg({
	message: 'YouTube Data API key',
	comment: 'Label for the YouTube Data API key field. YouTube Data API is a product name.',
});
const YOUTUBE_KEY_HELP_DESCRIPTOR = msg({
	message:
		'Shows the title and thumbnail when someone posts a YouTube link. Enable YouTube Data API v3 with a daily quota above 0. Restrict the key by IP address or not at all. Website restrictions block it.',
	comment:
		'Help under the YouTube Data API key field in the setup wizard. YouTube Data API v3 is a product name. Website restrictions means the Websites application restriction in the Google Cloud console.',
});
const KEY_ALREADY_SET_DESCRIPTOR = msg({
	message: 'Already set up',
	comment: 'Shown in place of an API key field when a key is already stored.',
});
const KEY_REPLACE_DESCRIPTOR = msg({
	message: 'Replace',
	comment: 'Button that lets the person enter a new API key in place of the stored one.',
});
const KEY_REPLACE_LABEL_DESCRIPTOR = msg({
	message: 'Replace the {key}',
	comment:
		'Accessible label of the button that replaces a stored API key. key is the field name, such as Klipy API key.',
});

const EMPTY_SERVICE_KEYS: ServiceKeyDraft = {
	klipyApiKey: '',
	youtubeApiKey: '',
	replaceKlipy: false,
	replaceYoutube: false,
};

export interface ServiceKeyDraft {
	klipyApiKey: string;
	youtubeApiKey: string;
	replaceKlipy: boolean;
	replaceYoutube: boolean;
}

const ServiceKeyField = ({
	label,
	help,
	value,
	stored,
	replacing,
	disabled,
	onChange,
	onReplace,
	flx,
}: {
	label: string;
	help: string;
	value: string;
	stored: boolean;
	replacing: boolean;
	disabled: boolean;
	onChange: (value: string) => void;
	onReplace: () => void;
	flx: string;
}) => {
	const {i18n} = useLingui();
	if (stored && !replacing) {
		return (
			<div className={styles.storedKey} data-flx={`app.self-hosted-setup-wizard-gate.${flx}-stored`}>
				<div className={styles.storedKeyText} data-flx={`app.self-hosted-setup-wizard-gate.${flx}-stored-text`}>
					<span className={styles.fieldLabel} data-flx="app.setup.setup-wizard-steps.service-key-field.field-label">
						{label}
					</span>
					<span className={styles.success} data-flx="app.setup.setup-wizard-steps.service-key-field.success">
						<CheckCircleIcon
							size={18}
							weight="fill"
							aria-hidden="true"
							data-flx="app.setup.setup-wizard-steps.service-key-field.check-circle-icon"
						/>
						{i18n._(KEY_ALREADY_SET_DESCRIPTOR)}
					</span>
				</div>
				<Button
					variant="secondary"
					small
					disabled={disabled}
					onClick={onReplace}
					aria-label={i18n._(KEY_REPLACE_LABEL_DESCRIPTOR, {key: label})}
					data-flx={`app.self-hosted-setup-wizard-gate.${flx}-replace`}
				>
					{i18n._(KEY_REPLACE_DESCRIPTOR)}
				</Button>
			</div>
		);
	}
	return (
		<Input
			label={label}
			value={value}
			autoComplete="off"
			autoCapitalize="none"
			autoCorrect="off"
			spellCheck={false}
			className={styles.monospaceInput}
			disabled={disabled}
			onChange={(event) => onChange(event.target.value)}
			footer={
				<p className={styles.fieldHelp} data-flx="app.setup.setup-wizard-steps.service-key-field.field-help">
					{help}
				</p>
			}
			data-flx={`app.self-hosted-setup-wizard-gate.${flx}-input`}
		/>
	);
};

export const ServicesStep = observer(
	({
		draftCache,
		formRef,
		klipyStored,
		youtubeStored,
		disabled,
		error,
		onSubmit,
		onSave,
		onAdvance,
	}: {
		draftCache: RefObject<ServiceKeyDraft | null>;
		formRef: RefObject<HTMLFormElement | null>;
		klipyStored: boolean;
		youtubeStored: boolean;
		disabled: boolean;
		error: string | null;
		onSubmit: (submit: () => Promise<void>) => Promise<void>;
		onSave: (patch: Parameters<typeof updateInstanceConfig>[0]) => Promise<boolean>;
		onAdvance: () => void;
	}) => {
		const {i18n} = useLingui();
		const [draft, setDraft] = useState(() => draftCache.current ?? EMPTY_SERVICE_KEYS);
		const updateDraft = (next: ServiceKeyDraft) => {
			draftCache.current = next;
			setDraft(next);
		};
		const onChange = (patch: Partial<ServiceKeyDraft>) => {
			updateDraft({...(draftCache.current ?? draft), ...patch});
		};
		const saveServices = async () => {
			const klipy = draft.klipyApiKey.trim();
			const youtube = draft.youtubeApiKey.trim();
			if (!klipy && !youtube) {
				onAdvance();
				return;
			}
			const saved = await onSave({
				integrations: {
					...(klipy ? {gif: {klipy_api_key: klipy}} : {}),
					...(youtube ? {youtube: {api_key: youtube}} : {}),
				},
			});
			if (saved) {
				updateDraft(EMPTY_SERVICE_KEYS);
				onAdvance();
			}
		};
		return (
			<form
				ref={formRef}
				noValidate
				onSubmit={(event) => {
					event.preventDefault();
					void onSubmit(saveServices);
				}}
				className={styles.step}
				data-flx="app.self-hosted-setup-wizard-gate.services-step"
			>
				<StepHeader
					title={i18n._(SERVICES_TITLE_DESCRIPTOR)}
					body={i18n._(SERVICES_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.services-step.step-header"
				/>
				<ServiceKeyField
					label={i18n._(KLIPY_KEY_LABEL_DESCRIPTOR)}
					help={i18n._(KLIPY_KEY_HELP_DESCRIPTOR)}
					value={draft.klipyApiKey}
					stored={klipyStored}
					replacing={draft.replaceKlipy}
					disabled={disabled}
					onChange={(klipyApiKey) => onChange({klipyApiKey})}
					onReplace={() => onChange({replaceKlipy: true})}
					flx="klipy-key"
					data-flx="app.setup.setup-wizard-steps.services-step.service-key-field.change"
				/>
				<ServiceKeyField
					label={i18n._(YOUTUBE_KEY_LABEL_DESCRIPTOR)}
					help={i18n._(YOUTUBE_KEY_HELP_DESCRIPTOR)}
					value={draft.youtubeApiKey}
					stored={youtubeStored}
					replacing={draft.replaceYoutube}
					disabled={disabled}
					onChange={(youtubeApiKey) => onChange({youtubeApiKey})}
					onReplace={() => onChange({replaceYoutube: true})}
					flx="youtube-key"
					data-flx="app.setup.setup-wizard-steps.services-step.service-key-field.change--2"
				/>
				<StepError error={error} data-flx="app.setup.setup-wizard-steps.services-step.step-error" />
			</form>
		);
	},
);
