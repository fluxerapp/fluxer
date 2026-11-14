// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import type {updateInstanceConfig} from '@app/features/app/components/setup/SetupWizardClient';
import {StepError, StepHeader} from '@app/features/app/components/setup/SetupWizardSteps';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {createContext, type ReactNode, type RefObject, useContext, useMemo, useState} from 'react';

const ATTACHMENTS_TITLE_DESCRIPTOR = msg({
	message: 'Should old attachments be deleted to save disk space?',
	comment: 'Setup wizard title for automatic attachment expiry.',
});
const ATTACHMENTS_YES_NAME_DESCRIPTOR = msg({
	message: 'Yes, delete old attachments',
	comment: 'Setup wizard option that turns on automatic attachment expiry.',
});
const ATTACHMENTS_YES_DESC_DESCRIPTOR = msg({
	message:
		'Small files are kept for about three years, and the largest ones for as little as 14 days. You can change these limits in the admin panel.',
	comment: 'Description of automatic attachment expiry in the setup wizard.',
});
const ATTACHMENTS_NO_NAME_DESCRIPTOR = msg({
	message: 'No, keep everything',
	comment: 'Setup wizard option that keeps attachments until someone deletes them.',
});
const ATTACHMENTS_NO_DESC_DESCRIPTOR = msg({
	message: 'Attachments stay until someone deletes them.',
	comment: 'Description of keeping attachments in the setup wizard.',
});

interface AttachmentsDraft {
	enabled: boolean;
	setEnabled: (enabled: boolean) => void;
}

const AttachmentsDraftContext = createContext<AttachmentsDraft | null>(null);

export function SetupAttachmentsDraftProvider({children}: {children: ReactNode}) {
	const [enabled, setEnabled] = useState(false);
	const draft = useMemo(() => ({enabled, setEnabled}), [enabled]);
	return <AttachmentsDraftContext.Provider value={draft}>{children}</AttachmentsDraftContext.Provider>;
}

export function useSetupAttachmentsDraft(): AttachmentsDraft {
	const draft = useContext(AttachmentsDraftContext);
	if (draft === null) throw new Error('Setup attachment draft provider is missing');
	return draft;
}

export const AttachmentsStep = observer(
	({
		formRef,
		error,
		onSubmit,
		onSave,
		onAdvance,
	}: {
		formRef: RefObject<HTMLFormElement | null>;
		error: string | null;
		onSubmit: (submit: () => Promise<void>) => Promise<void>;
		onSave: (patch: Parameters<typeof updateInstanceConfig>[0]) => Promise<boolean>;
		onAdvance: () => void;
	}) => {
		const {i18n} = useLingui();
		const {enabled, setEnabled} = useSetupAttachmentsDraft();
		const saveAttachments = async () => {
			const saved = await onSave({media: {attachment_decay: {enabled}}});
			if (saved) onAdvance();
		};
		const options: ReadonlyArray<RadioOption<'yes' | 'no'>> = [
			{value: 'yes', name: i18n._(ATTACHMENTS_YES_NAME_DESCRIPTOR), desc: i18n._(ATTACHMENTS_YES_DESC_DESCRIPTOR)},
			{value: 'no', name: i18n._(ATTACHMENTS_NO_NAME_DESCRIPTOR), desc: i18n._(ATTACHMENTS_NO_DESC_DESCRIPTOR)},
		];
		return (
			<form
				ref={formRef}
				noValidate
				onSubmit={(event) => {
					event.preventDefault();
					void onSubmit(saveAttachments);
				}}
				className={styles.step}
				data-flx="app.self-hosted-setup-wizard-gate.attachments-step"
			>
				<StepHeader
					title={i18n._(ATTACHMENTS_TITLE_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.attachments-step.step-header"
				/>
				<RadioGroup
					options={options}
					value={enabled ? 'yes' : 'no'}
					onChange={(value) => setEnabled(value === 'yes')}
					aria-label={i18n._(ATTACHMENTS_TITLE_DESCRIPTOR)}
					data-flx="app.self-hosted-setup-wizard-gate.attachments-radio-group"
				/>
				<StepError error={error} data-flx="app.setup.setup-wizard-steps.attachments-step.step-error" />
			</form>
		);
	},
);
