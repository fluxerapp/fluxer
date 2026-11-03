// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import {useSetupBrandingDraft} from '@app/features/app/components/setup/SetupBrandingDraft';
import {useSetupPolicyDraft} from '@app/features/app/components/setup/SetupPolicySteps';
import type {updateInstanceConfig} from '@app/features/app/components/setup/SetupWizardClient';
import {StepError, StepHeader} from '@app/features/app/components/setup/SetupWizardSteps';
import {Input} from '@app/features/ui/components/form/FormInput';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {RefObject} from 'react';

const MIN_INSTANCE_NAME_LENGTH = 1;
const MAX_INSTANCE_NAME_LENGTH = 80;

const NAME_TITLE_DESCRIPTOR = msg({
	message: 'Name your instance',
	comment: 'Setup wizard title for choosing the instance name.',
});
const NAME_BODY_DESCRIPTOR = msg({
	message: 'People see this name on the sign-in page and in the app.',
	comment: 'Setup wizard body for choosing the instance name.',
});
const NAME_LABEL_DESCRIPTOR = msg({
	message: 'Name',
	comment: 'Label for the instance name field in the setup wizard.',
});
const NAME_ERROR_DESCRIPTOR = msg({
	message: 'Enter a name between 1 and 80 characters.',
	comment: 'Error under the instance name field in the setup wizard.',
});

function isValidInstanceName(value: string): boolean {
	const length = value.trim().length;
	return length >= MIN_INSTANCE_NAME_LENGTH && length <= MAX_INSTANCE_NAME_LENGTH;
}

interface NameStepProps {
	config: InstanceConfigResponse | null;
	formRef: RefObject<HTMLFormElement | null>;
	showErrors: boolean;
	disabled: boolean;
	error: string | null;
	onSubmit: (submit: () => Promise<void>) => Promise<void>;
	onSave: (patch: Parameters<typeof updateInstanceConfig>[0]) => Promise<boolean>;
	onAdvance: () => void;
	onShowErrors: (showErrors: boolean) => void;
}

export const NameStep = observer(
	({config, formRef, showErrors, disabled, error, onSubmit, onSave, onAdvance, onShowErrors}: NameStepProps) => {
		const {i18n} = useLingui();
		const {productName, setProductName} = useSetupBrandingDraft();
		const {setDraft: setFinishDraft} = useSetupPolicyDraft();
		const saveName = async () => {
			onShowErrors(true);
			if (!isValidInstanceName(productName)) return;
			const saved = await onSave({app_public: {branding: {product_name: productName.trim()}}});
			if (saved) {
				setFinishDraft((current) =>
					current.singleCommunityName.trim() && current.singleCommunityName !== config?.app_public.branding.product_name
						? current
						: {...current, singleCommunityName: productName.trim()},
				);
				onAdvance();
			}
		};

		return (
			<form
				ref={formRef}
				noValidate
				onSubmit={(event) => {
					event.preventDefault();
					void onSubmit(saveName);
				}}
				className={styles.step}
				data-flx="app.self-hosted-setup-wizard-gate.name-step"
			>
				<StepHeader
					title={i18n._(NAME_TITLE_DESCRIPTOR)}
					body={i18n._(NAME_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.name-step.step-header"
				/>
				<Input
					label={i18n._(NAME_LABEL_DESCRIPTOR)}
					value={productName}
					maxLength={MAX_INSTANCE_NAME_LENGTH}
					autoComplete="off"
					disabled={disabled}
					error={showErrors && !isValidInstanceName(productName) ? i18n._(NAME_ERROR_DESCRIPTOR) : undefined}
					onChange={(event) => setProductName(event.target.value)}
					data-flx="app.self-hosted-setup-wizard-gate.name-input"
				/>
				<StepError error={error} data-flx="app.setup.setup-wizard-steps.name-step.step-error" />
			</form>
		);
	},
);
