// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import authStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {RegistrationLegalNotice} from '@app/features/auth/flow/RegistrationLegalNotice';
import {WHAT_SHOULD_PEOPLE_CALL_YOU_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import {Button} from '@app/features/ui/button/Button';
import inputStyles from '@app/features/ui/components/form/FormInput.module.css';
import {Trans, useLingui} from '@lingui/react/macro';

interface MockMinimalRegisterFormProps {
	submitLabel: React.ReactNode;
}

export function MockMinimalRegisterForm({submitLabel}: MockMinimalRegisterFormProps) {
	const {i18n} = useLingui();
	return (
		<div className={authStyles.form} data-flx="auth.flow.mock-minimal-register-form.div--4">
			<div className={inputStyles.fieldset} data-flx="auth.flow.mock-minimal-register-form.div--5">
				<div className={inputStyles.labelContainer} data-flx="auth.flow.mock-minimal-register-form.div--6">
					<span className={inputStyles.label} data-flx="auth.flow.mock-minimal-register-form.span">
						<Trans>Display name (optional)</Trans>
					</span>
				</div>
				<div className={inputStyles.inputGroup} data-flx="auth.flow.mock-minimal-register-form.div--7">
					<input
						type="text"
						data-flx="auth.flow.mock-minimal-register-form.input.text--4"
						{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
						readOnly
						tabIndex={-1}
						placeholder={i18n._(WHAT_SHOULD_PEOPLE_CALL_YOU_DESCRIPTOR)}
						className={inputStyles.input}
					/>
				</div>
			</div>
			<RegistrationLegalNotice
				config={{termsUrl: Routes.terms(), privacyUrl: Routes.privacy(), requirement: 'terms_and_privacy'}}
				data-flx="auth.flow.mock-minimal-register-form.registration-legal-notice"
			/>
			<Button type="button" fitContainer disabled data-flx="auth.flow.mock-minimal-register-form.button">
				{submitLabel}
			</Button>
		</div>
	);
}
