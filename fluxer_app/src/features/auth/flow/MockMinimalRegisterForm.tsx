// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import authStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import dobStyles from '@app/features/auth/flow/DateOfBirthField.module.css';
import {WHAT_SHOULD_PEOPLE_CALL_YOU_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import inputStyles from '@app/features/ui/components/form/FormInput.module.css';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {getDateFieldOrder} from '@fluxer/date_utils/src/DateIntrospection';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {useMemo} from 'react';

const MONTH_DESCRIPTOR = msg({
	message: 'Month',
	comment: 'Short label in the authentication mock minimal register form. Keep the tone plain and specific.',
});
const DAY_DESCRIPTOR = msg({
	message: 'Day',
	comment: 'Short label in the authentication mock minimal register form. Keep the tone plain and specific.',
});
const YEAR_DESCRIPTOR = msg({
	message: 'Year',
	comment: 'Short label in the authentication mock minimal register form. Keep the tone plain and specific.',
});

type DateFieldType = 'month' | 'day' | 'year';

interface MockMinimalRegisterFormProps {
	submitLabel: React.ReactNode;
}

export function MockMinimalRegisterForm({submitLabel}: MockMinimalRegisterFormProps) {
	const {i18n} = useLingui();
	const locale = getCurrentLocale();
	const fieldOrder = useMemo(() => getDateFieldOrder(locale), [locale]);
	const dateFields: Record<DateFieldType, React.ReactElement> = {
		month: (
			<div key="month" className={dobStyles.monthField} data-flx="auth.flow.mock-minimal-register-form.div">
				<input
					type="text"
					data-flx="auth.flow.mock-minimal-register-form.input.text"
					{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
					readOnly
					tabIndex={-1}
					placeholder={i18n._(MONTH_DESCRIPTOR)}
					className={inputStyles.input}
				/>
			</div>
		),
		day: (
			<div key="day" className={dobStyles.dayField} data-flx="auth.flow.mock-minimal-register-form.div--2">
				<input
					type="text"
					data-flx="auth.flow.mock-minimal-register-form.input.text--2"
					{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
					readOnly
					tabIndex={-1}
					placeholder={i18n._(DAY_DESCRIPTOR)}
					className={inputStyles.input}
				/>
			</div>
		),
		year: (
			<div key="year" className={dobStyles.yearField} data-flx="auth.flow.mock-minimal-register-form.div--3">
				<input
					type="text"
					data-flx="auth.flow.mock-minimal-register-form.input.text--3"
					{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
					readOnly
					tabIndex={-1}
					placeholder={i18n._(YEAR_DESCRIPTOR)}
					className={inputStyles.input}
				/>
			</div>
		),
	};
	const orderedFields = fieldOrder.map((fieldType) => dateFields[fieldType]);
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
			<div className={dobStyles.fieldset} data-flx="auth.flow.mock-minimal-register-form.div--8">
				<div className={dobStyles.labelContainer} data-flx="auth.flow.mock-minimal-register-form.div--9">
					<span className={dobStyles.legend} data-flx="auth.flow.mock-minimal-register-form.span--2">
						<Trans>Date of birth</Trans>
					</span>
				</div>
				<div className={dobStyles.inputsContainer} data-flx="auth.flow.mock-minimal-register-form.div--10">
					<div className={dobStyles.fieldsRow} data-flx="auth.flow.mock-minimal-register-form.div--11">
						{orderedFields}
					</div>
				</div>
			</div>
			<div className={authStyles.consentRow} data-flx="auth.flow.mock-minimal-register-form.div--12">
				<Checkbox checked={false} onChange={() => {}} disabled data-flx="auth.flow.mock-minimal-register-form.checkbox">
					<span className={authStyles.consentLabel} data-flx="auth.flow.mock-minimal-register-form.span--3">
						<Trans>
							I agree to the{' '}
							<ExternalLink
								href={Routes.terms()}
								className={authStyles.policyLink}
								data-flx="auth.flow.mock-minimal-register-form.external-link"
							>
								Terms of service
							</ExternalLink>{' '}
							and{' '}
							<ExternalLink
								href={Routes.privacy()}
								className={authStyles.policyLink}
								data-flx="auth.flow.mock-minimal-register-form.external-link--2"
							>
								Privacy policy
							</ExternalLink>
						</Trans>
					</span>
				</Checkbox>
			</div>
			<Button type="button" fitContainer disabled data-flx="auth.flow.mock-minimal-register-form.button">
				{submitLabel}
			</Button>
		</div>
	);
}
