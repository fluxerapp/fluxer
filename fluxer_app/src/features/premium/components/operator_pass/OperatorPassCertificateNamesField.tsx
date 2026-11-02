// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/operator_pass/OperatorPassWallNameField.module.css';
import type {OperatorPassCertificateNameProblem} from '@app/features/premium/utils/OperatorPassUtils';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {OPERATOR_PASS_CERTIFICATE_NAME_MAX_CODE_UNITS} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const NAME_LABEL_DESCRIPTOR = msg({
	message: 'Name on your certificate',
	comment:
		'Label of the text field for the name printed on the Operator certificate, a PDF the buyer gets for the Operator Pass. Never translate certificate as diploma, card, license or token.',
});
const NAME_NUMBERED_LABEL_DESCRIPTOR = msg({
	message: 'Name on page {number}',
	comment:
		'Label of one of several text fields for the names printed on the pages of the Operator certificate, one page per pass. A page is a page of the Operator certificate PDF, so use the same word for page as in "Download page {number} ({name})". {number} is the page number, such as 2.',
});
const NAME_HINT_DESCRIPTOR = msg({
	message: 'We print each name exactly as you type it. Buying for someone else? Put their name on their certificate.',
	comment:
		'Hint under the name field for the Operator certificate. The certificate can print names in any writing system, such as Latin, Arabic, Chinese or Devanagari. The buyer can put the name of a friend on the certificate when the pass is a gift.',
});
const SAME_NAME_DESCRIPTOR = msg({
	message: 'Use the same name on every page',
	comment:
		'Switch above the certificate name fields when buying more than one Operator Pass. A page is a page of the Operator certificate PDF, one page per pass, so use the same word for page as in "Download page {number} ({name})". When on, one name goes on every page of the certificate.',
});
const NAME_REQUIRED_DESCRIPTOR = msg({
	message: 'Enter the name you want on the certificate.',
	comment: 'Error under a certificate name field when it is empty.',
});
const NAME_TOO_LONG_DESCRIPTOR = msg({
	message: 'Use 48 characters or fewer.',
	comment: 'Error under a certificate name field when the name is longer than 48 characters.',
});
const NAME_REJECTED_DESCRIPTOR = msg({
	message: "We can't print this name on a certificate. Please choose another one.",
	comment:
		'Error under the certificate name fields when the server turned a name down, for example because it looks like it comes from Fluxer or another brand, or because it has characters such as emoji that the certificate cannot print. Never say which rule matched.',
});
export const OperatorPassCertificateNamesField = observer(function OperatorPassCertificateNamesField({
	id,
	names,
	quantity,
	sameName,
	problem,
	problemIndex,
	disabled,
	hint,
	onNameChange,
	onSameNameChange,
}: {
	id: string;
	names: ReadonlyArray<string>;
	quantity: number;
	sameName: boolean;
	problem: OperatorPassCertificateNameProblem | null;
	problemIndex: number | null;
	disabled: boolean;
	hint?: string;
	onNameChange: (index: number, name: string) => void;
	onSameNameChange: (sameName: boolean) => void;
}) {
	const {i18n} = useLingui();
	const fieldCount = quantity > 1 && !sameName ? quantity : 1;
	const hintId = `${id}-hint`;
	const errorText = (() => {
		switch (problem) {
			case 'required':
				return i18n._(NAME_REQUIRED_DESCRIPTOR);
			case 'too_long':
				return i18n._(NAME_TOO_LONG_DESCRIPTOR);
			case 'rejected':
				return i18n._(NAME_REJECTED_DESCRIPTOR);
			default:
				return null;
		}
	})();
	return (
		<div className={styles.field} data-flx="premium.operator-pass-certificate-names-field.field">
			{quantity > 1 && (
				<Switch
					label={i18n._(SAME_NAME_DESCRIPTOR)}
					value={sameName}
					onChange={onSameNameChange}
					disabled={disabled}
					data-flx="premium.operator-pass-certificate-names-field.switch.same-name"
				/>
			)}
			{Array.from({length: fieldCount}, (_, index) => {
				const showsError = errorText !== null && (problemIndex === null ? index === 0 : problemIndex === index);
				return (
					<Input
						key={index}
						id={index === 0 ? id : `${id}-${index + 1}`}
						label={
							fieldCount > 1
								? i18n._(NAME_NUMBERED_LABEL_DESCRIPTOR, {number: index + 1})
								: i18n._(NAME_LABEL_DESCRIPTOR)
						}
						value={names[index] ?? ''}
						dir="auto"
						onChange={(event) => onNameChange(index, event.target.value)}
						error={showsError ? errorText : null}
						aria-describedby={hintId}
						maxLength={OPERATOR_PASS_CERTIFICATE_NAME_MAX_CODE_UNITS}
						autoComplete="off"
						disabled={disabled}
						data-flx="premium.operator-pass-certificate-names-field.input.name"
					/>
				);
			})}
			<span id={hintId} className={styles.hint} data-flx="premium.operator-pass-certificate-names-field.hint">
				{hint ?? i18n._(NAME_HINT_DESCRIPTOR)}
			</span>
		</div>
	);
});
