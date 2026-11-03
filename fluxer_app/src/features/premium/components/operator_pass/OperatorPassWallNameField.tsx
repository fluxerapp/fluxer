// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/operator_pass/OperatorPassWallNameField.module.css';
import type {OperatorPassWallNameProblem} from '@app/features/premium/utils/OperatorPassUtils';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {OPERATOR_PASS_WALL_NAME_MAX_LENGTH} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const WALL_NAME_INPUT_OVERFLOW_ALLOWANCE = 16;

const WALL_NAME_LABEL_DESCRIPTOR = msg({
	message: "Your name on the Operators' wall",
	comment:
		"Label of the text field for the name shown on the Operators' wall, at checkout and when accepting an Operator Pass someone passed on. The Operators' wall is the list of Operators on the homepage.",
});
const WALL_NAME_SETTINGS_LABEL_DESCRIPTOR = msg({
	message: 'Name on the wall',
	comment:
		"Label of the text field for the name shown on the Operators' wall, in the settings. The wall is the list of Operators on the homepage.",
});
const SHOW_ON_WALL_DESCRIPTOR = msg({
	message: "Show me on the Operators' wall",
	comment:
		"Switch above the Operators' wall name field, on by default. When it is off, the person is not shown on the Operators' wall and the name field is hidden. The Operators' wall is the list of Operators on the homepage.",
});
const WALL_NAME_REQUIRED_DESCRIPTOR = msg({
	message: 'Enter a name, or turn off "Show me on the Operators\' wall".',
	comment:
		'Error under the Operators\' wall name field when it is empty and the switch "Show me on the Operators\' wall" is on. Use the same words as the label of that switch.',
});
const WALL_NAME_REJECTED_DESCRIPTOR = msg({
	message: "We can't show this name on the Operators' wall. Please choose another one.",
	comment:
		"Error under the Operators' wall name field when the server turned the name down, for example because it looks like it comes from Fluxer or another brand. Never say which rule matched.",
});
const WALL_NAME_TOO_LONG_DESCRIPTOR = msg({
	message: 'Use 32 characters or fewer.',
	comment: "Error under the Operators' wall name field when the name is longer than 32 characters.",
});

export const OperatorPassWallNameField = observer(function OperatorPassWallNameField({
	id,
	name,
	optOut,
	problem,
	disabled,
	context,
	onNameChange,
	onOptOutChange,
}: {
	id: string;
	name: string;
	optOut: boolean;
	problem: OperatorPassWallNameProblem | null;
	disabled: boolean;
	context: 'checkout' | 'offer' | 'settings';
	onNameChange: (name: string) => void;
	onOptOutChange: (optOut: boolean) => void;
}) {
	const {i18n} = useLingui();
	let error: string | null = null;
	if (problem === 'required') {
		error = i18n._(WALL_NAME_REQUIRED_DESCRIPTOR);
	} else if (problem === 'too_long') {
		error = i18n._(WALL_NAME_TOO_LONG_DESCRIPTOR);
	} else if (problem === 'rejected') {
		error = i18n._(WALL_NAME_REJECTED_DESCRIPTOR);
	}
	return (
		<div className={styles.field} data-flx="premium.operator-pass-wall-name-field.field">
			<Switch
				label={i18n._(SHOW_ON_WALL_DESCRIPTOR)}
				value={!optOut}
				onChange={(listed) => onOptOutChange(!listed)}
				disabled={disabled}
				data-flx="premium.operator-pass-wall-name-field.switch.listed"
			/>
			{!optOut && (
				<Input
					id={id}
					label={i18n._(context === 'settings' ? WALL_NAME_SETTINGS_LABEL_DESCRIPTOR : WALL_NAME_LABEL_DESCRIPTOR)}
					value={name}
					dir="auto"
					onChange={(event) => onNameChange(event.target.value)}
					error={error}
					maxLength={OPERATOR_PASS_WALL_NAME_MAX_LENGTH + WALL_NAME_INPUT_OVERFLOW_ALLOWANCE}
					autoComplete="off"
					disabled={disabled}
					data-flx="premium.operator-pass-wall-name-field.input.name"
				/>
			)}
		</div>
	);
});
