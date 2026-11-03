// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/operator_pass/OperatorPassWallNameField.module.css';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

export const LABS_GREETING_LABEL_DESCRIPTOR = msg({
	message: 'Welcome me in #operators',
	comment:
		'Checkbox and switch label, on by default. When on, the Fluxer system account welcomes the person by name in #operators, a channel in the Fluxer Labs community, once their Operator Pass is delivered. Keep #operators as is, it is the channel name.',
});
const LABS_GREETING_HINT_DESCRIPTOR = msg({
	message:
		'We add you to Fluxer Labs when your pass is delivered, unless you left or were removed from Fluxer Labs before. With this on, we welcome you by name in #operators soon after.',
	comment:
		'Hint under the checkbox or switch "Welcome me in #operators". Fluxer Labs is the name of a community on Fluxer, keep it as is. #operators is a channel there, keep it as is. Left or were removed from Fluxer Labs before means the person left the Fluxer Labs community, or was removed from it, at any time in the past.',
});
const LABS_GREETING_VISIONARY_HINT_DESCRIPTOR = msg({
	message:
		'We add you to Fluxer Labs when you turn on your pass, unless you left or were removed from Fluxer Labs before. With this on, we welcome you by name in #operators soon after.',
	comment:
		'Hint under the checkbox "Welcome me in #operators" in the dialog where a Visionary turns on the Operator Pass that comes with Visionary. Turn on your pass means the button in that dialog that switches the pass on, nothing is bought. Fluxer Labs is the name of a community on Fluxer, keep it as is. #operators is a channel there, keep it as is. Left or were removed from Fluxer Labs before means the person left the Fluxer Labs community, or was removed from it, at any time in the past.',
});

export function getStoredOperatorPassLabsGreeting(): boolean {
	return OperatorPassHoldings.passes?.labs?.greeting ?? true;
}

export function asksOperatorPassLabsGreeting(): boolean {
	const labs = OperatorPassHoldings.passes?.labs;
	return !labs?.greeted && !labs?.left;
}

export const OperatorPassLabsGreetingField = observer(function OperatorPassLabsGreetingField({
	id,
	checked,
	disabled,
	onChange,
	variant = 'purchase',
}: {
	id: string;
	checked: boolean;
	disabled: boolean;
	onChange: (checked: boolean) => void;
	variant?: 'purchase' | 'visionary';
}) {
	const {i18n} = useLingui();
	return (
		<div className={styles.field} data-flx="premium.operator-pass-labs-greeting-field.field">
			<Checkbox
				checked={checked}
				onChange={onChange}
				disabled={disabled}
				aria-describedby={`${id}-hint`}
				data-flx="premium.operator-pass-labs-greeting-field.checkbox.greeting"
			>
				<span className={styles.optOutLabel} data-flx="premium.operator-pass-labs-greeting-field.label">
					{i18n._(LABS_GREETING_LABEL_DESCRIPTOR)}
				</span>
			</Checkbox>
			<span id={`${id}-hint`} className={styles.hint} data-flx="premium.operator-pass-labs-greeting-field.hint">
				{i18n._(variant === 'visionary' ? LABS_GREETING_VISIONARY_HINT_DESCRIPTOR : LABS_GREETING_HINT_DESCRIPTOR)}
			</span>
		</div>
	);
});
