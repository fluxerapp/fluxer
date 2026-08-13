// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/ui/components/form/UsernameValidationRules.module.css';
import {Trans} from '@lingui/react/macro';
import {CheckIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const FLUXER_TAG_REGEX = /^[a-zA-Z0-9_]+$/;

export interface UsernameValidationResult {
	validLength: boolean;
	validCharacters: boolean;
	allValid: boolean;
}

function validateUsername(username: string): UsernameValidationResult {
	const trimmed = username.trim();
	const validLength = trimmed.length >= 1 && trimmed.length <= 32;
	const validCharacters = trimmed.length === 0 || FLUXER_TAG_REGEX.test(trimmed);
	const allValid = validLength && validCharacters;
	return {
		validLength,
		validCharacters,
		allValid,
	};
}

interface UsernameValidationRulesProps {
	username: string;
	className?: string;
}

export const UsernameValidationRules: React.FC<UsernameValidationRulesProps> = observer(({username, className}) => {
	const validation = validateUsername(username);
	const rules = [
		{
			key: 'length',
			valid: validation.validLength,
			label: <Trans>Between 1 and 32 characters</Trans>,
		},
		{
			key: 'characters',
			valid: validation.validCharacters,
			label: <Trans>Letters (a-z, A-Z), numbers (0-9), and underscores (_) only</Trans>,
		},
	];
	return (
		<div className={clsx(styles.container, className)} data-flx="ui.form.username-validation-rules.container">
			{rules.map((rule) => (
				<div key={rule.key} className={styles.rule} data-flx="ui.form.username-validation-rules.rule">
					<div className={styles.iconContainer} data-flx="ui.form.username-validation-rules.icon-container">
						{rule.valid ? (
							<CheckIcon
								weight="bold"
								size={remFromPx(16)}
								className={styles.iconValid}
								data-flx="ui.form.username-validation-rules.icon-valid"
							/>
						) : (
							<XIcon
								weight="bold"
								size={remFromPx(16)}
								className={styles.iconInvalid}
								data-flx="ui.form.username-validation-rules.icon-invalid"
							/>
						)}
					</div>
					<span
						className={rule.valid ? styles.labelValid : styles.labelInvalid}
						data-flx="ui.form.username-validation-rules.label-valid"
					>
						{rule.label}
					</span>
				</div>
			))}
		</div>
	);
});
