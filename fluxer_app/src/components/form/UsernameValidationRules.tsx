/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans} from '@lingui/react/macro';
import {CheckIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import styles from './UsernameValidationRules.module.css';

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
		<div className={clsx(styles.container, className)}>
			{rules.map((rule) => (
				<div key={rule.key} className={styles.rule}>
					<div className={styles.iconContainer}>
						{rule.valid ? (
							<CheckIcon weight="bold" size={16} className={styles.iconValid} />
						) : (
							<XIcon weight="bold" size={16} className={styles.iconInvalid} />
						)}
					</div>
					<span className={rule.valid ? styles.labelValid : styles.labelInvalid}>{rule.label}</span>
				</div>
			))}
		</div>
	);
});
