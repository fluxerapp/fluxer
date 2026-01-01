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

import {useLingui} from '@lingui/react/macro';
import {CheckIcon, CopyIcon} from '@phosphor-icons/react';
import type {ComponentProps, MouseEvent, ReactNode} from 'react';
import {Input} from '~/components/form/Input';
import {Button} from '~/components/uikit/Button/Button';
import styles from './CopyLinkSection.module.css';

interface CopyLinkSectionProps {
	label: ReactNode;
	value: string;
	placeholder?: string;
	onCopy?: () => void;
	copied?: boolean;
	copyDisabled?: boolean;
	onInputClick?: (event: MouseEvent<HTMLInputElement>) => void;
	rightElement?: ReactNode;
	inputProps?: Partial<ComponentProps<typeof Input>>;
	children?: ReactNode;
}

export const CopyLinkSection = ({
	label,
	value,
	placeholder,
	onCopy,
	copied,
	copyDisabled,
	onInputClick,
	rightElement,
	inputProps,
	children,
}: CopyLinkSectionProps) => {
	const {t} = useLingui();
	const defaultRightElement = onCopy && (
		<Button
			compact
			fitContent
			onClick={onCopy}
			disabled={!value || copyDisabled}
			leftIcon={copied ? <CheckIcon size={16} weight="bold" /> : <CopyIcon size={16} />}
		>
			{copied ? t`Copied` : t`Copy`}
		</Button>
	);

	return (
		<div className={styles.linkFooter}>
			<p className={styles.linkSectionLabel}>{label}</p>
			<Input
				readOnly
				value={value}
				placeholder={placeholder}
				onClick={onInputClick}
				rightElement={rightElement ?? defaultRightElement}
				{...inputProps}
			/>
			{children}
		</div>
	);
};
