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

import type {Icon, IconProps} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import React from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {TooltipWithKeybind} from '~/components/uikit/KeybindHint/KeybindHint';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import type {KeybindAction, KeyCombo} from '~/stores/KeybindStore';
import styles from './TextareaButton.module.css';

export type TextareaButtonProps = React.ButtonHTMLAttributes<HTMLButtonElement> & {
	icon: Icon;
	label: string;
	isSelected?: boolean;
	compact?: boolean;
	iconProps?: Partial<IconProps>;
	keybindAction?: KeybindAction;
	keybindCombo?: KeyCombo;
};

export const TextareaButton = React.forwardRef<HTMLButtonElement, TextareaButtonProps>(
	(
		{
			icon: Icon,
			label,
			onClick,
			disabled,
			isSelected,
			compact,
			iconProps,
			className,
			keybindAction,
			keybindCombo,
			...props
		},
		ref,
	) => {
		const button = (
			<button
				{...props}
				ref={ref}
				type="button"
				aria-label={label}
				disabled={disabled}
				onClick={onClick}
				className={clsx(compact ? styles.buttonCompact : styles.button, isSelected && styles.selected, className)}
			>
				<Icon className={styles.icon} {...iconProps} />
			</button>
		);

		const tooltipText = React.useCallback(
			() => <TooltipWithKeybind label={label} action={keybindAction} combo={keybindCombo} />,
			[label, keybindAction, keybindCombo],
		);

		return (
			<Tooltip text={tooltipText} position="top">
				<FocusRing offset={-2} enabled={!disabled}>
					{button}
				</FocusRing>
			</Tooltip>
		);
	},
);

TextareaButton.displayName = 'TextareaButton';
