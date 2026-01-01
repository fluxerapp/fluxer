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
import {PauseIcon, PlayIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback} from 'react';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import styles from './MediaPlayButton.module.css';

export interface MediaPlayButtonProps {
	isPlaying: boolean;
	onToggle: () => void;
	size?: 'small' | 'medium' | 'large' | 'xlarge';
	iconSize?: number;
	showTooltip?: boolean;
	className?: string;
	overlay?: boolean;
	disabled?: boolean;
}

const SIZE_MAP = {
	small: 16,
	medium: 20,
	large: 24,
	xlarge: 32,
};

export function MediaPlayButton({
	isPlaying,
	onToggle,
	size = 'medium',
	iconSize,
	showTooltip = true,
	className,
	overlay = false,
	disabled = false,
}: MediaPlayButtonProps) {
	const {t} = useLingui();
	const handleClick = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			if (!disabled) {
				onToggle();
			}
		},
		[onToggle, disabled],
	);

	const actualIconSize = iconSize ?? SIZE_MAP[size];
	const label = isPlaying ? t`Pause` : t`Play`;

	const Icon = isPlaying ? PauseIcon : PlayIcon;

	const button = (
		<button
			type="button"
			onClick={handleClick}
			className={clsx(styles.button, styles[size], overlay && styles.overlay, className)}
			aria-label={label}
			disabled={disabled}
		>
			<Icon size={actualIconSize} weight="fill" />
		</button>
	);

	if (showTooltip && !overlay) {
		return (
			<Tooltip text={label} position="top">
				{button}
			</Tooltip>
		);
	}

	return button;
}
