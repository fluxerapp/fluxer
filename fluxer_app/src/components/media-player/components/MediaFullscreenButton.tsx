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
import {CornersInIcon, CornersOutIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback} from 'react';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import styles from './MediaPlayButton.module.css';

export interface MediaFullscreenButtonProps {
	isFullscreen: boolean;
	supportsFullscreen?: boolean;
	onToggle: () => void;
	iconSize?: number;
	size?: 'small' | 'medium' | 'large';
	showTooltip?: boolean;
	className?: string;
}

export function MediaFullscreenButton({
	isFullscreen,
	supportsFullscreen = true,
	onToggle,
	iconSize = 20,
	size = 'medium',
	showTooltip = true,
	className,
}: MediaFullscreenButtonProps) {
	const {t} = useLingui();
	const handleClick = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			onToggle();
		},
		[onToggle],
	);

	if (!supportsFullscreen) {
		return null;
	}

	const label = isFullscreen ? t`Exit fullscreen` : t`Enter fullscreen`;
	const Icon = isFullscreen ? CornersInIcon : CornersOutIcon;

	const button = (
		<button
			type="button"
			onClick={handleClick}
			className={clsx(styles.button, styles[size], className)}
			aria-label={label}
		>
			<Icon size={iconSize} weight="bold" />
		</button>
	);

	if (showTooltip) {
		return (
			<Tooltip text={label} position="top">
				{button}
			</Tooltip>
		);
	}

	return button;
}
