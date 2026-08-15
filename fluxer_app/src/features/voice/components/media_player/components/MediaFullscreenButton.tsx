// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/voice/components/media_player/MediaPlayButton.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CornersInIcon, CornersOutIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback} from 'react';

const EXIT_FULLSCREEN_DESCRIPTOR = msg({
	message: 'Exit fullscreen',
	comment: 'Tooltip / aria label on the fullscreen toggle in the media player (currently in fullscreen).',
});
const ENTER_FULLSCREEN_DESCRIPTOR = msg({
	message: 'Enter fullscreen',
	comment: 'Tooltip / aria label on the fullscreen toggle in the media player (not in fullscreen).',
});

interface MediaFullscreenButtonProps {
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
	const {i18n} = useLingui();
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
	const label = isFullscreen ? i18n._(EXIT_FULLSCREEN_DESCRIPTOR) : i18n._(ENTER_FULLSCREEN_DESCRIPTOR);
	const Icon = isFullscreen ? CornersInIcon : CornersOutIcon;
	const button = (
		<FocusRing offset={-2} data-flx="voice.media-player.media-fullscreen-button.focus-ring">
			<button
				type="button"
				onClick={handleClick}
				className={clsx(styles.button, styles[size], className)}
				aria-label={label}
				data-flx="voice.media-player.media-fullscreen-button.button.click"
			>
				<Icon size={remFromPx(iconSize)} weight="bold" data-flx="voice.media-player.media-fullscreen-button.icon" />
			</button>
		</FocusRing>
	);
	if (showTooltip) {
		return (
			<Tooltip
				text={label}
				position="top"
				openOnMountHover={false}
				data-flx="voice.media-player.media-fullscreen-button.tooltip"
			>
				{button}
			</Tooltip>
		);
	}
	return button;
}
