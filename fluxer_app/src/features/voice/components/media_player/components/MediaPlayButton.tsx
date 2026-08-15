// SPDX-License-Identifier: AGPL-3.0-or-later

import {PAUSE_DESCRIPTOR, PLAY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/voice/components/media_player/MediaPlayButton.module.css';
import {useLingui} from '@lingui/react/macro';
import {PauseIcon, PlayIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback} from 'react';

interface MediaPlayButtonProps {
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
	const {i18n} = useLingui();
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
	const label = isPlaying ? i18n._(PAUSE_DESCRIPTOR) : i18n._(PLAY_DESCRIPTOR);
	const Icon = isPlaying ? PauseIcon : PlayIcon;
	const button = (
		<FocusRing offset={-2} enabled={!disabled} data-flx="voice.media-player.media-play-button.focus-ring">
			<button
				type="button"
				onClick={handleClick}
				className={clsx(styles.button, styles[size], overlay && styles.overlay, className)}
				aria-label={label}
				disabled={disabled}
				data-flx="voice.media-player.media-play-button.button.click"
			>
				<Icon size={remFromPx(actualIconSize)} weight="fill" data-flx="voice.media-player.media-play-button.icon" />
			</button>
		</FocusRing>
	);
	if (showTooltip && !overlay) {
		return (
			<Tooltip
				text={label}
				position="top"
				openOnMountHover={false}
				data-flx="voice.media-player.media-play-button.tooltip"
			>
				{button}
			</Tooltip>
		);
	}
	return button;
}
