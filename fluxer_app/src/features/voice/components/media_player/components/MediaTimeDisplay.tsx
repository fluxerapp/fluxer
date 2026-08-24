// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/voice/components/media_player/MediaTimeDisplay.module.css';
import {formatDuration} from '@fluxer/date_utils/src/DateDuration';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {memo} from 'react';

const TIME_OF_DESCRIPTOR = msg({
	message: 'Time: {currentFormatted} of {durationFormatted}',
	comment:
		'Aria label on the media player time display. {currentFormatted} and {durationFormatted} are pre-formatted mm:ss strings.',
});

interface MediaTimeDisplayProps {
	currentTime: number;
	duration: number;
	size?: 'small' | 'medium' | 'large';
	compact?: boolean;
	className?: string;
}

function areTimeDisplayPropsEqualToTheSecond(previous: MediaTimeDisplayProps, next: MediaTimeDisplayProps): boolean {
	return (
		Math.trunc(previous.currentTime) === Math.trunc(next.currentTime) &&
		Math.trunc(previous.duration) === Math.trunc(next.duration) &&
		previous.size === next.size &&
		previous.compact === next.compact &&
		previous.className === next.className
	);
}

function MediaTimeDisplayImpl({
	currentTime,
	duration,
	size = 'medium',
	compact = false,
	className,
}: MediaTimeDisplayProps) {
	const {i18n} = useLingui();
	const currentFormatted = formatDuration(currentTime);
	const durationFormatted = formatDuration(duration);
	return (
		<div
			className={clsx(styles.container, styles[size], compact && styles.compact, className)}
			aria-label={i18n._(TIME_OF_DESCRIPTOR, {currentFormatted, durationFormatted})}
			role="group"
			data-flx="voice.media-player.media-time-display.container"
		>
			<span className={styles.time} data-flx="voice.media-player.media-time-display.time">
				{currentFormatted}
			</span>
			{!compact && (
				<>
					<span className={styles.separator} data-flx="voice.media-player.media-time-display.separator">
						/
					</span>
					<span className={clsx(styles.time, styles.duration)} data-flx="voice.media-player.media-time-display.time--2">
						{durationFormatted}
					</span>
				</>
			)}
		</div>
	);
}

export const MediaTimeDisplay = memo(MediaTimeDisplayImpl, areTimeDisplayPropsEqualToTheSecond);
