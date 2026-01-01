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
import {clsx} from 'clsx';
import {formatTime} from '../utils/formatTime';
import styles from './MediaTimeDisplay.module.css';

export interface MediaTimeDisplayProps {
	currentTime: number;
	duration: number;
	size?: 'small' | 'medium' | 'large';
	compact?: boolean;
	className?: string;
}

export function MediaTimeDisplay({
	currentTime,
	duration,
	size = 'medium',
	compact = false,
	className,
}: MediaTimeDisplayProps) {
	const {t} = useLingui();
	const currentFormatted = formatTime(currentTime);
	const durationFormatted = formatTime(duration);

	return (
		<div
			className={clsx(styles.container, styles[size], compact && styles.compact, className)}
			aria-label={t`Time: ${currentFormatted} of ${durationFormatted}`}
			role="group"
		>
			<span className={styles.time}>{currentFormatted}</span>
			{!compact && (
				<>
					<span className={styles.separator}>/</span>
					<span className={clsx(styles.time, styles.duration)}>{durationFormatted}</span>
				</>
			)}
		</div>
	);
}
