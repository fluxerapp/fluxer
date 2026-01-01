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
import {ClockIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';

import styles from './SlowmodeIndicator.module.css';

interface SlowmodeIndicatorProps {
	slowmodeRemaining: number;
}

export const SlowmodeIndicator = observer(({slowmodeRemaining}: SlowmodeIndicatorProps) => {
	if (slowmodeRemaining <= 0) {
		return null;
	}

	const formatTime = (ms: number): string => {
		const totalSeconds = Math.ceil(ms / 1000);
		const hours = Math.floor(totalSeconds / 3600);
		const minutes = Math.floor((totalSeconds % 3600) / 60);
		const seconds = totalSeconds % 60;

		if (hours > 0) {
			return `${hours.toString().padStart(2, '0')}:${minutes.toString().padStart(2, '0')}:${seconds.toString().padStart(2, '0')}`;
		}

		return `${minutes.toString().padStart(2, '0')}:${seconds.toString().padStart(2, '0')}`;
	};

	return (
		<Tooltip text={() => <Trans>You are in slowmode. Please wait before sending another message.</Trans>}>
			<div className={styles.container}>
				<ClockIcon size={12} weight="fill" />
				<span className={styles.time}>{formatTime(slowmodeRemaining)}</span>
			</div>
		</Tooltip>
	);
});
