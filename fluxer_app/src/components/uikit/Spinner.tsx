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
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import styles from './Spinner.module.css';

interface SpinnerProps {
	className?: string;
	size?: 'small' | 'medium' | 'large';
}

export const Spinner = observer(function Spinner({className, size = 'medium'}: SpinnerProps) {
	return (
		<span className={clsx(styles.spinner, className)}>
			<span className={styles.spinnerInner}>
				<span className={clsx(styles.spinnerItem, styles[size])} />
				<span className={clsx(styles.spinnerItem, styles[size], styles.delay1)} />
				<span className={clsx(styles.spinnerItem, styles[size], styles.delay2)} />
			</span>
			<span className={styles.srOnly}>
				<Trans>Loading...</Trans>
			</span>
		</span>
	);
});
