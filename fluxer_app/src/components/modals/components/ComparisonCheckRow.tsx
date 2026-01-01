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

import {CheckIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import styles from './ComparisonCheckRow.module.css';

export const ComparisonCheckRow = observer(
	({feature, freeHas, plutoniumHas}: {feature: string; freeHas: boolean; plutoniumHas: boolean}) => (
		<div className={styles.row}>
			<div className={styles.feature}>
				<p className={styles.featureText}>{feature}</p>
			</div>
			<div className={styles.valuesContainer}>
				<div className={styles.valueCell}>
					{freeHas ? <CheckIcon className={styles.checkIcon} weight="bold" /> : <span className={styles.dash}>—</span>}
				</div>
				<div className={styles.valueCell}>
					{plutoniumHas ? (
						<CheckIcon className={styles.checkIcon} weight="bold" />
					) : (
						<span className={styles.dash}>—</span>
					)}
				</div>
			</div>
		</div>
	),
);
