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

import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import styles from './ListSection.module.css';

export const ListSection = observer(
	({
		title,
		count,
		children,
		marginBottom = false,
	}: {
		title: string;
		count: number;
		children: React.ReactNode;
		marginBottom?: boolean;
	}) => (
		<>
			<div className={styles.sectionTitle}>
				{title} — {count}
			</div>
			<div className={clsx(marginBottom ? styles.sectionContentWithMargin : styles.sectionContent)}>{children}</div>
		</>
	),
);
