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
import styles from './SectionHeader.module.css';

interface SectionHeaderProps {
	title: React.ReactNode;
	description?: React.ReactNode;
	align?: 'left' | 'center';
}

export const SectionHeader: React.FC<SectionHeaderProps> = observer(({title, description, align = 'left'}) => (
	<div className={clsx(styles.header, align === 'center' && styles.headerCenter)}>
		<h2 className={styles.title}>{title}</h2>
		{description ? <p className={styles.description}>{description}</p> : null}
	</div>
));
