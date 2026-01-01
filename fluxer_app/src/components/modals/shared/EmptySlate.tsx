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

import {observer} from 'mobx-react-lite';
import type React from 'react';
import styles from './EmptySlate.module.css';

interface EmptySlateProps {
	Icon: React.ComponentType<React.ComponentProps<'svg'>>;
	title: React.ReactNode;
	description: React.ReactNode;
	fullHeight?: boolean;
}

export const EmptySlate: React.FC<EmptySlateProps> = observer(({Icon, title, description, fullHeight = false}) => {
	return (
		<div className={`${styles.container} ${fullHeight ? styles.containerFullHeight : ''}`}>
			<Icon className={styles.icon} aria-hidden={true} />
			<h3 className={styles.title}>{title}</h3>
			<p className={styles.description}>{description}</p>
		</div>
	);
});
