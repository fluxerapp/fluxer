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
import type React from 'react';
import styles from './NagbarContent.module.css';

interface NagbarContentProps {
	message: React.ReactNode;
	actions?: React.ReactNode;
	isMobile: boolean;
}

export const NagbarContent = ({message, actions, isMobile}: NagbarContentProps) => {
	return (
		<div className={clsx(styles.container, isMobile && styles.containerMobile)}>
			<p className={styles.message}>{message}</p>
			{actions && <div className={clsx(styles.actions, isMobile && styles.actionsMobile)}>{actions}</div>}
		</div>
	);
};
