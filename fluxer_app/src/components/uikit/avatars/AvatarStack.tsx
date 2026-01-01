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
import React from 'react';
import styles from './AvatarStack.module.css';

export interface AvatarStackProps {
	children: React.ReactNode;
	size?: number;
	maxVisible?: number;
	className?: string;
}

export const AvatarStack: React.FC<AvatarStackProps> = observer(({children, size = 28, maxVisible = 3, className}) => {
	const childArray = React.Children.toArray(children).filter(Boolean);
	const totalCount = childArray.length;

	const visibleChildren = childArray.slice(0, maxVisible);
	const remainingCount = Math.max(0, totalCount - maxVisible);
	const computedOutline = Math.min(3, Math.max(1, Math.round(size * 0.07)));
	const overlap = Math.round(-0.35 * size);

	const cssVars = {
		'--avatar-size': `${size}px`,
		'--avatar-overlap': `${overlap}px`,
		'--avatar-outline': `${computedOutline}px`,
	} as React.CSSProperties;

	return (
		<div className={clsx(styles.container, className)} style={cssVars}>
			{visibleChildren.map((child, index) => (
				<div
					key={index}
					className={clsx(styles.avatar, (index < visibleChildren.length - 1 || remainingCount > 0) && styles.withMask)}
				>
					{child}
				</div>
			))}
			{remainingCount > 0 && <div className={styles.remainingCount}>+{remainingCount}</div>}
		</div>
	);
});
