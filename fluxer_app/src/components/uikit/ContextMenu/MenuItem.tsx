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

import React from 'react';
import {MenuItem as MenuItemPrimitive} from './ContextMenu';
import styles from './MenuItem.module.css';

type MenuItemPrimitiveProps = React.ComponentProps<typeof MenuItemPrimitive>;
type MenuItemSelectEvent = Parameters<NonNullable<MenuItemPrimitiveProps['onSelect']>>[0];

interface MenuItemProps {
	children?: React.ReactNode;
	icon?: React.ReactNode;
	danger?: boolean;
	disabled?: boolean;
	onClick?: ((event: MenuItemSelectEvent) => void) | (() => void);
	hint?: string;
	shortcut?: string;
	className?: string;
	closeOnSelect?: boolean;
}

export const MenuItem = React.forwardRef<HTMLDivElement, MenuItemProps>(
	(
		{children, icon, danger = false, disabled = false, onClick, hint, shortcut, className, closeOnSelect = true},
		ref,
	) => {
		const handleSelect = React.useCallback(
			(event: MenuItemSelectEvent) => {
				if (!onClick) return;
				if (onClick.length === 0) {
					(onClick as () => void)();
					return;
				}
				(onClick as (event: MenuItemSelectEvent) => void)(event);
			},
			[onClick],
		);

		const combinedClassName =
			`${styles.menuItem} ${danger ? styles.danger : ''} ${disabled ? styles.disabled : ''} ${className ?? ''}`.trim();

		return (
			<MenuItemPrimitive
				ref={ref}
				label=""
				className={combinedClassName}
				disabled={disabled}
				onSelect={handleSelect}
				danger={danger}
				icon={icon}
				closeOnSelect={closeOnSelect}
			>
				<div className={styles.labelContainer}>
					<span className={styles.label}>{children}</span>
					{hint && <div className={styles.subtext}>{hint}</div>}
				</div>
				{shortcut && <span className={styles.shortcut}>{shortcut}</span>}
			</MenuItemPrimitive>
		);
	},
);
