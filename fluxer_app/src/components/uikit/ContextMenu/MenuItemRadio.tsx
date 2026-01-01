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
import {MenuItem as AriaMenuItem} from 'react-aria-components';
import {useContextMenuClose} from './ContextMenu';
import styles from './ContextMenu.module.css';
import radioStyles from './MenuItemRadio.module.css';

interface MenuItemRadioProps {
	children?: React.ReactNode;
	icon?: React.ReactNode;
	selected: boolean;
	disabled?: boolean;
	onSelect?: () => void;
	closeOnSelect?: boolean;
}

export const MenuItemRadio = React.forwardRef<HTMLDivElement, MenuItemRadioProps>(
	({children, icon, selected, disabled = false, onSelect, closeOnSelect = false}, forwardedRef) => {
		const closeMenu = useContextMenuClose();

		const handleAction = React.useCallback(() => {
			if (disabled) return;
			onSelect?.();
			if (closeOnSelect) {
				closeMenu();
			}
		}, [closeMenu, closeOnSelect, disabled, onSelect]);

		return (
			<AriaMenuItem
				ref={forwardedRef}
				onAction={handleAction}
				isDisabled={disabled}
				className={`${styles.item} ${styles.checkboxItem} ${disabled ? styles.disabled : ''}`.trim()}
				textValue={typeof children === 'string' ? children : ''}
			>
				{icon && <div className={styles.itemIcon}>{icon}</div>}
				<div className={styles.itemLabel}>{children}</div>
				<div className={styles.checkboxIndicator}>
					<div
						className={`${radioStyles.radioButton} ${selected ? radioStyles.radioButtonSelected : radioStyles.radioButtonUnselected}`}
					>
						{selected && <div className={radioStyles.radioIndicator} />}
					</div>
				</div>
			</AriaMenuItem>
		);
	},
);

MenuItemRadio.displayName = 'MenuItemRadio';
