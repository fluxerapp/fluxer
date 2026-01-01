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
import React from 'react';
import {CheckboxItem} from './ContextMenu';
import styles from './ContextMenu.module.css';

interface MenuItemCheckboxProps {
	children?: React.ReactNode;
	description?: React.ReactNode;
	icon?: React.ReactNode;
	checked: boolean;
	disabled?: boolean;
	onChange?: (checked: boolean) => void;
	danger?: boolean;
	closeOnChange?: boolean;
}

export const MenuItemCheckbox: React.FC<MenuItemCheckboxProps> = observer(
	({children, description, icon, checked, disabled = false, onChange, danger = false, closeOnChange = false}) => {
		const handleCheckedChange = React.useCallback(
			(newChecked: boolean) => {
				onChange?.(newChecked);
			},
			[onChange],
		);

		return (
			<CheckboxItem
				label={children?.toString() || ''}
				icon={icon}
				checked={checked}
				disabled={disabled}
				danger={danger}
				onCheckedChange={handleCheckedChange}
				closeOnChange={closeOnChange}
			>
				<div className={styles.menuItemCheckboxLabel}>
					<span className={styles.menuItemCheckboxLabelPrimary}>{children}</span>
					{description && <span className={styles.menuItemCheckboxDescription}>{description}</span>}
				</div>
			</CheckboxItem>
		);
	},
);
