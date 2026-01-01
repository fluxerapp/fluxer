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
import {SubMenu} from './ContextMenu';

interface MenuItemSubmenuProps {
	label: string;
	icon?: React.ReactNode;
	disabled?: boolean;
	hint?: string;
	render: () => React.ReactNode;
	onTriggerSelect?: () => void;
	selectionMode?: 'none' | 'single' | 'multiple';
}

export const MenuItemSubmenu: React.FC<MenuItemSubmenuProps> = observer(
	({label, icon, disabled = false, hint, render, onTriggerSelect, selectionMode}) => {
		return (
			<SubMenu
				label={label}
				icon={icon}
				disabled={disabled}
				hint={hint}
				onTriggerSelect={onTriggerSelect}
				selectionMode={selectionMode}
			>
				{render()}
			</SubMenu>
		);
	},
);
