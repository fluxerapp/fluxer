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
import {MenuSeparator} from './ContextMenu';
import {MenuGroup} from './MenuGroup';

interface MenuGroupsProps {
	children?: React.ReactNode;
}

export const MenuGroups: React.FC<MenuGroupsProps> = observer(({children}) => {
	const groups = React.Children.toArray(children).filter((child) => {
		if (!child) return false;
		if (!React.isValidElement(child)) return false;
		return child.type === MenuGroup;
	});

	if (groups.length === 0) {
		return null;
	}

	return (
		<>
			{groups.map((group, index) => (
				<React.Fragment key={index}>
					{group}
					{index < groups.length - 1 && <MenuSeparator />}
				</React.Fragment>
			))}
		</>
	);
});
