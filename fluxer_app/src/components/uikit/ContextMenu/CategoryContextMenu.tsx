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
import {Permissions} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import PermissionStore from '~/stores/PermissionStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import {
	CategoryNotificationSettingsMenuItem,
	CollapseAllCategoriesMenuItem,
	CollapseCategoryMenuItem,
	CopyCategoryIdMenuItem,
	DeleteCategoryMenuItem,
	EditCategoryMenuItem,
	MarkCategoryAsReadMenuItem,
	MuteCategoryMenuItem,
} from './items/CategoryMenuItems';
import {DebugChannelMenuItem} from './items/DebugMenuItems';
import {MenuGroup} from './MenuGroup';

interface CategoryContextMenuProps {
	category: ChannelRecord;
	onClose: () => void;
}

export const CategoryContextMenu: React.FC<CategoryContextMenuProps> = observer(({category, onClose}) => {
	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {
		channelId: category.id,
		guildId: category.guildId,
	});
	const developerMode = UserSettingsStore.developerMode;

	return (
		<>
			<MenuGroup>
				<MarkCategoryAsReadMenuItem category={category} onClose={onClose} />
			</MenuGroup>

			<MenuGroup>
				<CollapseCategoryMenuItem category={category} onClose={onClose} />
				<CollapseAllCategoriesMenuItem category={category} onClose={onClose} />
			</MenuGroup>

			<MenuGroup>
				<MuteCategoryMenuItem category={category} onClose={onClose} />
				<CategoryNotificationSettingsMenuItem category={category} onClose={onClose} />
			</MenuGroup>

			{canManageChannels && (
				<MenuGroup>
					<EditCategoryMenuItem category={category} onClose={onClose} />
					<DeleteCategoryMenuItem category={category} onClose={onClose} />
				</MenuGroup>
			)}

			<MenuGroup>
				<CopyCategoryIdMenuItem category={category} onClose={onClose} />
				{developerMode && <DebugChannelMenuItem channel={category} onClose={onClose} />}
			</MenuGroup>
		</>
	);
});
