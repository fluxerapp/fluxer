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

import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, CaretUpIcon, PencilSimpleIcon, PlusCircleIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {RenameChannelModal} from '~/components/modals/RenameChannelModal';
import FavoritesStore from '~/stores/FavoritesStore';
import {MenuGroup} from './MenuGroup';
import {MenuItem} from './MenuItem';

interface FavoritesCategoryContextMenuProps {
	category: {id: string; name: string};
	onClose: () => void;
	onAddChannel: () => void;
}

export const FavoritesCategoryContextMenu: React.FC<FavoritesCategoryContextMenuProps> = observer(
	({category, onClose, onAddChannel}) => {
		const {t} = useLingui();
		const isCollapsed = FavoritesStore.isCategoryCollapsed(category.id);

		const handleRename = () => {
			onClose();
			ModalActionCreators.push(
				modal(() => (
					<RenameChannelModal
						currentName={category.name}
						onSave={(name) => {
							FavoritesStore.renameCategory(category.id, name);
						}}
					/>
				)),
			);
		};

		const handleToggleCollapse = () => {
			FavoritesStore.toggleCategoryCollapsed(category.id);
			onClose();
		};

		const handleRemove = () => {
			FavoritesStore.removeCategory(category.id);
			onClose();
		};

		const handleAddChannelClick = () => {
			onClose();
			onAddChannel();
		};

		return (
			<>
				<MenuGroup>
					<MenuItem icon={<PlusCircleIcon />} onClick={handleAddChannelClick}>
						{t`Add Channel`}
					</MenuItem>
					<MenuItem icon={<PencilSimpleIcon />} onClick={handleRename}>
						{t`Rename Category`}
					</MenuItem>
				</MenuGroup>

				<MenuGroup>
					<MenuItem icon={isCollapsed ? <CaretDownIcon /> : <CaretUpIcon />} onClick={handleToggleCollapse}>
						{isCollapsed ? t`Expand Category` : t`Collapse Category`}
					</MenuItem>
				</MenuGroup>

				<MenuGroup>
					<MenuItem icon={<TrashIcon />} onClick={handleRemove} danger>
						{t`Delete Category`}
					</MenuItem>
				</MenuGroup>
			</>
		);
	},
);
