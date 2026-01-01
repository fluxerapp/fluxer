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
import {EyeSlashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as FavoritesActionCreators from '~/actions/FavoritesActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import {FAVORITES_GUILD_ID} from '~/Constants';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import {MuteIcon} from './ContextMenuIcons';
import {MenuGroup} from './MenuGroup';
import {MenuItem} from './MenuItem';

interface FavoritesGuildContextMenuProps {
	onClose: () => void;
}

export const FavoritesGuildContextMenu: React.FC<FavoritesGuildContextMenuProps> = observer(({onClose}) => {
	const {t, i18n} = useLingui();
	const settings = UserGuildSettingsStore.getSettings(FAVORITES_GUILD_ID);
	const isMuted = settings?.muted ?? false;

	const handleToggleMute = () => {
		UserGuildSettingsActionCreators.updateGuildSettings(FAVORITES_GUILD_ID, {muted: !isMuted});
		onClose();
	};

	const handleHideFavorites = () => {
		onClose();
		FavoritesActionCreators.confirmHideFavorites(undefined, i18n);
	};

	return (
		<>
			<MenuGroup>
				<MenuItem icon={<MuteIcon />} onClick={handleToggleMute}>
					{isMuted ? t`Unmute Community` : t`Mute Community`}
				</MenuItem>
			</MenuGroup>
			<MenuGroup>
				<MenuItem icon={<EyeSlashIcon />} onClick={handleHideFavorites} danger>
					{t`Hide Favorites`}
				</MenuItem>
			</MenuGroup>
		</>
	);
});
