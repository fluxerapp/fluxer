// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	HIDE_FAVORITES_DESCRIPTOR,
	MUTE_FAVORITES_DESCRIPTOR,
	UNMUTE_FAVORITES_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as FavoritesCommands from '@app/features/messaging/commands/FavoritesCommands';
import {HideIcon, MuteIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {FAVORITES_GUILD_ID} from '@fluxer/constants/src/AppConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface FavoritesGuildContextMenuProps {
	onClose: () => void;
}

export const FavoritesGuildContextMenu: React.FC<FavoritesGuildContextMenuProps> = observer(({onClose}) => {
	const {i18n} = useLingui();
	const settings = UserGuildSettings.getSettingsForScope(FAVORITES_GUILD_ID);
	const isMuted = settings?.muted ?? false;
	const handleToggleMute = () => {
		UserGuildSettingsCommands.updateGuildSettings(FAVORITES_GUILD_ID, {muted: !isMuted});
		onClose();
	};
	const handleHideFavorites = () => {
		onClose();
		FavoritesCommands.confirmHideFavorites(undefined, i18n);
	};
	return (
		<>
			<MenuGroup data-flx="ui.action-menu.favorites-guild-context-menu.menu-group">
				<MenuItem
					icon={<MuteIcon data-flx="ui.action-menu.favorites-guild-context-menu.mute-icon" />}
					onClick={handleToggleMute}
					data-flx="ui.action-menu.favorites-guild-context-menu.menu-item.toggle-mute"
				>
					{isMuted ? i18n._(UNMUTE_FAVORITES_DESCRIPTOR) : i18n._(MUTE_FAVORITES_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
			<MenuGroup data-flx="ui.action-menu.favorites-guild-context-menu.menu-group--2">
				<MenuItem
					icon={<HideIcon data-flx="ui.action-menu.favorites-guild-context-menu.hide-icon" />}
					onClick={handleHideFavorites}
					danger
					data-flx="ui.action-menu.favorites-guild-context-menu.menu-item.hide-favorites"
				>
					{i18n._(HIDE_FAVORITES_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
		</>
	);
});
