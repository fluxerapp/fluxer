import {getMuteDurationOptions} from '@app/features/channel/components/MuteOptions';
import type {Channel} from '@app/features/channel/models/Channel';
import {MUTE_CATEGORY_DESCRIPTOR, UNMUTE_CATEGORY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {MuteIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {getMutedText} from '@app/lib/overlay/OverlayContextMenu';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

interface CategoryMenuItemProps {
	category: Channel;
	onClose: () => void;
}
export const MuteCategoryMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {i18n} = useLingui();
	const muteDurations = useMemo(() => getMuteDurationOptions(i18n), [i18n.locale]);
	const guildId = category.guildId!;
	const categoryOverride = UserGuildSettings.getChannelOverride(guildId, category.id);
	const isMuted = categoryOverride?.muted ?? false;
	const muteConfig = categoryOverride?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);
	const handleMute = useCallback(
		(duration: number | null) => {
			const nextMuteConfig = duration
				? {
						selected_time_window: duration,
						end_time: new Date(Date.now() + duration).toISOString(),
					}
				: null;
			UserGuildSettingsCommands.updateChannelOverride(guildId, category.id, {
				muted: true,
				mute_config: nextMuteConfig,
				collapsed: true,
			});
			onClose();
		},
		[guildId, category.id, onClose],
	);
	const handleUnmute = useCallback(() => {
		UserGuildSettingsCommands.updateChannelOverride(guildId, category.id, {
			muted: false,
			mute_config: null,
		});
		onClose();
	}, [guildId, category.id, onClose]);
	if (isMuted && mutedText) {
		return (
			<MenuItem
				icon={<MuteIcon data-flx="ui.action-menu.items.category-menu-items.mute-category-menu-item.mute-icon" />}
				onClick={handleUnmute}
				hint={mutedText}
				data-flx="ui.action-menu.items.category-menu-items.mute-category-menu-item.menu-item.unmute"
			>
				{i18n._(UNMUTE_CATEGORY_DESCRIPTOR)}
			</MenuItem>
		);
	}
	return (
		<MenuItemSubmenu
			label={i18n._(MUTE_CATEGORY_DESCRIPTOR)}
			onTriggerSelect={() => handleMute(null)}
			render={() => (
				<MenuGroup data-flx="ui.action-menu.items.category-menu-items.mute-category-menu-item.menu-group">
					{muteDurations.map((duration) => (
						<MenuItem
							key={duration.value ?? 'until'}
							onClick={() => handleMute(duration.value)}
							data-flx="ui.action-menu.items.category-menu-items.mute-category-menu-item.menu-item.mute"
						>
							{duration.label}
						</MenuItem>
					))}
				</MenuGroup>
			)}
			data-flx="ui.action-menu.items.category-menu-items.mute-category-menu-item.menu-item-submenu"
		/>
	);
});
