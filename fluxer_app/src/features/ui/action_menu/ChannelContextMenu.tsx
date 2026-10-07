// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {
	MUTE_CHANNEL_DESCRIPTOR,
	UNMUTE_CHANNEL_DESCRIPTOR,
} from '@app/features/channel/utils/ChannelMessageDescriptors';
import Guilds from '@app/features/guild/state/Guilds';
import {NOTIFICATION_SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {DataMenuItemsRenderer, DataMenuRenderer} from '@app/features/ui/action_menu/DataMenuRenderer';
import {useChannelMenuData} from '@app/features/ui/action_menu/items/ChannelMenuData';
import {MuteChannelMenuItem} from '@app/features/ui/action_menu/items/ChannelMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import type {MenuGroupType, MenuSheetItem} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {GUILD_TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

interface ChannelContextMenuProps {
	channel: Channel;
	onClose: () => void;
}

export const ChannelContextMenu: React.FC<ChannelContextMenuProps> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();
	const guild = channel.guildId ? Guilds.getGuild(channel.guildId) : undefined;
	const {groups} = useChannelMenuData(channel, guild, {
		onClose,
		preserveInitialMarkAsReadVisibility: true,
	});
	const excludeLabels = useMemo(
		() => [i18n._(MUTE_CHANNEL_DESCRIPTOR), i18n._(UNMUTE_CHANNEL_DESCRIPTOR)],
		[i18n.locale],
	);
	const showMuteMenuItem = GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type) || channel.isThreadOnly();
	const notificationSettingsLabel = i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR);
	const splitGroups = useMemo<{
		beforeBehavior: Array<MenuGroupType>;
		behaviorItems: Array<MenuSheetItem>;
		afterBehavior: Array<MenuGroupType>;
	}>(() => {
		const behaviorGroupIndex = groups.findIndex((group) =>
			group.items.some((item) => 'label' in item && item.label === notificationSettingsLabel),
		);
		if (!showMuteMenuItem || behaviorGroupIndex === -1) {
			return {
				beforeBehavior: groups,
				behaviorItems: [],
				afterBehavior: [],
			};
		}
		return {
			beforeBehavior: groups.slice(0, behaviorGroupIndex),
			behaviorItems: groups[behaviorGroupIndex].items,
			afterBehavior: groups.slice(behaviorGroupIndex + 1),
		};
	}, [groups, notificationSettingsLabel, showMuteMenuItem]);
	return (
		<>
			<DataMenuRenderer
				groups={splitGroups.beforeBehavior}
				excludeLabels={excludeLabels}
				data-flx="ui.action-menu.channel-context-menu.data-menu-renderer"
			/>
			{showMuteMenuItem && (
				<MenuGroup data-flx="ui.action-menu.channel-context-menu.menu-group">
					<MuteChannelMenuItem
						channel={channel}
						onClose={onClose}
						data-flx="ui.action-menu.channel-context-menu.mute-channel-menu-item"
					/>
					<DataMenuItemsRenderer
						items={splitGroups.behaviorItems}
						excludeLabels={excludeLabels}
						data-flx="ui.action-menu.channel-context-menu.data-menu-items-renderer"
					/>
				</MenuGroup>
			)}
			<DataMenuRenderer
				groups={splitGroups.afterBehavior}
				excludeLabels={excludeLabels}
				data-flx="ui.action-menu.channel-context-menu.data-menu-renderer--2"
			/>
		</>
	);
});
