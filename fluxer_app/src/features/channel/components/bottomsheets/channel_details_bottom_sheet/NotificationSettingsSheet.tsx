// SPDX-License-Identifier: AGPL-3.0-or-later

import {CATEGORY_DEFAULT_DESCRIPTOR} from '@app/features/channel/components/bottomsheets/channel_details_bottom_sheet/ChannelDetailsBottomSheetShared';
import type {Channel} from '@app/features/channel/models/Channel';
import {
	COMMUNITY_DEFAULT_DESCRIPTOR,
	COMMUNITY_NOTIFICATION_SETTINGS_DESCRIPTOR,
	NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR,
	NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR,
	NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR,
	NOTIFICATION_SETTINGS_DESCRIPTOR,
	OPEN_NAMED_LANDMARK_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {SettingsIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import type {MenuGroupType, MenuItemType, MenuRadioType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {getNotificationSettingsLabel} from '@app/lib/overlay/OverlayContextMenu';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useMemo} from 'react';

interface NotificationSettingsSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channel: Channel;
	guildId: string | null;
	onChangeLevel: (level: number) => void;
	onOpenGuildSettings: () => void;
}

export const NotificationSettingsSheet: React.FC<NotificationSettingsSheetProps> = ({
	isOpen,
	onClose,
	channel,
	guildId,
	onChangeLevel,
	onOpenGuildSettings,
}) => {
	const {i18n} = useLingui();
	const groups = useMemo((): Array<MenuGroupType> => {
		const categoryId = channel.parentId;
		const hasCategory = categoryId != null;
		const channelNotifications = UserGuildSettings.getChannelOverride(guildId, channel.id)?.message_notifications;
		const currentNotificationLevel = channelNotifications ?? MessageNotifications.INHERIT;
		const communityNotificationSettingsLabel = i18n._(COMMUNITY_NOTIFICATION_SETTINGS_DESCRIPTOR);
		const categoryDefaultLevel = guildId
			? UserGuildSettings.resolveEffectiveMessageNotifications({
					id: channel.parentId ?? '',
					guildId: channel.guildId,
					type: ChannelTypes.GUILD_CATEGORY,
				})
			: MessageNotifications.ALL_MESSAGES;
		const defaultSubtext = getNotificationSettingsLabel(categoryDefaultLevel) ?? undefined;
		return [
			{
				items: [
					{
						label: hasCategory ? i18n._(CATEGORY_DEFAULT_DESCRIPTOR) : i18n._(COMMUNITY_DEFAULT_DESCRIPTOR),
						subtext: defaultSubtext,
						selected: currentNotificationLevel === MessageNotifications.INHERIT,
						onSelect: () => onChangeLevel(MessageNotifications.INHERIT),
					},
					{
						label: i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR),
						selected: currentNotificationLevel === MessageNotifications.ALL_MESSAGES,
						onSelect: () => onChangeLevel(MessageNotifications.ALL_MESSAGES),
					},
					{
						label: i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR),
						selected: currentNotificationLevel === MessageNotifications.ONLY_MENTIONS,
						onSelect: () => onChangeLevel(MessageNotifications.ONLY_MENTIONS),
					},
					{
						label: i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR),
						selected: currentNotificationLevel === MessageNotifications.NO_MESSAGES,
						onSelect: () => onChangeLevel(MessageNotifications.NO_MESSAGES),
					},
				] as Array<MenuRadioType>,
			},
			{
				items: [
					{
						id: 'open-guild-settings',
						icon: <SettingsIcon size={20} data-flx="channel.channel-details-bottom-sheet.settings-icon" />,
						label: i18n._(OPEN_NAMED_LANDMARK_DESCRIPTOR, {
							landmarkName: communityNotificationSettingsLabel,
						}),
						onClick: onOpenGuildSettings,
					},
				] as Array<MenuItemType>,
			},
		];
	}, [guildId, channel.id, channel.parentId, onChangeLevel, onOpenGuildSettings, i18n.locale]);
	return (
		<MenuBottomSheet
			isOpen={isOpen}
			onClose={onClose}
			title={i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR)}
			groups={groups}
			data-flx="channel.channel-details-bottom-sheet.menu-bottom-sheet--2"
		/>
	);
};
