// SPDX-License-Identifier: AGPL-3.0-or-later

import {showChannelDeleteFailedModal} from '@app/features/app/components/alerts/ChannelDeleteFailedModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import {getMuteDurationOptions} from '@app/features/channel/components/MuteOptions';
import {ChannelSettingsModal} from '@app/features/channel/components/modals/ChannelSettingsModal';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {DELETE_CATEGORY_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {
	COMMUNITY_DEFAULT_DESCRIPTOR,
	MARK_AS_READ_DESCRIPTOR,
	MUTE_CATEGORY_DESCRIPTOR,
	NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR,
	NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR,
	NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR,
	NOTIFICATION_SETTINGS_DESCRIPTOR,
	UNMUTE_CATEGORY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {
	CopyIdIcon,
	DeleteIcon,
	MarkAsReadIcon,
	MuteIcon,
	SettingsIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import itemStyles from '@app/features/ui/action_menu/items/MenuItems.module.css';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import menuItemStyles from '@app/features/ui/action_menu/MenuItem.module.css';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {getMutedText, getNotificationSettingsLabel} from '@app/lib/overlay/OverlayContextMenu';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {msg} from '@lingui/core/macro';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const COLLAPSE_CATEGORY_DESCRIPTOR = msg({
	message: 'Collapse category',
	comment: 'Action that collapses the channel category section.',
});
const COLLAPSE_ALL_CATEGORIES_DESCRIPTOR = msg({
	message: 'Collapse all categories',
	comment: 'Action label that collapses every channel category.',
});
const EDIT_CATEGORY_DESCRIPTOR = msg({
	message: 'Edit category',
	comment: 'Action that opens the edit-category modal.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THE_DESCRIPTOR = msg({
	message: 'Are you sure you want to delete the category "{categoryName}"? This cannot be undone.',
	comment: 'Confirm dialog body before deleting an empty channel category.',
});
const CATEGORY_DELETED_DESCRIPTOR = msg({
	message: 'Category deleted',
	comment: 'Toast confirming a channel category was deleted.',
});
const COPY_CATEGORY_ID_DESCRIPTOR = msg({
	message: 'Copy category ID',
	comment: 'Developer-mode action that copies the category ID to the clipboard.',
});
const logger = new Logger('CategoryMenuItems');

interface CategoryMenuItemProps {
	category: Channel;
	onClose: () => void;
}

export const MarkCategoryAsReadMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {i18n} = useLingui();
	const guildId = category.guildId!;
	const channels = Channels.getGuildChannels(guildId);
	const channelsInCategory = useMemo(
		() => channels.filter((ch) => ch.parentId === category.id && ch.type !== ChannelTypes.GUILD_CATEGORY),
		[channels, category.id],
	);
	const hasUnread = channelsInCategory.some((ch) => ReadStates.hasUnread(ch.id));
	const handleMarkAsRead = useCallback(() => {
		for (const channel of channelsInCategory) {
			ReadStateCommands.ack(channel.id, true, true);
		}
		onClose();
	}, [channelsInCategory, onClose]);
	return (
		<MenuItem
			icon={
				<MarkAsReadIcon data-flx="ui.action-menu.items.category-menu-items.mark-category-as-read-menu-item.mark-as-read-icon" />
			}
			onClick={handleMarkAsRead}
			disabled={!hasUnread}
			data-flx="ui.action-menu.items.category-menu-items.mark-category-as-read-menu-item.menu-item.mark-as-read"
		>
			{i18n._(MARK_AS_READ_DESCRIPTOR)}
		</MenuItem>
	);
});
export const CollapseCategoryMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {i18n} = useLingui();
	const guildId = category.guildId!;
	const isCollapsed = UserGuildSettings.isChannelSectionCollapsed(guildId, category.id);
	const handleToggleCollapse = useCallback(() => {
		UserGuildSettingsCommands.toggleChannelCollapsed(guildId, category.id);
		onClose();
	}, [guildId, category.id, onClose]);
	return (
		<CheckboxItem
			checked={isCollapsed}
			onCheckedChange={handleToggleCollapse}
			data-flx="ui.action-menu.items.category-menu-items.collapse-category-menu-item.checkbox-item"
		>
			{i18n._(COLLAPSE_CATEGORY_DESCRIPTOR)}
		</CheckboxItem>
	);
});
export const CollapseAllCategoriesMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {i18n} = useLingui();
	const guildId = category.guildId!;
	const channels = Channels.getGuildChannels(guildId);
	const categoryIds = useMemo(
		() => channels.filter((ch) => ch.type === ChannelTypes.GUILD_CATEGORY).map((ch) => ch.id),
		[channels],
	);
	const allCategoriesCollapsed = useMemo(() => {
		if (categoryIds.length === 0) return false;
		return categoryIds.every((categoryId) => UserGuildSettings.isChannelSectionCollapsed(guildId, categoryId));
	}, [guildId, categoryIds]);
	const handleToggleCollapseAll = useCallback(() => {
		UserGuildSettingsCommands.toggleAllCategoriesCollapsed(guildId, categoryIds);
		onClose();
	}, [guildId, categoryIds, onClose]);
	return (
		<CheckboxItem
			checked={allCategoriesCollapsed}
			onCheckedChange={handleToggleCollapseAll}
			data-flx="ui.action-menu.items.category-menu-items.collapse-all-categories-menu-item.checkbox-item"
		>
			{i18n._(COLLAPSE_ALL_CATEGORIES_DESCRIPTOR)}
		</CheckboxItem>
	);
});
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
export const CategoryNotificationSettingsMenuItem: React.FC<CategoryMenuItemProps> = observer(({category}) => {
	const {i18n} = useLingui();
	const guildId = category.guildId!;
	const categoryNotifications = UserGuildSettings.getChannelOverride(guildId, category.id)?.message_notifications;
	const currentNotificationLevel = categoryNotifications ?? MessageNotifications.INHERIT;
	const guildNotificationLevel = UserGuildSettings.getGuildMessageNotifications(guildId);
	const effectiveCurrentNotificationLevel =
		currentNotificationLevel === MessageNotifications.INHERIT ? guildNotificationLevel : currentNotificationLevel;
	const currentStateText = getNotificationSettingsLabel(effectiveCurrentNotificationLevel);
	const defaultLabelParts = {
		main: i18n._(COMMUNITY_DEFAULT_DESCRIPTOR),
		sub: getNotificationSettingsLabel(guildNotificationLevel) ?? null,
	};
	const handleNotificationLevelChange = useCallback(
		(level: number) => {
			if (level === MessageNotifications.INHERIT) {
				UserGuildSettingsCommands.updateChannelOverride(guildId, category.id, {
					message_notifications: MessageNotifications.INHERIT,
				});
			} else {
				UserGuildSettingsCommands.updateMessageNotifications(guildId, level, category.id);
			}
		},
		[guildId, category.id],
	);
	return (
		<MenuItemSubmenu
			label={i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR)}
			hint={currentStateText}
			render={() => (
				<MenuGroup data-flx="ui.action-menu.items.category-menu-items.category-notification-settings-menu-item.menu-group">
					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.INHERIT}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.INHERIT)}
						data-flx="ui.action-menu.items.category-menu-items.category-notification-settings-menu-item.menu-item-radio.notification-level-change"
					>
						<div
							className={itemStyles.flexColumn}
							data-flx="ui.action-menu.items.category-menu-items.category-notification-settings-menu-item.div"
						>
							<span data-flx="ui.action-menu.items.category-menu-items.category-notification-settings-menu-item.span">
								{defaultLabelParts.main}
							</span>
							{defaultLabelParts.sub && (
								<div
									className={menuItemStyles.subtext}
									data-flx="ui.action-menu.items.category-menu-items.category-notification-settings-menu-item.div--2"
								>
									{defaultLabelParts.sub}
								</div>
							)}
						</div>
					</MenuItemRadio>
					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.ALL_MESSAGES}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.ALL_MESSAGES)}
						data-flx="ui.action-menu.items.category-menu-items.category-notification-settings-menu-item.menu-item-radio.notification-level-change--2"
					>
						{i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR)}
					</MenuItemRadio>
					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.ONLY_MENTIONS}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.ONLY_MENTIONS)}
						data-flx="ui.action-menu.items.category-menu-items.category-notification-settings-menu-item.menu-item-radio.notification-level-change--3"
					>
						{i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR)}
					</MenuItemRadio>
					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.NO_MESSAGES}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.NO_MESSAGES)}
						data-flx="ui.action-menu.items.category-menu-items.category-notification-settings-menu-item.menu-item-radio.notification-level-change--4"
					>
						{i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR)}
					</MenuItemRadio>
				</MenuGroup>
			)}
			data-flx="ui.action-menu.items.category-menu-items.category-notification-settings-menu-item.menu-item-submenu"
		/>
	);
});
export const EditCategoryMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {i18n} = useLingui();
	const guildId = category.guildId!;
	const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, {
		channelId: category.id,
		guildId,
	});
	const handleEditCategory = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ChannelSettingsModal
					channelId={category.id}
					data-flx="ui.action-menu.items.category-menu-items.handle-edit-category.channel-settings-modal"
				/>
			)),
		);
		onClose();
	}, [category.id, onClose]);
	if (!canManageChannels) return null;
	return (
		<MenuItem
			icon={<SettingsIcon data-flx="ui.action-menu.items.category-menu-items.edit-category-menu-item.settings-icon" />}
			onClick={handleEditCategory}
			data-flx="ui.action-menu.items.category-menu-items.edit-category-menu-item.menu-item.edit-category"
		>
			{i18n._(EDIT_CATEGORY_DESCRIPTOR)}
		</MenuItem>
	);
});
export const DeleteCategoryMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {i18n} = useLingui();
	const guildId = category.guildId!;
	const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, {
		channelId: category.id,
		guildId,
	});
	const channels = Channels.getGuildChannels(guildId);
	const channelsInCategory = useMemo(
		() => channels.filter((ch) => ch.parentId === category.id && ch.type !== ChannelTypes.GUILD_CATEGORY),
		[channels, category.id],
	);
	const handleDeleteCategory = useCallback(() => {
		onClose();
		const categoryName = category.name ?? '';
		const channelCount = channelsInCategory.length;
		const hasChannels = channelCount > 0;
		const description = hasChannels ? (
			<Trans>
				Are you sure you want to delete the category "{categoryName}"? All{' '}
				<Plural
					value={channelCount}
					one="# channel"
					other="# channels"
					data-flx="ui.action-menu.items.category-menu-items.handle-delete-category.plural"
				/>{' '}
				inside will be moved to the top of the channel list.
			</Trans>
		) : (
			i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THE_DESCRIPTOR, {categoryName})
		);
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(DELETE_CATEGORY_DESCRIPTOR)}
					description={description}
					primaryText={i18n._(DELETE_CATEGORY_DESCRIPTOR)}
					primaryVariant="danger"
					onPrimary={async () => {
						try {
							await ChannelCommands.remove(category.id);
							ToastCommands.createToast({
								type: 'success',
								children: i18n._(CATEGORY_DELETED_DESCRIPTOR),
							});
						} catch (error) {
							logger.error('Failed to delete category:', error);
							showChannelDeleteFailedModal(error, 'category');
						}
					}}
					data-flx="ui.action-menu.items.category-menu-items.handle-delete-category.confirm-modal"
				/>
			)),
		);
	}, [category.id, category.name, channelsInCategory.length, i18n, onClose]);
	if (!canManageChannels) return null;
	return (
		<MenuItem
			icon={<DeleteIcon data-flx="ui.action-menu.items.category-menu-items.delete-category-menu-item.delete-icon" />}
			onClick={handleDeleteCategory}
			danger
			data-flx="ui.action-menu.items.category-menu-items.delete-category-menu-item.menu-item.delete-category"
		>
			{i18n._(DELETE_CATEGORY_DESCRIPTOR)}
		</MenuItem>
	);
});
export const CopyCategoryIdMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {i18n} = useLingui();
	const handleCopyId = useCallback(() => {
		TextCopyCommands.copy(i18n, category.id);
		onClose();
	}, [category.id, onClose, i18n]);
	return (
		<MenuItem
			icon={<CopyIdIcon data-flx="ui.action-menu.items.category-menu-items.copy-category-id-menu-item.copy-id-icon" />}
			onClick={handleCopyId}
			data-flx="ui.action-menu.items.category-menu-items.copy-category-id-menu-item.menu-item.copy-id"
		>
			{i18n._(COPY_CATEGORY_ID_DESCRIPTOR)}
		</MenuItem>
	);
});
