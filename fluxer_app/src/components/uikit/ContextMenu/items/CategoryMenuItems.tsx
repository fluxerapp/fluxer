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
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ReadStateActionCreators from '~/actions/ReadStateActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import {ChannelTypes, MessageNotifications, Permissions} from '~/Constants';
import {ChannelSettingsModal} from '~/components/modals/ChannelSettingsModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import type {ChannelRecord} from '~/records/ChannelRecord';
import ChannelStore from '~/stores/ChannelStore';
import PermissionStore from '~/stores/PermissionStore';
import ReadStateStore from '~/stores/ReadStateStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import {getMutedText, getNotificationSettingsLabel} from '~/utils/ContextMenuUtils';
import {
	CopyIdIcon,
	DeleteIcon,
	MarkAsReadIcon,
	MuteIcon,
	NotificationSettingsIcon,
	SettingsIcon,
} from '../ContextMenuIcons';
import {MenuGroup} from '../MenuGroup';
import {MenuItem} from '../MenuItem';
import menuItemStyles from '../MenuItem.module.css';
import {MenuItemCheckbox} from '../MenuItemCheckbox';
import {MenuItemRadio} from '../MenuItemRadio';
import {MenuItemSubmenu} from '../MenuItemSubmenu';
import itemStyles from './MenuItems.module.css';

interface CategoryMenuItemProps {
	category: ChannelRecord;
	onClose: () => void;
}

export const MarkCategoryAsReadMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {t} = useLingui();
	const guildId = category.guildId!;
	const channels = ChannelStore.getGuildChannels(guildId);

	const channelsInCategory = React.useMemo(
		() => channels.filter((ch) => ch.parentId === category.id && ch.type !== ChannelTypes.GUILD_CATEGORY),
		[channels, category.id],
	);

	const hasUnread = channelsInCategory.some((ch) => ReadStateStore.hasUnread(ch.id));

	const handleMarkAsRead = React.useCallback(() => {
		for (const channel of channelsInCategory) {
			ReadStateActionCreators.ack(channel.id, true, true);
		}
		onClose();
	}, [channelsInCategory, onClose]);

	return (
		<MenuItem icon={<MarkAsReadIcon />} onClick={handleMarkAsRead} disabled={!hasUnread}>
			{t`Mark as Read`}
		</MenuItem>
	);
});

export const CollapseCategoryMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {t} = useLingui();
	const guildId = category.guildId!;
	const isCollapsed = UserGuildSettingsStore.isChannelCollapsed(guildId, category.id);

	const handleToggleCollapse = React.useCallback(() => {
		UserGuildSettingsActionCreators.toggleChannelCollapsed(guildId, category.id);
		onClose();
	}, [guildId, category.id, onClose]);

	return (
		<MenuItemCheckbox checked={isCollapsed} onChange={handleToggleCollapse}>{t`Collapse Category`}</MenuItemCheckbox>
	);
});

export const CollapseAllCategoriesMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {t} = useLingui();
	const guildId = category.guildId!;
	const channels = ChannelStore.getGuildChannels(guildId);

	const categoryIds = React.useMemo(
		() => channels.filter((ch) => ch.type === ChannelTypes.GUILD_CATEGORY).map((ch) => ch.id),
		[channels],
	);

	const allCategoriesCollapsed = React.useMemo(() => {
		if (categoryIds.length === 0) return false;
		return categoryIds.every((categoryId) => UserGuildSettingsStore.isChannelCollapsed(guildId, categoryId));
	}, [guildId, categoryIds]);

	const handleToggleCollapseAll = React.useCallback(() => {
		UserGuildSettingsActionCreators.toggleAllCategoriesCollapsed(guildId, categoryIds);
		onClose();
	}, [guildId, categoryIds, onClose]);

	return (
		<MenuItemCheckbox checked={allCategoriesCollapsed} onChange={handleToggleCollapseAll}>
			{t`Collapse All Categories`}
		</MenuItemCheckbox>
	);
});

interface MuteDuration {
	label: string;
	value: number | null;
}

export const MuteCategoryMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {t} = useLingui();
	const guildId = category.guildId!;
	const categoryOverride = UserGuildSettingsStore.getChannelOverride(guildId, category.id);
	const isMuted = categoryOverride?.muted ?? false;
	const muteConfig = categoryOverride?.mute_config;

	const getMuteDurations = React.useCallback(
		(): Array<MuteDuration> => [
			{label: t`For 15 Minutes`, value: 15 * 60 * 1000},
			{label: t`For 1 Hour`, value: 60 * 60 * 1000},
			{label: t`For 3 Hours`, value: 3 * 60 * 60 * 1000},
			{label: t`For 8 Hours`, value: 8 * 60 * 60 * 1000},
			{label: t`For 24 Hours`, value: 24 * 60 * 60 * 1000},
			{label: t`Until I turn it back on`, value: null},
		],
		[t],
	);

	const mutedText = getMutedText(isMuted, muteConfig);

	const handleMute = React.useCallback(
		(duration: number | null) => {
			const nextMuteConfig = duration
				? {
						selected_time_window: duration,
						end_time: new Date(Date.now() + duration).toISOString(),
					}
				: null;

			UserGuildSettingsActionCreators.updateChannelOverride(guildId, category.id, {
				muted: true,
				mute_config: nextMuteConfig,
				collapsed: true,
			});
			onClose();
		},
		[guildId, category.id, onClose],
	);

	const handleUnmute = React.useCallback(() => {
		UserGuildSettingsActionCreators.updateChannelOverride(guildId, category.id, {
			muted: false,
			mute_config: null,
		});
		onClose();
	}, [guildId, category.id, onClose]);

	if (isMuted && mutedText) {
		return (
			<MenuItem icon={<MuteIcon />} onClick={handleUnmute} hint={mutedText}>
				{t`Unmute Category`}
			</MenuItem>
		);
	}

	return (
		<MenuItemSubmenu
			label={t`Mute Category`}
			icon={<MuteIcon />}
			onTriggerSelect={() => handleMute(null)}
			render={() => (
				<MenuGroup>
					{getMuteDurations().map((duration) => (
						<MenuItem key={duration.label} onClick={() => handleMute(duration.value)}>
							{duration.label}
						</MenuItem>
					))}
				</MenuGroup>
			)}
		/>
	);
});

export const CategoryNotificationSettingsMenuItem: React.FC<CategoryMenuItemProps> = observer(({category}) => {
	const {t} = useLingui();
	const guildId = category.guildId!;
	const categoryNotifications = UserGuildSettingsStore.getChannelOverride(guildId, category.id)?.message_notifications;
	const currentNotificationLevel = categoryNotifications ?? MessageNotifications.INHERIT;

	const guildNotificationLevel = UserGuildSettingsStore.getGuildMessageNotifications(guildId);

	const effectiveCurrentNotificationLevel =
		currentNotificationLevel === MessageNotifications.INHERIT ? guildNotificationLevel : currentNotificationLevel;

	const currentStateText = getNotificationSettingsLabel(effectiveCurrentNotificationLevel);

	const defaultLabelParts = {
		main: t`Community Default`,
		sub: getNotificationSettingsLabel(guildNotificationLevel) ?? null,
	};

	const handleNotificationLevelChange = React.useCallback(
		(level: number) => {
			if (level === MessageNotifications.INHERIT) {
				UserGuildSettingsActionCreators.updateChannelOverride(guildId, category.id, {
					message_notifications: MessageNotifications.INHERIT,
				});
			} else {
				UserGuildSettingsActionCreators.updateMessageNotifications(guildId, level, category.id);
			}
		},
		[guildId, category.id],
	);

	return (
		<MenuItemSubmenu
			label={t`Notification Settings`}
			icon={<NotificationSettingsIcon />}
			hint={currentStateText}
			render={() => (
				<MenuGroup>
					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.INHERIT}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.INHERIT)}
					>
						<div className={itemStyles.flexColumn}>
							<span>{defaultLabelParts.main}</span>
							{defaultLabelParts.sub && <div className={menuItemStyles.subtext}>{defaultLabelParts.sub}</div>}
						</div>
					</MenuItemRadio>

					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.ALL_MESSAGES}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.ALL_MESSAGES)}
					>
						{t`All Messages`}
					</MenuItemRadio>

					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.ONLY_MENTIONS}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.ONLY_MENTIONS)}
					>
						{t`Only @mentions`}
					</MenuItemRadio>

					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.NO_MESSAGES}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.NO_MESSAGES)}
					>
						{t`Nothing`}
					</MenuItemRadio>
				</MenuGroup>
			)}
		/>
	);
});

export const EditCategoryMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {t} = useLingui();
	const guildId = category.guildId!;

	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {
		channelId: category.id,
		guildId,
	});

	const handleEditCategory = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <ChannelSettingsModal channelId={category.id} />));
		onClose();
	}, [category.id, onClose]);

	if (!canManageChannels) return null;

	return (
		<MenuItem icon={<SettingsIcon />} onClick={handleEditCategory}>
			{t`Edit Category`}
		</MenuItem>
	);
});

export const DeleteCategoryMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {t} = useLingui();
	const guildId = category.guildId!;

	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {
		channelId: category.id,
		guildId,
	});

	const channels = ChannelStore.getGuildChannels(guildId);
	const channelsInCategory = React.useMemo(
		() => channels.filter((ch) => ch.parentId === category.id && ch.type !== ChannelTypes.GUILD_CATEGORY),
		[channels, category.id],
	);

	const handleDeleteCategory = React.useCallback(() => {
		onClose();

		const categoryName = category.name ?? '';
		const channelCount = channelsInCategory.length;
		const hasChannels = channelCount > 0;

		const description = hasChannels
			? channelCount === 1
				? t`Are you sure you want to delete the category "${categoryName}"? All ${channelCount} channel inside will be moved to the top of the channel list.`
				: t`Are you sure you want to delete the category "${categoryName}"? All ${channelCount} channels inside will be moved to the top of the channel list.`
			: t`Are you sure you want to delete the category "${categoryName}"? This cannot be undone.`;

		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Delete Category`}
					description={description}
					primaryText={t`Delete Category`}
					primaryVariant="danger-primary"
					onPrimary={async () => {
						try {
							await ChannelActionCreators.remove(category.id);
							ToastActionCreators.createToast({
								type: 'success',
								children: t`Category deleted`,
							});
						} catch (error) {
							console.error('Failed to delete category:', error);
							ToastActionCreators.createToast({
								type: 'error',
								children: t`Failed to delete category`,
							});
						}
					}}
				/>
			)),
		);
	}, [category.id, category.name, channelsInCategory.length, onClose, t]);

	if (!canManageChannels) return null;

	return (
		<MenuItem icon={<DeleteIcon />} onClick={handleDeleteCategory} danger>
			{t`Delete Category`}
		</MenuItem>
	);
});

export const CopyCategoryIdMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {t, i18n} = useLingui();

	const handleCopyId = React.useCallback(() => {
		TextCopyActionCreators.copy(i18n, category.id);
		onClose();
	}, [category.id, onClose, i18n]);

	return (
		<MenuItem icon={<CopyIdIcon />} onClick={handleCopyId}>
			{t`Copy Category ID`}
		</MenuItem>
	);
});
