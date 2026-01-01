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
import {StarIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ReadStateActionCreators from '~/actions/ReadStateActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import {ChannelTypes, ME, MessageNotifications, Permissions} from '~/Constants';
import {createMuteConfig, getMuteDurationOptions} from '~/components/channel/muteOptions';
import {ChannelSettingsModal} from '~/components/modals/ChannelSettingsModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {GuildNotificationSettingsModal} from '~/components/modals/GuildNotificationSettingsModal';
import {InviteModal} from '~/components/modals/InviteModal';
import type {ChannelRecord} from '~/records/ChannelRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import FavoritesStore from '~/stores/FavoritesStore';
import PermissionStore from '~/stores/PermissionStore';
import ReadStateStore from '~/stores/ReadStateStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import {getMutedText, getNotificationSettingsLabel} from '~/utils/ContextMenuUtils';
import * as InviteUtils from '~/utils/InviteUtils';
import {buildChannelLink} from '~/utils/messageLinkUtils';
import {
	CopyIdIcon,
	CopyLinkIcon,
	DeleteIcon,
	EditIcon,
	InviteIcon,
	MarkAsReadIcon,
	MuteIcon,
	NotificationSettingsIcon,
} from '../ContextMenuIcons';
import {MenuGroup} from '../MenuGroup';
import {MenuItem} from '../MenuItem';
import menuItemStyles from '../MenuItem.module.css';
import {MenuItemRadio} from '../MenuItemRadio';
import {MenuItemSubmenu} from '../MenuItemSubmenu';
import itemStyles from './MenuItems.module.css';

interface ChannelMenuItemProps {
	channel: ChannelRecord;
	onClose: () => void;
}

export const MarkChannelAsReadMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {t} = useLingui();
	const hasUnread = ReadStateStore.hasUnread(channel.id);

	const handleMarkAsRead = React.useCallback(() => {
		ReadStateActionCreators.ack(channel.id, true, true);
		onClose();
	}, [channel.id, onClose]);

	return (
		<MenuItem icon={<MarkAsReadIcon />} onClick={handleMarkAsRead} disabled={!hasUnread}>
			{t`Mark as Read`}
		</MenuItem>
	);
});

export const InvitePeopleToChannelMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {t} = useLingui();
	const canInvite = InviteUtils.canInviteToChannel(channel.id, channel.guildId);

	const handleInvite = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <InviteModal channelId={channel.id} />));
		onClose();
	}, [channel.id, onClose]);

	if (!canInvite) return null;

	return (
		<MenuItem icon={<InviteIcon />} onClick={handleInvite}>
			{t`Invite People`}
		</MenuItem>
	);
});

export const CopyChannelLinkMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {t, i18n} = useLingui();
	const handleCopyLink = React.useCallback(() => {
		const channelLink = buildChannelLink({
			guildId: channel.guildId,
			channelId: channel.id,
		});
		TextCopyActionCreators.copy(i18n, channelLink);
		onClose();
	}, [channel.id, channel.guildId, onClose, i18n]);

	return (
		<MenuItem icon={<CopyLinkIcon />} onClick={handleCopyLink}>
			{t`Copy Link`}
		</MenuItem>
	);
});

export const MuteChannelMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {t} = useLingui();
	const guildId = channel.guildId!;
	const channelOverride = UserGuildSettingsStore.getChannelOverride(guildId, channel.id);
	const isMuted = channelOverride?.muted ?? false;
	const muteConfig = channelOverride?.mute_config;

	const mutedText = getMutedText(isMuted, muteConfig);

	const handleMute = React.useCallback(
		(duration: number | null) => {
			UserGuildSettingsActionCreators.updateChannelOverride(
				channel.guildId!,
				channel.id,
				{
					muted: true,
					mute_config: createMuteConfig(duration),
				},
				{persistImmediately: true},
			);
			onClose();
		},
		[channel.guildId, channel.id, onClose],
	);

	const handleUnmute = React.useCallback(() => {
		UserGuildSettingsActionCreators.updateChannelOverride(
			channel.guildId!,
			channel.id,
			{
				muted: false,
				mute_config: null,
			},
			{persistImmediately: true},
		);
		onClose();
	}, [channel.guildId, channel.id, onClose]);

	if (isMuted) {
		return (
			<MenuItem icon={<MuteIcon />} onClick={handleUnmute} hint={mutedText ?? undefined}>
				{t`Unmute Channel`}
			</MenuItem>
		);
	}

	return (
		<MenuItemSubmenu
			label={t`Mute Channel`}
			icon={<MuteIcon />}
			onTriggerSelect={() => handleMute(null)}
			render={() => (
				<MenuGroup>
					{getMuteDurationOptions(t).map((option) => (
						<MenuItem key={option.label} onClick={() => handleMute(option.value)}>
							{option.label}
						</MenuItem>
					))}
				</MenuGroup>
			)}
		/>
	);
});

export const ChannelNotificationSettingsMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {t} = useLingui();
	const guildId = channel.guildId;

	const handleNotificationLevelChange = React.useCallback(
		(level: number) => {
			if (!guildId) return;
			if (level === MessageNotifications.INHERIT) {
				UserGuildSettingsActionCreators.updateChannelOverride(
					guildId,
					channel.id,
					{
						message_notifications: MessageNotifications.INHERIT,
					},
					{persistImmediately: true},
				);
			} else {
				UserGuildSettingsActionCreators.updateMessageNotifications(guildId, level, channel.id, {
					persistImmediately: true,
				});
			}
		},
		[guildId, channel.id],
	);

	const handleOpenGuildNotificationSettings = React.useCallback(() => {
		if (!guildId) return;
		ModalActionCreators.push(modal(() => <GuildNotificationSettingsModal guildId={guildId} />));
		onClose();
	}, [guildId, onClose]);

	if (!guildId) return null;
	const channelNotifications = UserGuildSettingsStore.getChannelOverride(guildId, channel.id)?.message_notifications;
	const currentNotificationLevel = channelNotifications ?? MessageNotifications.INHERIT;

	const guildNotificationLevel = UserGuildSettingsStore.getGuildMessageNotifications(guildId);

	const categoryId = channel.parentId;
	const categoryOverride = UserGuildSettingsStore.getChannelOverride(guildId, categoryId ?? '');
	const categoryNotifications = categoryId ? categoryOverride?.message_notifications : undefined;

	const resolveEffectiveLevel = (level: number | undefined, fallback: number): number => {
		if (level === undefined || level === MessageNotifications.INHERIT) {
			return fallback;
		}
		return level;
	};

	const categoryDefaultLevel = resolveEffectiveLevel(categoryNotifications, guildNotificationLevel);
	const effectiveCurrentNotificationLevel =
		currentNotificationLevel === MessageNotifications.INHERIT ? categoryDefaultLevel : currentNotificationLevel;
	const hasCategory = categoryId != null;

	const currentStateText = getNotificationSettingsLabel(effectiveCurrentNotificationLevel);
	const defaultLabelParts = {
		main: hasCategory ? t`Category Default` : t`Community Default`,
		sub: getNotificationSettingsLabel(categoryDefaultLevel) ?? null,
	};

	return (
		<MenuItemSubmenu
			label={t`Notification Settings`}
			icon={<NotificationSettingsIcon />}
			hint={currentStateText}
			onTriggerSelect={handleOpenGuildNotificationSettings}
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

export const EditChannelMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {t} = useLingui();
	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {
		channelId: channel.id,
		guildId: channel.guildId,
	});

	const handleEditChannel = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <ChannelSettingsModal channelId={channel.id} />));
		onClose();
	}, [channel.id, onClose]);

	if (!canManageChannels) return null;

	return (
		<MenuItem icon={<EditIcon />} onClick={handleEditChannel}>
			{t`Edit Channel`}
		</MenuItem>
	);
});

export const DeleteChannelMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {t} = useLingui();
	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {
		channelId: channel.id,
		guildId: channel.guildId,
	});

	const handleDeleteChannel = React.useCallback(() => {
		onClose();
		const channelType = channel.type === ChannelTypes.GUILD_VOICE ? t`Voice Channel` : t`Text Channel`;
		const channelName = channel.name ?? 'this channel';

		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Delete ${channelType}`}
					description={t`Are you sure you want to delete #${channelName}? This cannot be undone.`}
					primaryText={t`Delete Channel`}
					primaryVariant="danger-primary"
					onPrimary={async () => {
						try {
							await ChannelActionCreators.remove(channel.id);
							ToastActionCreators.createToast({
								type: 'success',
								children: t`Channel deleted`,
							});
						} catch (error) {
							console.error('Failed to delete channel:', error);
							ToastActionCreators.createToast({
								type: 'error',
								children: t`Failed to delete channel`,
							});
						}
					}}
				/>
			)),
		);
	}, [channel.id, channel.name, channel.type, onClose]);

	if (!canManageChannels) return null;

	return (
		<MenuItem icon={<DeleteIcon />} onClick={handleDeleteChannel} danger>
			{t`Delete Channel`}
		</MenuItem>
	);
});

export const CopyChannelIdMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {t, i18n} = useLingui();
	const handleCopyId = React.useCallback(() => {
		TextCopyActionCreators.copy(i18n, channel.id);
		onClose();
	}, [channel.id, onClose, i18n]);

	return (
		<MenuItem icon={<CopyIdIcon />} onClick={handleCopyId}>
			{t`Copy Channel ID`}
		</MenuItem>
	);
});

export const FavoriteChannelMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {t} = useLingui();
	const categories = FavoritesStore.sortedCategories;
	const isAlreadyFavorite = !!FavoritesStore.getChannel(channel.id);
	const favoriteLabel = React.useMemo(() => {
		if (channel.isDM()) {
			return t`Favorite DM`;
		}
		if (channel.isGroupDM()) {
			return t`Favorite Group DM`;
		}
		return t`Favorite Channel`;
	}, [channel]);
	const unfavoriteLabel = React.useMemo(() => {
		if (channel.isDM()) {
			return t`Unfavorite DM`;
		}
		if (channel.isGroupDM()) {
			return t`Unfavorite Group DM`;
		}
		return t`Unfavorite Channel`;
	}, [channel]);

	const handleAddToCategory = React.useCallback(
		(categoryId: string | null) => {
			const guildId = channel.guildId ?? ME;
			FavoritesStore.addChannel(channel.id, guildId, categoryId);
			ToastActionCreators.createToast({type: 'success', children: t`Channel added to favorites`});
			onClose();
		},
		[channel.id, channel.guildId, onClose],
	);

	const handleRemoveFromFavorites = React.useCallback(() => {
		FavoritesStore.removeChannel(channel.id);
		ToastActionCreators.createToast({type: 'success', children: t`Channel removed from favorites`});
		onClose();
	}, [channel.id, onClose]);

	if (!AccessibilityStore.showFavorites) return null;

	if (isAlreadyFavorite) {
		return (
			<MenuItem icon={<StarIcon weight="fill" />} onClick={handleRemoveFromFavorites}>
				{unfavoriteLabel}
			</MenuItem>
		);
	}

	if (categories.length === 0) {
		return (
			<MenuItem icon={<StarIcon weight="regular" />} onClick={() => handleAddToCategory(null)}>
				{favoriteLabel}
			</MenuItem>
		);
	}

	return (
		<MenuItemSubmenu
			label={favoriteLabel}
			icon={<StarIcon weight="regular" />}
			render={() => (
				<MenuGroup>
					<MenuItem onClick={() => handleAddToCategory(null)}>{t`Uncategorized`}</MenuItem>
					{categories.map((category) => (
						<MenuItem key={category.id} onClick={() => handleAddToCategory(category.id)}>
							{category.name}
						</MenuItem>
					))}
				</MenuGroup>
			)}
		/>
	);
});
