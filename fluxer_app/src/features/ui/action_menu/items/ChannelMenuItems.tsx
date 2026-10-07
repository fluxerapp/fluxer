// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {showChannelDeleteFailedModal} from '@app/features/app/components/alerts/ChannelDeleteFailedModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {ChannelSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import {createMuteConfig, getMuteDurationOptions} from '@app/features/channel/components/MuteOptions';
import type {Channel} from '@app/features/channel/models/Channel';
import {
	DELETE_CHANNEL_DESCRIPTOR,
	MUTE_CHANNEL_DESCRIPTOR,
	UNMUTE_CHANNEL_DESCRIPTOR,
} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {GuildNotificationSettingsModal} from '@app/features/guild/components/modals/GuildNotificationSettingsModal';
import {
	CHANNEL_ADDED_TO_FAVORITES_DESCRIPTOR,
	CHANNEL_DELETED_DESCRIPTOR,
	CHANNEL_REMOVED_FROM_FAVORITES_DESCRIPTOR,
	COMMUNITY_DEFAULT_DESCRIPTOR,
	COPY_CHANNEL_ID_DESCRIPTOR,
	COPY_LINK_DESCRIPTOR,
	INVITE_PEOPLE_DESCRIPTOR,
	MARK_AS_READ_DESCRIPTOR,
	NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR,
	NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR,
	NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR,
	NOTIFICATION_SETTINGS_DESCRIPTOR,
	UNCATEGORIZED_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {InviteModal} from '@app/features/invite/components/modals/InviteModal';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import Favorites from '@app/features/messaging/state/Favorites';
import {buildChannelLink} from '@app/features/messaging/utils/MessageLinkUtils';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {
	CopyIdIcon,
	CopyLinkIcon,
	DeleteIcon,
	EditSimpleIcon,
	FavoriteIcon,
	InviteIcon,
	MarkAsReadIcon,
	MuteIcon,
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
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes, GUILD_TEXT_BASED_CHANNEL_TYPES, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const CATEGORY_DEFAULT_DESCRIPTOR = msg({
	message: 'Category default',
	comment: 'Option label that uses the parent category default value.',
});
const EDIT_CHANNEL_DESCRIPTOR = msg({
	message: 'Edit channel',
	comment: 'Action that opens the edit-channel modal.',
});
const VOICE_CHANNEL_DESCRIPTOR = msg({
	message: 'voice channel',
	comment: 'Generic channel-type token used inside a wider delete confirmation string.',
});
const TEXT_CHANNEL_DESCRIPTOR = msg({
	message: 'text channel',
	comment: 'Generic channel-type token used inside a wider delete confirmation string.',
});
const THIS_CHANNEL_DESCRIPTOR = msg({
	message: 'this channel',
	comment: 'Generic fallback token used inside a wider delete confirmation string.',
});
const DELETE_DESCRIPTOR = msg({
	message: 'Delete {channelType}',
	comment: 'Confirm dialog title before deleting a channel; channelType is a localized channel-type label.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR = msg({
	message: 'Are you sure you want to delete {channelLabel}? This cannot be undone.',
	comment: 'Confirm dialog body before deleting a channel.',
});
const FAVORITE_DM_DESCRIPTOR = msg({
	message: 'Favorite DM',
	comment: 'Action that adds a DM to the favorites list.',
});
const FAVORITE_GROUP_DM_DESCRIPTOR = msg({
	message: 'Favorite group DM',
	comment: 'Action that adds a group DM to the favorites list.',
});
const FAVORITE_CHANNEL_DESCRIPTOR = msg({
	message: 'Favorite channel',
	comment: 'Action that adds a channel to the favorites list.',
});
const UNFAVORITE_DM_DESCRIPTOR = msg({
	message: 'Unfavorite DM',
	comment: 'Action that removes a DM from the favorites list.',
});
const UNFAVORITE_GROUP_DM_DESCRIPTOR = msg({
	message: 'Unfavorite group DM',
	comment: 'Action that removes a group DM from the favorites list.',
});
const UNFAVORITE_CHANNEL_DESCRIPTOR = msg({
	message: 'Unfavorite channel',
	comment: 'Action that removes a channel from the favorites list.',
});
const logger = new Logger('ChannelMenuItems');

interface ChannelMenuItemProps {
	channel: Channel;
	onClose: () => void;
}

interface GuildChannelMenuItemProps extends ChannelMenuItemProps {
	guildId: string;
}

export const MarkChannelAsReadMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();
	const hasUnread = ReadStates.hasUnread(channel.id);
	const handleMarkAsRead = useCallback(() => {
		ReadStateCommands.ack(channel.id, true, true);
		onClose();
	}, [channel.id, onClose]);
	return (
		<MenuItem
			icon={
				<MarkAsReadIcon data-flx="ui.action-menu.items.channel-menu-items.mark-channel-as-read-menu-item.mark-as-read-icon" />
			}
			onClick={handleMarkAsRead}
			disabled={!hasUnread}
			data-flx="ui.action-menu.items.channel-menu-items.mark-channel-as-read-menu-item.menu-item.mark-as-read"
		>
			{i18n._(MARK_AS_READ_DESCRIPTOR)}
		</MenuItem>
	);
});
export const InvitePeopleToChannelMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();
	const canInvite = InviteUtils.canInviteToChannel(channel.id, channel.guildId);
	const handleInvite = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<InviteModal
					channelId={channel.id}
					data-flx="ui.action-menu.items.channel-menu-items.handle-invite.invite-modal"
				/>
			)),
		);
		onClose();
	}, [channel.id, onClose]);
	if (!canInvite) return null;
	return (
		<MenuItem
			icon={
				<InviteIcon data-flx="ui.action-menu.items.channel-menu-items.invite-people-to-channel-menu-item.invite-icon" />
			}
			onClick={handleInvite}
			data-flx="ui.action-menu.items.channel-menu-items.invite-people-to-channel-menu-item.menu-item.invite"
		>
			{i18n._(INVITE_PEOPLE_DESCRIPTOR)}
		</MenuItem>
	);
});
export const CopyChannelLinkMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();
	const handleCopyLink = useCallback(() => {
		const channelLink = buildChannelLink({
			guildId: channel.guildId,
			channelId: channel.id,
		});
		TextCopyCommands.copy(i18n, channelLink);
		onClose();
	}, [channel.id, channel.guildId, onClose, i18n]);
	return (
		<MenuItem
			icon={
				<CopyLinkIcon data-flx="ui.action-menu.items.channel-menu-items.copy-channel-link-menu-item.copy-link-icon" />
			}
			onClick={handleCopyLink}
			data-flx="ui.action-menu.items.channel-menu-items.copy-channel-link-menu-item.menu-item.copy-link"
		>
			{i18n._(COPY_LINK_DESCRIPTOR)}
		</MenuItem>
	);
});
const ResolvedMuteChannelMenuItem: React.FC<GuildChannelMenuItemProps> = observer(({channel, onClose, guildId}) => {
	const {i18n} = useLingui();
	const channelOverride = UserGuildSettings.getChannelOverride(guildId, channel.id);
	const isMuted = channelOverride?.muted ?? false;
	const muteConfig = channelOverride?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);
	const handleMute = useCallback(
		(duration: number | null) => {
			UserGuildSettingsCommands.updateChannelOverride(
				guildId,
				channel.id,
				{
					muted: true,
					mute_config: createMuteConfig(duration),
				},
				{persistImmediately: true},
			);
			onClose();
		},
		[guildId, channel.id, onClose],
	);
	const handleUnmute = useCallback(() => {
		UserGuildSettingsCommands.updateChannelOverride(
			guildId,
			channel.id,
			{
				muted: false,
				mute_config: null,
			},
			{persistImmediately: true},
		);
		onClose();
	}, [guildId, channel.id, onClose]);
	if (isMuted) {
		return (
			<MenuItem
				icon={<MuteIcon data-flx="ui.action-menu.items.channel-menu-items.mute-channel-menu-item.mute-icon" />}
				onClick={handleUnmute}
				hint={mutedText ?? undefined}
				data-flx="ui.action-menu.items.channel-menu-items.mute-channel-menu-item.menu-item.unmute"
			>
				{i18n._(UNMUTE_CHANNEL_DESCRIPTOR)}
			</MenuItem>
		);
	}
	return (
		<MenuItemSubmenu
			label={i18n._(MUTE_CHANNEL_DESCRIPTOR)}
			onTriggerSelect={() => handleMute(null)}
			render={() => (
				<MenuGroup data-flx="ui.action-menu.items.channel-menu-items.mute-channel-menu-item.menu-group">
					{getMuteDurationOptions(i18n).map((option) => (
						<MenuItem
							key={option.label}
							onClick={() => handleMute(option.value)}
							data-flx="ui.action-menu.items.channel-menu-items.mute-channel-menu-item.menu-item.mute"
						>
							{option.label}
						</MenuItem>
					))}
				</MenuGroup>
			)}
			data-flx="ui.action-menu.items.channel-menu-items.mute-channel-menu-item.menu-item-submenu"
		/>
	);
});
export const MuteChannelMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const isChannelMuteable = GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type) || channel.isThreadOnly();
	const guildId = channel.guildId;
	if (!isChannelMuteable || !guildId) return null;
	return (
		<ResolvedMuteChannelMenuItem
			channel={channel}
			onClose={onClose}
			guildId={guildId}
			data-flx="ui.action-menu.items.channel-menu-items.mute-channel-menu-item.resolved-mute-channel-menu-item"
		/>
	);
});
export const ChannelNotificationSettingsMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();
	const guildId = channel.guildId;
	const handleNotificationLevelChange = useCallback(
		(level: number) => {
			if (!guildId) return;
			if (level === MessageNotifications.INHERIT) {
				UserGuildSettingsCommands.updateChannelOverride(
					guildId,
					channel.id,
					{
						message_notifications: MessageNotifications.INHERIT,
					},
					{persistImmediately: true},
				);
			} else {
				UserGuildSettingsCommands.updateMessageNotifications(guildId, level, channel.id, {
					persistImmediately: true,
				});
			}
		},
		[guildId, channel.id],
	);
	const handleOpenGuildNotificationSettings = useCallback(() => {
		if (!guildId) return;
		ModalCommands.push(
			modal(() => (
				<GuildNotificationSettingsModal
					guildId={guildId}
					data-flx="ui.action-menu.items.channel-menu-items.handle-open-guild-notification-settings.guild-notification-settings-modal"
				/>
			)),
		);
		onClose();
	}, [guildId, onClose]);
	if (!guildId) return null;
	const channelNotifications = UserGuildSettings.getChannelOverride(guildId, channel.id)?.message_notifications;
	const currentNotificationLevel = channelNotifications ?? MessageNotifications.INHERIT;
	const guildNotificationLevel = UserGuildSettings.getGuildMessageNotifications(guildId);
	const categoryId = channel.parentId;
	const categoryOverride = UserGuildSettings.getChannelOverride(guildId, categoryId ?? '');
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
		main: hasCategory ? i18n._(CATEGORY_DEFAULT_DESCRIPTOR) : i18n._(COMMUNITY_DEFAULT_DESCRIPTOR),
		sub: getNotificationSettingsLabel(categoryDefaultLevel) ?? null,
	};
	return (
		<MenuItemSubmenu
			label={i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR)}
			hint={currentStateText}
			onTriggerSelect={handleOpenGuildNotificationSettings}
			render={() => (
				<MenuGroup data-flx="ui.action-menu.items.channel-menu-items.channel-notification-settings-menu-item.menu-group">
					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.INHERIT}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.INHERIT)}
						data-flx="ui.action-menu.items.channel-menu-items.channel-notification-settings-menu-item.menu-item-radio.notification-level-change"
					>
						<div
							className={itemStyles.flexColumn}
							data-flx="ui.action-menu.items.channel-menu-items.channel-notification-settings-menu-item.div"
						>
							<span data-flx="ui.action-menu.items.channel-menu-items.channel-notification-settings-menu-item.span">
								{defaultLabelParts.main}
							</span>
							{defaultLabelParts.sub && (
								<div
									className={menuItemStyles.subtext}
									data-flx="ui.action-menu.items.channel-menu-items.channel-notification-settings-menu-item.div--2"
								>
									{defaultLabelParts.sub}
								</div>
							)}
						</div>
					</MenuItemRadio>
					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.ALL_MESSAGES}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.ALL_MESSAGES)}
						data-flx="ui.action-menu.items.channel-menu-items.channel-notification-settings-menu-item.menu-item-radio.notification-level-change--2"
					>
						{i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR)}
					</MenuItemRadio>
					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.ONLY_MENTIONS}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.ONLY_MENTIONS)}
						data-flx="ui.action-menu.items.channel-menu-items.channel-notification-settings-menu-item.menu-item-radio.notification-level-change--3"
					>
						{i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR)}
					</MenuItemRadio>
					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.NO_MESSAGES}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.NO_MESSAGES)}
						data-flx="ui.action-menu.items.channel-menu-items.channel-notification-settings-menu-item.menu-item-radio.notification-level-change--4"
					>
						{i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR)}
					</MenuItemRadio>
				</MenuGroup>
			)}
			data-flx="ui.action-menu.items.channel-menu-items.channel-notification-settings-menu-item.menu-item-submenu"
		/>
	);
});
export const EditChannelMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();
	const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, {
		channelId: channel.id,
		guildId: channel.guildId,
	});
	const canUpdateRtcRegion =
		channel.type === ChannelTypes.GUILD_VOICE &&
		Permission.can(Permissions.UPDATE_RTC_REGION, {channelId: channel.id, guildId: channel.guildId});
	const handleEditChannel = useCallback(() => {
		ModalCommands.push(
			modal(
				() => (
					<ChannelSettingsModal
						channelId={channel.id}
						data-flx="ui.action-menu.items.channel-menu-items.handle-edit-channel.channel-settings-modal"
					/>
				),
				'channel-settings',
			),
		);
		onClose();
	}, [channel.id, onClose]);
	if (!canManageChannels && !canUpdateRtcRegion) return null;
	return (
		<MenuItem
			icon={
				<EditSimpleIcon data-flx="ui.action-menu.items.channel-menu-items.edit-channel-menu-item.edit-simple-icon" />
			}
			onClick={handleEditChannel}
			data-flx="ui.action-menu.items.channel-menu-items.edit-channel-menu-item.menu-item.edit-channel"
		>
			{i18n._(EDIT_CHANNEL_DESCRIPTOR)}
		</MenuItem>
	);
});
export const DeleteChannelMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();
	const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, {
		channelId: channel.id,
		guildId: channel.guildId,
	});
	const handleDeleteChannel = useCallback(() => {
		onClose();
		const channelType =
			channel.type === ChannelTypes.GUILD_VOICE ? i18n._(VOICE_CHANNEL_DESCRIPTOR) : i18n._(TEXT_CHANNEL_DESCRIPTOR);
		const channelLabel = channel.name ? `#${channel.name}` : i18n._(THIS_CHANNEL_DESCRIPTOR);
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(DELETE_DESCRIPTOR, {channelType})}
					description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR, {channelLabel})}
					primaryText={i18n._(DELETE_CHANNEL_DESCRIPTOR)}
					primaryVariant="danger"
					onPrimary={async () => {
						try {
							await ChannelCommands.remove(channel.id);
							ToastCommands.createToast({
								type: 'success',
								children: i18n._(CHANNEL_DELETED_DESCRIPTOR),
							});
						} catch (error) {
							logger.error('Failed to delete channel:', error);
							showChannelDeleteFailedModal(error, 'channel');
						}
					}}
					data-flx="ui.action-menu.items.channel-menu-items.handle-delete-channel.confirm-modal"
				/>
			)),
		);
	}, [channel.id, channel.name, channel.type, i18n, onClose]);
	if (!canManageChannels) return null;
	return (
		<MenuItem
			icon={<DeleteIcon data-flx="ui.action-menu.items.channel-menu-items.delete-channel-menu-item.delete-icon" />}
			onClick={handleDeleteChannel}
			danger
			data-flx="ui.action-menu.items.channel-menu-items.delete-channel-menu-item.menu-item.delete-channel"
		>
			{i18n._(DELETE_CHANNEL_DESCRIPTOR)}
		</MenuItem>
	);
});
export const CopyChannelIdMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();
	const handleCopyId = useCallback(() => {
		TextCopyCommands.copy(i18n, channel.id);
		onClose();
	}, [channel.id, onClose, i18n]);
	return (
		<MenuItem
			icon={<CopyIdIcon data-flx="ui.action-menu.items.channel-menu-items.copy-channel-id-menu-item.copy-id-icon" />}
			onClick={handleCopyId}
			data-flx="ui.action-menu.items.channel-menu-items.copy-channel-id-menu-item.menu-item.copy-id"
		>
			{i18n._(COPY_CHANNEL_ID_DESCRIPTOR)}
		</MenuItem>
	);
});
export const FavoriteChannelMenuItem: React.FC<ChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();
	const categories = Favorites.sortedCategories;
	const isAlreadyFavorite = !!Favorites.getChannel(channel.id);
	const favoriteLabel = useMemo(() => {
		if (channel.isDM()) {
			return i18n._(FAVORITE_DM_DESCRIPTOR);
		}
		if (channel.isGroupDM()) {
			return i18n._(FAVORITE_GROUP_DM_DESCRIPTOR);
		}
		return i18n._(FAVORITE_CHANNEL_DESCRIPTOR);
	}, [channel, i18n.locale]);
	const unfavoriteLabel = useMemo(() => {
		if (channel.isDM()) {
			return i18n._(UNFAVORITE_DM_DESCRIPTOR);
		}
		if (channel.isGroupDM()) {
			return i18n._(UNFAVORITE_GROUP_DM_DESCRIPTOR);
		}
		return i18n._(UNFAVORITE_CHANNEL_DESCRIPTOR);
	}, [channel, i18n.locale]);
	const handleAddToCategory = useCallback(
		(categoryId: string | null) => {
			const guildId = channel.guildId ?? ME;
			Favorites.addChannel(channel.id, guildId, categoryId);
			ToastCommands.createToast({type: 'success', children: i18n._(CHANNEL_ADDED_TO_FAVORITES_DESCRIPTOR)});
			onClose();
		},
		[channel.id, channel.guildId, onClose],
	);
	const handleRemoveFromFavorites = useCallback(() => {
		Favorites.removeChannel(channel.id);
		ToastCommands.createToast({type: 'success', children: i18n._(CHANNEL_REMOVED_FROM_FAVORITES_DESCRIPTOR)});
		onClose();
	}, [channel.id, onClose]);
	if (!Accessibility.showFavorites) return null;
	if (isAlreadyFavorite) {
		return (
			<MenuItem
				icon={
					<FavoriteIcon
						filled
						data-flx="ui.action-menu.items.channel-menu-items.favorite-channel-menu-item.favorite-icon"
					/>
				}
				onClick={handleRemoveFromFavorites}
				data-flx="ui.action-menu.items.channel-menu-items.favorite-channel-menu-item.menu-item.remove-from-favorites"
			>
				{unfavoriteLabel}
			</MenuItem>
		);
	}
	if (categories.length === 0) {
		return (
			<MenuItem
				icon={
					<FavoriteIcon data-flx="ui.action-menu.items.channel-menu-items.favorite-channel-menu-item.favorite-icon--2" />
				}
				onClick={() => handleAddToCategory(null)}
				data-flx="ui.action-menu.items.channel-menu-items.favorite-channel-menu-item.menu-item.add-to-category"
			>
				{favoriteLabel}
			</MenuItem>
		);
	}
	return (
		<MenuItemSubmenu
			label={favoriteLabel}
			render={() => (
				<MenuGroup data-flx="ui.action-menu.items.channel-menu-items.favorite-channel-menu-item.menu-group">
					<MenuItem
						onClick={() => handleAddToCategory(null)}
						data-flx="ui.action-menu.items.channel-menu-items.favorite-channel-menu-item.menu-item.add-to-category--2"
					>
						{i18n._(UNCATEGORIZED_DESCRIPTOR)}
					</MenuItem>
					{categories.map((category) => (
						<MenuItem
							key={category.id}
							onClick={() => handleAddToCategory(category.id)}
							data-flx="ui.action-menu.items.channel-menu-items.favorite-channel-menu-item.menu-item.add-to-category--3"
						>
							{category.name}
						</MenuItem>
					))}
				</MenuGroup>
			)}
			data-flx="ui.action-menu.items.channel-menu-items.favorite-channel-menu-item.menu-item-submenu"
		/>
	);
});
