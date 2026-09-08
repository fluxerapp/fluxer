// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {showChannelDeleteFailedModal} from '@app/features/app/components/alerts/ChannelDeleteFailedModal';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import Authentication from '@app/features/auth/state/Authentication';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import * as LinkChannelCommands from '@app/features/channel/commands/LinkChannelCommands';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {ChannelDuplicateModal} from '@app/features/channel/components/modals/ChannelDuplicateModal';
import {ChannelSettingsModal} from '@app/features/channel/components/modals/ChannelSettingsModal';
import {EditGroupModal} from '@app/features/channel/components/modals/EditGroupModal';
import {GroupInvitesModal} from '@app/features/channel/components/modals/GroupInvitesModal';
import {useDeleteMyMessagesInChannel} from '@app/features/channel/hooks/useDeleteMyMessagesInChannel';
import type {Channel} from '@app/features/channel/models/Channel';
import {duplicateChannel, getDuplicateChannelDefaultValues} from '@app/features/channel/utils/ChannelCreateModalUtils';
import {
	CLOSE_DM_DESCRIPTOR,
	DELETE_CHANNEL_DESCRIPTOR,
	DELETE_MY_MESSAGES_DESCRIPTOR,
	LEAVE_GROUP_DESCRIPTOR,
	MUTE_CHANNEL_DESCRIPTOR,
	PIN_DM_DESCRIPTOR,
	PIN_GROUP_DM_DESCRIPTOR,
	UNMUTE_CHANNEL_DESCRIPTOR,
	UNPIN_DM_DESCRIPTOR,
	UNPIN_GROUP_DM_DESCRIPTOR,
} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {ChannelDebugModal} from '@app/features/devtools/components/debug/ChannelDebugModal';
import {GuildNotificationSettingsModal} from '@app/features/guild/components/modals/GuildNotificationSettingsModal';
import {useLeaveGroup} from '@app/features/guild/hooks/useLeaveGroup';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import {
	ADD_TO_FAVORITES_DESCRIPTOR,
	ADDED_TO_FAVORITES_TOAST_DESCRIPTOR,
	CHANNEL_DEBUG_DESCRIPTOR,
	CHANNEL_DELETED_DESCRIPTOR,
	COPY_CHANNEL_ID_DESCRIPTOR,
	DEBUG_CHANNEL_DESCRIPTOR,
	EDIT_GROUP_DESCRIPTOR,
	INVITE_PEOPLE_DESCRIPTOR,
	INVITES_DESCRIPTOR,
	MARK_AS_READ_DESCRIPTOR,
	NOTIFICATION_SETTINGS_DESCRIPTOR,
	OPEN_LINK_DESCRIPTOR,
	PINNED_DM_DESCRIPTOR,
	REMOVE_FROM_FAVORITES_DESCRIPTOR,
	REMOVED_FROM_FAVORITES_TOAST_DESCRIPTOR,
	RESET_MATURE_CONTENT_AGREE_STATE_DESCRIPTOR,
	UNPINNED_DM_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {InviteModal} from '@app/features/invite/components/modals/InviteModal';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import Favorites from '@app/features/messaging/state/Favorites';
import {buildChannelLink} from '@app/features/messaging/utils/MessageLinkUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {
	CloseDMIcon,
	CopyIcon,
	CopyIdIcon,
	CopyLinkIcon,
	DebugChannelIcon,
	DeleteIcon,
	EditGroupIcon,
	FavoriteIcon,
	InviteIcon,
	LeaveIcon,
	MarkAsReadIcon,
	MessageUserIcon,
	MuteIcon,
	NotificationSettingsIcon,
	OpenLinkIcon,
	PinIcon,
	SendInvitesIcon,
	SettingsIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {MenuActionEvent, MenuGroupType, MenuItemType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import UserSettings from '@app/features/user/state/UserSettings';
import CompactVoiceCallHeight, {getGuildVoiceCallExpansionKey} from '@app/features/voice/state/CompactVoiceCallHeight';
import {getMutedText} from '@app/lib/overlay/OverlayContextMenu';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useMemo} from 'react';

const CHANNEL_LINK_COPIED_DESCRIPTOR = msg({
	message: 'Channel link copied',
	comment: 'Toast confirming the channel link was copied to the clipboard.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR = msg({
	message: "Delete {channelLabel}? Can't be undone.",
	comment: 'Confirm dialog body before deleting a channel.',
});
const CHANNEL_ID_COPIED_DESCRIPTOR = msg({
	message: 'Channel ID copied',
	comment: 'Toast confirming the channel ID was copied to the clipboard.',
});
const PINNED_GROUP_DM_DESCRIPTOR = msg({
	message: 'Pinned group DM',
	comment: 'Toast confirming a group DM was pinned in the sidebar.',
});
const UNPINNED_GROUP_DM_DESCRIPTOR = msg({
	message: 'Unpinned group DM',
	comment: 'Toast confirming a group DM was unpinned in the sidebar.',
});
const PIN_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't pin this conversation",
	comment: 'Title of the error modal shown when pinning a DM or group DM fails.',
});
const UNPIN_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't unpin this conversation",
	comment: 'Title of the error modal shown when unpinning a DM or group DM fails.',
});
const PIN_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong. Please try again in a moment.',
	comment: 'Body of the error modal shown when pinning or unpinning a DM or group DM fails.',
});
const COPY_CHANNEL_LINK_DESCRIPTOR = msg({
	message: 'Copy channel link',
	comment: 'Action that copies a deep link to the selected channel.',
});
const DUPLICATE_CHANNEL_DESCRIPTOR = msg({
	message: 'Duplicate channel',
	comment: 'Action that creates a copy of the selected channel.',
});
const DUPLICATE_CHANNEL_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't duplicate this channel",
	comment: 'Title of the error modal shown when duplicating a channel fails.',
});
const DUPLICATE_CHANNEL_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong. Please try again in a moment.',
	comment: 'Body of the error modal shown when duplicating a channel fails.',
});
const EDIT_CHANNEL_DESCRIPTOR = msg({
	message: 'Edit channel',
	comment: 'Action that opens the edit-channel modal.',
});
const OPEN_CHAT_DESCRIPTOR = msg({
	message: 'Open chat',
	comment: "Context-menu action that opens a voice channel's embedded chat view without connecting to voice.",
});
const logger = new Logger('ChannelMenuData');

export interface ChannelMenuDataOptions {
	onClose: () => void;
	onOpenMuteSheet?: () => void;
	preserveInitialMarkAsReadVisibility?: boolean;
}

export interface ChannelMenuData {
	groups: Array<MenuGroupType>;
	handlers: ChannelMenuHandlers;
	state: ChannelMenuState;
}

export interface ChannelMenuHandlers {
	handleMarkAsRead: () => void;
	handleToggleFavorite: () => void;
	handleInviteMembers: () => void;
	handleCopyChannelLink: () => Promise<void>;
	handleOpenChannelLink: () => void;
	handleOpenChat: () => void;
	handleOpenMuteSheet: () => void;
	handleNotificationSettings: () => void;
	handleDuplicateChannel: (event?: MenuActionEvent) => void;
	handleChannelSettings: () => void;
	handleDeleteChannel: () => void;
	handleCopyChannelId: () => Promise<void>;
	handleEditGroup: () => void;
	handleShowInvites: () => void;
	handlePinChannel: () => Promise<void>;
	handleUnpinChannel: () => Promise<void>;
	handleLeaveGroup: () => void;
	handleDeleteMyMessagesInChannel: () => void;
	handleCloseDM: () => void;
	handleDebugChannel: () => void;
	handleResetMatureContentAgreeState: () => void;
}

export interface ChannelMenuState {
	isGroupDM: boolean;
	isDM: boolean;
	isTextChannel: boolean;
	isVoiceChannel: boolean;
	isLinkChannel: boolean;
	isOwner: boolean;
	isMuted: boolean;
	isFavorited: boolean;
	hasUnread: boolean;
	canManageChannels: boolean;
	canEditChannel: boolean;
	canInvite: boolean;
	developerMode: boolean;
	isPinned: boolean;
	mutedText: string | undefined;
}

function getChannelMenuState(channel: Channel, guild: Guild | undefined): ChannelMenuState {
	const currentUserId = Authentication.currentUserId;
	const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
	const isDM = channel.type === ChannelTypes.DM;
	const isTextChannel = channel.type === ChannelTypes.GUILD_TEXT;
	const isVoiceChannel = channel.type === ChannelTypes.GUILD_VOICE;
	const isLinkChannel = channel.type === ChannelTypes.GUILD_LINK;
	const isOwner = isGroupDM && channel.ownerId === currentUserId;
	const settingsGuildId = guild?.id ?? null;
	const channelOverride = UserGuildSettings.getChannelOverride(settingsGuildId, channel.id);
	const isMuted = channelOverride?.muted ?? false;
	const muteConfig = channelOverride?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);
	const isFavorited = !!Favorites.getChannel(channel.id);
	const readState = ReadStates.get(channel.id);
	const hasUnread = readState.hasUnread();
	const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, {
		channelId: channel.id,
		guildId: channel.guildId,
	});
	const canUpdateRtcRegion =
		isVoiceChannel &&
		Permission.can(Permissions.UPDATE_RTC_REGION, {
			channelId: channel.id,
			guildId: channel.guildId,
		});
	const canEditChannel = canManageChannels || canUpdateRtcRegion;
	const canInvite = InviteUtils.canInviteToChannel(channel.id, channel.guildId);
	const developerMode = UserSettings.developerMode;
	const isPinned = channel.isPinned;
	return {
		isGroupDM,
		isDM,
		isTextChannel,
		isVoiceChannel,
		isLinkChannel,
		isOwner,
		isMuted,
		isFavorited,
		hasUnread,
		canManageChannels,
		canEditChannel,
		canInvite,
		developerMode,
		isPinned,
		mutedText,
	};
}

export function useChannelMenuData(
	channel: Channel,
	guild: Guild | undefined,
	options: ChannelMenuDataOptions,
): ChannelMenuData {
	const {i18n} = useLingui();
	const {onClose, onOpenMuteSheet, preserveInitialMarkAsReadVisibility = false} = options;
	const leaveGroup = useLeaveGroup();
	const deleteMyMessagesInChannel = useDeleteMyMessagesInChannel();
	const state = getChannelMenuState(channel, guild);
	const initialHasUnread = useMemo(() => ReadStates.hasUnread(channel.id), [channel.id]);
	const showMarkAsReadItem = preserveInitialMarkAsReadVisibility ? initialHasUnread : state.hasUnread;
	const handlers = useMemo(
		() => ({
			handleMarkAsRead: () => {
				ReadStateCommands.ack(channel.id, true, true);
				onClose();
			},
			handleToggleFavorite: () => {
				onClose();
				const guildId = channel.guildId ?? ME;
				const isFavorited = !!Favorites.getChannel(channel.id);
				if (isFavorited) {
					Favorites.removeChannel(channel.id);
					ToastCommands.createToast({type: 'success', children: i18n._(REMOVED_FROM_FAVORITES_TOAST_DESCRIPTOR)});
				} else {
					Favorites.addChannel(channel.id, guildId, null);
					ToastCommands.createToast({type: 'success', children: i18n._(ADDED_TO_FAVORITES_TOAST_DESCRIPTOR)});
				}
			},
			handleInviteMembers: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<InviteModal
							channelId={channel.id}
							data-flx="ui.action-menu.items.channel-menu-data.handle-invite-members.invite-modal"
						/>
					)),
				);
			},
			handleCopyChannelLink: async () => {
				const link = buildChannelLink({
					guildId: channel.guildId,
					channelId: channel.id,
				});
				await TextCopyCommands.copy(i18n, link, true);
				ToastCommands.createToast({
					type: 'success',
					children: i18n._(CHANNEL_LINK_COPIED_DESCRIPTOR),
				});
				onClose();
			},
			handleOpenChannelLink: () => {
				LinkChannelCommands.openLinkChannel(channel);
				onClose();
			},
			handleOpenChat: () => {
				if (channel.guildId) {
					CompactVoiceCallHeight.setExpandedForKey(getGuildVoiceCallExpansionKey(channel.id), false);
					NavigationCommands.selectChannel(channel.guildId, channel.id);
				}
				onClose();
			},
			handleOpenMuteSheet: () => {
				onOpenMuteSheet?.();
			},
			handleNotificationSettings: () => {
				if (guild) {
					ModalCommands.pushAfterBottomSheetClose(
						onClose,
						modal(() => (
							<GuildNotificationSettingsModal
								guildId={guild.id}
								data-flx="ui.action-menu.items.channel-menu-data.handle-notification-settings.guild-notification-settings-modal"
							/>
						)),
					);
				}
			},
			handleDuplicateChannel: (event?: MenuActionEvent) => {
				if (!guild) return;
				if (event?.shiftKey) {
					ModalCommands.runAfterBottomSheetClose(onClose, () => {
						void duplicateChannel(guild.id, channel, getDuplicateChannelDefaultValues(channel), {
							closeModal: false,
						}).catch((error) => {
							logger.error('Failed to duplicate channel:', error);
							ModalCommands.push(
								modal(() => (
									<GenericErrorModal
										title={i18n._(DUPLICATE_CHANNEL_FAILED_TITLE_DESCRIPTOR)}
										message={i18n._(DUPLICATE_CHANNEL_FAILED_MESSAGE_DESCRIPTOR)}
										data-flx="ui.action-menu.items.channel-menu-data.handle-duplicate-channel.generic-error-modal"
									/>
								)),
							);
						});
					});
					return;
				}
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<ChannelDuplicateModal
							guildId={guild.id}
							channel={channel}
							data-flx="ui.action-menu.items.channel-menu-data.handle-duplicate-channel.channel-duplicate-modal"
						/>
					)),
				);
			},
			handleChannelSettings: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<ChannelSettingsModal
							channelId={channel.id}
							data-flx="ui.action-menu.items.channel-menu-data.handle-channel-settings.channel-settings-modal"
						/>
					)),
				);
			},
			handleDeleteChannel: () => {
				const channelLabel = `#${channel.name ?? channel.id}`;
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<ConfirmModal
							title={i18n._(DELETE_CHANNEL_DESCRIPTOR)}
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
							data-flx="ui.action-menu.items.channel-menu-data.handle-delete-channel.confirm-modal"
						/>
					)),
				);
			},
			handleCopyChannelId: async () => {
				await TextCopyCommands.copy(i18n, channel.id, true);
				ToastCommands.createToast({
					type: 'success',
					children: i18n._(CHANNEL_ID_COPIED_DESCRIPTOR),
				});
				onClose();
			},
			handleEditGroup: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<EditGroupModal
							channelId={channel.id}
							data-flx="ui.action-menu.items.channel-menu-data.handle-edit-group.edit-group-modal"
						/>
					)),
				);
			},
			handleShowInvites: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<GroupInvitesModal
							channelId={channel.id}
							data-flx="ui.action-menu.items.channel-menu-data.handle-show-invites.group-invites-modal"
						/>
					)),
				);
			},
			handlePinChannel: async () => {
				onClose();
				const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
				try {
					await PrivateChannelCommands.pinDmChannel(channel.id);
					ToastCommands.createToast({
						type: 'success',
						children: isGroupDM ? i18n._(PINNED_GROUP_DM_DESCRIPTOR) : i18n._(PINNED_DM_DESCRIPTOR),
					});
				} catch (error) {
					logger.error('Failed to pin:', error);
					ModalCommands.push(
						modal(() => (
							<GenericErrorModal
								title={i18n._(PIN_FAILED_TITLE_DESCRIPTOR)}
								message={i18n._(PIN_FAILED_MESSAGE_DESCRIPTOR)}
								data-flx="ui.action-menu.items.channel-menu-data.pin-channel.generic-error-modal"
							/>
						)),
					);
				}
			},
			handleUnpinChannel: async () => {
				onClose();
				const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
				try {
					await PrivateChannelCommands.unpinDmChannel(channel.id);
					ToastCommands.createToast({
						type: 'success',
						children: isGroupDM ? i18n._(UNPINNED_GROUP_DM_DESCRIPTOR) : i18n._(UNPINNED_DM_DESCRIPTOR),
					});
				} catch (error) {
					logger.error('Failed to unpin:', error);
					ModalCommands.push(
						modal(() => (
							<GenericErrorModal
								title={i18n._(UNPIN_FAILED_TITLE_DESCRIPTOR)}
								message={i18n._(PIN_FAILED_MESSAGE_DESCRIPTOR)}
								data-flx="ui.action-menu.items.channel-menu-data.unpin-channel.generic-error-modal"
							/>
						)),
					);
				}
			},
			handleLeaveGroup: () => {
				ModalCommands.runAfterBottomSheetClose(onClose, () => leaveGroup?.(channel.id));
			},
			handleDeleteMyMessagesInChannel: () => {
				ModalCommands.runAfterBottomSheetClose(onClose, () => deleteMyMessagesInChannel(channel.id));
			},
			handleCloseDM: () => {
				onClose();
				ChannelCommands.remove(channel.id);
			},
			handleDebugChannel: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<ChannelDebugModal
							title={i18n._(CHANNEL_DEBUG_DESCRIPTOR)}
							channel={channel}
							data-flx="ui.action-menu.items.channel-menu-data.handle-debug-channel.channel-debug-modal"
						/>
					)),
				);
			},
			handleResetMatureContentAgreeState: () => {
				GuildMatureContentAgree.revokeChannel(channel.id);
				onClose();
			},
		}),
		[channel, deleteMyMessagesInChannel, guild, i18n.locale, leaveGroup, onClose, onOpenMuteSheet],
	);
	const groups = ((): Array<MenuGroupType> => {
		const menuGroups: Array<MenuGroupType> = [];
		if (state.isGroupDM) {
			const primaryItems: Array<MenuItemType> = [
				{
					icon: <EditGroupIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.edit-group-icon" />,
					label: i18n._(EDIT_GROUP_DESCRIPTOR),
					onClick: handlers.handleEditGroup,
				},
				state.isPinned
					? {
							icon: <PinIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.pin-icon" />,
							label: i18n._(UNPIN_GROUP_DM_DESCRIPTOR),
							onClick: handlers.handleUnpinChannel,
						}
					: {
							icon: <PinIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.pin-icon--2" />,
							label: i18n._(PIN_GROUP_DM_DESCRIPTOR),
							onClick: handlers.handlePinChannel,
						},
			];
			if (state.isOwner) {
				primaryItems.push({
					icon: (
						<SendInvitesIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.send-invites-icon" />
					),
					label: i18n._(INVITES_DESCRIPTOR),
					onClick: handlers.handleShowInvites,
				});
			}
			menuGroups.push({items: primaryItems});
			const secondaryItems: Array<MenuItemType> = [
				{
					icon: <DeleteIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.delete-icon" />,
					label: i18n._(DELETE_MY_MESSAGES_DESCRIPTOR),
					onClick: handlers.handleDeleteMyMessagesInChannel,
					danger: true,
				},
				{
					icon: <LeaveIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.leave-icon" />,
					label: i18n._(LEAVE_GROUP_DESCRIPTOR),
					onClick: handlers.handleLeaveGroup,
					danger: true,
				},
			];
			if (state.developerMode) {
				secondaryItems.push({
					icon: (
						<DebugChannelIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.debug-channel-icon" />
					),
					label: i18n._(DEBUG_CHANNEL_DESCRIPTOR),
					onClick: handlers.handleDebugChannel,
				});
			}
			secondaryItems.push({
				icon: <CopyIdIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.copy-id-icon" />,
				label: i18n._(COPY_CHANNEL_ID_DESCRIPTOR),
				onClick: handlers.handleCopyChannelId,
			});
			menuGroups.push({items: secondaryItems});
			return menuGroups;
		}
		if (state.isDM) {
			const items: Array<MenuItemType> = [
				state.isPinned
					? {
							icon: <PinIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.pin-icon--3" />,
							label: i18n._(UNPIN_DM_DESCRIPTOR),
							onClick: handlers.handleUnpinChannel,
						}
					: {
							icon: <PinIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.pin-icon--4" />,
							label: i18n._(PIN_DM_DESCRIPTOR),
							onClick: handlers.handlePinChannel,
						},
				{
					icon: <DeleteIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.delete-icon--2" />,
					label: i18n._(DELETE_MY_MESSAGES_DESCRIPTOR),
					onClick: handlers.handleDeleteMyMessagesInChannel,
					danger: true,
				},
				{
					icon: <CloseDMIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.close-dm-icon" />,
					label: i18n._(CLOSE_DM_DESCRIPTOR),
					onClick: handlers.handleCloseDM,
					danger: true,
				},
			];
			if (state.developerMode) {
				items.push({
					icon: (
						<DebugChannelIcon
							size={20}
							data-flx="ui.action-menu.items.channel-menu-data.groups.debug-channel-icon--2"
						/>
					),
					label: i18n._(DEBUG_CHANNEL_DESCRIPTOR),
					onClick: handlers.handleDebugChannel,
				});
			}
			items.push({
				icon: <CopyIdIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.copy-id-icon--2" />,
				label: i18n._(COPY_CHANNEL_ID_DESCRIPTOR),
				onClick: handlers.handleCopyChannelId,
			});
			menuGroups.push({items});
			return menuGroups;
		}
		if (guild && (state.isTextChannel || state.isVoiceChannel || state.isLinkChannel)) {
			if (state.isVoiceChannel && !Accessibility.voiceChannelJoinRequiresDoubleClick) {
				menuGroups.push({
					items: [
						{
							icon: (
								<MessageUserIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.open-chat-icon" />
							),
							label: i18n._(OPEN_CHAT_DESCRIPTOR),
							onClick: handlers.handleOpenChat,
						},
					],
				});
			}
			const metaItems: Array<MenuItemType> = [];
			if (showMarkAsReadItem) {
				metaItems.push({
					icon: <MarkAsReadIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.mark-as-read-icon" />,
					label: i18n._(MARK_AS_READ_DESCRIPTOR),
					onClick: handlers.handleMarkAsRead,
				});
			}
			if (Accessibility.showFavorites) {
				metaItems.push({
					icon: (
						<FavoriteIcon
							filled={state.isFavorited}
							size={20}
							data-flx="ui.action-menu.items.channel-menu-data.groups.favorite-icon"
						/>
					),
					label: state.isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR),
					onClick: handlers.handleToggleFavorite,
				});
			}
			if (metaItems.length > 0) {
				menuGroups.push({items: metaItems});
			}
			const inviteItems: Array<MenuItemType> = [];
			if (state.canInvite) {
				inviteItems.push({
					icon: <InviteIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.invite-icon" />,
					label: i18n._(INVITE_PEOPLE_DESCRIPTOR),
					onClick: handlers.handleInviteMembers,
				});
			}
			if (state.isLinkChannel && channel.url) {
				inviteItems.push({
					icon: <OpenLinkIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.open-link-icon" />,
					label: i18n._(OPEN_LINK_DESCRIPTOR),
					onClick: handlers.handleOpenChannelLink,
				});
			}
			inviteItems.push({
				icon: <CopyLinkIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.copy-link-icon" />,
				label: i18n._(COPY_CHANNEL_LINK_DESCRIPTOR),
				onClick: handlers.handleCopyChannelLink,
			});
			menuGroups.push({items: inviteItems});
			const notificationItems: Array<MenuItemType> = [];
			if (!state.isLinkChannel) {
				notificationItems.push({
					icon: state.isMuted ? (
						<MuteIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.mute-icon" />
					) : (
						<NotificationSettingsIcon
							size={20}
							data-flx="ui.action-menu.items.channel-menu-data.groups.notification-settings-icon"
						/>
					),
					label: i18n._(state.isMuted ? UNMUTE_CHANNEL_DESCRIPTOR : MUTE_CHANNEL_DESCRIPTOR),
					onClick: handlers.handleOpenMuteSheet,
				});
			}
			notificationItems.push({
				icon: (
					<NotificationSettingsIcon
						size={20}
						data-flx="ui.action-menu.items.channel-menu-data.groups.notification-settings-icon--2"
					/>
				),
				label: i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR),
				onClick: handlers.handleNotificationSettings,
			});
			menuGroups.push({items: notificationItems});
			if (state.canEditChannel) {
				const manageItems: Array<MenuItemType> = [
					{
						icon: <SettingsIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.settings-icon" />,
						label: i18n._(EDIT_CHANNEL_DESCRIPTOR),
						onClick: handlers.handleChannelSettings,
					},
				];
				if (state.canManageChannels) {
					manageItems.push({
						icon: <CopyIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.copy-icon" />,
						label: i18n._(DUPLICATE_CHANNEL_DESCRIPTOR),
						onClick: handlers.handleDuplicateChannel,
					});
				}
				menuGroups.push({items: manageItems});
			}
			const debugItems: Array<MenuItemType> = [];
			if (state.developerMode) {
				debugItems.push({
					icon: (
						<DebugChannelIcon
							size={20}
							data-flx="ui.action-menu.items.channel-menu-data.groups.debug-channel-icon--3"
						/>
					),
					label: i18n._(DEBUG_CHANNEL_DESCRIPTOR),
					onClick: handlers.handleDebugChannel,
				});
				if (GuildMatureContentAgree.hasAgreedToChannel(channel.id)) {
					debugItems.push({
						icon: (
							<DebugChannelIcon
								size={20}
								data-flx="ui.action-menu.items.channel-menu-data.groups.debug-channel-icon--4"
							/>
						),
						label: i18n._(RESET_MATURE_CONTENT_AGREE_STATE_DESCRIPTOR),
						onClick: handlers.handleResetMatureContentAgreeState,
					});
				}
			}
			debugItems.push({
				icon: <CopyIdIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.copy-id-icon--3" />,
				label: i18n._(COPY_CHANNEL_ID_DESCRIPTOR),
				onClick: handlers.handleCopyChannelId,
			});
			menuGroups.push({items: debugItems});
			const destructiveItems: Array<MenuItemType> = [];
			if (state.canManageChannels) {
				destructiveItems.push({
					icon: <DeleteIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.delete-icon--4" />,
					label: i18n._(DELETE_CHANNEL_DESCRIPTOR),
					onClick: handlers.handleDeleteChannel,
					danger: true,
				});
			}
			if (state.isTextChannel || state.isVoiceChannel) {
				destructiveItems.push({
					icon: <DeleteIcon size={20} data-flx="ui.action-menu.items.channel-menu-data.groups.delete-icon--3" />,
					label: i18n._(DELETE_MY_MESSAGES_DESCRIPTOR),
					onClick: handlers.handleDeleteMyMessagesInChannel,
					danger: true,
				});
			}
			if (destructiveItems.length > 0) {
				menuGroups.push({items: destructiveItems});
			}
			return menuGroups;
		}
		return [];
	})();
	return {
		groups,
		handlers,
		state,
	};
}
