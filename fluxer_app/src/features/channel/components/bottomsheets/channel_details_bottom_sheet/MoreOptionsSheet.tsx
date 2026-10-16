// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ADD_FRIENDS_TO_GROUP_DESCRIPTOR,
	DEBUG_USER_DESCRIPTOR,
	EDIT_CHANNEL_DESCRIPTOR,
} from '@app/features/channel/components/bottomsheets/channel_details_bottom_sheet/ChannelDetailsBottomSheetShared';
import type {Channel} from '@app/features/channel/models/Channel';
import {
	CLOSE_DM_DESCRIPTOR,
	DELETE_CHANNEL_DESCRIPTOR,
	DELETE_MY_MESSAGES_DESCRIPTOR,
	LEAVE_GROUP_DESCRIPTOR,
	PIN_DM_DESCRIPTOR,
	PIN_GROUP_DM_DESCRIPTOR,
	UNPIN_DM_DESCRIPTOR,
	UNPIN_GROUP_DM_DESCRIPTOR,
} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {isGroupDmFull} from '@app/features/channel/utils/GroupDmUtils';
import {
	ADD_TO_FAVORITES_DESCRIPTOR,
	COPY_CHANNEL_ID_DESCRIPTOR,
	COPY_LINK_DESCRIPTOR,
	COPY_USER_ID_DESCRIPTOR,
	DEBUG_CHANNEL_DESCRIPTOR,
	EDIT_GROUP_DESCRIPTOR,
	INVITE_PEOPLE_DESCRIPTOR,
	INVITES_DESCRIPTOR,
	MARK_AS_READ_DESCRIPTOR,
	NOTIFICATION_SETTINGS_DESCRIPTOR,
	REMOVE_FROM_FAVORITES_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import Permission from '@app/features/permissions/state/Permission';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {AddFriendsToGroupModal} from '@app/features/relationship/components/modals/AddFriendsToGroupModal';
import {
	CloseDMIcon,
	CopyIdIcon,
	CopyLinkIcon,
	DebugMessageIcon,
	DeleteIcon,
	EditIcon,
	FavoriteIcon,
	InviteIcon,
	InvitesIcon,
	LeaveIcon,
	MarkAsReadIcon,
	NotificationSettingsIcon,
	PinIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {MenuGroupType, MenuItemType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import type {User} from '@app/features/user/models/User';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useMemo} from 'react';

interface MoreOptionsSheetProps {
	isOpen: boolean;
	onClose: () => void;
	title: string;
	channel: Channel;
	recipient: User | null | undefined;
	showFavorites: boolean;
	isFavorited: boolean;
	isPersonalNotes: boolean;
	isDM: boolean;
	isGroupDM: boolean;
	isGuildChannel: boolean;
	isGroupDMOwner: boolean;
	developerMode: boolean;
	onToggleFavorite: () => void;
	onMarkAsRead: () => void;
	onPinDM: () => void;
	onUnpinDM: () => void;
	onInvite: () => void;
	onCopyLink: () => void;
	onOpenNotificationSheet: () => void;
	onEditGroup: () => void;
	onShowInvites: () => void;
	onEditChannel: () => void;
	onDeleteChannel: () => void;
	onDeleteMyMessages: () => void;
	onCloseDM: () => void;
	onLeaveGroup: () => void;
	onDebugChannel: () => void;
	onDebugUser: () => void;
	onCopyUserId: () => void;
	onCopyChannelId: () => void;
}

export const MoreOptionsSheet: React.FC<MoreOptionsSheetProps> = ({
	isOpen,
	onClose,
	title,
	channel,
	recipient,
	showFavorites,
	isFavorited,
	isPersonalNotes,
	isDM,
	isGroupDM,
	isGuildChannel,
	isGroupDMOwner,
	developerMode,
	onToggleFavorite,
	onMarkAsRead,
	onPinDM,
	onUnpinDM,
	onInvite,
	onCopyLink,
	onOpenNotificationSheet,
	onEditGroup,
	onShowInvites,
	onEditChannel,
	onDeleteChannel,
	onDeleteMyMessages,
	onCloseDM,
	onLeaveGroup,
	onDebugChannel,
	onDebugUser,
	onCopyUserId,
	onCopyChannelId,
}) => {
	const {i18n} = useLingui();
	const groups = useMemo(() => {
		const out: Array<MenuGroupType> = [];
		const hasUnread = ReadStates.hasUnread(channel.id);
		const commonItems: Array<MenuItemType> = [];
		if (showFavorites && !isPersonalNotes) {
			commonItems.push({
				id: 'favorite',
				icon: (
					<FavoriteIcon filled={isFavorited} size={20} data-flx="channel.channel-details-bottom-sheet.favorite-icon" />
				),
				label: isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR),
				onClick: () => {
					onToggleFavorite();
					onClose();
				},
			});
		}
		if (hasUnread) {
			commonItems.push({
				id: 'mark-as-read',
				icon: <MarkAsReadIcon size={20} data-flx="channel.channel-details-bottom-sheet.mark-as-read-icon" />,
				label: i18n._(MARK_AS_READ_DESCRIPTOR),
				onClick: onMarkAsRead,
			});
		}
		if (isDM || isGroupDM) {
			commonItems.push(
				channel.isPinned
					? {
							id: 'unpin',
							icon: <PinIcon size={20} data-flx="channel.channel-details-bottom-sheet.pin-icon" />,
							label: isGroupDM ? i18n._(UNPIN_GROUP_DM_DESCRIPTOR) : i18n._(UNPIN_DM_DESCRIPTOR),
							onClick: onUnpinDM,
						}
					: {
							id: 'pin',
							icon: <PinIcon size={20} data-flx="channel.channel-details-bottom-sheet.pin-icon--2" />,
							label: isGroupDM ? i18n._(PIN_GROUP_DM_DESCRIPTOR) : i18n._(PIN_DM_DESCRIPTOR),
							onClick: onPinDM,
						},
			);
		}
		const canInvite = isGuildChannel ? InviteUtils.canInviteToChannel(channel.id, channel.guildId) : false;
		if (canInvite) {
			commonItems.push({
				id: 'invite',
				icon: <InviteIcon size={20} data-flx="channel.channel-details-bottom-sheet.invite-icon" />,
				label: i18n._(INVITE_PEOPLE_DESCRIPTOR),
				onClick: onInvite,
			});
		}
		if (isGuildChannel) {
			commonItems.push(
				{
					id: 'copy-link',
					icon: <CopyLinkIcon size={20} data-flx="channel.channel-details-bottom-sheet.copy-link-icon" />,
					label: i18n._(COPY_LINK_DESCRIPTOR),
					onClick: onCopyLink,
				},
				{
					id: 'notification-settings',
					icon: (
						<NotificationSettingsIcon
							size={20}
							data-flx="channel.channel-details-bottom-sheet.notification-settings-icon"
						/>
					),
					label: i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR),
					onClick: () => {
						onClose();
						onOpenNotificationSheet();
					},
				},
			);
		}
		if (commonItems.length > 0) {
			out.push({items: commonItems});
		}
		if (isGroupDM) {
			const groupItems: Array<MenuItemType> = [
				{
					id: 'edit-group',
					icon: <EditIcon size={20} data-flx="channel.channel-details-bottom-sheet.edit-icon" />,
					label: i18n._(EDIT_GROUP_DESCRIPTOR),
					onClick: onEditGroup,
				},
			];
			if (!isGroupDmFull(channel)) {
				groupItems.push({
					id: 'add-friends',
					icon: <InviteIcon size={20} data-flx="channel.channel-details-bottom-sheet.invite-icon--2" />,
					label: i18n._(ADD_FRIENDS_TO_GROUP_DESCRIPTOR),
					onClick: () => {
						ModalCommands.pushAfterBottomSheetClose(
							onClose,
							modal(() => (
								<AddFriendsToGroupModal
									channelId={channel.id}
									data-flx="channel.channel-details-bottom-sheet.on-click.add-friends-to-group-modal"
								/>
							)),
						);
					},
				});
			}
			if (isGroupDMOwner) {
				groupItems.push({
					id: 'invites',
					icon: <InvitesIcon size={20} data-flx="channel.channel-details-bottom-sheet.invites-icon" />,
					label: i18n._(INVITES_DESCRIPTOR),
					onClick: onShowInvites,
				});
			}
			out.push({items: groupItems});
		}
		if (isGuildChannel) {
			const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, {
				channelId: channel.id,
				guildId: channel.guildId,
			});
			if (canManageChannels) {
				const managerItems: Array<MenuItemType> = [
					{
						id: 'edit-channel',
						icon: <EditIcon size={20} data-flx="channel.channel-details-bottom-sheet.edit-icon--2" />,
						label: i18n._(EDIT_CHANNEL_DESCRIPTOR),
						onClick: onEditChannel,
					},
				];
				managerItems.push({
					id: 'delete-channel',
					icon: <DeleteIcon size={20} data-flx="channel.channel-details-bottom-sheet.delete-icon" />,
					label: i18n._(DELETE_CHANNEL_DESCRIPTOR),
					onClick: onDeleteChannel,
					danger: true,
				});
				out.push({items: managerItems});
			}
		}
		if (isDM) {
			out.push({
				items: [
					{
						id: 'delete-my-messages',
						icon: <DeleteIcon size={20} data-flx="channel.channel-details-bottom-sheet.delete-icon--2" />,
						label: i18n._(DELETE_MY_MESSAGES_DESCRIPTOR),
						onClick: onDeleteMyMessages,
						danger: true,
					},
					{
						id: 'close-dm',
						icon: <CloseDMIcon size={20} data-flx="channel.channel-details-bottom-sheet.close-dm-icon" />,
						label: i18n._(CLOSE_DM_DESCRIPTOR),
						onClick: onCloseDM,
						danger: true,
					},
				],
			});
		}
		if (isGroupDM) {
			out.push({
				items: [
					{
						id: 'delete-my-messages',
						icon: <DeleteIcon size={20} data-flx="channel.channel-details-bottom-sheet.delete-icon--3" />,
						label: i18n._(DELETE_MY_MESSAGES_DESCRIPTOR),
						onClick: onDeleteMyMessages,
						danger: true,
					},
					{
						id: 'leave-group',
						icon: <LeaveIcon size={20} data-flx="channel.channel-details-bottom-sheet.leave-icon" />,
						label: i18n._(LEAVE_GROUP_DESCRIPTOR),
						onClick: onLeaveGroup,
						danger: true,
					},
				],
			});
		}
		const miscItems: Array<MenuItemType> = [];
		if (developerMode) {
			miscItems.push({
				id: 'debug-channel',
				icon: <DebugMessageIcon size={20} data-flx="channel.channel-details-bottom-sheet.debug-message-icon" />,
				label: i18n._(DEBUG_CHANNEL_DESCRIPTOR),
				onClick: onDebugChannel,
			});
			if (isDM && recipient) {
				miscItems.push({
					id: 'debug-user',
					icon: <DebugMessageIcon size={20} data-flx="channel.channel-details-bottom-sheet.debug-message-icon--2" />,
					label: i18n._(DEBUG_USER_DESCRIPTOR),
					onClick: onDebugUser,
				});
			}
		}
		if (isDM && recipient) {
			miscItems.push({
				id: 'copy-user-id',
				icon: <CopyIdIcon size={20} data-flx="channel.channel-details-bottom-sheet.copy-id-icon" />,
				label: i18n._(COPY_USER_ID_DESCRIPTOR),
				onClick: onCopyUserId,
			});
		}
		miscItems.push({
			id: 'copy-channel-id',
			icon: <CopyIdIcon size={20} data-flx="channel.channel-details-bottom-sheet.copy-id-icon--2" />,
			label: i18n._(COPY_CHANNEL_ID_DESCRIPTOR),
			onClick: onCopyChannelId,
		});
		if (miscItems.length > 0) {
			out.push({items: miscItems});
		}
		return out;
	}, [
		channel,
		recipient,
		showFavorites,
		isFavorited,
		isPersonalNotes,
		isDM,
		isGroupDM,
		isGuildChannel,
		isGroupDMOwner,
		developerMode,
		onToggleFavorite,
		onMarkAsRead,
		onPinDM,
		onUnpinDM,
		onInvite,
		onCopyLink,
		onOpenNotificationSheet,
		onEditGroup,
		onShowInvites,
		onEditChannel,
		onDeleteChannel,
		onDeleteMyMessages,
		onCloseDM,
		onLeaveGroup,
		onDebugChannel,
		onDebugUser,
		onCopyUserId,
		onCopyChannelId,
		onClose,
		i18n.locale,
	]);
	return (
		<MenuBottomSheet
			isOpen={isOpen}
			onClose={onClose}
			title={title}
			groups={groups}
			data-flx="channel.channel-details-bottom-sheet.menu-bottom-sheet"
		/>
	);
};
