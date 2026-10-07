// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import type {InviteCandidate} from '@app/features/channel/components/direct_message/DMListHelpers';
import type {DMListItemHandlers} from '@app/features/channel/components/direct_message/useDMListItemHandlers';
import {getMuteDurationOptions} from '@app/features/channel/components/MuteOptions';
import type {Channel} from '@app/features/channel/models/Channel';
import {
	CLOSE_DM_DESCRIPTOR,
	DELETE_MY_MESSAGES_DESCRIPTOR,
	INVITE_TO_COMMUNITY_DESCRIPTOR,
	LEAVE_GROUP_DESCRIPTOR,
	PIN_DM_DESCRIPTOR,
	UNPIN_DM_DESCRIPTOR,
} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {ChannelDebugModal} from '@app/features/devtools/components/debug/ChannelDebugModal';
import {UserDebugModal} from '@app/features/devtools/components/debug/UserDebugModal';
import {
	ADD_NOTE_DESCRIPTOR,
	CHANGE_FRIEND_NICKNAME_DESCRIPTOR,
	CHANNEL_DEBUG_DESCRIPTOR,
	COPY_CHANNEL_ID_DESCRIPTOR,
	COPY_USER_ID_DESCRIPTOR,
	DEBUG_CHANNEL_DESCRIPTOR,
	DEBUG_USER_DESCRIPTOR,
	MARK_AS_READ_DESCRIPTOR,
	START_VOICE_CALL_DESCRIPTOR,
	UNCATEGORIZED_DESCRIPTOR,
	USER_DEBUG_DESCRIPTOR,
	VIEW_PROFILE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Favorites from '@app/features/messaging/state/Favorites';
import {BLOCK_DESCRIPTOR} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import {
	ACCEPT_FRIEND_REQUEST_DESCRIPTOR,
	ADD_FRIEND_DESCRIPTOR,
	IGNORE_FRIEND_REQUEST_DESCRIPTOR,
	OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR,
	REMOVE_FRIEND_DESCRIPTOR,
	UNBLOCK_USER_ACTION_DESCRIPTOR,
} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {
	AcceptFriendRequestIcon,
	AddNoteIcon,
	BlockUserIcon,
	CancelFriendRequestIcon,
	CloseDMIcon,
	CopyIdIcon,
	DebugIcon,
	DeleteIcon,
	EditIcon,
	FavoriteIcon,
	IgnoreFriendRequestIcon,
	InviteToCommunityIcon,
	LeaveIcon,
	MarkAsReadIcon,
	MuteIcon,
	PinIcon,
	RemoveFriendIcon,
	SendFriendRequestIcon,
	SendInviteToCommunityIcon,
	ViewProfileIcon,
	VoiceCallIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {MenuGroupType, MenuItemType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import type {User} from '@app/features/user/models/User';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import {hasActiveDirectCallWithUser} from '@app/features/voice/utils/PrivateCallMenuUtils';
import {getMutedText} from '@app/lib/overlay/OverlayContextMenu';
import {PublicUserFlags, RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const FAVORITE_DM_DESCRIPTOR = msg({
	message: 'Favorite DM',
	comment: 'DM list context menu item that adds a one-on-one DM to favorites.',
});
const FAVORITE_GROUP_DM_DESCRIPTOR = msg({
	message: 'Favorite group DM',
	comment: 'DM list context menu item that adds a group DM to favorites.',
});
const FAVORITE_CHANNEL_DESCRIPTOR = msg({
	message: 'Favorite channel',
	comment: 'DM list context menu item that adds a community channel to favorites.',
});
const UNFAVORITE_DM_DESCRIPTOR = msg({
	message: 'Unfavorite DM',
	comment: 'DM list context menu item that removes a one-on-one DM from favorites.',
});
const UNFAVORITE_GROUP_DM_DESCRIPTOR = msg({
	message: 'Unfavorite group DM',
	comment: 'DM list context menu item that removes a group DM from favorites.',
});
const UNFAVORITE_CHANNEL_DESCRIPTOR = msg({
	message: 'Unfavorite channel',
	comment: 'DM list context menu item that removes a community channel from favorites.',
});
const UNMUTE_DM_DESCRIPTOR = msg({
	message: 'Unmute DM',
	comment: 'DM list context menu item that removes an existing mute on the DM channel.',
});
const MUTE_DM_DESCRIPTOR = msg({
	message: 'Mute DM',
	comment: 'DM list context menu item that opens a submenu of mute duration options for the DM channel.',
});

interface BuildMenuGroupsParams {
	channel: Channel;
	recipient: User | null;
	relationshipType: number | undefined;
	restrictRecipientActions: boolean;
	inviteCandidates: Array<InviteCandidate>;
	hasUnreadMessages: boolean;
	handlers: DMListItemHandlers;
	openNestedSheet: (title: string, groups: Array<MenuGroupType>) => void;
	closeAllSheets: () => void;
	i18n: I18n;
}

function buildFavoriteGroups(i18n: I18n, handlers: DMListItemHandlers): Array<MenuGroupType> {
	const favoritesCategories = Favorites.sortedCategories;
	return [
		{
			items: [
				{
					icon: (
						<FavoriteIcon
							size={20}
							filled={true}
							data-flx="channel.direct-message.dm-list-item-menu-groups.build-favorite-groups.favorite-icon"
						/>
					),
					label: i18n._(UNCATEGORIZED_DESCRIPTOR),
					onClick: () => handlers.handleAddToFavorites(null),
				},
				...favoritesCategories.map((category) => ({
					icon: (
						<FavoriteIcon
							size={20}
							filled={true}
							data-flx="channel.direct-message.dm-list-item-menu-groups.build-favorite-groups.favorite-icon--2"
						/>
					),
					label: category.name,
					onClick: () => handlers.handleAddToFavorites(category.id),
				})),
			],
		},
	];
}

function buildInviteGroups(
	inviteCandidates: Array<InviteCandidate>,
	handlers: DMListItemHandlers,
): Array<MenuGroupType> {
	return [
		{
			items: inviteCandidates.map((candidate) => ({
				icon: (
					<SendInviteToCommunityIcon
						size={20}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-invite-groups.send-invite-to-community-icon"
					/>
				),
				label: candidate.guild.name,
				onClick: () => handlers.handleSendInvite(candidate),
			})),
		},
	];
}

function buildMuteGroups(i18n: I18n, handlers: DMListItemHandlers): Array<MenuGroupType> {
	const muteDurationOptions = getMuteDurationOptions(i18n);
	return [
		{
			items: muteDurationOptions.map((duration) => ({
				label: duration.label,
				onClick: () => handlers.handleMute(duration.value),
			})),
		},
	];
}

export function buildMobileMenuGroups({
	channel,
	recipient,
	relationshipType,
	restrictRecipientActions,
	inviteCandidates,
	hasUnreadMessages,
	handlers,
	openNestedSheet,
	closeAllSheets,
	i18n,
}: BuildMenuGroupsParams): Array<MenuGroupType> {
	const mobileMenuGroups: Array<MenuGroupType> = [];
	mobileMenuGroups.push({
		items: [
			{
				icon: (
					<MarkAsReadIcon
						size={20}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.mark-as-read-icon"
					/>
				),
				label: i18n._(MARK_AS_READ_DESCRIPTOR),
				onClick: handlers.handleMarkAsRead,
				disabled: !hasUnreadMessages,
			},
			{
				icon: (
					<PinIcon
						size={20}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.pin-icon"
					/>
				),
				label: channel.isPinned ? i18n._(UNPIN_DM_DESCRIPTOR) : i18n._(PIN_DM_DESCRIPTOR),
				onClick: channel.isPinned ? handlers.handleUnpinChannel : handlers.handlePinChannel,
			},
		],
	});
	if (Accessibility.showFavorites) {
		const favoriteEntry = Favorites.getChannel(channel.id);
		const favoritesCategories = Favorites.sortedCategories;
		const favoriteLabel = (() => {
			if (channel.isDM()) {
				return i18n._(FAVORITE_DM_DESCRIPTOR);
			}
			if (channel.isGroupDM()) {
				return i18n._(FAVORITE_GROUP_DM_DESCRIPTOR);
			}
			return i18n._(FAVORITE_CHANNEL_DESCRIPTOR);
		})();
		const unfavoriteLabel = (() => {
			if (channel.isDM()) {
				return i18n._(UNFAVORITE_DM_DESCRIPTOR);
			}
			if (channel.isGroupDM()) {
				return i18n._(UNFAVORITE_GROUP_DM_DESCRIPTOR);
			}
			return i18n._(UNFAVORITE_CHANNEL_DESCRIPTOR);
		})();
		const favoriteItems: Array<MenuItemType> = [];
		if (favoriteEntry) {
			favoriteItems.push({
				icon: (
					<FavoriteIcon
						size={20}
						filled={true}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.favorite-icon"
					/>
				),
				label: unfavoriteLabel,
				onClick: handlers.handleRemoveFromFavorites,
			});
		} else if (favoritesCategories.length === 0) {
			favoriteItems.push({
				icon: (
					<FavoriteIcon
						size={20}
						filled={true}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.favorite-icon--2"
					/>
				),
				label: favoriteLabel,
				onClick: () => handlers.handleAddToFavorites(null),
			});
		} else {
			favoriteItems.push({
				icon: (
					<FavoriteIcon
						size={20}
						filled={true}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.favorite-icon--3"
					/>
				),
				label: favoriteLabel,
				onClick: () => openNestedSheet(favoriteLabel, buildFavoriteGroups(i18n, handlers)),
			});
		}
		if (favoriteItems.length > 0) {
			mobileMenuGroups.push({items: favoriteItems});
		}
	}
	if (recipient && !recipient.bot) {
		const profileItems: Array<MenuItemType> = [
			{
				icon: (
					<ViewProfileIcon
						size={20}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.view-profile-icon"
					/>
				),
				label: i18n._(VIEW_PROFILE_DESCRIPTOR),
				onClick: handlers.handleViewProfile,
			},
		];
		if (!hasActiveDirectCallWithUser(recipient.id)) {
			profileItems.push({
				icon: (
					<VoiceCallIcon
						size={20}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.voice-call-icon"
					/>
				),
				label: i18n._(START_VOICE_CALL_DESCRIPTOR),
				onClick: handlers.handleStartVoiceCall,
			});
		}
		if (!restrictRecipientActions) {
			if (!StreamerMode.shouldHidePersonalInformation) {
				profileItems.push({
					icon: (
						<AddNoteIcon
							size={20}
							data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.add-note-icon"
						/>
					),
					label: i18n._(ADD_NOTE_DESCRIPTOR),
					onClick: handlers.handleAddNote,
				});
			}
			profileItems.push(
				relationshipType === RelationshipTypes.FRIEND
					? {
							icon: (
								<EditIcon
									size={20}
									data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.edit-icon"
								/>
							),
							label: i18n._(CHANGE_FRIEND_NICKNAME_DESCRIPTOR),
							onClick: handlers.handleChangeFriendNickname,
						}
					: {
							icon: (
								<EditIcon
									size={20}
									data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.edit-icon--2"
								/>
							),
							label: i18n._(CHANGE_FRIEND_NICKNAME_DESCRIPTOR),
							onClick: handlers.handleChangeFriendNickname,
							disabled: true,
						},
			);
		}
		mobileMenuGroups.push({items: profileItems});
	}
	mobileMenuGroups.push({
		items: [
			{
				icon: (
					<DeleteIcon
						size={20}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.delete-icon"
					/>
				),
				label: i18n._(DELETE_MY_MESSAGES_DESCRIPTOR),
				onClick: handlers.handleDeleteMyMessagesInChannel,
				danger: true,
			},
			channel.isGroupDM()
				? {
						icon: (
							<LeaveIcon
								size={20}
								data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.leave-icon"
							/>
						),
						label: i18n._(LEAVE_GROUP_DESCRIPTOR),
						onClick: handlers.handleLeaveGroup,
						danger: true,
					}
				: {
						icon: (
							<CloseDMIcon
								size={20}
								data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.close-dm-icon"
							/>
						),
						label: i18n._(CLOSE_DM_DESCRIPTOR),
						onClick: handlers.handleCloseDm,
						danger: true,
					},
		],
	});
	const relationshipItems: Array<MenuItemType> = [];
	if (recipient && !restrictRecipientActions) {
		if (inviteCandidates.length > 0) {
			relationshipItems.push({
				icon: (
					<InviteToCommunityIcon
						size={20}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.invite-to-community-icon"
					/>
				),
				label: i18n._(INVITE_TO_COMMUNITY_DESCRIPTOR),
				onClick: () =>
					openNestedSheet(i18n._(INVITE_TO_COMMUNITY_DESCRIPTOR), buildInviteGroups(inviteCandidates, handlers)),
			});
		}
		if (recipient.bot && !(recipient.flags & PublicUserFlags.FRIENDLY_BOT)) {
			if (relationshipType === RelationshipTypes.FRIEND) {
				relationshipItems.push({
					icon: (
						<RemoveFriendIcon
							size={20}
							data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.remove-friend-icon"
						/>
					),
					label: i18n._(REMOVE_FRIEND_DESCRIPTOR),
					onClick: handlers.handleRemoveFriend,
				});
			} else if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
				relationshipItems.push(
					{
						icon: (
							<AcceptFriendRequestIcon
								size={20}
								data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.accept-friend-request-icon"
							/>
						),
						label: i18n._(ACCEPT_FRIEND_REQUEST_DESCRIPTOR),
						onClick: handlers.handleAcceptFriendRequest,
					},
					{
						icon: (
							<IgnoreFriendRequestIcon
								size={20}
								data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.ignore-friend-request-icon"
							/>
						),
						label: i18n._(IGNORE_FRIEND_REQUEST_DESCRIPTOR),
						onClick: handlers.handleIgnoreFriendRequest,
					},
				);
			} else if (relationshipType === RelationshipTypes.OUTGOING_REQUEST) {
				relationshipItems.push({
					icon: (
						<CancelFriendRequestIcon
							size={20}
							data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.cancel-friend-request-icon"
						/>
					),
					label: i18n._(OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR),
					onClick: () => undefined,
					disabled: true,
				});
			} else if (Users.currentUser?.verified !== false) {
				relationshipItems.push({
					icon: (
						<SendFriendRequestIcon
							size={20}
							data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.send-friend-request-icon"
						/>
					),
					label: i18n._(ADD_FRIEND_DESCRIPTOR),
					onClick: handlers.handleSendFriendRequest,
				});
			}
		} else {
			switch (relationshipType) {
				case RelationshipTypes.FRIEND:
					relationshipItems.push({
						icon: (
							<RemoveFriendIcon
								size={20}
								data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.remove-friend-icon--2"
							/>
						),
						label: i18n._(REMOVE_FRIEND_DESCRIPTOR),
						onClick: handlers.handleRemoveFriend,
					});
					break;
				case RelationshipTypes.INCOMING_REQUEST:
					relationshipItems.push(
						{
							icon: (
								<AcceptFriendRequestIcon
									size={20}
									data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.accept-friend-request-icon--2"
								/>
							),
							label: i18n._(ACCEPT_FRIEND_REQUEST_DESCRIPTOR),
							onClick: handlers.handleAcceptFriendRequest,
						},
						{
							icon: (
								<IgnoreFriendRequestIcon
									size={20}
									data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.ignore-friend-request-icon--2"
								/>
							),
							label: i18n._(IGNORE_FRIEND_REQUEST_DESCRIPTOR),
							onClick: handlers.handleIgnoreFriendRequest,
						},
					);
					break;
				case RelationshipTypes.OUTGOING_REQUEST:
					relationshipItems.push({
						icon: (
							<CancelFriendRequestIcon
								size={20}
								data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.cancel-friend-request-icon--2"
							/>
						),
						label: i18n._(OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR),
						onClick: () => undefined,
						disabled: true,
					});
					break;
				default:
					if (Users.currentUser?.verified !== false) {
						relationshipItems.push({
							icon: (
								<SendFriendRequestIcon
									size={20}
									data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.send-friend-request-icon--2"
								/>
							),
							label: i18n._(ADD_FRIEND_DESCRIPTOR),
							onClick: handlers.handleSendFriendRequest,
						});
					}
			}
		}
		if (relationshipType === RelationshipTypes.BLOCKED) {
			relationshipItems.push({
				icon: (
					<BlockUserIcon
						size={20}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.block-user-icon"
					/>
				),
				label: i18n._(UNBLOCK_USER_ACTION_DESCRIPTOR),
				onClick: handlers.handleUnblockUser,
			});
		} else {
			relationshipItems.push({
				icon: (
					<BlockUserIcon
						size={20}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.block-user-icon--2"
					/>
				),
				label: i18n._(BLOCK_DESCRIPTOR),
				onClick: handlers.handleBlockUser,
			});
		}
	}
	if (relationshipItems.length > 0) {
		mobileMenuGroups.push({items: relationshipItems});
	}
	const channelOverride = UserGuildSettings.getChannelOverride(null, channel.id);
	const bottomSheetIsMuted = channelOverride?.muted ?? false;
	const bottomSheetMutedHint = getMutedText(bottomSheetIsMuted, channelOverride?.mute_config);
	if (bottomSheetIsMuted) {
		mobileMenuGroups.push({
			items: [
				{
					icon: (
						<MuteIcon
							size={20}
							data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.mute-icon"
						/>
					),
					label: i18n._(UNMUTE_DM_DESCRIPTOR),
					onClick: handlers.handleUnmute,
					hint: bottomSheetMutedHint ?? undefined,
				},
			],
		});
	} else {
		mobileMenuGroups.push({
			items: [
				{
					icon: (
						<MuteIcon
							size={20}
							data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.mute-icon--2"
						/>
					),
					label: i18n._(MUTE_DM_DESCRIPTOR),
					onClick: () => openNestedSheet(i18n._(MUTE_DM_DESCRIPTOR), buildMuteGroups(i18n, handlers)),
					hint: bottomSheetMutedHint ?? undefined,
				},
			],
		});
	}
	if (UserSettings.developerMode) {
		const developerItems: Array<MenuItemType> = [];
		if (recipient) {
			developerItems.push({
				icon: (
					<DebugIcon
						size={20}
						data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.debug-icon"
					/>
				),
				label: i18n._(DEBUG_USER_DESCRIPTOR),
				onClick: () => {
					ModalCommands.pushAfterBottomSheetClose(
						closeAllSheets,
						modal(() => (
							<UserDebugModal
								title={i18n._(USER_DEBUG_DESCRIPTOR)}
								user={recipient}
								data-flx="channel.direct-message.dm-list-item-menu-groups.on-click.user-debug-modal"
							/>
						)),
					);
				},
			});
		}
		developerItems.push({
			icon: (
				<DebugIcon
					size={20}
					data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.debug-icon--2"
				/>
			),
			label: i18n._(DEBUG_CHANNEL_DESCRIPTOR),
			onClick: () => {
				ModalCommands.pushAfterBottomSheetClose(
					closeAllSheets,
					modal(() => (
						<ChannelDebugModal
							title={i18n._(CHANNEL_DEBUG_DESCRIPTOR)}
							channel={channel}
							data-flx="channel.direct-message.dm-list-item-menu-groups.on-click.channel-debug-modal"
						/>
					)),
				);
			},
		});
		mobileMenuGroups.push({items: developerItems});
	}
	const copyItems: Array<MenuItemType> = [];
	if (recipient) {
		copyItems.push({
			icon: (
				<CopyIdIcon
					size={20}
					data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.copy-id-icon"
				/>
			),
			label: i18n._(COPY_USER_ID_DESCRIPTOR),
			onClick: handlers.handleCopyUserId,
		});
	}
	copyItems.push({
		icon: (
			<CopyIdIcon
				size={20}
				data-flx="channel.direct-message.dm-list-item-menu-groups.build-mobile-menu-groups.copy-id-icon--2"
			/>
		),
		label: i18n._(COPY_CHANNEL_ID_DESCRIPTOR),
		onClick: handlers.handleCopyChannelId,
	});
	mobileMenuGroups.push({items: copyItems});
	return mobileMenuGroups;
}
