// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {DMCloseFailedModal} from '@app/features/channel/components/alerts/DMCloseFailedModal';
import {EditGroupModal} from '@app/features/channel/components/modals/EditGroupModal';
import {GroupInvitesModal} from '@app/features/channel/components/modals/GroupInvitesModal';
import {useDeleteMyMessagesInChannel} from '@app/features/channel/hooks/useDeleteMyMessagesInChannel';
import type {Channel} from '@app/features/channel/models/Channel';
import {
	CLOSE_DM_DESCRIPTOR,
	DELETE_MY_MESSAGES_DESCRIPTOR,
	INVITE_TO_COMMUNITY_DESCRIPTOR,
	LEAVE_GROUP_DESCRIPTOR,
	MUTE_CONVERSATION_DESCRIPTOR,
	PIN_DM_DESCRIPTOR,
	PIN_GROUP_DM_DESCRIPTOR,
	UNMUTE_CONVERSATION_DESCRIPTOR,
	UNPIN_DM_DESCRIPTOR,
	UNPIN_GROUP_DM_DESCRIPTOR,
} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {ChannelDebugModal} from '@app/features/devtools/components/debug/ChannelDebugModal';
import {UserDebugModal} from '@app/features/devtools/components/debug/UserDebugModal';
import {useLeaveGroup} from '@app/features/guild/hooks/useLeaveGroup';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {
	ADD_NOTE_DESCRIPTOR,
	ADD_TO_FAVORITES_DESCRIPTOR,
	ADDED_TO_FAVORITES_TOAST_DESCRIPTOR,
	CHANGE_FRIEND_NICKNAME_DESCRIPTOR,
	CHANNEL_DEBUG_DESCRIPTOR,
	COPY_CHANNEL_ID_DESCRIPTOR,
	COPY_USER_ID_DESCRIPTOR,
	DEBUG_CHANNEL_DESCRIPTOR,
	DEBUG_USER_DESCRIPTOR,
	DM_CLOSED_DESCRIPTOR,
	EDIT_GROUP_DESCRIPTOR,
	INVITES_DESCRIPTOR,
	MARK_AS_READ_DESCRIPTOR,
	PINNED_DM_DESCRIPTOR,
	REMOVE_FROM_FAVORITES_DESCRIPTOR,
	REMOVED_FROM_FAVORITES_TOAST_DESCRIPTOR,
	SOMETHING_WENT_WRONG_DESCRIPTOR,
	UNPINNED_DM_DESCRIPTOR,
	USER_DEBUG_DESCRIPTOR,
	VIEW_PROFILE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import Favorites from '@app/features/messaging/state/Favorites';
import {BLOCK_DESCRIPTOR} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {ChangeFriendNicknameModal} from '@app/features/relationship/components/modals/ChangeFriendNicknameModal';
import Relationships from '@app/features/relationship/state/Relationships';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {
	ACCEPT_FRIEND_REQUEST_DESCRIPTOR,
	ADD_FRIEND_DESCRIPTOR,
	REMOVE_FRIEND_DESCRIPTOR,
	UNBLOCK_USER_ACTION_DESCRIPTOR,
} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import {
	AcceptFriendRequestIcon,
	AddNoteIcon,
	BlockUserIcon,
	CloseDMIcon,
	CopyIdIcon,
	DebugIcon,
	DeleteIcon,
	EditGroupIcon,
	EditIcon,
	FavoriteIcon,
	GroupInvitesIcon,
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
import {
	ARE_YOU_SURE_YOU_WANT_TO_CLOSE_YOUR_DESCRIPTOR,
	CHANNEL_ID_COPIED_DESCRIPTOR,
	FAILED_TO_PIN_DM_DESCRIPTOR,
	FAILED_TO_PIN_GROUP_DM_DESCRIPTOR,
	FAILED_TO_UNPIN_DM_DESCRIPTOR,
	FAILED_TO_UNPIN_GROUP_DM_DESCRIPTOR,
	INVITE_SENT_TO_DESCRIPTOR,
	PINNED_GROUP_DM_DESCRIPTOR,
	UNPINNED_GROUP_DM_DESCRIPTOR,
	USER_ID_COPIED_DESCRIPTOR,
} from '@app/features/ui/action_menu/items/dm_menu_data/shared';
import {
	beginInviteToCommunityGuard,
	getInviteToCommunityGuardKey,
	scheduleInviteToCommunityGuardRelease,
} from '@app/features/ui/action_menu/items/InviteToCommunityGuard';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {
	MenuGroupType,
	MenuItemType,
	MenuSubmenuItemType,
} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import type {User} from '@app/features/user/models/User';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import * as CallUtils from '@app/features/voice/utils/CallUtils';
import {hasActiveDirectCallWithUser} from '@app/features/voice/utils/PrivateCallMenuUtils';
import {VOICE_CALL_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {getMutedText} from '@app/lib/overlay/OverlayContextMenu';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {PublicUserFlags, RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {fromTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useMemo} from 'react';

const logger = new Logger('DMMenuData');

function showDmMenuErrorModal(i18n: I18n, message: MessageDescriptor): void {
	showGenericErrorModal({
		title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
		message: () => i18n._(message),
		dataFlx: 'ui.dm-menu-data.error-modal',
	});
}

interface InviteCandidate {
	guild: Guild;
	channelId: string;
}

const getDefaultInviteChannelId = (guildId: string): string | null => {
	return InviteUtils.getDefaultCommunityInviteChannelId(guildId) ?? null;
};

export interface DMMenuHandlers {
	handleMarkAsRead: () => void;
	handleToggleFavorite: () => void;
	handleViewProfile: () => void;
	handleStartVoiceCall: () => Promise<void>;
	handleAddNote: () => void;
	handleChangeFriendNickname: () => void;
	handleInviteToCommunity: (guildId: string, channelId: string, guildName: string) => Promise<void>;
	handleSendFriendRequest: () => void;
	handleAcceptFriendRequest: () => void;
	handleRemoveFriend: () => void;
	handleBlockUser: () => void;
	handleUnblockUser: () => void;
	handleCloseDM: () => void;
	handleLeaveGroup: () => void;
	handleDeleteMyMessagesInChannel: () => void;
	handlePinDM: () => Promise<void>;
	handleUnpinDM: () => Promise<void>;
	handleEditGroup: () => void;
	handleShowInvites: () => void;
	handleCopyChannelId: () => Promise<void>;
	handleCopyUserId: () => Promise<void>;
	handleDebugUser: () => void;
	handleDebugChannel: () => void;
}

export interface DMMenuDataOptions {
	onClose: () => void;
	onOpenMuteSheet?: () => void;
	preserveInitialMarkAsReadVisibility?: boolean;
}

export interface DMMenuData {
	groups: Array<MenuGroupType>;
	handlers: DMMenuHandlers;
	invitableCommunities: Array<InviteCandidate>;
	isGroupDM: boolean;
	isOwner: boolean;
	isMuted: boolean;
	mutedText: string | null;
	isFavorited: boolean;
	relationshipType: number | undefined;
	isRecipientBot: boolean | undefined;
	isRecipientSystem: boolean;
	restrictRecipientActions: boolean;
	developerMode: boolean;
}

export function useDMMenuData(
	channel: Channel,
	recipient: User | null | undefined,
	options: DMMenuDataOptions,
): DMMenuData {
	const {i18n} = useLingui();
	const {onClose, onOpenMuteSheet, preserveInitialMarkAsReadVisibility = false} = options;
	const leaveGroup = useLeaveGroup();
	const deleteMyMessagesInChannel = useDeleteMyMessagesInChannel();
	const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
	const currentUserId = Authentication.currentUserId;
	const isOwner = isGroupDM && channel.ownerId === currentUserId;
	const developerMode = UserSettings.developerMode;
	const channelOverride = UserGuildSettings.getChannelOverride(null, channel.id);
	const isMuted = channelOverride?.muted ?? false;
	const muteConfig = channelOverride?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);
	const initialHasUnread = useMemo(() => ReadStates.hasUnread(channel.id), [channel.id]);
	const showMarkAsReadItem = preserveInitialMarkAsReadVisibility ? initialHasUnread : ReadStates.hasUnread(channel.id);
	const isFavorited = !!Favorites.getChannel(channel.id);
	const isRecipientBot = recipient?.bot;
	const recipientFlags = recipient?.flags ?? 0;
	const isFriendlyBot = Boolean(
		isRecipientBot && (recipientFlags & PublicUserFlags.FRIENDLY_BOT) === PublicUserFlags.FRIENDLY_BOT,
	);
	const relationship = recipient ? Relationships.getRelationship(recipient.id) : null;
	const relationshipType = relationship?.type;
	const currentUserUnclaimed = !(Users.currentUser?.isClaimed() ?? true);
	const isRecipientSystem = recipient?.system ?? false;
	const restrictRecipientActions = isRecipientSystem;
	const hasActiveDirectCall = recipient ? hasActiveDirectCallWithUser(recipient.id) : false;
	const invitableCommunities = useMemo(() => {
		if (!recipient || isRecipientBot) return [];
		return Guilds.getGuilds()
			.filter((guild) => !GuildMembers.getMember(guild.id, recipient.id))
			.map((guild): InviteCandidate | null => {
				const channelId = getDefaultInviteChannelId(guild.id);
				return channelId ? {guild, channelId} : null;
			})
			.filter((candidate): candidate is InviteCandidate => candidate !== null)
			.sort((a, b) => a.guild.name.localeCompare(b.guild.name));
	}, [recipient, isRecipientBot]);
	const handleMarkAsRead = useCallback(() => {
		ReadStateCommands.ack(channel.id, true, true);
		onClose();
	}, [channel.id, onClose]);
	const handleToggleFavorite = useCallback(() => {
		onClose();
		if (isFavorited) {
			Favorites.removeChannel(channel.id);
			ToastCommands.createToast({type: 'success', children: i18n._(REMOVED_FROM_FAVORITES_TOAST_DESCRIPTOR)});
		} else {
			Favorites.addChannel(channel.id, ME, null);
			ToastCommands.createToast({type: 'success', children: i18n._(ADDED_TO_FAVORITES_TOAST_DESCRIPTOR)});
		}
	}, [channel.id, isFavorited, onClose]);
	const handleViewProfile = useCallback(() => {
		if (!recipient) return;
		ModalCommands.runAfterBottomSheetClose(onClose, () => UserProfileCommands.openUserProfile(recipient.id));
	}, [recipient, onClose]);
	const handleStartVoiceCall = useCallback(async () => {
		if (!recipient) return;
		ModalCommands.runAfterBottomSheetClose(onClose, () => {
			void (async () => {
				try {
					const channelId = await PrivateChannelCommands.ensureDMChannel(recipient.id);
					await CallUtils.requestStartCall(i18n, channelId, {kind: 'voice'});
				} catch (error) {
					logger.error('Failed to start voice call:', error);
				}
			})();
		});
	}, [i18n, recipient, onClose]);
	const handleAddNote = useCallback(() => {
		if (!recipient) return;
		ModalCommands.runAfterBottomSheetClose(onClose, () =>
			UserProfileCommands.openUserProfile(recipient.id, undefined, true),
		);
	}, [recipient, onClose]);
	const handleChangeFriendNickname = useCallback(() => {
		if (!recipient) return;
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<ChangeFriendNicknameModal
					user={recipient}
					data-flx="ui.action-menu.items.dm-menu-data.handle-change-friend-nickname.change-friend-nickname-modal"
				/>
			)),
		);
	}, [recipient, onClose]);
	const handleInviteToCommunity = useCallback(
		async (guildId: string, channelId: string, guildName: string) => {
			if (!recipient) return;
			const guardKey = getInviteToCommunityGuardKey(recipient.id, guildId, channelId);
			if (!beginInviteToCommunityGuard(guardKey)) return;
			onClose();
			try {
				let invite: Invite;
				let inviteUrl: string;
				const inviteCapability = InviteUtils.getInviteCapability(channelId, guildId);
				if (inviteCapability.useVanityUrl && inviteCapability.vanityUrlCode) {
					inviteUrl = InviteUtils.getVanityInviteUrl(inviteCapability.vanityUrlCode);
				} else {
					try {
						invite = await InviteCommands.create(channelId);
					} catch {
						return;
					}
					inviteUrl = `${RuntimeConfig.inviteEndpoint}/${invite.code}`;
				}
				const dmChannelId = await PrivateChannelCommands.ensureDMChannel(recipient.id);
				try {
					const result = await MessageCommands.send(dmChannelId, {
						content: inviteUrl,
						nonce: fromTimestamp(Date.now()),
					});
					if (result) {
						ToastCommands.createToast({
							type: 'success',
							children: i18n._(INVITE_SENT_TO_DESCRIPTOR, {guildName}),
						});
					}
				} catch (error) {
					logger.error('Failed to send community invite:', error);
					showDmActionErrorModal(error);
				}
			} finally {
				scheduleInviteToCommunityGuardRelease(guardKey);
			}
		},
		[recipient, onClose],
	);
	const handleSendFriendRequest = useCallback(() => {
		if (!recipient) return;
		RelationshipActionUtils.sendFriendRequest(i18n, recipient.id);
		onClose();
	}, [recipient, i18n, onClose]);
	const handleAcceptFriendRequest = useCallback(
		(event?: {shiftKey?: boolean}) => {
			if (!recipient) return;
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showAcceptFriendRequestConfirmation(i18n, recipient, {
					bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
					showShiftBypassConfirmationTip: true,
				}),
			);
		},
		[recipient, i18n, onClose],
	);
	const handleRemoveFriend = useCallback(
		(event?: {shiftKey?: boolean}) => {
			if (!recipient) return;
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showRemoveFriendConfirmation(i18n, recipient, {
					bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
					showShiftBypassConfirmationTip: true,
				}),
			);
		},
		[recipient, i18n, onClose],
	);
	const handleBlockUser = useCallback(
		(event?: {shiftKey?: boolean}) => {
			if (!recipient) return;
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showBlockUserConfirmation(i18n, recipient, {
					bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
					showShiftBypassConfirmationTip: true,
				}),
			);
		},
		[recipient, i18n, onClose],
	);
	const handleUnblockUser = useCallback(
		(event?: {shiftKey?: boolean}) => {
			if (!recipient) return;
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showUnblockUserConfirmation(i18n, recipient, {
					bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
					showShiftBypassConfirmationTip: true,
				}),
			);
		},
		[recipient, i18n, onClose],
	);
	const handleCloseDM = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<ConfirmModal
					title={i18n._(CLOSE_DM_DESCRIPTOR)}
					description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_CLOSE_YOUR_DESCRIPTOR, {
						recipientUsername: recipient ? NicknameUtils.getNickname(recipient, null) : '',
					})}
					primaryText={i18n._(CLOSE_DM_DESCRIPTOR)}
					primaryVariant="danger"
					onPrimary={async () => {
						try {
							await ChannelCommands.remove(channel.id);
							const selectedChannel = SelectedChannel.selectedChannelIds.get(ME);
							if (selectedChannel === channel.id) {
								RouterUtils.transitionTo(Routes.ME);
							}
							ToastCommands.createToast({
								type: 'success',
								children: i18n._(DM_CLOSED_DESCRIPTOR),
							});
						} catch (error) {
							logger.error('Failed to close DM:', error);
							window.setTimeout(() => {
								ModalCommands.push(
									modal(() => (
										<DMCloseFailedModal data-flx="ui.action-menu.items.dm-menu-data.handle-close-dm.dm-close-failed-modal" />
									)),
								);
							}, 0);
						}
					}}
					data-flx="ui.action-menu.items.dm-menu-data.handle-close-dm.confirm-modal"
				/>
			)),
		);
	}, [channel.id, i18n, recipient, onClose]);
	const handleLeaveGroup = useCallback(() => {
		ModalCommands.runAfterBottomSheetClose(onClose, () => leaveGroup(channel.id));
	}, [channel.id, leaveGroup, onClose]);
	const handleDeleteMyMessagesInChannel = useCallback(() => {
		ModalCommands.runAfterBottomSheetClose(onClose, () => deleteMyMessagesInChannel(channel.id));
	}, [channel.id, deleteMyMessagesInChannel, onClose]);
	const handlePinDM = useCallback(async () => {
		onClose();
		try {
			await PrivateChannelCommands.pinDmChannel(channel.id);
			ToastCommands.createToast({
				type: 'success',
				children: isGroupDM ? i18n._(PINNED_GROUP_DM_DESCRIPTOR) : i18n._(PINNED_DM_DESCRIPTOR),
			});
		} catch (error) {
			logger.error('Failed to pin:', error);
			showDmMenuErrorModal(i18n, isGroupDM ? FAILED_TO_PIN_GROUP_DM_DESCRIPTOR : FAILED_TO_PIN_DM_DESCRIPTOR);
		}
	}, [channel.id, isGroupDM, onClose]);
	const handleUnpinDM = useCallback(async () => {
		onClose();
		try {
			await PrivateChannelCommands.unpinDmChannel(channel.id);
			ToastCommands.createToast({
				type: 'success',
				children: isGroupDM ? i18n._(UNPINNED_GROUP_DM_DESCRIPTOR) : i18n._(UNPINNED_DM_DESCRIPTOR),
			});
		} catch (error) {
			logger.error('Failed to unpin:', error);
			showDmMenuErrorModal(i18n, isGroupDM ? FAILED_TO_UNPIN_GROUP_DM_DESCRIPTOR : FAILED_TO_UNPIN_DM_DESCRIPTOR);
		}
	}, [channel.id, isGroupDM, onClose]);
	const handleEditGroup = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<EditGroupModal
					channelId={channel.id}
					data-flx="ui.action-menu.items.dm-menu-data.handle-edit-group.edit-group-modal"
				/>
			)),
		);
	}, [channel.id, onClose]);
	const handleShowInvites = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<GroupInvitesModal
					channelId={channel.id}
					data-flx="ui.action-menu.items.dm-menu-data.handle-show-invites.group-invites-modal"
				/>
			)),
		);
	}, [channel.id, onClose]);
	const handleCopyChannelId = useCallback(async () => {
		await TextCopyCommands.copy(i18n, channel.id, true);
		ToastCommands.createToast({
			type: 'success',
			children: i18n._(CHANNEL_ID_COPIED_DESCRIPTOR),
		});
		onClose();
	}, [channel.id, i18n, onClose]);
	const handleCopyUserId = useCallback(async () => {
		if (!recipient) return;
		await TextCopyCommands.copy(i18n, recipient.id, true);
		ToastCommands.createToast({
			type: 'success',
			children: i18n._(USER_ID_COPIED_DESCRIPTOR),
		});
		onClose();
	}, [recipient, i18n, onClose]);
	const handleDebugUser = useCallback(() => {
		if (!recipient) return;
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<UserDebugModal
					title={i18n._(USER_DEBUG_DESCRIPTOR)}
					user={recipient}
					data-flx="ui.action-menu.items.dm-menu-data.handle-debug-user.user-debug-modal"
				/>
			)),
		);
	}, [recipient, onClose]);
	const handleDebugChannel = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<ChannelDebugModal
					title={i18n._(CHANNEL_DEBUG_DESCRIPTOR)}
					channel={channel}
					data-flx="ui.action-menu.items.dm-menu-data.handle-debug-channel.channel-debug-modal"
				/>
			)),
		);
	}, [channel, onClose]);
	const handlers: DMMenuHandlers = useMemo(
		() => ({
			handleMarkAsRead,
			handleToggleFavorite,
			handleViewProfile,
			handleStartVoiceCall,
			handleAddNote,
			handleChangeFriendNickname,
			handleInviteToCommunity,
			handleSendFriendRequest,
			handleAcceptFriendRequest,
			handleRemoveFriend,
			handleBlockUser,
			handleUnblockUser,
			handleCloseDM,
			handleLeaveGroup,
			handleDeleteMyMessagesInChannel,
			handlePinDM,
			handleUnpinDM,
			handleEditGroup,
			handleShowInvites,
			handleCopyChannelId,
			handleCopyUserId,
			handleDebugUser,
			handleDebugChannel,
		}),
		[
			handleMarkAsRead,
			handleToggleFavorite,
			handleViewProfile,
			handleStartVoiceCall,
			handleAddNote,
			handleChangeFriendNickname,
			handleInviteToCommunity,
			handleSendFriendRequest,
			handleAcceptFriendRequest,
			handleRemoveFriend,
			handleBlockUser,
			handleUnblockUser,
			handleCloseDM,
			handleLeaveGroup,
			handleDeleteMyMessagesInChannel,
			handlePinDM,
			handleUnpinDM,
			handleEditGroup,
			handleShowInvites,
			handleCopyChannelId,
			handleCopyUserId,
			handleDebugUser,
			handleDebugChannel,
		],
	);
	const groups = useMemo(() => {
		const menuGroups: Array<MenuGroupType> = [];
		if (showMarkAsReadItem) {
			menuGroups.push({
				items: [
					{
						icon: <MarkAsReadIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.mark-as-read-icon" />,
						label: i18n._(MARK_AS_READ_DESCRIPTOR),
						onClick: handleMarkAsRead,
					},
				],
			});
		}
		if (Accessibility.showFavorites) {
			menuGroups.push({
				items: [
					{
						icon: (
							<FavoriteIcon
								size={20}
								filled={isFavorited}
								data-flx="ui.action-menu.items.dm-menu-data.groups.favorite-icon"
							/>
						),
						label: isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR),
						onClick: handleToggleFavorite,
					},
				],
			});
		}
		if (recipient && !isGroupDM) {
			const recipientItems: Array<MenuItemType> = [
				{
					icon: <ViewProfileIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.view-profile-icon" />,
					label: i18n._(VIEW_PROFILE_DESCRIPTOR),
					onClick: handleViewProfile,
				},
			];
			if (!restrictRecipientActions && !isRecipientBot && !hasActiveDirectCall) {
				recipientItems.push({
					icon: <VoiceCallIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.voice-call-icon" />,
					label: i18n._(VOICE_CALL_DESCRIPTOR),
					onClick: handleStartVoiceCall,
				});
			}
			if (!restrictRecipientActions) {
				recipientItems.push({
					icon: <AddNoteIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.add-note-icon" />,
					label: i18n._(ADD_NOTE_DESCRIPTOR),
					onClick: handleAddNote,
				});
			}
			if (!restrictRecipientActions && relationshipType === RelationshipTypes.FRIEND) {
				recipientItems.push({
					icon: <EditIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.edit-icon" />,
					label: i18n._(CHANGE_FRIEND_NICKNAME_DESCRIPTOR),
					onClick: handleChangeFriendNickname,
				});
			}
			menuGroups.push({items: recipientItems});
		}
		if (onOpenMuteSheet) {
			menuGroups.push({
				items: [
					{
						icon: <MuteIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.mute-icon" />,
						label: i18n._(isMuted ? UNMUTE_CONVERSATION_DESCRIPTOR : MUTE_CONVERSATION_DESCRIPTOR),
						hint: mutedText || undefined,
						onClick: onOpenMuteSheet,
					},
				],
			});
		}
		const groupActionsItems: Array<MenuItemType> = [];
		if (isGroupDM) {
			groupActionsItems.push({
				icon: <EditGroupIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.edit-group-icon" />,
				label: i18n._(EDIT_GROUP_DESCRIPTOR),
				onClick: handleEditGroup,
			});
			if (isOwner) {
				groupActionsItems.push({
					icon: <GroupInvitesIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.group-invites-icon" />,
					label: i18n._(INVITES_DESCRIPTOR),
					onClick: handleShowInvites,
				});
			}
		}
		groupActionsItems.push(
			channel.isPinned
				? {
						icon: <PinIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.pin-icon" />,
						label: isGroupDM ? i18n._(UNPIN_GROUP_DM_DESCRIPTOR) : i18n._(UNPIN_DM_DESCRIPTOR),
						onClick: handleUnpinDM,
					}
				: {
						icon: <PinIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.pin-icon--2" />,
						label: isGroupDM ? i18n._(PIN_GROUP_DM_DESCRIPTOR) : i18n._(PIN_DM_DESCRIPTOR),
						onClick: handlePinDM,
					},
		);
		menuGroups.push({items: groupActionsItems});
		if (recipient && !isGroupDM && !restrictRecipientActions && (!isRecipientBot || isFriendlyBot)) {
			const relationshipItems: Array<MenuItemType | MenuSubmenuItemType> = [];
			if (invitableCommunities.length > 0) {
				const submenuItems: Array<MenuItemType> = invitableCommunities.map(({guild, channelId}) => ({
					icon: (
						<SendInviteToCommunityIcon
							size={20}
							data-flx="ui.action-menu.items.dm-menu-data.submenu-items.send-invite-to-community-icon"
						/>
					),
					label: guild.name,
					onClick: () => handleInviteToCommunity(guild.id, channelId, guild.name),
				}));
				relationshipItems.push({
					icon: (
						<InviteToCommunityIcon
							size={20}
							data-flx="ui.action-menu.items.dm-menu-data.groups.invite-to-community-icon"
						/>
					),
					label: i18n._(INVITE_TO_COMMUNITY_DESCRIPTOR),
					items: submenuItems,
				});
			}
			if (relationshipType === RelationshipTypes.FRIEND) {
				relationshipItems.push({
					icon: <RemoveFriendIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.remove-friend-icon" />,
					label: i18n._(REMOVE_FRIEND_DESCRIPTOR),
					onClick: handleRemoveFriend,
					danger: true,
				});
			} else if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
				relationshipItems.push({
					icon: (
						<AcceptFriendRequestIcon
							size={20}
							data-flx="ui.action-menu.items.dm-menu-data.groups.accept-friend-request-icon"
						/>
					),
					label: i18n._(ACCEPT_FRIEND_REQUEST_DESCRIPTOR),
					onClick: handleAcceptFriendRequest,
				});
			} else if (
				relationshipType !== RelationshipTypes.OUTGOING_REQUEST &&
				relationshipType !== RelationshipTypes.BLOCKED &&
				!currentUserUnclaimed &&
				Users.currentUser?.verified !== false
			) {
				relationshipItems.push({
					icon: (
						<SendFriendRequestIcon
							size={20}
							data-flx="ui.action-menu.items.dm-menu-data.groups.send-friend-request-icon"
						/>
					),
					label: i18n._(ADD_FRIEND_DESCRIPTOR),
					onClick: handleSendFriendRequest,
				});
			}
			if (relationshipType === RelationshipTypes.BLOCKED) {
				relationshipItems.push({
					icon: <BlockUserIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.block-user-icon" />,
					label: i18n._(UNBLOCK_USER_ACTION_DESCRIPTOR),
					onClick: handleUnblockUser,
				});
			} else {
				relationshipItems.push({
					icon: <BlockUserIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.block-user-icon--2" />,
					label: i18n._(BLOCK_DESCRIPTOR),
					onClick: handleBlockUser,
					danger: true,
				});
			}
			if (relationshipItems.length > 0) {
				menuGroups.push({items: relationshipItems});
			}
		}
		const closeItems: Array<MenuItemType> = [
			{
				icon: <DeleteIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.delete-icon" />,
				label: i18n._(DELETE_MY_MESSAGES_DESCRIPTOR),
				onClick: handleDeleteMyMessagesInChannel,
				danger: true,
			},
			isGroupDM
				? {
						icon: <LeaveIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.leave-icon" />,
						label: i18n._(LEAVE_GROUP_DESCRIPTOR),
						onClick: handleLeaveGroup,
						danger: true,
					}
				: {
						icon: <CloseDMIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.close-dm-icon" />,
						label: i18n._(CLOSE_DM_DESCRIPTOR),
						onClick: handleCloseDM,
						danger: true,
					},
		];
		menuGroups.push({items: closeItems});
		const advancedItems: Array<MenuItemType> = [];
		if (developerMode && recipient) {
			advancedItems.push({
				icon: <DebugIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.debug-icon" />,
				label: i18n._(DEBUG_USER_DESCRIPTOR),
				onClick: handleDebugUser,
			});
		}
		if (developerMode) {
			advancedItems.push({
				icon: <DebugIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.debug-icon--2" />,
				label: i18n._(DEBUG_CHANNEL_DESCRIPTOR),
				onClick: handleDebugChannel,
			});
		}
		if (advancedItems.length > 0) {
			menuGroups.push({items: advancedItems});
		}
		const copyItems: Array<MenuItemType> = [];
		if (recipient) {
			copyItems.push({
				icon: <CopyIdIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.copy-id-icon" />,
				label: i18n._(COPY_USER_ID_DESCRIPTOR),
				onClick: handleCopyUserId,
			});
		}
		copyItems.push({
			icon: <CopyIdIcon size={20} data-flx="ui.action-menu.items.dm-menu-data.groups.copy-id-icon--2" />,
			label: i18n._(COPY_CHANNEL_ID_DESCRIPTOR),
			onClick: handleCopyChannelId,
		});
		menuGroups.push({items: copyItems});
		return menuGroups;
	}, [
		showMarkAsReadItem,
		isFavorited,
		recipient,
		isGroupDM,
		restrictRecipientActions,
		isRecipientBot,
		hasActiveDirectCall,
		relationshipType,
		isMuted,
		mutedText,
		onOpenMuteSheet,
		isOwner,
		channel.isPinned,
		isFriendlyBot,
		invitableCommunities,
		currentUserUnclaimed,
		developerMode,
		handleMarkAsRead,
		handleToggleFavorite,
		handleViewProfile,
		handleStartVoiceCall,
		handleAddNote,
		handleChangeFriendNickname,
		handleEditGroup,
		handleShowInvites,
		handleUnpinDM,
		handlePinDM,
		handleInviteToCommunity,
		handleRemoveFriend,
		handleAcceptFriendRequest,
		handleSendFriendRequest,
		handleUnblockUser,
		handleBlockUser,
		handleLeaveGroup,
		handleDeleteMyMessagesInChannel,
		handleCloseDM,
		handleDebugUser,
		handleDebugChannel,
		handleCopyUserId,
		handleCopyChannelId,
	]);
	return {
		groups,
		handlers,
		invitableCommunities,
		isGroupDM,
		isOwner,
		isMuted,
		mutedText: mutedText ?? null,
		isFavorited,
		relationshipType,
		isRecipientBot,
		isRecipientSystem,
		restrictRecipientActions,
		developerMode,
	};
}
