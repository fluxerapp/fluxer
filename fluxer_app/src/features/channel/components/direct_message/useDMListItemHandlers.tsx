// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {showChannelErrorModal} from '@app/features/channel/components/alerts/ChannelErrorModalUtils';
import {DMCloseFailedModal} from '@app/features/channel/components/alerts/DMCloseFailedModal';
import type {InviteCandidate} from '@app/features/channel/components/direct_message/DMListHelpers';
import {createMuteConfig} from '@app/features/channel/components/MuteOptions';
import {EditGroupModal} from '@app/features/channel/components/modals/EditGroupModal';
import {GroupInvitesModal} from '@app/features/channel/components/modals/GroupInvitesModal';
import {useDeleteMyMessagesInChannel} from '@app/features/channel/hooks/useDeleteMyMessagesInChannel';
import type {Channel} from '@app/features/channel/models/Channel';
import {CLOSE_DM_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {
	CHANNEL_ADDED_TO_FAVORITES_DESCRIPTOR,
	CHANNEL_REMOVED_FROM_FAVORITES_DESCRIPTOR,
	DM_CLOSED_DESCRIPTOR,
	FAILED_TO_SEND_INVITE_DESCRIPTOR,
	PINNED_DM_DESCRIPTOR,
	TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR,
	UNPINNED_DM_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import Favorites from '@app/features/messaging/state/Favorites';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import {ChangeFriendNicknameModal} from '@app/features/relationship/components/modals/ChangeFriendNicknameModal';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import * as LayoutCommands from '@app/features/ui/commands/LayoutCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import * as CallUtils from '@app/features/voice/utils/CallUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {fromTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import type React from 'react';
import {useCallback} from 'react';

const ARE_YOU_SURE_YOU_WANT_TO_CLOSE_THIS_DESCRIPTOR = msg({
	message: 'Close this group DM? You can reopen it anytime.',
	comment: 'Body of the close confirmation alert for a group DM. Reassures that the DM can be reopened.',
});
const ARE_YOU_SURE_YOU_WANT_TO_CLOSE_YOUR_DESCRIPTOR = msg({
	message: 'Close your DM with {username}? You can reopen it anytime.',
	comment: 'Body of the close confirmation alert for a one-on-one DM. username is the recipient display name.',
});
const INVITE_SENT_FOR_DESCRIPTOR = msg({
	message: 'Invite sent for {name}',
	comment: 'Toast confirmation shown after sending a community invite to the DM recipient. name is the community name.',
});
const FAILED_TO_SEND_INVITE_TITLE_DESCRIPTOR = msg({
	message: "Couldn't send invite",
	comment: 'Title of the error modal shown when sending a community invite through a DM fails.',
});
const FAILED_TO_PIN_DM_DESCRIPTOR = msg({
	message: 'Failed to pin DM',
	comment: 'Error modal title shown when pinning a DM in the DM list fails.',
});
const FAILED_TO_UNPIN_DM_DESCRIPTOR = msg({
	message: 'Failed to unpin DM',
	comment: 'Error modal title shown when unpinning a DM in the DM list fails.',
});
const logger = new Logger('DMListItem');

interface UseDMListItemHandlersParams {
	channel: Channel;
	recipient: User | null;
	isGroupDM: boolean;
	isMobile: boolean;
	relationshipType: number | undefined;
	restrictRecipientActions: boolean;
	closeAllSheets: () => void;
	setMenuOpen: (open: boolean) => void;
	setEditGroupSheetOpen: (open: boolean) => void;
	setInvitesSheetOpen: (open: boolean) => void;
	leaveGroup: (channelId: string) => void;
	i18n: I18n;
}

export interface DMListItemHandlers {
	navigateTo: () => void;
	handleRemoveChannel: () => void;
	handleCopyChannelId: () => void;
	handleCopyUserId: () => void;
	handleMarkAsRead: () => void;
	handleCloseDm: () => void;
	handleViewProfile: () => void;
	handleStartVoiceCall: () => void;
	handleAddNote: () => void;
	handleChangeFriendNickname: () => void;
	handleMute: (duration: number | null) => void;
	handleUnmute: () => void;
	handleAddToFavorites: (categoryId: string | null) => void;
	handleRemoveFromFavorites: () => void;
	handleSendFriendRequest: () => void;
	handleAcceptFriendRequest: () => void;
	handleIgnoreFriendRequest: () => void;
	handleRemoveFriend: () => void;
	handleBlockUser: () => void;
	handleUnblockUser: () => void;
	handleSendInvite: (candidate: InviteCandidate) => Promise<void>;
	handleEditGroup: () => void;
	handleShowInvites: () => void;
	handleLeaveGroup: () => void;
	handleDeleteMyMessagesInChannel: () => void;
	handlePinChannel: () => Promise<void>;
	handleUnpinChannel: () => Promise<void>;
}

export function useDMListItemHandlers({
	channel,
	recipient,
	isGroupDM,
	isMobile,
	relationshipType,
	restrictRecipientActions,
	closeAllSheets,
	setMenuOpen,
	setEditGroupSheetOpen,
	setInvitesSheetOpen,
	leaveGroup,
	i18n,
}: UseDMListItemHandlersParams): DMListItemHandlers {
	const deleteMyMessagesInChannel = useDeleteMyMessagesInChannel();
	const navigateTo = useCallback(() => {
		NavigationCommands.selectChannel(ME, channel.id);
		if (MobileLayout.isMobileLayout()) {
			LayoutCommands.updateMobileLayoutState(false, true);
		}
	}, [channel.id]);
	const handleRemoveChannel = useCallback(
		(e?: React.MouseEvent | React.KeyboardEvent) => {
			if (e) {
				e.preventDefault();
				e.stopPropagation();
			}
			setMenuOpen(false);
			if (isGroupDM) {
				leaveGroup(channel.id);
				return;
			}
			ChannelCommands.remove(channel.id);
			const selectedChannel = SelectedChannel.selectedChannelIds.get(ME);
			if (selectedChannel === channel.id) {
				NavigationCommands.deselectGuild();
			}
		},
		[channel.id, isGroupDM, leaveGroup, setMenuOpen],
	);
	const handleCopyChannelId = useCallback(async () => {
		await TextCopyCommands.copy(i18n, channel.id);
		closeAllSheets();
	}, [channel.id, closeAllSheets, i18n]);
	const handleCopyUserId = useCallback(async () => {
		if (!recipient) return;
		await TextCopyCommands.copy(i18n, recipient.id);
		closeAllSheets();
	}, [recipient, closeAllSheets, i18n]);
	const handleMarkAsRead = useCallback(() => {
		ReadStateCommands.ack(channel.id, true, true);
		closeAllSheets();
	}, [channel.id, closeAllSheets]);
	const handleCloseDm = useCallback(() => {
		const username = recipient ? NicknameUtils.getNickname(recipient, null) : '';
		const description = isGroupDM
			? i18n._(ARE_YOU_SURE_YOU_WANT_TO_CLOSE_THIS_DESCRIPTOR)
			: i18n._(ARE_YOU_SURE_YOU_WANT_TO_CLOSE_YOUR_DESCRIPTOR, {username});
		ModalCommands.pushAfterBottomSheetClose(
			closeAllSheets,
			modal(() => (
				<ConfirmModal
					title={i18n._(CLOSE_DM_DESCRIPTOR)}
					description={description}
					primaryText={i18n._(CLOSE_DM_DESCRIPTOR)}
					primaryVariant="danger"
					onPrimary={async () => {
						try {
							await ChannelCommands.remove(channel.id);
							const selectedChannel = SelectedChannel.selectedChannelIds.get(ME);
							if (selectedChannel === channel.id) {
								NavigationCommands.deselectGuild();
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
										<DMCloseFailedModal data-flx="channel.direct-message.use-dm-list-item-handlers.handle-close-dm.dm-close-failed-modal" />
									)),
								);
							}, 0);
						}
					}}
					data-flx="channel.direct-message.use-dm-list-item-handlers.handle-close-dm.confirm-modal"
				/>
			)),
		);
	}, [channel.id, closeAllSheets, i18n, isGroupDM, recipient]);
	const handleViewProfile = useCallback(() => {
		if (!recipient) return;
		ModalCommands.runAfterBottomSheetClose(closeAllSheets, () => UserProfileCommands.openUserProfile(recipient.id));
	}, [recipient, closeAllSheets]);
	const handleStartVoiceCall = useCallback(async () => {
		if (!recipient || recipient.bot) return;
		ModalCommands.runAfterBottomSheetClose(closeAllSheets, () => {
			void (async () => {
				try {
					const dmChannelId = await PrivateChannelCommands.ensureDMChannel(recipient.id);
					await CallUtils.requestStartCall(i18n, dmChannelId, {kind: 'voice'});
				} catch (error) {
					logger.error('Failed to start voice call:', error);
				}
			})();
		});
	}, [closeAllSheets, i18n, recipient]);
	const handleAddNote = useCallback(() => {
		if (!recipient || recipient.bot || restrictRecipientActions) return;
		ModalCommands.runAfterBottomSheetClose(closeAllSheets, () =>
			UserProfileCommands.openUserProfile(recipient.id, undefined, true),
		);
	}, [recipient, restrictRecipientActions, closeAllSheets]);
	const handleChangeFriendNickname = useCallback(() => {
		if (!recipient || relationshipType !== RelationshipTypes.FRIEND) return;
		ModalCommands.pushAfterBottomSheetClose(
			closeAllSheets,
			modal(() => (
				<ChangeFriendNicknameModal
					user={recipient}
					data-flx="channel.direct-message.use-dm-list-item-handlers.handle-change-friend-nickname.change-friend-nickname-modal"
				/>
			)),
		);
	}, [recipient, relationshipType, closeAllSheets]);
	const handleMute = useCallback(
		(duration: number | null) => {
			closeAllSheets();
			UserGuildSettingsCommands.updateChannelOverride(
				null,
				channel.id,
				{
					muted: true,
					mute_config: duration ? createMuteConfig(duration) : null,
				},
				{persistImmediately: true},
			);
		},
		[channel.id, closeAllSheets],
	);
	const handleUnmute = useCallback(() => {
		closeAllSheets();
		UserGuildSettingsCommands.updateChannelOverride(
			null,
			channel.id,
			{
				muted: false,
				mute_config: null,
			},
			{persistImmediately: true},
		);
	}, [channel.id, closeAllSheets]);
	const handleAddToFavorites = useCallback(
		(categoryId: string | null) => {
			closeAllSheets();
			const guildId = channel.guildId ?? ME;
			Favorites.addChannel(channel.id, guildId, categoryId);
			ToastCommands.createToast({type: 'success', children: i18n._(CHANNEL_ADDED_TO_FAVORITES_DESCRIPTOR)});
		},
		[channel.id, channel.guildId, closeAllSheets, i18n],
	);
	const handleRemoveFromFavorites = useCallback(() => {
		closeAllSheets();
		Favorites.removeChannel(channel.id);
		ToastCommands.createToast({type: 'success', children: i18n._(CHANNEL_REMOVED_FROM_FAVORITES_DESCRIPTOR)});
	}, [channel.id, closeAllSheets, i18n]);
	const handleSendFriendRequest = useCallback(async () => {
		if (!recipient) return;
		closeAllSheets();
		await RelationshipActionUtils.sendFriendRequest(i18n, recipient.id);
	}, [recipient, closeAllSheets, i18n]);
	const handleAcceptFriendRequest = useCallback(
		(event?: {shiftKey?: boolean}) => {
			if (!recipient) return;
			ModalCommands.runAfterBottomSheetClose(closeAllSheets, () =>
				RelationshipActionUtils.showAcceptFriendRequestConfirmation(i18n, recipient, {
					bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
					showShiftBypassConfirmationTip: true,
				}),
			);
		},
		[recipient, closeAllSheets, i18n],
	);
	const handleIgnoreFriendRequest = useCallback(() => {
		if (!recipient) return;
		closeAllSheets();
		RelationshipActionUtils.ignoreFriendRequest(i18n, recipient.id);
	}, [recipient, closeAllSheets, i18n]);
	const handleRemoveFriend = useCallback(
		(event?: {shiftKey?: boolean}) => {
			if (!recipient) return;
			ModalCommands.runAfterBottomSheetClose(closeAllSheets, () =>
				RelationshipActionUtils.showRemoveFriendConfirmation(i18n, recipient, {
					bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
					showShiftBypassConfirmationTip: true,
				}),
			);
		},
		[recipient, closeAllSheets, i18n],
	);
	const handleBlockUser = useCallback(
		(event?: {shiftKey?: boolean}) => {
			if (!recipient) return;
			ModalCommands.runAfterBottomSheetClose(closeAllSheets, () =>
				RelationshipActionUtils.showBlockUserConfirmation(i18n, recipient, {
					bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
					showShiftBypassConfirmationTip: true,
				}),
			);
		},
		[recipient, closeAllSheets, i18n],
	);
	const handleUnblockUser = useCallback(
		(event?: {shiftKey?: boolean}) => {
			if (!recipient) return;
			ModalCommands.runAfterBottomSheetClose(closeAllSheets, () =>
				RelationshipActionUtils.showUnblockUserConfirmation(i18n, recipient, {
					bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
					showShiftBypassConfirmationTip: true,
				}),
			);
		},
		[recipient, closeAllSheets, i18n],
	);
	const handleSendInvite = useCallback(
		async (candidate: InviteCandidate) => {
			closeAllSheets();
			if (!recipient) return;
			try {
				const invite = await InviteCommands.create(candidate.channelId);
				const inviteUrl = `${RuntimeConfig.inviteEndpoint}/${invite.code}`;
				const dmChannelId = await PrivateChannelCommands.ensureDMChannel(recipient.id);
				await MessageCommands.send(dmChannelId, {
					content: inviteUrl,
					nonce: fromTimestamp(Date.now()),
				});
				ToastCommands.createToast({
					type: 'success',
					children: i18n._(INVITE_SENT_FOR_DESCRIPTOR, {name: candidate.guild.name}),
				});
			} catch (error) {
				logger.error('Failed to send invite via DM sheet:', error);
				showChannelErrorModal({
					title: i18n._(FAILED_TO_SEND_INVITE_TITLE_DESCRIPTOR),
					message: i18n._(FAILED_TO_SEND_INVITE_DESCRIPTOR),
					dataFlx: 'channel.direct-message.use-dm-list-item-handlers.send-invite-failed.generic-error-modal',
				});
			}
		},
		[recipient, closeAllSheets, i18n],
	);
	const handleEditGroup = useCallback(() => {
		if (isMobile) {
			ModalCommands.runAfterBottomSheetClose(
				() => setMenuOpen(false),
				() => setEditGroupSheetOpen(true),
			);
		} else {
			setMenuOpen(false);
			ModalCommands.push(
				modal(() => (
					<EditGroupModal
						channelId={channel.id}
						data-flx="channel.direct-message.use-dm-list-item-handlers.handle-edit-group.edit-group-modal"
					/>
				)),
			);
		}
	}, [channel.id, isMobile, setMenuOpen, setEditGroupSheetOpen]);
	const handleShowInvites = useCallback(() => {
		if (isMobile) {
			ModalCommands.runAfterBottomSheetClose(
				() => setMenuOpen(false),
				() => setInvitesSheetOpen(true),
			);
		} else {
			setMenuOpen(false);
			ModalCommands.push(
				modal(() => (
					<GroupInvitesModal
						channelId={channel.id}
						data-flx="channel.direct-message.use-dm-list-item-handlers.handle-show-invites.group-invites-modal"
					/>
				)),
			);
		}
	}, [channel.id, isMobile, setMenuOpen, setInvitesSheetOpen]);
	const handleLeaveGroup = useCallback(() => {
		ModalCommands.runAfterBottomSheetClose(
			() => setMenuOpen(false),
			() => leaveGroup(channel.id),
		);
	}, [channel.id, leaveGroup, setMenuOpen]);
	const handleDeleteMyMessagesInChannel = useCallback(() => {
		ModalCommands.runAfterBottomSheetClose(closeAllSheets, () => deleteMyMessagesInChannel(channel.id));
	}, [channel.id, closeAllSheets, deleteMyMessagesInChannel]);
	const handlePinChannel = useCallback(async () => {
		closeAllSheets();
		try {
			await PrivateChannelCommands.pinDmChannel(channel.id);
			ToastCommands.createToast({type: 'success', children: i18n._(PINNED_DM_DESCRIPTOR)});
		} catch (error) {
			logger.error('Failed to pin DM:', error);
			showChannelErrorModal({
				title: i18n._(FAILED_TO_PIN_DM_DESCRIPTOR),
				message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx: 'channel.direct-message.use-dm-list-item-handlers.pin-dm-failed.generic-error-modal',
			});
		}
	}, [channel.id, closeAllSheets, i18n]);
	const handleUnpinChannel = useCallback(async () => {
		closeAllSheets();
		try {
			await PrivateChannelCommands.unpinDmChannel(channel.id);
			ToastCommands.createToast({type: 'success', children: i18n._(UNPINNED_DM_DESCRIPTOR)});
		} catch (error) {
			logger.error('Failed to unpin DM:', error);
			showChannelErrorModal({
				title: i18n._(FAILED_TO_UNPIN_DM_DESCRIPTOR),
				message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx: 'channel.direct-message.use-dm-list-item-handlers.unpin-dm-failed.generic-error-modal',
			});
		}
	}, [channel.id, closeAllSheets, i18n]);
	return {
		navigateTo,
		handleRemoveChannel,
		handleCopyChannelId,
		handleCopyUserId,
		handleMarkAsRead,
		handleCloseDm,
		handleViewProfile,
		handleStartVoiceCall,
		handleAddNote,
		handleChangeFriendNickname,
		handleMute,
		handleUnmute,
		handleAddToFavorites,
		handleRemoveFromFavorites,
		handleSendFriendRequest,
		handleAcceptFriendRequest,
		handleIgnoreFriendRequest,
		handleRemoveFriend,
		handleBlockUser,
		handleUnblockUser,
		handleSendInvite,
		handleEditGroup,
		handleShowInvites,
		handleLeaveGroup,
		handleDeleteMyMessagesInChannel,
		handlePinChannel,
		handleUnpinChannel,
	};
}
