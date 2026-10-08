// SPDX-License-Identifier: AGPL-3.0-or-later

import {GroupRemoveUserFailedModal} from '@app/features/app/components/alerts/GroupRemoveUserFailedModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import Authentication from '@app/features/auth/state/Authentication';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {ChangeGroupDMNicknameModal} from '@app/features/channel/components/modals/ChangeGroupDMNicknameModal';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {
	MUTE_CONVERSATION_DESCRIPTOR,
	UNMUTE_CONVERSATION_DESCRIPTOR,
} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {GroupOwnershipTransferFailedModal} from '@app/features/guild/components/alerts/GroupOwnershipTransferFailedModal';
import {TRANSFER_OWNERSHIP_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Relationships from '@app/features/relationship/state/Relationships';
import {
	ChangeNicknameIcon,
	EditIcon,
	RemoveFromGroupIcon,
	TransferOwnershipIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {DataMenuRenderer} from '@app/features/ui/action_menu/DataMenuRenderer';
import {StartVoiceCallMenuItem} from '@app/features/ui/action_menu/items/CallMenuItems';
import {CopyUserIdMenuItem} from '@app/features/ui/action_menu/items/CopyMenuItems';
import {DebugUserMenuItem} from '@app/features/ui/action_menu/items/DebugMenuItems';
import {useDMMenuData} from '@app/features/ui/action_menu/items/DMMenuData';
import {MuteDMMenuItem} from '@app/features/ui/action_menu/items/DMMenuItems';
import {InviteToCommunityMenuItem} from '@app/features/ui/action_menu/items/InviteMenuItems';
import {MentionUserMenuItem} from '@app/features/ui/action_menu/items/MentionUserMenuItem';
import {MessageUserMenuItem} from '@app/features/ui/action_menu/items/MessageUserMenuItem';
import {
	BlockUserMenuItem,
	ChangeFriendNicknameMenuItem,
	RelationshipActionMenuItem,
	UnblockUserMenuItem,
} from '@app/features/ui/action_menu/items/RelationshipMenuItems';
import {AddNoteMenuItem} from '@app/features/ui/action_menu/items/UserNoteMenuItems';
import {UserProfileMenuItem} from '@app/features/ui/action_menu/items/UserProfileMenuItem';
import {
	EntranceSoundListenerSubmenu,
	LocalMuteParticipantMenuItem,
	ParticipantVolumeSlider,
} from '@app/features/ui/action_menu/items/VoiceParticipantMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {hasActiveDirectCallWithUser, isActiveCallParticipant} from '@app/features/voice/utils/PrivateCallMenuUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const REMOVE_FROM_GROUP_DESCRIPTOR = msg({
	message: 'Remove from group',
	comment: 'Group DM action that removes the selected member from the group.',
});
const ARE_YOU_SURE_YOU_WANT_TO_REMOVE_FROM_DESCRIPTOR = msg({
	message: 'Remove {userName} from the group?',
	comment: 'Confirm dialog body before removing a member from a group DM.',
});
const REMOVE_DESCRIPTOR = msg({
	message: 'Remove',
	comment: 'Action label for removing the selected item from a list or relationship.',
});
const REMOVED_FROM_GROUP_DESCRIPTOR = msg({
	message: 'Removed from group',
	comment: 'Toast confirming the selected member was removed from the group DM.',
});
const ARE_YOU_SURE_YOU_WANT_TO_MAKE_THE_DESCRIPTOR = msg({
	message: "Make {userName} the group owner? You'll lose owner privileges.",
	comment: 'Confirm dialog body before transferring ownership of a group DM.',
});
const OWNERSHIP_TRANSFERRED_DESCRIPTOR = msg({
	message: 'Ownership transferred',
	comment: 'Toast confirming group DM ownership was transferred.',
});
const CHANGE_MY_GROUP_NICKNAME_DESCRIPTOR = msg({
	message: 'Change my group nickname',
	comment: 'Group DM action that lets the current user change their own nickname in the group.',
});
const MAKE_GROUP_OWNER_DESCRIPTOR = msg({
	message: 'Make group owner',
	comment: 'Group DM action that transfers ownership of the group to the selected member.',
});
const CHANGE_GROUP_NICKNAME_DESCRIPTOR = msg({
	message: 'Change group nickname',
	comment: 'Group DM action that opens the change-nickname modal for the selected user.',
});
const logger = new Logger('GroupDMContextMenu');

interface GroupDMContextMenuProps {
	channel: Channel;
	onClose: () => void;
}

export const GroupDMContextMenu: React.FC<GroupDMContextMenuProps> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();
	const {groups} = useDMMenuData(channel, null, {
		onClose,
		preserveInitialMarkAsReadVisibility: true,
	});
	const excludeLabels = useMemo(
		() => [i18n._(MUTE_CONVERSATION_DESCRIPTOR), i18n._(UNMUTE_CONVERSATION_DESCRIPTOR)],
		[i18n.locale],
	);
	return (
		<>
			<DataMenuRenderer
				groups={groups}
				excludeLabels={excludeLabels}
				data-flx="ui.action-menu.group-dm-context-menu.data-menu-renderer"
			/>
			<MenuGroup data-flx="ui.action-menu.group-dm-context-menu.menu-group">
				<MuteDMMenuItem
					channel={channel}
					onClose={onClose}
					data-flx="ui.action-menu.group-dm-context-menu.mute-dm-menu-item"
				/>
			</MenuGroup>
		</>
	);
});

interface GroupDMMemberContextMenuProps {
	userId: string;
	channelId: string;
	onClose: () => void;
}

export const GroupDMMemberContextMenu: React.FC<GroupDMMemberContextMenuProps> = observer(
	({userId, channelId, onClose}) => {
		const {i18n} = useLingui();
		const currentUserId = Authentication.currentUserId;
		const channel = Channels.getChannel(channelId);
		const user = Users.getUser(userId);
		const userDisplayName = user ? NicknameUtils.getNickname(user, undefined, channelId) : '';
		const developerMode = UserSettings.developerMode;
		const handleChangeNickname = useCallback(() => {
			if (!user) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<ChangeGroupDMNicknameModal
						channelId={channelId}
						user={user}
						data-flx="ui.action-menu.group-dm-context-menu.handle-change-nickname.change-group-dm-nickname-modal"
					/>
				)),
			);
		}, [channelId, onClose, user]);
		const handleRemoveFromGroup = useCallback(() => {
			if (!user) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<ConfirmModal
						title={i18n._(REMOVE_FROM_GROUP_DESCRIPTOR)}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_REMOVE_FROM_DESCRIPTOR, {userName: userDisplayName})}
						primaryText={i18n._(REMOVE_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={async () => {
							try {
								await PrivateChannelCommands.removeRecipient(channelId, userId);
								ToastCommands.createToast({
									type: 'success',
									children: i18n._(REMOVED_FROM_GROUP_DESCRIPTOR),
								});
							} catch (error) {
								logger.error('Failed to remove from group:', error);
								window.setTimeout(() => {
									ModalCommands.push(
										modal(() => (
											<GroupRemoveUserFailedModal
												username={userDisplayName}
												data-flx="ui.action-menu.group-dm-context-menu.handle-remove-from-group.group-remove-user-failed-modal"
											/>
										)),
									);
								}, 0);
							}
						}}
						data-flx="ui.action-menu.group-dm-context-menu.handle-remove-from-group.confirm-modal"
					/>
				)),
			);
		}, [channelId, onClose, user, userDisplayName, userId, i18n]);
		const handleMakeGroupOwner = useCallback(() => {
			if (!user) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<ConfirmModal
						title={i18n._(TRANSFER_OWNERSHIP_DESCRIPTOR)}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_MAKE_THE_DESCRIPTOR, {userName: userDisplayName})}
						primaryText={i18n._(TRANSFER_OWNERSHIP_DESCRIPTOR)}
						onPrimary={async () => {
							try {
								await ChannelCommands.update(channelId, {owner_id: userId});
								ToastCommands.createToast({
									type: 'success',
									children: i18n._(OWNERSHIP_TRANSFERRED_DESCRIPTOR),
								});
							} catch (error) {
								logger.error('Failed to transfer ownership:', error);
								window.setTimeout(() => {
									ModalCommands.push(
										modal(() => (
											<GroupOwnershipTransferFailedModal
												username={userDisplayName}
												data-flx="ui.action-menu.group-dm-context-menu.handle-make-group-owner.group-ownership-transfer-failed-modal"
											/>
										)),
									);
								}, 0);
							}
						}}
						data-flx="ui.action-menu.group-dm-context-menu.handle-make-group-owner.confirm-modal"
					/>
				)),
			);
		}, [channelId, onClose, user, userDisplayName, userId, i18n]);
		if (!user || !channel) {
			return null;
		}
		const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
		if (!isGroupDM) {
			return null;
		}
		const isSelf = userId === currentUserId;
		const isOwner = channel.ownerId === currentUserId;
		const relationship = Relationships.getRelationship(userId);
		const relationshipType = relationship?.type;
		const showCallParticipantItems = !isSelf && isActiveCallParticipant(channel, userId);
		const showStartVoiceCall = !user.bot && !hasActiveDirectCallWithUser(userId);
		const advancedMenuGroup = (
			<MenuGroup data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.menu-group">
				{developerMode && (
					<DebugUserMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.debug-user-menu-item"
					/>
				)}
				<CopyUserIdMenuItem
					user={user}
					onClose={onClose}
					data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.copy-user-id-menu-item"
				/>
			</MenuGroup>
		);
		if (isSelf) {
			return (
				<>
					<MenuGroup data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.menu-group--2">
						<UserProfileMenuItem
							user={user}
							guildId={undefined}
							onClose={onClose}
							data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.user-profile-menu-item"
						/>
						<MentionUserMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.mention-user-menu-item"
						/>
						<MenuItem
							icon={<EditIcon data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.edit-icon" />}
							onClick={handleChangeNickname}
							data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.menu-item.change-nickname"
						>
							{i18n._(CHANGE_MY_GROUP_NICKNAME_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
					{advancedMenuGroup}
				</>
			);
		}
		return (
			<>
				<MenuGroup data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.menu-group--3">
					<UserProfileMenuItem
						user={user}
						guildId={undefined}
						onClose={onClose}
						data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.user-profile-menu-item--2"
					/>
					<MentionUserMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.mention-user-menu-item--2"
					/>
					<MessageUserMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.message-user-menu-item"
					/>
					{showStartVoiceCall && (
						<StartVoiceCallMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.start-voice-call-menu-item"
						/>
					)}
					<AddNoteMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.add-note-menu-item"
					/>
					<ChangeFriendNicknameMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.change-friend-nickname-menu-item"
					/>
				</MenuGroup>
				{showCallParticipantItems && (
					<MenuGroup data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.menu-group--4">
						<ParticipantVolumeSlider
							userId={user.id}
							data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.participant-volume-slider"
						/>
					</MenuGroup>
				)}
				{isOwner && (
					<MenuGroup data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.menu-group--5">
						<MenuItem
							icon={
								<RemoveFromGroupIcon data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.remove-from-group-icon" />
							}
							onClick={handleRemoveFromGroup}
							danger
							data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.menu-item.remove-from-group"
						>
							{i18n._(REMOVE_FROM_GROUP_DESCRIPTOR)}
						</MenuItem>
						{!user.bot && (
							<MenuItem
								icon={
									<TransferOwnershipIcon data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.transfer-ownership-icon" />
								}
								onClick={handleMakeGroupOwner}
								danger
								data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.menu-item.make-group-owner"
							>
								{i18n._(MAKE_GROUP_OWNER_DESCRIPTOR)}
							</MenuItem>
						)}
						<MenuItem
							icon={
								<ChangeNicknameIcon data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.change-nickname-icon" />
							}
							onClick={handleChangeNickname}
							data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.menu-item.change-nickname--2"
						>
							{i18n._(CHANGE_GROUP_NICKNAME_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
				)}
				<MenuGroup data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.menu-group--6">
					<InviteToCommunityMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.invite-to-community-menu-item"
					/>
					<RelationshipActionMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.relationship-action-menu-item"
					/>
					{relationshipType === RelationshipTypes.BLOCKED ? (
						<UnblockUserMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.unblock-user-menu-item"
						/>
					) : (
						<BlockUserMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.block-user-menu-item"
						/>
					)}
				</MenuGroup>
				{showCallParticipantItems && (
					<MenuGroup data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.menu-group--7">
						<LocalMuteParticipantMenuItem
							userId={user.id}
							onClose={onClose}
							data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.local-mute-participant-menu-item"
						/>
						<EntranceSoundListenerSubmenu
							userId={user.id}
							data-flx="ui.action-menu.group-dm-context-menu.group-dm-member-context-menu.entrance-sound-submenu"
						/>
					</MenuGroup>
				)}
				{advancedMenuGroup}
			</>
		);
	},
);
