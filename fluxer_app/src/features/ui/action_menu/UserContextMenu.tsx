// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import Authentication from '@app/features/auth/state/Authentication';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {DMCloseFailedModal} from '@app/features/channel/components/alerts/DMCloseFailedModal';
import {ChangeGroupDMNicknameModal} from '@app/features/channel/components/modals/ChangeGroupDMNicknameModal';
import Channels from '@app/features/channel/state/Channels';
import {CLOSE_DM_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import DeveloperMode from '@app/features/devtools/state/DeveloperMode';
import {DM_CLOSED_DESCRIPTOR, TRANSFER_OWNERSHIP_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Relationships from '@app/features/relationship/state/Relationships';
import {
	ChangeNicknameIcon,
	CloseDMIcon,
	RemoveFromGroupIcon,
	TransferOwnershipIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {RingUserMenuItem, StartVoiceCallMenuItem} from '@app/features/ui/action_menu/items/CallMenuItems';
import {FavoriteChannelMenuItem} from '@app/features/ui/action_menu/items/ChannelMenuItems';
import {CopyUserIdMenuItem} from '@app/features/ui/action_menu/items/CopyMenuItems';
import {DebugUserMenuItem} from '@app/features/ui/action_menu/items/DebugMenuItems';
import {MarkDMAsReadMenuItem} from '@app/features/ui/action_menu/items/DMMenuItems';
import {InviteToCommunityMenuItem} from '@app/features/ui/action_menu/items/InviteMenuItems';
import {MentionUserMenuItem} from '@app/features/ui/action_menu/items/MentionUserMenuItem';
import {MessageUserMenuItem} from '@app/features/ui/action_menu/items/MessageUserMenuItem';
import {
	BlockUserMenuItem,
	ChangeFriendNicknameMenuItem,
	RelationshipActionMenuItem,
	UnblockUserMenuItem,
} from '@app/features/ui/action_menu/items/RelationshipMenuItems';
import {ReportMessageMenuItem} from '@app/features/ui/action_menu/items/ReportMessageMenuItem';
import {ReportUserMenuItem} from '@app/features/ui/action_menu/items/ReportUserMenuItem';
import {shouldShowSpammerOverrideMenuItems} from '@app/features/ui/action_menu/items/SpammerOverrideMenuItems';
import {StaffDeveloperUserControlsMenuItem} from '@app/features/ui/action_menu/items/StaffDeveloperUserControlsMenuItem';
import {shouldShowStaffUserControlsMenuItems} from '@app/features/ui/action_menu/items/StaffUserControlsMenuItems';
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
import type {User} from '@app/features/user/models/User';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import CallState from '@app/features/voice/state/CallState';
import {hasActiveDirectCallWithUser, isActiveCallParticipant} from '@app/features/voice/utils/PrivateCallMenuUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

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
const CHANGE_OWNER_DESCRIPTOR = msg({
	message: 'Change owner',
	comment: 'Action that transfers ownership of the selected group or community.',
});
const ARE_YOU_SURE_YOU_WANT_TO_TRANSFER_OWNERSHIP_DESCRIPTOR = msg({
	message: 'Transfer group ownership to {userName}?',
	comment: 'Confirm dialog body before transferring ownership of a group DM.',
});
const ARE_YOU_SURE_YOU_WANT_TO_CLOSE_YOUR_DESCRIPTOR = msg({
	message: 'Close your DM with {displayName}? You can reopen it anytime.',
	comment: 'Confirm dialog body before closing a DM channel.',
});
const CHANGE_GROUP_NICKNAME_DESCRIPTOR = msg({
	message: 'Change group nickname',
	comment: 'Group DM action that opens the change-nickname modal for the selected user.',
});
const MAKE_GROUP_OWNER_DESCRIPTOR = msg({
	message: 'Make group owner',
	comment: 'Group DM action that transfers ownership of the group to the selected member.',
});
const logger = new Logger('UserContextMenu');

interface UserContextMenuProps {
	user: User;
	onClose: () => void;
	guildId?: string;
	channelId?: string;
	isCallContext?: boolean;
	message?: Message;
}

export const UserContextMenu: React.FC<UserContextMenuProps> = observer(
	({user, onClose, guildId, channelId, isCallContext = false, message}) => {
		const {i18n} = useLingui();
		const channel = channelId ? Channels.getChannel(channelId) : null;
		const canSendMessages = channel
			? channel.isPrivate() || Permission.can(Permissions.SEND_MESSAGES, {channelId, guildId})
			: true;
		const canMention = channel !== null && canSendMessages;
		const isCurrentUser = user.id === Authentication.currentUserId;
		const currentUser = Users.currentUser;
		const relationship = Relationships.getRelationship(user.id);
		const relationshipType = relationship?.type;
		const developerMode = UserSettings.developerMode;
		const isDeveloper = DeveloperMode.isDeveloper;
		const currentUserId = Authentication.currentUserId;
		const isSystemUser = user.system;
		const restrictUserActions = isSystemUser;
		const canShowStaffUserControls = shouldShowStaffUserControlsMenuItems({
			currentUser,
			user,
			isCurrentUser,
			restrictUserActions,
		});
		const dmPartnerId = channel?.isDM()
			? (channel.recipientIds.find((id) => id !== currentUserId) ?? channel.recipientIds[0])
			: null;
		const dmPartner = dmPartnerId ? Users.getUser(dmPartnerId) : null;
		const isGroupDM = channel?.isGroupDM();
		const isOwner = channel?.ownerId === currentUserId;
		const isRecipient = channel?.recipientIds.includes(user.id);
		const isBot = user.bot;
		const call = channelId ? CallState.getCall(channelId) : null;
		const showCallItems = isCallContext && call && !isCurrentUser;
		const showPrivateCallParticipantItems =
			!isCurrentUser && !restrictUserActions && isActiveCallParticipant(channel, user.id);
		const showStartVoiceCall = !isCurrentUser && !isBot && !hasActiveDirectCallWithUser(user.id);
		const renderReportActions = (restricted: boolean) => {
			if (restricted || isCurrentUser) {
				return null;
			}
			return (
				<MenuGroup data-flx="ui.action-menu.user-context-menu.render-report-actions.menu-group">
					{message && (
						<ReportMessageMenuItem
							message={message}
							onClose={onClose}
							data-flx="ui.action-menu.user-context-menu.render-report-actions.report-message-menu-item"
						/>
					)}
					<ReportUserMenuItem
						user={user}
						guildId={guildId}
						message={message}
						onClose={onClose}
						data-flx="ui.action-menu.user-context-menu.render-report-actions.report-user-menu-item"
					/>
					{relationshipType === RelationshipTypes.BLOCKED ? (
						<UnblockUserMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.user-context-menu.render-report-actions.unblock-user-menu-item"
						/>
					) : (
						<BlockUserMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.user-context-menu.render-report-actions.block-user-menu-item"
						/>
					)}
				</MenuGroup>
			);
		};
		const handleChangeGroupNickname = useCallback(() => {
			if (!channel) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<ChangeGroupDMNicknameModal
						channelId={channel.id}
						user={user}
						data-flx="ui.action-menu.user-context-menu.handle-change-group-nickname.change-group-dm-nickname-modal"
					/>
				)),
			);
		}, [channel, onClose, user]);
		const userDisplayName = NicknameUtils.getNickname(user, channel?.guildId ?? null, channel?.id);
		const handleRemoveFromGroup = useCallback(() => {
			if (!channel) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<ConfirmModal
						title={i18n._(REMOVE_FROM_GROUP_DESCRIPTOR)}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_REMOVE_FROM_DESCRIPTOR, {userName: userDisplayName})}
						primaryText={i18n._(REMOVE_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={() => PrivateChannelCommands.removeRecipient(channel.id, user.id)}
						data-flx="ui.action-menu.user-context-menu.handle-remove-from-group.confirm-modal"
					/>
				)),
			);
		}, [channel, onClose, user.id, userDisplayName, i18n]);
		const handleMakeGroupOwner = useCallback(() => {
			if (!channel) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<ConfirmModal
						title={i18n._(CHANGE_OWNER_DESCRIPTOR)}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_TRANSFER_OWNERSHIP_DESCRIPTOR, {
							userName: userDisplayName,
						})}
						primaryText={i18n._(TRANSFER_OWNERSHIP_DESCRIPTOR)}
						onPrimary={() => {
							ChannelCommands.update(channel.id, {owner_id: user.id});
						}}
						data-flx="ui.action-menu.user-context-menu.handle-make-group-owner.confirm-modal"
					/>
				)),
			);
		}, [channel, onClose, user.id, userDisplayName, i18n]);
		const handleCloseDM = useCallback(() => {
			if (!channel || !channel.isDM()) return;
			onClose();
			const displayName = dmPartner ? NicknameUtils.getNickname(dmPartner) : NicknameUtils.getNickname(user);
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(CLOSE_DM_DESCRIPTOR)}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_CLOSE_YOUR_DESCRIPTOR, {displayName})}
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
											<DMCloseFailedModal data-flx="ui.action-menu.user-context-menu.handle-close-dm.dm-close-failed-modal" />
										)),
									);
								}, 0);
							}
						}}
						data-flx="ui.action-menu.user-context-menu.handle-close-dm.confirm-modal"
					/>
				)),
			);
		}, [channel, dmPartner, i18n, onClose, user]);
		const renderAdvancedMenuGroup = () => (
			<MenuGroup data-flx="ui.action-menu.user-context-menu.render-advanced-menu-group.menu-group">
				{developerMode && (
					<DebugUserMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.user-context-menu.render-advanced-menu-group.debug-user-menu-item"
					/>
				)}
				<CopyUserIdMenuItem
					user={user}
					onClose={onClose}
					data-flx="ui.action-menu.user-context-menu.render-advanced-menu-group.copy-user-id-menu-item"
				/>
			</MenuGroup>
		);
		const renderStaffDeveloperControlsMenuGroup = (restricted: boolean) => {
			const canShowSpammerOverrideControls =
				!restricted && !isCurrentUser && shouldShowSpammerOverrideMenuItems({user, developerMode: isDeveloper});
			if (!canShowStaffUserControls && !canShowSpammerOverrideControls) {
				return null;
			}
			return (
				<MenuGroup data-flx="ui.action-menu.user-context-menu.render-staff-developer-controls-menu-group.menu-group">
					<StaffDeveloperUserControlsMenuItem
						user={user}
						showStaffControls={canShowStaffUserControls}
						showSpammerOverrideControls={canShowSpammerOverrideControls}
						developerMode={isDeveloper}
						data-flx="ui.action-menu.user-context-menu.render-staff-developer-controls-menu-group.staff-developer-user-controls-menu-item"
					/>
				</MenuGroup>
			);
		};
		const renderDmSelfMenu = (restricted: boolean) => {
			if (!channel) return renderDefaultMenu(restricted);
			return (
				<>
					<MenuGroup data-flx="ui.action-menu.user-context-menu.render-dm-self-menu.menu-group">
						<MarkDMAsReadMenuItem
							channel={channel}
							onClose={onClose}
							data-flx="ui.action-menu.user-context-menu.render-dm-self-menu.mark-dm-as-read-menu-item"
						/>
					</MenuGroup>
					<MenuGroup data-flx="ui.action-menu.user-context-menu.render-dm-self-menu.menu-group--2">
						<FavoriteChannelMenuItem
							channel={channel}
							onClose={onClose}
							data-flx="ui.action-menu.user-context-menu.render-dm-self-menu.favorite-channel-menu-item"
						/>
					</MenuGroup>
					<MenuGroup data-flx="ui.action-menu.user-context-menu.render-dm-self-menu.menu-group--3">
						<UserProfileMenuItem
							user={user}
							guildId={guildId}
							onClose={onClose}
							data-flx="ui.action-menu.user-context-menu.render-dm-self-menu.user-profile-menu-item"
						/>
						<MenuItem
							icon={<CloseDMIcon data-flx="ui.action-menu.user-context-menu.render-dm-self-menu.close-dm-icon" />}
							onClick={handleCloseDM}
							data-flx="ui.action-menu.user-context-menu.render-dm-self-menu.menu-item.close-dm"
						>
							{i18n._(CLOSE_DM_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
					{renderAdvancedMenuGroup()}
				</>
			);
		};
		const renderDmOtherMenu = (restricted: boolean) => {
			if (!channel) return renderDefaultMenu(restricted);
			return (
				<>
					<MenuGroup data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.menu-group">
						<MarkDMAsReadMenuItem
							channel={channel}
							onClose={onClose}
							data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.mark-dm-as-read-menu-item"
						/>
					</MenuGroup>
					<MenuGroup data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.menu-group--2">
						<FavoriteChannelMenuItem
							channel={channel}
							onClose={onClose}
							data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.favorite-channel-menu-item"
						/>
					</MenuGroup>
					<MenuGroup data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.menu-group--3">
						<UserProfileMenuItem
							user={user}
							guildId={guildId}
							onClose={onClose}
							data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.user-profile-menu-item"
						/>
						{showCallItems && channelId && (
							<RingUserMenuItem
								userId={user.id}
								channelId={channelId}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.ring-user-menu-item"
							/>
						)}
						{!restricted && showStartVoiceCall && (
							<StartVoiceCallMenuItem
								user={user}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.start-voice-call-menu-item"
							/>
						)}
						{!restricted && (
							<AddNoteMenuItem
								user={user}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.add-note-menu-item"
							/>
						)}
						{!restricted && (
							<ChangeFriendNicknameMenuItem
								user={user}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.change-friend-nickname-menu-item"
							/>
						)}
						<MenuItem
							icon={<CloseDMIcon data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.close-dm-icon" />}
							onClick={handleCloseDM}
							data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.menu-item.close-dm"
						>
							{i18n._(CLOSE_DM_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
					{showPrivateCallParticipantItems && (
						<MenuGroup data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.menu-group--4">
							<ParticipantVolumeSlider
								userId={user.id}
								data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.participant-volume-slider"
							/>
						</MenuGroup>
					)}
					{!restricted && (
						<MenuGroup data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.menu-group--5">
							{!isBot && (
								<InviteToCommunityMenuItem
									user={user}
									onClose={onClose}
									data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.invite-to-community-menu-item"
								/>
							)}
							{!isBot && (
								<RelationshipActionMenuItem
									user={user}
									onClose={onClose}
									data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.relationship-action-menu-item"
								/>
							)}
						</MenuGroup>
					)}
					{showPrivateCallParticipantItems && (
						<MenuGroup data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.menu-group--6">
							<LocalMuteParticipantMenuItem
								userId={user.id}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.local-mute-participant-menu-item"
							/>
							<EntranceSoundListenerSubmenu
								userId={user.id}
								data-flx="ui.action-menu.user-context-menu.render-dm-other-menu.entrance-sound-submenu"
							/>
						</MenuGroup>
					)}
					{renderReportActions(restricted)}
					{renderStaffDeveloperControlsMenuGroup(restricted)}
					{renderAdvancedMenuGroup()}
				</>
			);
		};
		const renderRestrictedMenu = () => (
			<>
				<MenuGroup data-flx="ui.action-menu.user-context-menu.render-restricted-menu.menu-group">
					<UserProfileMenuItem
						user={user}
						guildId={guildId}
						onClose={onClose}
						data-flx="ui.action-menu.user-context-menu.render-restricted-menu.user-profile-menu-item"
					/>
				</MenuGroup>
				{renderAdvancedMenuGroup()}
			</>
		);
		const renderDefaultMenu = (restricted: boolean) => {
			if (restricted) {
				return renderRestrictedMenu();
			}
			return (
				<>
					<MenuGroup data-flx="ui.action-menu.user-context-menu.render-default-menu.menu-group">
						<UserProfileMenuItem
							user={user}
							guildId={guildId}
							onClose={onClose}
							data-flx="ui.action-menu.user-context-menu.render-default-menu.user-profile-menu-item"
						/>
						{isGroupDM && isCurrentUser && (
							<MenuItem
								icon={
									<ChangeNicknameIcon data-flx="ui.action-menu.user-context-menu.render-default-menu.change-nickname-icon" />
								}
								onClick={handleChangeGroupNickname}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.menu-item.change-group-nickname"
							>
								{i18n._(CHANGE_GROUP_NICKNAME_DESCRIPTOR)}
							</MenuItem>
						)}
						{canMention && (
							<MentionUserMenuItem
								user={user}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.mention-user-menu-item"
							/>
						)}
						{!isCurrentUser && (
							<MessageUserMenuItem
								user={user}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.message-user-menu-item"
							/>
						)}
						{showCallItems && channelId && (
							<RingUserMenuItem
								userId={user.id}
								channelId={channelId}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.ring-user-menu-item"
							/>
						)}
						{showStartVoiceCall && !isCallContext && !restricted && (
							<StartVoiceCallMenuItem
								user={user}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.start-voice-call-menu-item"
							/>
						)}
						{!isCurrentUser && !restricted && (
							<AddNoteMenuItem
								user={user}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.add-note-menu-item"
							/>
						)}
						{!restricted && (
							<ChangeFriendNicknameMenuItem
								user={user}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.change-friend-nickname-menu-item"
							/>
						)}
					</MenuGroup>
					{showCallItems && !restricted && (
						<MenuGroup data-flx="ui.action-menu.user-context-menu.render-default-menu.menu-group--2">
							<ParticipantVolumeSlider
								userId={user.id}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.participant-volume-slider"
							/>
						</MenuGroup>
					)}
					{isGroupDM && isOwner && isRecipient && !isCurrentUser && (
						<MenuGroup data-flx="ui.action-menu.user-context-menu.render-default-menu.menu-group--3">
							<MenuItem
								icon={
									<RemoveFromGroupIcon data-flx="ui.action-menu.user-context-menu.render-default-menu.remove-from-group-icon" />
								}
								onClick={handleRemoveFromGroup}
								danger
								data-flx="ui.action-menu.user-context-menu.render-default-menu.menu-item.remove-from-group"
							>
								{i18n._(REMOVE_FROM_GROUP_DESCRIPTOR)}
							</MenuItem>
							{!isBot && (
								<MenuItem
									icon={
										<TransferOwnershipIcon data-flx="ui.action-menu.user-context-menu.render-default-menu.transfer-ownership-icon" />
									}
									onClick={handleMakeGroupOwner}
									danger
									data-flx="ui.action-menu.user-context-menu.render-default-menu.menu-item.make-group-owner"
								>
									{i18n._(MAKE_GROUP_OWNER_DESCRIPTOR)}
								</MenuItem>
							)}
							<MenuItem
								icon={
									<ChangeNicknameIcon data-flx="ui.action-menu.user-context-menu.render-default-menu.change-nickname-icon--2" />
								}
								onClick={handleChangeGroupNickname}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.menu-item.change-group-nickname--2"
							>
								{i18n._(CHANGE_GROUP_NICKNAME_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
					)}
					{showCallItems && !restricted && (
						<MenuGroup data-flx="ui.action-menu.user-context-menu.render-default-menu.menu-group--4">
							<LocalMuteParticipantMenuItem
								userId={user.id}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.local-mute-participant-menu-item"
							/>
							<EntranceSoundListenerSubmenu
								userId={user.id}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.entrance-sound-submenu"
							/>
						</MenuGroup>
					)}
					<MenuGroup data-flx="ui.action-menu.user-context-menu.render-default-menu.menu-group--5">
						{!restricted && !isCurrentUser && !isBot && (
							<InviteToCommunityMenuItem
								user={user}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.invite-to-community-menu-item"
							/>
						)}
						{!restricted && !isCurrentUser && !isBot && (
							<RelationshipActionMenuItem
								user={user}
								onClose={onClose}
								data-flx="ui.action-menu.user-context-menu.render-default-menu.relationship-action-menu-item"
							/>
						)}
					</MenuGroup>
					{renderReportActions(restricted)}
					{renderStaffDeveloperControlsMenuGroup(restricted)}
					{renderAdvancedMenuGroup()}
				</>
			);
		};
		if (channel?.isDM()) {
			return isCurrentUser ? renderDmSelfMenu(restrictUserActions) : renderDmOtherMenu(restrictUserActions);
		}
		return renderDefaultMenu(restrictUserActions);
	},
);
