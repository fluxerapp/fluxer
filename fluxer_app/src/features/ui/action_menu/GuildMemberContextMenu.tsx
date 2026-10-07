// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import Channels from '@app/features/channel/state/Channels';
import DeveloperMode from '@app/features/devtools/state/DeveloperMode';
import {TransferOwnershipModal} from '@app/features/guild/components/modals/TransferOwnershipModal';
import Guilds from '@app/features/guild/state/Guilds';
import {resolveGuildModerationCapabilities} from '@app/features/guild/utils/GuildModerationCapabilityUtils';
import {KICK_MEMBER_DESCRIPTOR, TRANSFER_OWNERSHIP_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {BanMemberModal} from '@app/features/moderation/components/modals/BanMemberModal';
import {KickMemberModal} from '@app/features/moderation/components/modals/KickMemberModal';
import {RemoveTimeoutModal} from '@app/features/moderation/components/modals/RemoveTimeoutModal';
import {TimeoutMemberModal} from '@app/features/moderation/components/modals/TimeoutMemberModal';
import {
	REMOVE_TIMEOUT_DESCRIPTOR,
	TIMEOUT_DESCRIPTOR,
} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import {useRoleHierarchy} from '@app/features/permissions/hooks/useRoleHierarchy';
import Permission from '@app/features/permissions/state/Permission';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import Relationships from '@app/features/relationship/state/Relationships';
import {getRemovableThread, RemoveFromThreadMenuItem} from '@app/features/threads/components/RemoveFromThreadMenuItem';
import {
	BanMemberIcon,
	KickMemberIcon,
	TimeoutIcon,
	TransferOwnershipIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {StartVoiceCallMenuItem} from '@app/features/ui/action_menu/items/CallMenuItems';
import {CopyUserIdMenuItem} from '@app/features/ui/action_menu/items/CopyMenuItems';
import {DebugGuildMemberMenuItem, DebugUserMenuItem} from '@app/features/ui/action_menu/items/DebugMenuItems';
import {ChangeNicknameMenuItem, ManageRolesMenuItem} from '@app/features/ui/action_menu/items/GuildMemberMenuItems';
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
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {User} from '@app/features/user/models/User';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const BAN_MEMBER_DESCRIPTOR = msg({
	message: 'Ban member',
	comment: 'Moderation action that bans the selected member from the community.',
});

interface GuildMemberContextMenuProps {
	user: User;
	onClose: () => void;
	guildId: string;
	channelId?: string;
	member?: GuildMember;
	message?: Message;
}

export const GuildMemberContextMenu: React.FC<GuildMemberContextMenuProps> = observer(
	({user, onClose, guildId, channelId, member: providedMember, message}) => {
		const {i18n} = useLingui();
		const channel = channelId ? Channels.getChannel(channelId) : null;
		const canSendMessages = channel ? Permission.can(Permissions.SEND_MESSAGES, {channelId, guildId}) : true;
		const canMention = channel !== null && canSendMessages;
		const isCurrentUser = user.id === Authentication.currentUserId;
		const relationship = Relationships.getRelationship(user.id);
		const relationshipType = relationship?.type;
		const guild = Guilds.getGuild(guildId);
		const member = providedMember ?? GuildMembers.getMember(guildId, user.id);
		const currentUserId = Authentication.currentUserId;
		const isBot = user.bot;
		const canKickMembers = Permission.can(Permissions.KICK_MEMBERS, {guildId});
		const canBanMembers = Permission.can(Permissions.BAN_MEMBERS, {guildId});
		const canModerateMembers = Permission.can(Permissions.MODERATE_MEMBERS, {guildId});
		const isOwner = guild?.ownerId === currentUserId;
		const {canManageTarget} = useRoleHierarchy(guild);
		const guildSnapshot = guild?.toJSON();
		const targetHasAdministratorPermission =
			guildSnapshot !== undefined && PermissionUtils.can(Permissions.ADMINISTRATOR, user.id, guildSnapshot);
		const {canKick, canBan, canTimeout} = resolveGuildModerationCapabilities({
			isCurrentUser,
			canManageTarget: canManageTarget(user.id),
			canKickMembers,
			canBanMembers,
			canModerateMembers,
			targetHasAdministratorPermission,
		});
		const canTransfer = Boolean(member) && !isCurrentUser && !isBot && isOwner;
		const developerMode = UserSettings.developerMode;
		const isDeveloper = DeveloperMode.isDeveloper;
		const currentUser = Users.currentUser;
		const restrictUserActions = user.system;
		const canShowStaffUserControls = shouldShowStaffUserControlsMenuItems({
			currentUser,
			user,
			isCurrentUser,
			restrictUserActions,
		});
		const canShowSpammerOverrideControls =
			!isCurrentUser && shouldShowSpammerOverrideMenuItems({user, developerMode: isDeveloper});
		const canShowStaffDeveloperControls = canShowStaffUserControls || canShowSpammerOverrideControls;
		const hasChangeNicknamePermission = Permission.can(Permissions.CHANGE_NICKNAME, {guildId});
		const hasManageNicknamesPermission = Permission.can(Permissions.MANAGE_NICKNAMES, {guildId});
		const canManageNicknames = Boolean(
			member &&
				((isCurrentUser && hasChangeNicknamePermission && !member.isTimedOut()) ||
					(hasManageNicknamesPermission && canManageTarget(user.id))),
		);
		const hasRoles = guild && Object.values(guild.roles).some((r) => !r.isEveryone);
		const canManageRoles = Permission.can(Permissions.MANAGE_ROLES, {guildId});
		const hasVisibleRoles = Boolean(member && hasRoles && (canManageRoles || member.roles.size > 0));
		const hasModerationActions = canTransfer || Boolean(((canTimeout || canKick) && member) || canBan);
		const isTimedOut = member?.isTimedOut() ?? false;
		const removableThread = getRemovableThread(guildId, channelId, user.id);
		const handleTimeout = useCallback(() => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<TimeoutMemberModal
						guildId={guildId}
						targetUser={user}
						data-flx="ui.action-menu.guild-member-context-menu.handle-timeout.timeout-member-modal"
					/>
				)),
			);
		}, [guildId, user, onClose]);
		const handleRemoveTimeout = useCallback(() => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<RemoveTimeoutModal
						guildId={guildId}
						targetUser={user}
						data-flx="ui.action-menu.guild-member-context-menu.handle-remove-timeout.remove-timeout-modal"
					/>
				)),
			);
		}, [guildId, user, onClose]);
		const handleKickMember = useCallback(() => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<KickMemberModal
						guildId={guildId}
						targetUser={user}
						data-flx="ui.action-menu.guild-member-context-menu.handle-kick-member.kick-member-modal"
					/>
				)),
			);
		}, [guildId, user, onClose]);
		const handleBanMember = useCallback(() => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<BanMemberModal
						guildId={guildId}
						targetUser={user}
						data-flx="ui.action-menu.guild-member-context-menu.handle-ban-member.ban-member-modal"
					/>
				)),
			);
		}, [guildId, user, onClose]);
		const handleTransferOwnership = useCallback(() => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<TransferOwnershipModal
						guildId={guildId}
						targetUser={user}
						targetMember={member!}
						data-flx="ui.action-menu.guild-member-context-menu.handle-transfer-ownership.transfer-ownership-modal"
					/>
				)),
			);
		}, [guildId, user, member, onClose]);
		return (
			<>
				<MenuGroup data-flx="ui.action-menu.guild-member-context-menu.menu-group">
					<UserProfileMenuItem
						user={user}
						guildId={guildId}
						onClose={onClose}
						data-flx="ui.action-menu.guild-member-context-menu.user-profile-menu-item"
					/>
					{canMention && (
						<MentionUserMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.guild-member-context-menu.mention-user-menu-item"
						/>
					)}
					{!isCurrentUser && (
						<MessageUserMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.guild-member-context-menu.message-user-menu-item"
						/>
					)}
					{!isCurrentUser && !isBot && (
						<StartVoiceCallMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.guild-member-context-menu.start-voice-call-menu-item"
						/>
					)}
					{!isCurrentUser && (
						<AddNoteMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.guild-member-context-menu.add-note-menu-item"
						/>
					)}
					<ChangeFriendNicknameMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.guild-member-context-menu.change-friend-nickname-menu-item"
					/>
				</MenuGroup>
				<MenuGroup data-flx="ui.action-menu.guild-member-context-menu.menu-group--2">
					{canManageNicknames && member && (
						<ChangeNicknameMenuItem
							guildId={guildId}
							user={user}
							member={member}
							onClose={onClose}
							data-flx="ui.action-menu.guild-member-context-menu.change-nickname-menu-item"
						/>
					)}
					{!isCurrentUser && !isBot && (
						<InviteToCommunityMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.guild-member-context-menu.invite-to-community-menu-item"
						/>
					)}
					{!isCurrentUser && !isBot && (
						<RelationshipActionMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.guild-member-context-menu.relationship-action-menu-item"
						/>
					)}
				</MenuGroup>
				{!isCurrentUser && (
					<MenuGroup data-flx="ui.action-menu.guild-member-context-menu.menu-group--3">
						{message && (
							<ReportMessageMenuItem
								message={message}
								onClose={onClose}
								data-flx="ui.action-menu.guild-member-context-menu.report-message-menu-item"
							/>
						)}
						<ReportUserMenuItem
							user={user}
							guildId={guildId}
							message={message}
							onClose={onClose}
							data-flx="ui.action-menu.guild-member-context-menu.report-user-menu-item"
						/>
						{relationshipType === RelationshipTypes.BLOCKED ? (
							<UnblockUserMenuItem
								user={user}
								onClose={onClose}
								data-flx="ui.action-menu.guild-member-context-menu.unblock-user-menu-item"
							/>
						) : (
							<BlockUserMenuItem
								user={user}
								onClose={onClose}
								data-flx="ui.action-menu.guild-member-context-menu.block-user-menu-item"
							/>
						)}
					</MenuGroup>
				)}
				{canShowStaffDeveloperControls && (
					<MenuGroup data-flx="ui.action-menu.guild-member-context-menu.menu-group--4">
						<StaffDeveloperUserControlsMenuItem
							user={user}
							showStaffControls={canShowStaffUserControls}
							showSpammerOverrideControls={canShowSpammerOverrideControls}
							developerMode={isDeveloper}
							data-flx="ui.action-menu.guild-member-context-menu.staff-developer-user-controls-menu-item"
						/>
					</MenuGroup>
				)}
				{removableThread && (
					<MenuGroup data-flx="ui.action-menu.guild-member-context-menu.menu-group--thread">
						<RemoveFromThreadMenuItem
							thread={removableThread}
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.guild-member-context-menu.remove-from-thread-menu-item"
						/>
					</MenuGroup>
				)}
				{hasModerationActions && (
					<MenuGroup data-flx="ui.action-menu.guild-member-context-menu.menu-group--5">
						{canTransfer && member && (
							<MenuItem
								icon={
									<TransferOwnershipIcon
										size={16}
										data-flx="ui.action-menu.guild-member-context-menu.transfer-ownership-icon"
									/>
								}
								onClick={handleTransferOwnership}
								danger
								data-flx="ui.action-menu.guild-member-context-menu.menu-item.transfer-ownership"
							>
								{i18n._(TRANSFER_OWNERSHIP_DESCRIPTOR)}
							</MenuItem>
						)}
						{canTimeout && member && (
							<MenuItem
								icon={<TimeoutIcon size={16} data-flx="ui.action-menu.guild-member-context-menu.timeout-icon" />}
								onClick={isTimedOut ? handleRemoveTimeout : handleTimeout}
								danger={!isTimedOut}
								data-flx="ui.action-menu.guild-member-context-menu.menu-item.remove-timeout"
							>
								{isTimedOut ? i18n._(REMOVE_TIMEOUT_DESCRIPTOR) : i18n._(TIMEOUT_DESCRIPTOR)}
							</MenuItem>
						)}
						{canKick && (
							<MenuItem
								icon={<KickMemberIcon size={16} data-flx="ui.action-menu.guild-member-context-menu.kick-member-icon" />}
								onClick={handleKickMember}
								danger
								data-flx="ui.action-menu.guild-member-context-menu.menu-item.kick-member"
							>
								{i18n._(KICK_MEMBER_DESCRIPTOR)}
							</MenuItem>
						)}
						{canBan && (
							<MenuItem
								icon={<BanMemberIcon size={16} data-flx="ui.action-menu.guild-member-context-menu.ban-member-icon" />}
								onClick={handleBanMember}
								danger
								data-flx="ui.action-menu.guild-member-context-menu.menu-item.ban-member"
							>
								{i18n._(BAN_MEMBER_DESCRIPTOR)}
							</MenuItem>
						)}
					</MenuGroup>
				)}
				{hasVisibleRoles && member && (
					<MenuGroup data-flx="ui.action-menu.guild-member-context-menu.menu-group--6">
						<ManageRolesMenuItem
							guildId={guildId}
							member={member}
							data-flx="ui.action-menu.guild-member-context-menu.manage-roles-menu-item"
						/>
					</MenuGroup>
				)}
				<MenuGroup data-flx="ui.action-menu.guild-member-context-menu.menu-group--7">
					{developerMode && (
						<DebugUserMenuItem
							user={user}
							onClose={onClose}
							data-flx="ui.action-menu.guild-member-context-menu.debug-user-menu-item"
						/>
					)}
					{developerMode && member && (
						<DebugGuildMemberMenuItem
							member={member}
							onClose={onClose}
							data-flx="ui.action-menu.guild-member-context-menu.debug-guild-member-menu-item"
						/>
					)}
					<CopyUserIdMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.guild-member-context-menu.copy-user-id-menu-item"
					/>
				</MenuGroup>
			</>
		);
	},
);
