// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import {canReportMessage} from '@app/features/channel/components/MessageActionUtils';
import {TransferOwnershipModal} from '@app/features/guild/components/modals/TransferOwnershipModal';
import Guilds from '@app/features/guild/state/Guilds';
import {
	resolveGuildModerationCapabilities,
	resolveGuildScopedModerationActionKeys,
} from '@app/features/guild/utils/GuildModerationCapabilityUtils';
import {
	CHANGE_NICKNAME_DESCRIPTOR,
	COPY_USER_ID_DESCRIPTOR,
	COPY_USERNAME_DESCRIPTOR,
	REPORT_MESSAGE_DESCRIPTOR,
	TRANSFER_OWNERSHIP_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {BanMemberModal} from '@app/features/moderation/components/modals/BanMemberModal';
import {KickMemberModal} from '@app/features/moderation/components/modals/KickMemberModal';
import {RemoveTimeoutModal} from '@app/features/moderation/components/modals/RemoveTimeoutModal';
import {TimeoutMemberSheet} from '@app/features/moderation/components/modals/TimeoutMemberSheet';
import {
	BAN_ACTION_DESCRIPTOR,
	BLOCK_DESCRIPTOR,
	REMOVE_TIMEOUT_DESCRIPTOR,
	REPORT_USER_DESCRIPTOR,
	TIMEOUT_DESCRIPTOR,
} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import {openReportMessageModal, openReportUserModal} from '@app/features/moderation/utils/ReportActionUtils';
import {useRoleHierarchy} from '@app/features/permissions/hooks/useRoleHierarchy';
import Permission from '@app/features/permissions/state/Permission';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import Relationships from '@app/features/relationship/state/Relationships';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {
	REMOVE_FRIEND_DESCRIPTOR,
	UNBLOCK_USER_ACTION_DESCRIPTOR,
} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {MenuBottomSheet, type MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {ChangeNicknameModal} from '@app/features/user/components/modals/ChangeNicknameModal';
import styles from '@app/features/user/components/modals/UserProfileActionsSheet.module.css';
import type {User} from '@app/features/user/models/User';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	ClockIcon,
	CopyIcon,
	CrownIcon,
	FlagIcon,
	GavelIcon,
	GlobeIcon,
	IdentificationCardIcon,
	PencilIcon,
	ProhibitIcon,
	SignOutIcon,
	UserMinusIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const VIEW_COMMUNITY_PROFILE_DESCRIPTOR = msg({
	message: 'View community profile',
	comment: 'Button or menu action label in the user profile actions sheet. Keep it concise.',
});
const VIEW_GLOBAL_PROFILE_DESCRIPTOR = msg({
	message: 'View global profile',
	comment: 'Button or menu action label in the user profile actions sheet. Keep it concise.',
});
const KICK_DESCRIPTOR = msg({
	message: 'Kick',
	comment:
		'Button or menu action label in the user profile actions sheet. Keep it concise. Keep the tone plain and specific.',
});

interface UserProfileActionsSheetProps {
	isOpen: boolean;
	onClose: () => void;
	user: User;
	isCurrentUser?: boolean;
	hasGuildProfile?: boolean;
	showGlobalProfile?: boolean;
	onToggleProfileView?: () => void;
	guildId?: string;
	guildMember?: GuildMember | null;
	message?: Message;
}

export const UserProfileActionsSheet: React.FC<UserProfileActionsSheetProps> = observer(
	({
		isOpen,
		onClose,
		user,
		isCurrentUser = false,
		hasGuildProfile = false,
		showGlobalProfile = false,
		onToggleProfileView,
		guildId,
		guildMember,
		message,
	}) => {
		const {i18n} = useLingui();
		const relationshipType = Relationships.getRelationship(user.id)?.type;
		const guild = guildId ? Guilds.getGuild(guildId) : null;
		const member = guildMember ?? (guildId ? GuildMembers.getMember(guildId, user.id) : null);
		const currentUserId = Authentication.currentUserId;
		const canKickMembers = guildId ? Permission.can(Permissions.KICK_MEMBERS, {guildId}) : false;
		const canBanMembers = guildId ? Permission.can(Permissions.BAN_MEMBERS, {guildId}) : false;
		const canModerateMembers = guildId ? Permission.can(Permissions.MODERATE_MEMBERS, {guildId}) : false;
		const hasChangeNicknamePermission = guildId ? Permission.can(Permissions.CHANGE_NICKNAME, {guildId}) : false;
		const hasManageNicknamesPermission = guildId ? Permission.can(Permissions.MANAGE_NICKNAMES, {guildId}) : false;
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
		const canTransfer = !isCurrentUser && !user.bot && isOwner && member;
		const memberIsTimedOut = member?.isTimedOut() ?? false;
		const canManageNicknames =
			member &&
			((isCurrentUser && hasChangeNicknamePermission && !memberIsTimedOut) ||
				(hasManageNicknamesPermission && canManageTarget(user.id)));
		const handleCopyFluxerTag = () => {
			TextCopyCommands.copy(i18n, user.tag, true);
			onClose();
		};
		const handleCopyUserId = () => {
			TextCopyCommands.copy(i18n, user.id, true);
			onClose();
		};
		const handleRemoveFriend = () => {
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showRemoveFriendConfirmation(i18n, user),
			);
		};
		const handleBlockUser = () => {
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showBlockUserConfirmation(i18n, user),
			);
		};
		const handleUnblockUser = () => {
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showUnblockUserConfirmation(i18n, user),
			);
		};
		const handleChangeNickname = () => {
			if (!guildId || !member) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<ChangeNicknameModal
						guildId={guildId}
						user={user}
						member={member}
						data-flx="user.user-profile-actions-sheet.handle-change-nickname.change-nickname-modal"
					/>
				)),
			);
		};
		const handleKickMember = () => {
			if (!guildId) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<KickMemberModal
						guildId={guildId}
						targetUser={user}
						data-flx="user.user-profile-actions-sheet.handle-kick-member.kick-member-modal"
					/>
				)),
			);
		};
		const handleBanMember = () => {
			if (!guildId) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<BanMemberModal
						guildId={guildId}
						targetUser={user}
						data-flx="user.user-profile-actions-sheet.handle-ban-member.ban-member-modal"
					/>
				)),
			);
		};
		const handleTimeoutMember = () => {
			if (!guildId || !member) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<TimeoutMemberSheet
						isOpen={true}
						onClose={() => ModalCommands.pop()}
						guildId={guildId}
						targetUser={user}
						targetMember={member}
						data-flx="user.user-profile-actions-sheet.handle-timeout-member.timeout-member-sheet"
					/>
				)),
			);
		};
		const handleRemoveTimeout = () => {
			if (!guildId) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<RemoveTimeoutModal
						guildId={guildId}
						targetUser={user}
						data-flx="user.user-profile-actions-sheet.handle-remove-timeout.remove-timeout-modal"
					/>
				)),
			);
		};
		const handleTransferOwnership = () => {
			if (!guildId || !member) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<TransferOwnershipModal
						guildId={guildId}
						targetUser={user}
						targetMember={member}
						data-flx="user.user-profile-actions-sheet.handle-transfer-ownership.transfer-ownership-modal"
					/>
				)),
			);
		};
		const handleReportUser = () => {
			ModalCommands.runAfterBottomSheetClose(onClose, () => openReportUserModal({i18n, user, guildId, message}));
		};
		const handleReportMessage = () => {
			if (!message || !canReportMessage(message)) {
				return;
			}
			ModalCommands.runAfterBottomSheetClose(onClose, () => openReportMessageModal(message));
		};
		const menuGroups: Array<MenuGroupType> = [];
		if (hasGuildProfile && onToggleProfileView) {
			menuGroups.push({
				items: [
					{
						icon: <GlobeIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon" />,
						label: showGlobalProfile
							? i18n._(VIEW_COMMUNITY_PROFILE_DESCRIPTOR)
							: i18n._(VIEW_GLOBAL_PROFILE_DESCRIPTOR),
						onClick: () => {
							onToggleProfileView();
							onClose();
						},
					},
				],
			});
		}
		menuGroups.push({
			items: [
				{
					icon: <CopyIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--2" />,
					label: i18n._(COPY_USERNAME_DESCRIPTOR),
					onClick: handleCopyFluxerTag,
				},
				{
					icon: <IdentificationCardIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--3" />,
					label: i18n._(COPY_USER_ID_DESCRIPTOR),
					onClick: handleCopyUserId,
				},
			],
		});
		if (guildId) {
			const guildItems = [];
			if (member && canManageNicknames) {
				guildItems.push({
					icon: <PencilIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--4" />,
					label: isCurrentUser ? i18n._(CHANGE_NICKNAME_DESCRIPTOR) : i18n._(CHANGE_NICKNAME_DESCRIPTOR),
					onClick: handleChangeNickname,
				});
			}
			if (guildItems.length > 0) {
				menuGroups.push({items: guildItems});
			}
			if (member && canTransfer) {
				menuGroups.push({
					items: [
						{
							icon: <CrownIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--5" />,
							label: i18n._(TRANSFER_OWNERSHIP_DESCRIPTOR),
							onClick: handleTransferOwnership,
							danger: true,
						},
					],
				});
			}
			const moderationActionKeys = resolveGuildScopedModerationActionKeys({
				hasMember: member != null,
				canTimeout,
				isTimedOut: memberIsTimedOut,
				canKick,
				canBan,
			});
			if (moderationActionKeys.length > 0) {
				const moderationItems = [];
				for (const actionKey of moderationActionKeys) {
					switch (actionKey) {
						case 'timeout':
							moderationItems.push({
								icon: <ClockIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--6" />,
								label: i18n._(TIMEOUT_DESCRIPTOR),
								onClick: handleTimeoutMember,
								danger: true,
							});
							break;
						case 'remove_timeout':
							moderationItems.push({
								icon: <ClockIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--7" />,
								label: i18n._(REMOVE_TIMEOUT_DESCRIPTOR),
								onClick: handleRemoveTimeout,
								danger: true,
							});
							break;
						case 'kick':
							moderationItems.push({
								icon: <SignOutIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--8" />,
								label: i18n._(KICK_DESCRIPTOR),
								onClick: handleKickMember,
								danger: true,
							});
							break;
						case 'ban':
							moderationItems.push({
								icon: <GavelIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--9" />,
								label: i18n._(BAN_ACTION_DESCRIPTOR),
								onClick: handleBanMember,
								danger: true,
							});
							break;
					}
				}
				menuGroups.push({items: moderationItems});
			}
		}
		if (!isCurrentUser) {
			if (relationshipType === RelationshipTypes.FRIEND && !RuntimeConfig.directMessagesDisabled) {
				menuGroups.push({
					items: [
						{
							icon: <UserMinusIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--10" />,
							label: i18n._(REMOVE_FRIEND_DESCRIPTOR),
							onClick: handleRemoveFriend,
							danger: true,
						},
					],
				});
			}
			const reportBlockItems = [
				...(message && canReportMessage(message)
					? [
							{
								icon: <FlagIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--11" />,
								label: i18n._(REPORT_MESSAGE_DESCRIPTOR),
								onClick: handleReportMessage,
								danger: true,
							},
						]
					: []),
				{
					icon: <FlagIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--12" />,
					label: i18n._(REPORT_USER_DESCRIPTOR),
					onClick: handleReportUser,
					danger: true,
				},
			];
			if (!user.system) {
				if (relationshipType !== RelationshipTypes.BLOCKED) {
					reportBlockItems.push({
						icon: <ProhibitIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--13" />,
						label: i18n._(BLOCK_DESCRIPTOR),
						onClick: handleBlockUser,
						danger: true,
					});
				} else {
					reportBlockItems.push({
						icon: <ProhibitIcon className={styles.icon} data-flx="user.user-profile-actions-sheet.icon--14" />,
						label: i18n._(UNBLOCK_USER_ACTION_DESCRIPTOR),
						onClick: handleUnblockUser,
						danger: false,
					});
				}
			}
			menuGroups.push({items: reportBlockItems});
		}
		return (
			<MenuBottomSheet
				isOpen={isOpen}
				onClose={onClose}
				groups={menuGroups}
				data-flx="user.user-profile-actions-sheet.menu-bottom-sheet"
			/>
		);
	},
);
