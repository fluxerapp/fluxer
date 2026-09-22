// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {canReportMessage} from '@app/features/channel/components/MessageActionUtils';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildMemberActionsSheet.module.css';
import {TransferOwnershipModal} from '@app/features/guild/components/modals/TransferOwnershipModal';
import {ManageRolesBottomSheet} from '@app/features/guild/components/RoleManagement';
import Guilds from '@app/features/guild/state/Guilds';
import {
	resolveGuildModerationCapabilities,
	resolveGuildScopedModerationActionKeys,
} from '@app/features/guild/utils/GuildModerationCapabilityUtils';
import {
	BLOCKED_USER_DM_WARNING_DESCRIPTOR,
	CHANGE_NICKNAME_DESCRIPTOR,
	COPY_USER_ID_DESCRIPTOR,
	OPEN_DM_DESCRIPTOR,
	REPORT_MESSAGE_DESCRIPTOR,
	TRANSFER_OWNERSHIP_DESCRIPTOR,
	VIEW_PROFILE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {GuildMember} from '@app/features/member/models/GuildMember';
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
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as RelationshipCommands from '@app/features/relationship/commands/RelationshipCommands';
import Relationships from '@app/features/relationship/state/Relationships';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {
	ACCEPT_FRIEND_REQUEST_DESCRIPTOR,
	REMOVE_FRIEND_DESCRIPTOR,
	UNBLOCK_USER_ACTION_DESCRIPTOR,
} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import {MenuBottomSheet, type MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {ChangeNicknameModal} from '@app/features/user/components/modals/ChangeNicknameModal';
import type {User} from '@app/features/user/models/User';
import UserProfileMobile from '@app/features/user/state/UserProfileMobile';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import * as CallUtils from '@app/features/voice/utils/CallUtils';
import {hasActiveDirectCallWithUser} from '@app/features/voice/utils/PrivateCallMenuUtils';
import {VOICE_CALL_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	ChatTeardropIcon,
	ClockIcon,
	CrownIcon,
	FlagIcon,
	GavelIcon,
	IdentificationCardIcon,
	PencilIcon,
	ProhibitIcon,
	SignOutIcon,
	UserCircleIcon,
	UserGearIcon,
	UserMinusIcon,
	UserPlusIcon,
	VideoCameraIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type {FC} from 'react';
import {useState} from 'react';

const MESSAGE_DESCRIPTOR = msg({
	message: 'Message',
	context: 'message-action',
	comment:
		'Member actions sheet menu item. Sends a direct message to the target user. Standalone verb used as a menu label.',
});
const SEND_FRIEND_REQUEST_DESCRIPTOR = msg({
	message: 'Send friend request',
	comment: 'Member actions sheet menu item. Sends a friend request to the target user.',
});
const KICK_DESCRIPTOR = msg({
	message: 'Kick',
	comment:
		'Member actions sheet menu item. Opens the kick-member confirmation (moderation action). Short verb used as a menu label.',
});
const logger = new Logger('GuildMemberActionsSheet');

interface GuildMemberActionsSheetProps {
	isOpen: boolean;
	onClose: () => void;
	user: User;
	member: GuildMember;
	guildId: string;
	message?: Message;
}

export const GuildMemberActionsSheet: FC<GuildMemberActionsSheetProps> = observer(
	({isOpen, onClose, user, member, guildId, message}) => {
		const {i18n} = useLingui();
		const currentUserId = Authentication.currentUserId;
		const isCurrentUser = user.id === currentUserId;
		const isBot = user.bot;
		const currentUserUnclaimed = !(Users.currentUser?.isClaimed() ?? true);
		const relationship = Relationships.getRelationship(user.id);
		const relationshipType = relationship?.type;
		const isBlocked = relationshipType === RelationshipTypes.BLOCKED;
		const canKickMembers = Permission.can(Permissions.KICK_MEMBERS, {guildId});
		const canBanMembers = Permission.can(Permissions.BAN_MEMBERS, {guildId});
		const canModerateMembers = Permission.can(Permissions.MODERATE_MEMBERS, {guildId});
		const guild = Guilds.getGuild(guildId);
		const guildSnapshot = guild?.toJSON();
		const {canManageTarget} = useRoleHierarchy(guild);
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
		const memberIsTimedOut = member.isTimedOut();
		const isOwner = guild?.ownerId === currentUserId;
		const canTransfer = !isCurrentUser && !isBot && isOwner;
		const hasRoles = guild && Object.values(guild.roles).some((r) => !r.isEveryone);
		const manageRolesLabel = PermissionUtils.formatPermissionLabel(i18n, Permissions.MANAGE_ROLES);
		const [manageRolesSheetOpen, setManageRolesSheetOpen] = useState(false);
		const hasChangeNicknamePermission = Permission.can(Permissions.CHANGE_NICKNAME, {guildId});
		const hasManageNicknamesPermission = Permission.can(Permissions.MANAGE_NICKNAMES, {guildId});
		const canManageNicknames =
			(isCurrentUser && hasChangeNicknamePermission && !memberIsTimedOut) ||
			(!isCurrentUser && hasManageNicknamesPermission);
		const displayName = NicknameUtils.getNickname(user, guildId);
		const handleViewProfile = () => {
			ModalCommands.runAfterBottomSheetClose(onClose, () => UserProfileMobile.open(user.id, guildId));
		};
		const openDmChannel = async () => {
			try {
				await PrivateChannelCommands.openDMChannel(user.id);
			} catch (error) {
				logger.error('Failed to open DM channel', error);
			}
		};
		const handleMessage = async () => {
			onClose();
			await openDmChannel();
		};
		const handleOpenBlockedDm = () => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<ConfirmModal
						title={i18n._(OPEN_DM_DESCRIPTOR)}
						description={i18n._(BLOCKED_USER_DM_WARNING_DESCRIPTOR, {userName: displayName})}
						primaryText={i18n._(OPEN_DM_DESCRIPTOR)}
						primaryVariant="primary"
						onPrimary={openDmChannel}
						data-flx="guild.guild-tabs.guild-member-actions-sheet.handle-open-blocked-dm.confirm-modal"
					/>
				)),
			);
		};
		const handleStartVoiceCall = async () => {
			ModalCommands.runAfterBottomSheetClose(onClose, () => {
				void (async () => {
					try {
						const channelId = await PrivateChannelCommands.ensureDMChannel(user.id);
						await CallUtils.requestStartCall(i18n, channelId, {kind: 'voice'});
					} catch (error) {
						logger.error('Failed to start voice call', error);
					}
				})();
			});
		};
		const handleChangeNickname = () => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<ChangeNicknameModal
						guildId={guildId}
						user={user}
						member={member}
						data-flx="guild.guild-tabs.guild-member-actions-sheet.handle-change-nickname.change-nickname-modal"
					/>
				)),
			);
		};
		const handleSendFriendRequest = () => {
			RelationshipCommands.sendFriendRequest(user.id);
			onClose();
		};
		const handleAcceptFriendRequest = () => {
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showAcceptFriendRequestConfirmation(i18n, user),
			);
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
		const handleKickMember = () => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<KickMemberModal
						guildId={guildId}
						targetUser={user}
						data-flx="guild.guild-tabs.guild-member-actions-sheet.handle-kick-member.kick-member-modal"
					/>
				)),
			);
		};
		const handleBanMember = () => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<BanMemberModal
						guildId={guildId}
						targetUser={user}
						data-flx="guild.guild-tabs.guild-member-actions-sheet.handle-ban-member.ban-member-modal"
					/>
				)),
			);
		};
		const handleCopyUserId = () => {
			TextCopyCommands.copy(i18n, user.id, true);
			onClose();
		};
		const handleManageRoles = () => {
			setManageRolesSheetOpen(true);
		};
		const handleTransferOwnership = () => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<TransferOwnershipModal
						guildId={guildId}
						targetUser={user}
						targetMember={member}
						data-flx="guild.guild-tabs.guild-member-actions-sheet.handle-transfer-ownership.transfer-ownership-modal"
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
		const profileItems = [
			{
				icon: <UserCircleIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon" />,
				label: i18n._(VIEW_PROFILE_DESCRIPTOR),
				onClick: handleViewProfile,
			},
		];
		if (!isCurrentUser && !RuntimeConfig.directMessagesDisabled) {
			profileItems.push({
				icon: (
					<ChatTeardropIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--2" />
				),
				label: isBlocked ? i18n._(OPEN_DM_DESCRIPTOR) : i18n._(MESSAGE_DESCRIPTOR),
				onClick: isBlocked ? handleOpenBlockedDm : handleMessage,
			});
			if (!isBot && relationshipType === RelationshipTypes.FRIEND && !hasActiveDirectCallWithUser(user.id)) {
				profileItems.push({
					icon: (
						<VideoCameraIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--3" />
					),
					label: i18n._(VOICE_CALL_DESCRIPTOR),
					onClick: handleStartVoiceCall,
				});
			}
		}
		menuGroups.push({items: profileItems});
		const guildItems = [];
		if (canManageNicknames) {
			guildItems.push({
				icon: <PencilIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--4" />,
				label: i18n._(CHANGE_NICKNAME_DESCRIPTOR),
				onClick: handleChangeNickname,
			});
		}
		if (guildItems.length > 0) {
			menuGroups.push({items: guildItems});
		}
		if (!isCurrentUser && !isBot && !RuntimeConfig.directMessagesDisabled) {
			const relationshipItems = [];
			if (relationshipType === RelationshipTypes.FRIEND) {
				relationshipItems.push({
					icon: (
						<UserMinusIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--5" />
					),
					label: i18n._(REMOVE_FRIEND_DESCRIPTOR),
					onClick: handleRemoveFriend,
					danger: true,
				});
			} else if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
				relationshipItems.push({
					icon: <UserPlusIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--6" />,
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
					icon: <UserPlusIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--7" />,
					label: i18n._(SEND_FRIEND_REQUEST_DESCRIPTOR),
					onClick: handleSendFriendRequest,
				});
			}
			if (relationshipItems.length > 0) {
				menuGroups.push({items: relationshipItems});
			}
		}
		if (hasRoles) {
			menuGroups.push({
				items: [
					{
						icon: (
							<UserGearIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--8" />
						),
						label: manageRolesLabel,
						onClick: handleManageRoles,
					},
				],
			});
		}
		if (canTransfer) {
			menuGroups.push({
				items: [
					{
						icon: <CrownIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--9" />,
						label: i18n._(TRANSFER_OWNERSHIP_DESCRIPTOR),
						onClick: handleTransferOwnership,
					},
				],
			});
		}
		const moderationActionKeys = resolveGuildScopedModerationActionKeys({
			hasMember: true,
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
							icon: (
								<ClockIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--10" />
							),
							label: i18n._(TIMEOUT_DESCRIPTOR),
							onClick: () => {
								ModalCommands.pushAfterBottomSheetClose(
									onClose,
									modal(() => (
										<TimeoutMemberSheet
											isOpen={true}
											onClose={() => ModalCommands.pop()}
											guildId={guildId}
											targetUser={user}
											targetMember={member}
											data-flx="guild.guild-tabs.guild-member-actions-sheet.on-click.timeout-member-sheet"
										/>
									)),
								);
							},
							danger: true,
						});
						break;
					case 'remove_timeout':
						moderationItems.push({
							icon: (
								<ClockIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--11" />
							),
							label: i18n._(REMOVE_TIMEOUT_DESCRIPTOR),
							onClick: () => {
								ModalCommands.pushAfterBottomSheetClose(
									onClose,
									modal(() => (
										<RemoveTimeoutModal
											guildId={guildId}
											targetUser={user}
											data-flx="guild.guild-tabs.guild-member-actions-sheet.on-click.remove-timeout-modal"
										/>
									)),
								);
							},
							danger: false,
						});
						break;
					case 'kick':
						moderationItems.push({
							icon: (
								<SignOutIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--12" />
							),
							label: i18n._(KICK_DESCRIPTOR),
							onClick: handleKickMember,
							danger: true,
						});
						break;
					case 'ban':
						moderationItems.push({
							icon: (
								<GavelIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--13" />
							),
							label: i18n._(BAN_ACTION_DESCRIPTOR),
							onClick: handleBanMember,
							danger: true,
						});
						break;
				}
			}
			menuGroups.push({items: moderationItems});
		}
		if (!isCurrentUser) {
			const reportBlockItems = [
				...(message && canReportMessage(message)
					? [
							{
								icon: (
									<FlagIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--14" />
								),
								label: i18n._(REPORT_MESSAGE_DESCRIPTOR),
								onClick: handleReportMessage,
								danger: true,
							},
						]
					: []),
				{
					icon: <FlagIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--15" />,
					label: i18n._(REPORT_USER_DESCRIPTOR),
					onClick: handleReportUser,
					danger: true,
				},
			];
			if (relationshipType === RelationshipTypes.BLOCKED) {
				reportBlockItems.push({
					icon: (
						<ProhibitIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--16" />
					),
					label: i18n._(UNBLOCK_USER_ACTION_DESCRIPTOR),
					onClick: handleUnblockUser,
					danger: false,
				});
			} else {
				reportBlockItems.push({
					icon: (
						<ProhibitIcon className={styles.icon} data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--17" />
					),
					label: i18n._(BLOCK_DESCRIPTOR),
					onClick: handleBlockUser,
					danger: true,
				});
			}
			menuGroups.push({items: reportBlockItems});
		}
		menuGroups.push({
			items: [
				{
					icon: (
						<IdentificationCardIcon
							className={styles.icon}
							data-flx="guild.guild-tabs.guild-member-actions-sheet.icon--18"
						/>
					),
					label: i18n._(COPY_USER_ID_DESCRIPTOR),
					onClick: handleCopyUserId,
				},
			],
		});
		const headerContent = (
			<div className={styles.header} data-flx="guild.guild-tabs.guild-member-actions-sheet.header">
				<StatusAwareAvatar
					user={user}
					size={48}
					guildId={guildId}
					data-flx="guild.guild-tabs.guild-member-actions-sheet.status-aware-avatar"
				/>
				<div className={styles.headerInfo} data-flx="guild.guild-tabs.guild-member-actions-sheet.header-info">
					<span className={styles.headerName} data-flx="guild.guild-tabs.guild-member-actions-sheet.header-name">
						{NicknameUtils.getNickname(user, guildId)}
					</span>
					<span className={styles.headerTag} data-flx="guild.guild-tabs.guild-member-actions-sheet.header-tag">
						{NicknameUtils.formatUserTagForStreamerMode(user)}
					</span>
				</div>
			</div>
		);
		return (
			<>
				<MenuBottomSheet
					isOpen={isOpen}
					onClose={onClose}
					groups={menuGroups}
					headerContent={headerContent}
					data-flx="guild.guild-tabs.guild-member-actions-sheet.menu-bottom-sheet"
				/>
				<ManageRolesBottomSheet
					isOpen={manageRolesSheetOpen}
					onClose={() => setManageRolesSheetOpen(false)}
					guildId={guildId}
					userId={user.id}
					data-flx="guild.guild-tabs.guild-member-actions-sheet.manage-roles-bottom-sheet"
				/>
			</>
		);
	},
);
