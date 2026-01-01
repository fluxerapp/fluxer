/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import {
	ChatTeardropIcon,
	ClockIcon,
	GavelIcon,
	IdentificationCardIcon,
	PencilIcon,
	PhoneIcon,
	ProhibitIcon,
	SignOutIcon,
	UserCircleIcon,
	UserMinusIcon,
	UserPlusIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type {FC} from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as RelationshipActionCreators from '~/actions/RelationshipActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import {Permissions, RelationshipTypes} from '~/Constants';
import {BanMemberModal} from '~/components/modals/BanMemberModal';
import {ChangeNicknameModal} from '~/components/modals/ChangeNicknameModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {KickMemberModal} from '~/components/modals/KickMemberModal';
import {RemoveTimeoutModal} from '~/components/modals/RemoveTimeoutModal';
import {TimeoutMemberSheet} from '~/components/modals/TimeoutMemberSheet';
import {MenuBottomSheet, type MenuGroupType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import GuildStore from '~/stores/GuildStore';
import PermissionStore from '~/stores/PermissionStore';
import RelationshipStore from '~/stores/RelationshipStore';
import UserProfileMobileStore from '~/stores/UserProfileMobileStore';
import * as CallUtils from '~/utils/CallUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';
import * as PermissionUtils from '~/utils/PermissionUtils';
import styles from './GuildMemberActionsSheet.module.css';

interface GuildMemberActionsSheetProps {
	isOpen: boolean;
	onClose: () => void;
	user: UserRecord;
	member: GuildMemberRecord;
	guildId: string;
}

export const GuildMemberActionsSheet: FC<GuildMemberActionsSheetProps> = observer(
	({isOpen, onClose, user, member, guildId}) => {
		const {t, i18n} = useLingui();

		const currentUserId = AuthenticationStore.currentUserId;
		const isCurrentUser = user.id === currentUserId;
		const isBot = user.bot;

		const relationship = RelationshipStore.getRelationship(user.id);
		const relationshipType = relationship?.type;

		const canKickMembers = PermissionStore.can(Permissions.KICK_MEMBERS, {guildId});
		const canBanMembers = PermissionStore.can(Permissions.BAN_MEMBERS, {guildId});
		const canModerateMembers = PermissionStore.can(Permissions.MODERATE_MEMBERS, {guildId});

		const guild = GuildStore.getGuild(guildId);
		const guildSnapshot = guild?.toJSON();
		const targetHasModerateMembersPermission =
			guildSnapshot !== undefined && PermissionUtils.can(Permissions.MODERATE_MEMBERS, user.id, guildSnapshot);

		const canKick = !isCurrentUser && canKickMembers;
		const canBan = !isCurrentUser && canBanMembers;
		const canTimeout = !isCurrentUser && canModerateMembers && !targetHasModerateMembersPermission;
		const memberIsTimedOut = member.isTimedOut();

		const hasChangeNicknamePermission = PermissionStore.can(Permissions.CHANGE_NICKNAME, {guildId});
		const hasManageNicknamesPermission = PermissionStore.can(Permissions.MANAGE_NICKNAMES, {guildId});
		const canManageNicknames =
			(isCurrentUser && hasChangeNicknamePermission) || (!isCurrentUser && hasManageNicknamesPermission);

		const handleViewProfile = () => {
			onClose();
			UserProfileMobileStore.open(user.id, guildId);
		};

		const handleMessage = async () => {
			onClose();
			try {
				await PrivateChannelActionCreators.openDMChannel(user.id);
			} catch (error) {
				console.error('Failed to open DM channel:', error);
			}
		};

		const handleStartVoiceCall = async () => {
			onClose();
			try {
				const channelId = await PrivateChannelActionCreators.ensureDMChannel(user.id);
				await CallUtils.checkAndStartCall(channelId);
			} catch (error) {
				console.error('Failed to start voice call:', error);
			}
		};

		const handleChangeNickname = () => {
			onClose();
			ModalActionCreators.push(modal(() => <ChangeNicknameModal guildId={guildId} user={user} member={member} />));
		};

		const handleSendFriendRequest = () => {
			RelationshipActionCreators.sendFriendRequest(user.id);
			onClose();
		};

		const handleAcceptFriendRequest = () => {
			RelationshipActionCreators.acceptFriendRequest(user.id);
			onClose();
		};

		const handleRemoveFriend = () => {
			onClose();
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Remove Friend`}
						description={t`Are you sure you want to remove ${user.username} as a friend?`}
						primaryText={t`Remove Friend`}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							RelationshipActionCreators.removeRelationship(user.id);
						}}
					/>
				)),
			);
		};

		const handleBlockUser = () => {
			onClose();
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Block User`}
						description={t`Are you sure you want to block ${user.username}? They won't be able to message you or send you friend requests.`}
						primaryText={t`Block`}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							RelationshipActionCreators.blockUser(user.id);
						}}
					/>
				)),
			);
		};

		const handleUnblockUser = () => {
			onClose();
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Unblock User`}
						description={t`Are you sure you want to unblock ${user.username}?`}
						primaryText={t`Unblock`}
						primaryVariant="primary"
						onPrimary={async () => {
							RelationshipActionCreators.removeRelationship(user.id);
						}}
					/>
				)),
			);
		};

		const handleKickMember = () => {
			onClose();
			ModalActionCreators.push(modal(() => <KickMemberModal guildId={guildId} targetUser={user} />));
		};

		const handleBanMember = () => {
			onClose();
			ModalActionCreators.push(modal(() => <BanMemberModal guildId={guildId} targetUser={user} />));
		};

		const handleCopyUserId = () => {
			TextCopyActionCreators.copy(i18n, user.id, true);
			onClose();
		};

		const menuGroups: Array<MenuGroupType> = [];

		const profileItems = [
			{
				icon: <UserCircleIcon className={styles.icon} />,
				label: t`View Profile`,
				onClick: handleViewProfile,
			},
		];

		if (!isCurrentUser) {
			profileItems.push({
				icon: <ChatTeardropIcon className={styles.icon} />,
				label: t`Message`,
				onClick: handleMessage,
			});

			if (!isBot && relationshipType === RelationshipTypes.FRIEND) {
				profileItems.push({
					icon: <PhoneIcon className={styles.icon} />,
					label: t`Voice Call`,
					onClick: handleStartVoiceCall,
				});
			}
		}

		menuGroups.push({items: profileItems});

		const guildItems = [];

		if (canManageNicknames) {
			guildItems.push({
				icon: <PencilIcon className={styles.icon} />,
				label: t`Change Nickname`,
				onClick: handleChangeNickname,
			});
		}

		if (guildItems.length > 0) {
			menuGroups.push({items: guildItems});
		}

		if (!isCurrentUser && !isBot) {
			const relationshipItems = [];

			if (relationshipType === RelationshipTypes.FRIEND) {
				relationshipItems.push({
					icon: <UserMinusIcon className={styles.icon} />,
					label: t`Remove Friend`,
					onClick: handleRemoveFriend,
					danger: true,
				});
			} else if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
				relationshipItems.push({
					icon: <UserPlusIcon className={styles.icon} />,
					label: t`Accept Friend Request`,
					onClick: handleAcceptFriendRequest,
				});
			} else if (
				relationshipType !== RelationshipTypes.OUTGOING_REQUEST &&
				relationshipType !== RelationshipTypes.BLOCKED
			) {
				relationshipItems.push({
					icon: <UserPlusIcon className={styles.icon} />,
					label: t`Send Friend Request`,
					onClick: handleSendFriendRequest,
				});
			}

			if (relationshipItems.length > 0) {
				menuGroups.push({items: relationshipItems});
			}
		}

		if (canTimeout || canKick || canBan) {
			const moderationItems = [];

			if (canTimeout) {
				moderationItems.push({
					icon: <ClockIcon className={styles.icon} />,
					label: memberIsTimedOut ? t`Remove Timeout` : t`Timeout`,
					onClick: () => {
						onClose();
						if (memberIsTimedOut) {
							ModalActionCreators.push(modal(() => <RemoveTimeoutModal guildId={guildId} targetUser={user} />));
						} else {
							ModalActionCreators.push(
								modal(() => (
									<TimeoutMemberSheet
										isOpen={true}
										onClose={() => ModalActionCreators.pop()}
										guildId={guildId}
										targetUser={user}
										targetMember={member}
									/>
								)),
							);
						}
					},
					danger: !memberIsTimedOut,
				});
			}

			if (canKick) {
				moderationItems.push({
					icon: <SignOutIcon className={styles.icon} />,
					label: t`Kick`,
					onClick: handleKickMember,
					danger: true,
				});
			}

			if (canBan) {
				moderationItems.push({
					icon: <GavelIcon className={styles.icon} />,
					label: t`Ban`,
					onClick: handleBanMember,
					danger: true,
				});
			}

			menuGroups.push({items: moderationItems});
		}

		if (!isCurrentUser) {
			if (relationshipType === RelationshipTypes.BLOCKED) {
				menuGroups.push({
					items: [
						{
							icon: <ProhibitIcon className={styles.icon} />,
							label: t`Unblock`,
							onClick: handleUnblockUser,
						},
					],
				});
			} else {
				menuGroups.push({
					items: [
						{
							icon: <ProhibitIcon className={styles.icon} />,
							label: t`Block`,
							onClick: handleBlockUser,
							danger: true,
						},
					],
				});
			}
		}

		menuGroups.push({
			items: [
				{
					icon: <IdentificationCardIcon className={styles.icon} />,
					label: t`Copy User ID`,
					onClick: handleCopyUserId,
				},
			],
		});

		const headerContent = (
			<div className={styles.header}>
				<StatusAwareAvatar user={user} size={48} guildId={guildId} />
				<div className={styles.headerInfo}>
					<span className={styles.headerName}>{NicknameUtils.getNickname(user, guildId)}</span>
					<span className={styles.headerTag}>{user.tag}</span>
				</div>
			</div>
		);

		return <MenuBottomSheet isOpen={isOpen} onClose={onClose} groups={menuGroups} headerContent={headerContent} />;
	},
);
