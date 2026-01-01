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
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as RelationshipActionCreators from '~/actions/RelationshipActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import {Permissions, RelationshipTypes} from '~/Constants';
import {BanMemberModal} from '~/components/modals/BanMemberModal';
import {ChangeNicknameModal} from '~/components/modals/ChangeNicknameModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {KickMemberModal} from '~/components/modals/KickMemberModal';
import {TransferOwnershipModal} from '~/components/modals/TransferOwnershipModal';
import {MenuBottomSheet, type MenuGroupType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {useRoleHierarchy} from '~/hooks/useRoleHierarchy';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import PermissionStore from '~/stores/PermissionStore';
import RelationshipStore from '~/stores/RelationshipStore';
import styles from './UserProfileActionsSheet.module.css';

interface UserProfileActionsSheetProps {
	isOpen: boolean;
	onClose: () => void;
	user: UserRecord;
	isCurrentUser?: boolean;
	hasGuildProfile?: boolean;
	showGlobalProfile?: boolean;
	onToggleProfileView?: () => void;
	guildId?: string;
	guildMember?: GuildMemberRecord | null;
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
	}) => {
		const {t, i18n} = useLingui();
		const relationshipType = RelationshipStore.getRelationship(user.id)?.type;

		const guild = guildId ? GuildStore.getGuild(guildId) : null;
		const member = guildMember ?? (guildId ? GuildMemberStore.getMember(guildId, user.id) : null);
		const currentUserId = AuthenticationStore.currentUserId;

		const canKickMembers = guildId ? PermissionStore.can(Permissions.KICK_MEMBERS, {guildId}) : false;
		const canBanMembers = guildId ? PermissionStore.can(Permissions.BAN_MEMBERS, {guildId}) : false;
		const hasChangeNicknamePermission = guildId ? PermissionStore.can(Permissions.CHANGE_NICKNAME, {guildId}) : false;
		const hasManageNicknamesPermission = guildId ? PermissionStore.can(Permissions.MANAGE_NICKNAMES, {guildId}) : false;
		const isOwner = guild?.ownerId === currentUserId;
		const {canManageTarget} = useRoleHierarchy(guild);

		const canKick = !isCurrentUser && canKickMembers && member && canManageTarget(user.id);
		const canBan = !isCurrentUser && canBanMembers && member && canManageTarget(user.id);
		const canTransfer = !isCurrentUser && isOwner && member;
		const canManageNicknames =
			member &&
			((isCurrentUser && hasChangeNicknamePermission) || (hasManageNicknamesPermission && canManageTarget(user.id)));

		const handleCopyFluxerTag = () => {
			TextCopyActionCreators.copy(i18n, `${user.username}#${user.discriminator}`, true);
			onClose();
		};

		const handleCopyUserId = () => {
			TextCopyActionCreators.copy(i18n, user.id, true);
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

		const handleChangeNickname = () => {
			if (!guildId || !member) return;
			onClose();
			ModalActionCreators.push(modal(() => <ChangeNicknameModal guildId={guildId} user={user} member={member} />));
		};

		const handleKickMember = () => {
			if (!guildId) return;
			onClose();
			ModalActionCreators.push(modal(() => <KickMemberModal guildId={guildId} targetUser={user} />));
		};

		const handleBanMember = () => {
			if (!guildId) return;
			onClose();
			ModalActionCreators.push(modal(() => <BanMemberModal guildId={guildId} targetUser={user} />));
		};

		const handleTransferOwnership = () => {
			if (!guildId || !member) return;
			onClose();
			ModalActionCreators.push(
				modal(() => <TransferOwnershipModal guildId={guildId} targetUser={user} targetMember={member} />),
			);
		};

		const menuGroups: Array<MenuGroupType> = [];

		if (hasGuildProfile && onToggleProfileView) {
			menuGroups.push({
				items: [
					{
						icon: <GlobeIcon className={styles.icon} />,
						label: showGlobalProfile ? t`View Community Profile` : t`View Global Profile`,
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
					icon: <CopyIcon className={styles.icon} />,
					label: t`Copy FluxerTag`,
					onClick: handleCopyFluxerTag,
				},
				{
					icon: <IdentificationCardIcon className={styles.icon} />,
					label: t`Copy User ID`,
					onClick: handleCopyUserId,
				},
			],
		});

		if (guildId && member) {
			const guildItems = [];

			if (canManageNicknames) {
				guildItems.push({
					icon: <PencilIcon className={styles.icon} />,
					label: isCurrentUser ? t`Change Nickname` : t`Change Nickname`,
					onClick: handleChangeNickname,
				});
			}

			if (guildItems.length > 0) {
				menuGroups.push({items: guildItems});
			}

			if (canTransfer) {
				menuGroups.push({
					items: [
						{
							icon: <CrownIcon className={styles.icon} />,
							label: t`Transfer Ownership`,
							onClick: handleTransferOwnership,
							danger: true,
						},
					],
				});
			}

			if (canKick || canBan) {
				const moderationItems = [];

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
		}

		if (!isCurrentUser) {
			if (relationshipType === RelationshipTypes.FRIEND) {
				menuGroups.push({
					items: [
						{
							icon: <UserMinusIcon className={styles.icon} />,
							label: t`Remove Friend`,
							onClick: handleRemoveFriend,
							danger: true,
						},
					],
				});
			}

			const reportBlockItems = [
				{
					icon: <FlagIcon className={styles.icon} />,
					label: t`Report User`,
					onClick: () => {},
					danger: true,
				},
			];

			if (relationshipType !== RelationshipTypes.BLOCKED) {
				reportBlockItems.push({
					icon: <ProhibitIcon className={styles.icon} />,
					label: t`Block`,
					onClick: handleBlockUser,
					danger: true,
				});
			} else {
				reportBlockItems.push({
					icon: <ProhibitIcon className={styles.icon} />,
					label: t`Unblock`,
					onClick: handleUnblockUser,
					danger: false,
				});
			}

			menuGroups.push({items: reportBlockItems});
		}

		return <MenuBottomSheet isOpen={isOpen} onClose={onClose} groups={menuGroups} />;
	},
);
