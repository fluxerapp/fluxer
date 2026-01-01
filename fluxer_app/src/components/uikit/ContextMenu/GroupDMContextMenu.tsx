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
import {CrownIcon, PencilIcon, PushPinIcon, SignOutIcon, TicketIcon, UserMinusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {ChannelTypes, RelationshipTypes} from '~/Constants';
import {GroupOwnershipTransferFailedModal} from '~/components/alerts/GroupOwnershipTransferFailedModal';
import {GroupRemoveUserFailedModal} from '~/components/alerts/GroupRemoveUserFailedModal';
import {ChangeGroupDMNicknameModal} from '~/components/modals/ChangeGroupDMNicknameModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {EditGroupModal} from '~/components/modals/EditGroupModal';
import {GroupInvitesModal} from '~/components/modals/GroupInvitesModal';
import {useLeaveGroup} from '~/hooks/useLeaveGroup';
import type {ChannelRecord} from '~/records/ChannelRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import RelationshipStore from '~/stores/RelationshipStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import UserStore from '~/stores/UserStore';
import {StartVoiceCallMenuItem} from './items/CallMenuItems';
import {CopyChannelIdMenuItem, FavoriteChannelMenuItem} from './items/ChannelMenuItems';
import {CopyUserIdMenuItem} from './items/CopyMenuItems';
import {DebugChannelMenuItem, DebugUserMenuItem} from './items/DebugMenuItems';
import {MarkDMAsReadMenuItem, MuteDMMenuItem} from './items/DMMenuItems';
import {InviteToCommunityMenuItem} from './items/InviteMenuItems';
import {MentionUserMenuItem} from './items/MentionUserMenuItem';
import {MessageUserMenuItem} from './items/MessageUserMenuItem';
import {
	BlockUserMenuItem,
	ChangeFriendNicknameMenuItem,
	RelationshipActionMenuItem,
	UnblockUserMenuItem,
} from './items/RelationshipMenuItems';
import {AddNoteMenuItem} from './items/UserNoteMenuItems';
import {UserProfileMenuItem} from './items/UserProfileMenuItem';
import {MenuGroup} from './MenuGroup';
import {MenuItem} from './MenuItem';

interface GroupDMContextMenuProps {
	channel: ChannelRecord;
	onClose: () => void;
}

export const GroupDMContextMenu: React.FC<GroupDMContextMenuProps> = observer(({channel, onClose}) => {
	const {t} = useLingui();
	const currentUserId = AuthenticationStore.currentUserId;
	const isOwner = channel.ownerId === currentUserId;
	const developerMode = UserSettingsStore.developerMode;
	const leaveGroup = useLeaveGroup();

	const handleEditGroup = React.useCallback(() => {
		onClose();
		ModalActionCreators.push(modal(() => <EditGroupModal channelId={channel.id} />));
	}, [channel.id, onClose]);

	const handleShowInvites = React.useCallback(() => {
		onClose();
		ModalActionCreators.push(modal(() => <GroupInvitesModal channelId={channel.id} />));
	}, [channel.id, onClose]);

	const handleLeaveGroup = React.useCallback(() => {
		onClose();
		leaveGroup(channel.id);
	}, [channel.id, onClose, leaveGroup]);

	const handlePinDM = React.useCallback(async () => {
		onClose();
		try {
			await PrivateChannelActionCreators.pinDmChannel(channel.id);
			ToastActionCreators.createToast({
				type: 'success',
				children: t`Pinned group`,
			});
		} catch (error) {
			console.error('Failed to pin group:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: t`Failed to pin group`,
			});
		}
	}, [channel.id, onClose, t]);

	const handleUnpinDM = React.useCallback(async () => {
		onClose();
		try {
			await PrivateChannelActionCreators.unpinDmChannel(channel.id);
			ToastActionCreators.createToast({
				type: 'success',
				children: t`Unpinned group`,
			});
		} catch (error) {
			console.error('Failed to unpin group:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: t`Failed to unpin group`,
			});
		}
	}, [channel.id, onClose, t]);

	return (
		<>
			<MenuGroup>
				<MarkDMAsReadMenuItem channel={channel} onClose={onClose} />
				{channel.isPinned ? (
					<MenuItem icon={<PushPinIcon />} onClick={handleUnpinDM}>
						{t`Unpin Group DM`}
					</MenuItem>
				) : (
					<MenuItem icon={<PushPinIcon />} onClick={handlePinDM}>
						{t`Pin Group DM`}
					</MenuItem>
				)}
			</MenuGroup>

			<MenuGroup>
				<FavoriteChannelMenuItem channel={channel} onClose={onClose} />
			</MenuGroup>

			<MenuGroup>
				{isOwner && (
					<MenuItem icon={<TicketIcon />} onClick={handleShowInvites}>
						{t`Invites`}
					</MenuItem>
				)}
				<MenuItem icon={<PencilIcon />} onClick={handleEditGroup}>
					{t`Edit Group`}
				</MenuItem>
			</MenuGroup>

			<MenuGroup>
				<MuteDMMenuItem channel={channel} onClose={onClose} />
			</MenuGroup>

			<MenuGroup>
				<MenuItem icon={<SignOutIcon />} onClick={handleLeaveGroup} danger>
					{t`Leave Group`}
				</MenuItem>
			</MenuGroup>

			{developerMode && (
				<MenuGroup>
					<DebugChannelMenuItem channel={channel} onClose={onClose} />
				</MenuGroup>
			)}

			<MenuGroup>
				<CopyChannelIdMenuItem channel={channel} onClose={onClose} />
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
		const {t} = useLingui();
		const currentUserId = AuthenticationStore.currentUserId;
		const channel = ChannelStore.getChannel(channelId);
		const user = UserStore.getUser(userId);
		const developerMode = UserSettingsStore.developerMode;

		const handleChangeNickname = React.useCallback(() => {
			if (!user) return;
			onClose();
			ModalActionCreators.push(modal(() => <ChangeGroupDMNicknameModal channelId={channelId} user={user} />));
		}, [channelId, onClose, user]);

		const handleRemoveFromGroup = React.useCallback(() => {
			if (!user) return;
			onClose();
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Remove from Group`}
						description={t`Are you sure you want to remove ${user.username} from the group?`}
						primaryText={t`Remove`}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							try {
								await PrivateChannelActionCreators.removeRecipient(channelId, userId);
								ToastActionCreators.createToast({
									type: 'success',
									children: t`Removed from group`,
								});
							} catch (error) {
								console.error('Failed to remove from group:', error);
								ModalActionCreators.push(modal(() => <GroupRemoveUserFailedModal username={user.username} />));
							}
						}}
					/>
				)),
			);
		}, [channelId, onClose, t, user, userId]);

		const handleMakeGroupOwner = React.useCallback(() => {
			if (!user) return;
			onClose();
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Transfer Ownership`}
						description={t`Are you sure you want to make ${user.username} the group owner? You will lose owner privileges.`}
						primaryText={t`Transfer Ownership`}
						onPrimary={async () => {
							try {
								await ChannelActionCreators.update(channelId, {owner_id: userId});
								ToastActionCreators.createToast({
									type: 'success',
									children: t`Ownership transferred`,
								});
							} catch (error) {
								console.error('Failed to transfer ownership:', error);
								ModalActionCreators.push(modal(() => <GroupOwnershipTransferFailedModal username={user.username} />));
							}
						}}
					/>
				)),
			);
		}, [channelId, onClose, t, user, userId]);

		if (!user || !channel) {
			return null;
		}

		const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
		if (!isGroupDM) {
			return null;
		}

		const isSelf = userId === currentUserId;
		const isOwner = channel.ownerId === currentUserId;
		const relationship = RelationshipStore.getRelationship(userId);
		const relationshipType = relationship?.type;

		if (isSelf) {
			return (
				<>
					<MenuGroup>
						<UserProfileMenuItem user={user} guildId={undefined} onClose={onClose} />
						<MentionUserMenuItem user={user} onClose={onClose} />
						<MenuItem icon={<PencilIcon />} onClick={handleChangeNickname}>
							{t`Change My Group Nickname`}
						</MenuItem>
					</MenuGroup>

					{developerMode && (
						<MenuGroup>
							<DebugUserMenuItem user={user} onClose={onClose} />
						</MenuGroup>
					)}

					<MenuGroup>
						<CopyUserIdMenuItem user={user} onClose={onClose} />
					</MenuGroup>
				</>
			);
		}

		return (
			<>
				<MenuGroup>
					<UserProfileMenuItem user={user} guildId={undefined} onClose={onClose} />
					<MentionUserMenuItem user={user} onClose={onClose} />
					<MessageUserMenuItem user={user} onClose={onClose} />
					<StartVoiceCallMenuItem user={user} onClose={onClose} />
					<AddNoteMenuItem user={user} onClose={onClose} />
					<ChangeFriendNicknameMenuItem user={user} onClose={onClose} />
				</MenuGroup>

				{isOwner && (
					<MenuGroup>
						<MenuItem icon={<UserMinusIcon />} onClick={handleRemoveFromGroup} danger>
							{t`Remove from Group`}
						</MenuItem>
						<MenuItem icon={<CrownIcon />} onClick={handleMakeGroupOwner} danger>
							{t`Make Group Owner`}
						</MenuItem>
						<MenuItem icon={<PencilIcon />} onClick={handleChangeNickname}>
							{t`Change Group Nickname`}
						</MenuItem>
					</MenuGroup>
				)}

				<MenuGroup>
					<InviteToCommunityMenuItem user={user} onClose={onClose} />
					<RelationshipActionMenuItem user={user} onClose={onClose} />
					{relationshipType === RelationshipTypes.BLOCKED ? (
						<UnblockUserMenuItem user={user} onClose={onClose} />
					) : (
						<BlockUserMenuItem user={user} onClose={onClose} />
					)}
				</MenuGroup>

				{developerMode && (
					<MenuGroup>
						<DebugUserMenuItem user={user} onClose={onClose} />
					</MenuGroup>
				)}

				<MenuGroup>
					<CopyUserIdMenuItem user={user} onClose={onClose} />
				</MenuGroup>
			</>
		);
	},
);
