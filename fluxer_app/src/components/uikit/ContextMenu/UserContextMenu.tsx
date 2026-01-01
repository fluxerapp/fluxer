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
import {CrownIcon, PencilSimpleIcon, UserMinusIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {ME, Permissions, RelationshipTypes} from '~/Constants';
import {DMCloseFailedModal} from '~/components/alerts/DMCloseFailedModal';
import {ChangeGroupDMNicknameModal} from '~/components/modals/ChangeGroupDMNicknameModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {Routes} from '~/Routes';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import CallStateStore from '~/stores/CallStateStore';
import ChannelStore from '~/stores/ChannelStore';
import PermissionStore from '~/stores/PermissionStore';
import RelationshipStore from '~/stores/RelationshipStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import UserStore from '~/stores/UserStore';
import * as RouterUtils from '~/utils/RouterUtils';
import {RingUserMenuItem, StartVoiceCallMenuItem} from './items/CallMenuItems';
import {FavoriteChannelMenuItem} from './items/ChannelMenuItems';
import {CopyUserIdMenuItem} from './items/CopyMenuItems';
import {DebugUserMenuItem} from './items/DebugMenuItems';
import {MarkDMAsReadMenuItem} from './items/DMMenuItems';
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
import {LocalMuteParticipantMenuItem, ParticipantVolumeSlider} from './items/VoiceParticipantMenuItems';
import {MenuGroup} from './MenuGroup';
import {MenuItem} from './MenuItem';

interface UserContextMenuProps {
	user: UserRecord;
	onClose: () => void;
	guildId?: string;
	channelId?: string;
	isCallContext?: boolean;
}

export const UserContextMenu: React.FC<UserContextMenuProps> = observer(
	({user, onClose, guildId, channelId, isCallContext = false}) => {
		const {t} = useLingui();
		const channel = channelId ? ChannelStore.getChannel(channelId) : null;

		const canSendMessages = channel
			? channel.isPrivate() || PermissionStore.can(Permissions.SEND_MESSAGES, {channelId, guildId})
			: true;

		const canMention = channel !== null && canSendMessages;
		const isCurrentUser = user.id === AuthenticationStore.currentUserId;

		const relationship = RelationshipStore.getRelationship(user.id);
		const relationshipType = relationship?.type;

		const developerMode = UserSettingsStore.developerMode;
		const currentUserId = AuthenticationStore.currentUserId;

		const dmPartnerId = channel?.isDM()
			? (channel.recipientIds.find((id) => id !== currentUserId) ?? channel.recipientIds[0])
			: null;

		const dmPartner = dmPartnerId ? UserStore.getUser(dmPartnerId) : null;

		const isGroupDM = channel?.isGroupDM();
		const isOwner = channel?.ownerId === currentUserId;
		const isRecipient = channel?.recipientIds.includes(user.id);
		const isBot = user.bot;

		const call = channelId ? CallStateStore.getCall(channelId) : null;
		const showCallItems = isCallContext && call && !isCurrentUser;

		const handleChangeGroupNickname = React.useCallback(() => {
			if (!channel) return;
			onClose();
			ModalActionCreators.push(modal(() => <ChangeGroupDMNicknameModal channelId={channel.id} user={user} />));
		}, [channel, onClose, user]);

		const handleRemoveFromGroup = React.useCallback(() => {
			if (!channel) return;
			onClose();
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Remove from Group`}
						description={t`Are you sure you want to remove ${user.username} from the group?`}
						primaryText={t`Remove`}
						primaryVariant="danger-primary"
						onPrimary={() => PrivateChannelActionCreators.removeRecipient(channel.id, user.id)}
					/>
				)),
			);
		}, [channel, onClose, t, user.id, user.username]);

		const handleMakeGroupOwner = React.useCallback(() => {
			if (!channel) return;
			onClose();
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Change Owner`}
						description={t`Are you sure you want to transfer ownership of the group to ${user.username}?`}
						primaryText={t`Transfer Ownership`}
						onPrimary={() => {
							ChannelActionCreators.update(channel.id, {owner_id: user.id});
						}}
					/>
				)),
			);
		}, [channel, onClose, t, user.id, user.username]);

		const handleCloseDM = React.useCallback(() => {
			if (!channel || !channel.isDM()) return;

			onClose();

			const displayName = dmPartner?.username ?? user.username;

			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Close DM`}
						description={t`Are you sure you want to close your DM with ${displayName}? You can always reopen it later.`}
						primaryText={t`Close DM`}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							try {
								await ChannelActionCreators.remove(channel.id);

								const selectedChannel = SelectedChannelStore.selectedChannelIds.get(ME);
								if (selectedChannel === channel.id) {
									RouterUtils.transitionTo(Routes.ME);
								}

								ToastActionCreators.createToast({
									type: 'success',
									children: t`DM closed`,
								});
							} catch (error) {
								console.error('Failed to close DM:', error);
								ModalActionCreators.push(modal(() => <DMCloseFailedModal />));
							}
						}}
					/>
				)),
			);
		}, [channel, dmPartner?.username, onClose, t, user.username]);

		const renderDmSelfMenu = () => {
			if (!channel) return renderDefaultMenu();

			return (
				<>
					<MenuGroup>
						<MarkDMAsReadMenuItem channel={channel} onClose={onClose} />
					</MenuGroup>

					<MenuGroup>
						<FavoriteChannelMenuItem channel={channel} onClose={onClose} />
					</MenuGroup>

					<MenuGroup>
						<UserProfileMenuItem user={user} guildId={guildId} onClose={onClose} />
						<MenuItem icon={<XIcon weight="bold" />} onClick={handleCloseDM}>
							{t`Close DM`}
						</MenuItem>
					</MenuGroup>

					<MenuGroup>{developerMode && <DebugUserMenuItem user={user} onClose={onClose} />}</MenuGroup>

					<MenuGroup>
						<CopyUserIdMenuItem user={user} onClose={onClose} />
					</MenuGroup>
				</>
			);
		};

		const renderDmOtherMenu = () => {
			if (!channel) return renderDefaultMenu();

			return (
				<>
					<MenuGroup>
						<MarkDMAsReadMenuItem channel={channel} onClose={onClose} />
					</MenuGroup>

					<MenuGroup>
						<FavoriteChannelMenuItem channel={channel} onClose={onClose} />
					</MenuGroup>

					<MenuGroup>
						<UserProfileMenuItem user={user} guildId={guildId} onClose={onClose} />
						{showCallItems && channelId && (
							<RingUserMenuItem userId={user.id} channelId={channelId} onClose={onClose} />
						)}
						{!isBot && <StartVoiceCallMenuItem user={user} onClose={onClose} />}
						<AddNoteMenuItem user={user} onClose={onClose} />
						<ChangeFriendNicknameMenuItem user={user} onClose={onClose} />
						<MenuItem icon={<XIcon weight="bold" />} onClick={handleCloseDM}>
							{t`Close DM`}
						</MenuItem>
					</MenuGroup>

					<MenuGroup>
						{!isBot && <InviteToCommunityMenuItem user={user} onClose={onClose} />}
						{!isBot && <RelationshipActionMenuItem user={user} onClose={onClose} />}
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
		};

		const renderDefaultMenu = () => (
			<>
				<MenuGroup>
					<UserProfileMenuItem user={user} guildId={guildId} onClose={onClose} />

					{isGroupDM && isCurrentUser && (
						<MenuItem icon={<PencilSimpleIcon />} onClick={handleChangeGroupNickname}>
							{t`Change Group Nickname`}
						</MenuItem>
					)}

					{canMention && <MentionUserMenuItem user={user} onClose={onClose} />}
					{!isCurrentUser && <MessageUserMenuItem user={user} onClose={onClose} />}

					{showCallItems && channelId && <RingUserMenuItem userId={user.id} channelId={channelId} onClose={onClose} />}

					{!isCurrentUser && !isBot && !isCallContext && <StartVoiceCallMenuItem user={user} onClose={onClose} />}

					{!isCurrentUser && <AddNoteMenuItem user={user} onClose={onClose} />}

					<ChangeFriendNicknameMenuItem user={user} onClose={onClose} />
				</MenuGroup>

				{showCallItems && (
					<MenuGroup>
						<ParticipantVolumeSlider userId={user.id} />
					</MenuGroup>
				)}

				{isGroupDM && isOwner && isRecipient && !isCurrentUser && (
					<MenuGroup>
						<MenuItem icon={<UserMinusIcon />} onClick={handleRemoveFromGroup} danger>
							{t`Remove from Group`}
						</MenuItem>

						<MenuItem icon={<CrownIcon />} onClick={handleMakeGroupOwner} danger>
							{t`Make Group Owner`}
						</MenuItem>

						<MenuItem icon={<PencilSimpleIcon />} onClick={handleChangeGroupNickname}>
							{t`Change Group Nickname`}
						</MenuItem>
					</MenuGroup>
				)}

				{showCallItems && (
					<MenuGroup>
						<LocalMuteParticipantMenuItem userId={user.id} onClose={onClose} />
					</MenuGroup>
				)}

				<MenuGroup>
					{!isCurrentUser && !isBot && <InviteToCommunityMenuItem user={user} onClose={onClose} />}
					{!isCurrentUser && !isBot && <RelationshipActionMenuItem user={user} onClose={onClose} />}

					{!isCurrentUser &&
						(relationshipType === RelationshipTypes.BLOCKED ? (
							<UnblockUserMenuItem user={user} onClose={onClose} />
						) : (
							<BlockUserMenuItem user={user} onClose={onClose} />
						))}
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

		if (channel?.isDM()) {
			return isCurrentUser ? renderDmSelfMenu() : renderDmOtherMenu();
		}

		return renderDefaultMenu();
	},
);
