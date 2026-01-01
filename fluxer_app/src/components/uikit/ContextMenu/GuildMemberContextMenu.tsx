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

import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Permissions, RelationshipTypes} from '~/Constants';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import PermissionStore from '~/stores/PermissionStore';
import RelationshipStore from '~/stores/RelationshipStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import * as PermissionUtils from '~/utils/PermissionUtils';
import {StartVoiceCallMenuItem} from './items/CallMenuItems';
import {CopyUserIdMenuItem} from './items/CopyMenuItems';
import {DebugGuildMemberMenuItem, DebugUserMenuItem} from './items/DebugMenuItems';
import {
	BanMemberMenuItem,
	ChangeNicknameMenuItem,
	KickMemberMenuItem,
	ManageRolesMenuItem,
	TimeoutMemberMenuItem,
	TransferOwnershipMenuItem,
} from './items/GuildMemberMenuItems';
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

interface GuildMemberContextMenuProps {
	user: UserRecord;
	onClose: () => void;
	guildId: string;
	channelId?: string;
}

export const GuildMemberContextMenu: React.FC<GuildMemberContextMenuProps> = observer(
	({user, onClose, guildId, channelId}) => {
		const channel = channelId ? ChannelStore.getChannel(channelId) : null;
		const canSendMessages = channel ? PermissionStore.can(Permissions.SEND_MESSAGES, {channelId, guildId}) : true;
		const canMention = channel !== null && canSendMessages;
		const isCurrentUser = user.id === AuthenticationStore.currentUserId;
		const relationship = RelationshipStore.getRelationship(user.id);
		const relationshipType = relationship?.type;

		const guild = GuildStore.getGuild(guildId);
		const member = GuildMemberStore.getMember(guildId, user.id);
		const currentUserId = AuthenticationStore.currentUserId;
		const isBot = user.bot;

		const canKickMembers = PermissionStore.can(Permissions.KICK_MEMBERS, {guildId});
		const canBanMembers = PermissionStore.can(Permissions.BAN_MEMBERS, {guildId});
		const canModerateMembers = PermissionStore.can(Permissions.MODERATE_MEMBERS, {guildId});
		const isOwner = guild?.ownerId === currentUserId;

		const canKick = !isCurrentUser && canKickMembers;
		const canBan = !isCurrentUser && canBanMembers;
		const guildSnapshot = guild?.toJSON();
		const targetHasModerateMembersPermission =
			guildSnapshot !== undefined && PermissionUtils.can(Permissions.MODERATE_MEMBERS, user.id, guildSnapshot);
		const canTimeout = !isCurrentUser && canModerateMembers && !targetHasModerateMembersPermission;
		const canTransfer = !isCurrentUser && isOwner;
		const developerMode = UserSettingsStore.developerMode;

		const hasManageNicknamesPermission = PermissionStore.can(Permissions.MANAGE_NICKNAMES, {guildId});
		const canManageNicknames = member && (isCurrentUser || hasManageNicknamesPermission);
		const hasRoles = guild && Object.values(guild.roles).some((r) => !r.isEveryone);

		return (
			<>
				<MenuGroup>
					<UserProfileMenuItem user={user} guildId={guildId} onClose={onClose} />
					{canMention && <MentionUserMenuItem user={user} onClose={onClose} />}
					{!isCurrentUser && <MessageUserMenuItem user={user} onClose={onClose} />}
					{!isCurrentUser && !isBot && <StartVoiceCallMenuItem user={user} onClose={onClose} />}
					{!isCurrentUser && <AddNoteMenuItem user={user} onClose={onClose} />}
					<ChangeFriendNicknameMenuItem user={user} onClose={onClose} />
				</MenuGroup>

				<MenuGroup>
					{canManageNicknames && member && (
						<ChangeNicknameMenuItem guildId={guildId} user={user} member={member} onClose={onClose} />
					)}
					{!isCurrentUser && !isBot && <InviteToCommunityMenuItem user={user} onClose={onClose} />}
					{!isCurrentUser && !isBot && <RelationshipActionMenuItem user={user} onClose={onClose} />}
					{!isCurrentUser &&
						(relationshipType === RelationshipTypes.BLOCKED ? (
							<UnblockUserMenuItem user={user} onClose={onClose} />
						) : (
							<BlockUserMenuItem user={user} onClose={onClose} />
						))}
				</MenuGroup>

				{canTransfer && member && (
					<MenuGroup>
						<TransferOwnershipMenuItem guildId={guildId} user={user} member={member} onClose={onClose} />
					</MenuGroup>
				)}

				{(canTimeout || canKick || canBan) && member && (
					<MenuGroup>
						{canTimeout && <TimeoutMemberMenuItem guildId={guildId} user={user} member={member} onClose={onClose} />}
						{canKick && <KickMemberMenuItem guildId={guildId} user={user} onClose={onClose} />}
						{canBan && <BanMemberMenuItem guildId={guildId} user={user} onClose={onClose} />}
					</MenuGroup>
				)}

				{member && hasRoles && (
					<MenuGroup>
						<ManageRolesMenuItem guildId={guildId} member={member} />
					</MenuGroup>
				)}

				{developerMode && (
					<MenuGroup>
						<DebugUserMenuItem user={user} onClose={onClose} />
						{member && <DebugGuildMemberMenuItem member={member} onClose={onClose} />}
					</MenuGroup>
				)}

				<MenuGroup>
					<CopyUserIdMenuItem user={user} onClose={onClose} />
				</MenuGroup>
			</>
		);
	},
);
