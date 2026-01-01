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
import {UsersIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as VoiceSettingsActionCreators from '~/actions/VoiceSettingsActionCreators';
import {Permissions, RelationshipTypes} from '~/Constants';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import PermissionStore from '~/stores/PermissionStore';
import RelationshipStore from '~/stores/RelationshipStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import VoiceSettingsStore from '~/stores/VoiceSettingsStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import type {VoiceState} from '~/stores/voice/VoiceStateManager';
import {CopyUserIdMenuItem} from './items/CopyMenuItems';
import {DebugUserMenuItem} from './items/DebugMenuItems';
import {
	BanMemberMenuItem,
	ChangeNicknameMenuItem,
	KickMemberMenuItem,
	ManageRolesMenuItem,
} from './items/GuildMemberMenuItems';
import {MessageUserMenuItem} from './items/MessageUserMenuItem';
import {MoveToChannelSubmenu} from './items/MoveToChannelSubmenu';
import {BlockUserMenuItem, RelationshipActionMenuItem, UnblockUserMenuItem} from './items/RelationshipMenuItems';
import {UserProfileMenuItem} from './items/UserProfileMenuItem';
import {
	BulkCameraDevicesMenuItem,
	BulkDeafenDevicesMenuItem,
	BulkDisconnectDevicesMenuItem,
	BulkMuteDevicesMenuItem,
	CopyDeviceIdMenuItem,
	DisconnectParticipantMenuItem,
	FocusParticipantMenuItem,
	GuildDeafenMenuItem,
	GuildMuteMenuItem,
	LocalDisableVideoMenuItem,
	LocalMuteParticipantMenuItem,
	ParticipantVolumeSlider,
	SelfDeafenMenuItem,
	SelfMuteMenuItem,
	SelfTurnOffCameraMenuItem,
	SelfTurnOffStreamMenuItem,
	TurnOffDeviceCameraMenuItem,
	TurnOffDeviceStreamMenuItem,
	VoiceVideoSettingsMenuItem,
} from './items/VoiceParticipantMenuItems';
import {MenuGroup} from './MenuGroup';
import {MenuItemCheckbox} from './MenuItemCheckbox';

interface VoiceParticipantContextMenuProps {
	user: UserRecord;
	participantName: string;
	onClose: () => void;
	guildId?: string;
	connectionId?: string;
	isGroupedItem?: boolean;
	isParentGroupedItem?: boolean;
}

export const VoiceParticipantContextMenu: React.FC<VoiceParticipantContextMenuProps> = observer(
	({user, participantName, onClose, guildId, connectionId, isGroupedItem = false, isParentGroupedItem = false}) => {
		const {t} = useLingui();
		const member = GuildMemberStore.getMember(guildId ?? '', user.id);
		const isCurrentUser = user.id === AuthenticationStore.currentUserId;
		const developerMode = UserSettingsStore.developerMode;
		const relationship = RelationshipStore.getRelationship(user.id);
		const relationshipType = relationship?.type;

		const canMuteMembers = guildId ? PermissionStore.can(Permissions.MUTE_MEMBERS, {guildId}) : false;
		const canMoveMembers = guildId ? PermissionStore.can(Permissions.MOVE_MEMBERS, {guildId}) : false;
		const canKickMembers = guildId ? PermissionStore.can(Permissions.KICK_MEMBERS, {guildId}) : false;
		const canBanMembers = guildId ? PermissionStore.can(Permissions.BAN_MEMBERS, {guildId}) : false;

		const userVoiceStates = React.useMemo(() => {
			if (!guildId) return [] as Array<{connectionId: string; voiceState: VoiceState}>;
			const allStates = MediaEngineStore.getAllVoiceStates();
			const acc: Array<{connectionId: string; voiceState: VoiceState}> = [];
			Object.entries(allStates).forEach(([g, guildData]) => {
				if (g === guildId) {
					Object.entries(guildData).forEach(([, channelData]) => {
						Object.entries(channelData).forEach(([cid, vs]) => {
							if (vs.user_id === user.id) acc.push({connectionId: cid, voiceState: vs});
						});
					});
				}
			});
			return acc;
		}, [guildId, user.id]);

		const hasMultipleConnections = userVoiceStates.length > 1;
		const connectionIds = React.useMemo(() => userVoiceStates.map((u) => u.connectionId), [userVoiceStates]);

		return (
			<>
				<MenuGroup>
					<UserProfileMenuItem user={user} guildId={guildId} onClose={onClose} />
					{connectionId && guildId && (
						<FocusParticipantMenuItem userId={user.id} connectionId={connectionId} onClose={onClose} />
					)}
					{!isCurrentUser && <MessageUserMenuItem user={user} onClose={onClose} />}
				</MenuGroup>

				{isCurrentUser ? (
					<>
						<MenuGroup>
							{isGroupedItem && connectionId ? (
								<>
									<SelfMuteMenuItem
										onClose={onClose}
										connectionId={connectionId}
										isDeviceSpecific={true}
										label={t`Mute Device`}
									/>
									<SelfDeafenMenuItem
										onClose={onClose}
										connectionId={connectionId}
										isDeviceSpecific={true}
										label={t`Deafen Device`}
									/>
									<TurnOffDeviceCameraMenuItem onClose={onClose} connectionId={connectionId} />
									<TurnOffDeviceStreamMenuItem onClose={onClose} connectionId={connectionId} />
									<CopyDeviceIdMenuItem connectionId={connectionId} onClose={onClose} />
									{guildId && (
										<MoveToChannelSubmenu
											userId={user.id}
											guildId={guildId}
											connectionId={connectionId}
											onClose={onClose}
											label={t`Move Device To...`}
										/>
									)}
								</>
							) : (
								<>
									<SelfMuteMenuItem onClose={onClose} />
									<SelfDeafenMenuItem onClose={onClose} />
									<SelfTurnOffCameraMenuItem onClose={onClose} />
									<SelfTurnOffStreamMenuItem onClose={onClose} />
									<VoiceVideoSettingsMenuItem onClose={onClose} />
								</>
							)}
							{guildId && (
								<DisconnectParticipantMenuItem
									userId={user.id}
									guildId={guildId}
									participantName={participantName}
									connectionId={connectionId}
									onClose={onClose}
									label={isGroupedItem ? t`Disconnect Device` : undefined}
								/>
							)}
						</MenuGroup>

						<MenuGroup>
							<MenuItemCheckbox
								icon={<UsersIcon weight="fill" style={{width: 16, height: 16}} />}
								checked={VoiceSettingsStore.showMyOwnCamera}
								onChange={(checked) => VoiceSettingsActionCreators.update({showMyOwnCamera: checked})}
							>
								{t`Show My Own Camera`}
							</MenuItemCheckbox>
							<MenuItemCheckbox
								icon={<UsersIcon weight="fill" style={{width: 16, height: 16}} />}
								checked={VoiceSettingsStore.showNonVideoParticipants}
								onChange={(checked) => VoiceSettingsActionCreators.update({showNonVideoParticipants: checked})}
							>
								{t`Show Non-Video Participants`}
							</MenuItemCheckbox>
						</MenuGroup>
					</>
				) : (
					<MenuGroup>
						<ParticipantVolumeSlider userId={user.id} />
						<LocalMuteParticipantMenuItem userId={user.id} onClose={onClose} />
						{connectionId && (
							<>
								<LocalDisableVideoMenuItem userId={user.id} connectionId={connectionId} onClose={onClose} />
								<CopyDeviceIdMenuItem connectionId={connectionId} onClose={onClose} />
							</>
						)}
					</MenuGroup>
				)}

				{isParentGroupedItem && hasMultipleConnections && (
					<MenuGroup>
						{isCurrentUser ? (
							<>
								<BulkMuteDevicesMenuItem userVoiceStates={userVoiceStates} onClose={onClose} />
								<BulkDeafenDevicesMenuItem userVoiceStates={userVoiceStates} onClose={onClose} />
								<BulkCameraDevicesMenuItem userVoiceStates={userVoiceStates} onClose={onClose} />
								<MoveToChannelSubmenu
									userId={user.id}
									guildId={guildId!}
									connectionIds={connectionIds}
									onClose={onClose}
									label={t`Move All Devices To...`}
								/>
								<BulkDisconnectDevicesMenuItem userVoiceStates={userVoiceStates} onClose={onClose} />
							</>
						) : (
							canMoveMembers && (
								<>
									<MoveToChannelSubmenu
										userId={user.id}
										guildId={guildId!}
										connectionIds={connectionIds}
										onClose={onClose}
										label={t`Move All Devices To...`}
									/>
									<BulkDisconnectDevicesMenuItem userVoiceStates={userVoiceStates} onClose={onClose} />
								</>
							)
						)}
					</MenuGroup>
				)}

				{guildId && (canMuteMembers || canMoveMembers) && (
					<MenuGroup>
						{canMuteMembers && (
							<>
								<GuildMuteMenuItem userId={user.id} guildId={guildId} onClose={onClose} />
								<GuildDeafenMenuItem userId={user.id} guildId={guildId} onClose={onClose} />
							</>
						)}
						{canMoveMembers && !isParentGroupedItem && !isCurrentUser && (
							<>
								<MoveToChannelSubmenu
									userId={user.id}
									guildId={guildId!}
									connectionId={connectionId}
									onClose={onClose}
									label={connectionId ? t`Move Device To...` : t`Move To...`}
								/>
								<DisconnectParticipantMenuItem
									userId={user.id}
									guildId={guildId!}
									participantName={participantName}
									connectionId={connectionId}
									onClose={onClose}
									label={connectionId ? t`Disconnect Device` : t`Disconnect`}
								/>
							</>
						)}
					</MenuGroup>
				)}

				{!isCurrentUser && (
					<MenuGroup>
						{guildId && member && (
							<ChangeNicknameMenuItem guildId={guildId} user={user} member={member} onClose={onClose} />
						)}
						<RelationshipActionMenuItem user={user} onClose={onClose} />
						{relationshipType === RelationshipTypes.BLOCKED ? (
							<UnblockUserMenuItem user={user} onClose={onClose} />
						) : (
							<BlockUserMenuItem user={user} onClose={onClose} />
						)}
					</MenuGroup>
				)}

				{guildId && !isCurrentUser && (canKickMembers || canBanMembers) && (
					<MenuGroup>
						{canKickMembers && <KickMemberMenuItem guildId={guildId!} user={user} onClose={onClose} />}
						{canBanMembers && <BanMemberMenuItem guildId={guildId!} user={user} onClose={onClose} />}
					</MenuGroup>
				)}

				{guildId && member && (
					<MenuGroup>
						<ManageRolesMenuItem guildId={guildId} member={member} />
					</MenuGroup>
				)}

				<MenuGroup>
					{developerMode && <DebugUserMenuItem user={user} onClose={onClose} />}
					<CopyUserIdMenuItem user={user} onClose={onClose} />
				</MenuGroup>
			</>
		);
	},
);
