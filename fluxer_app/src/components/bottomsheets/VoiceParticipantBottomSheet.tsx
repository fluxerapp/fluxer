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
	BellSlashIcon,
	CopyIcon,
	EnvelopeIcon,
	MicrophoneIcon,
	MicrophoneSlashIcon,
	PhoneDisconnectIcon,
	PhoneXIcon,
	SpeakerHighIcon,
	SpeakerSlashIcon,
	UserIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as GuildActionCreators from '~/actions/GuildActionCreators';
import * as GuildMemberActionCreators from '~/actions/GuildMemberActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import * as VoiceStateActionCreators from '~/actions/VoiceStateActionCreators';
import {Permissions} from '~/Constants';
import type {MenuGroupType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {MenuBottomSheet} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ConnectionStore from '~/stores/ConnectionStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import ParticipantVolumeStore from '~/stores/ParticipantVolumeStore';
import PermissionStore from '~/stores/PermissionStore';
import type {LivekitParticipantSnapshot} from '~/stores/voice/MediaEngineFacade';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import sharedStyles from './shared.module.css';

interface VoiceParticipantBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	user: UserRecord;
	guildId?: string;
	connectionId?: string;
	isConnectionItem?: boolean;
	participant?: LivekitParticipantSnapshot;
}

export const VoiceParticipantBottomSheet: React.FC<VoiceParticipantBottomSheetProps> = observer(
	({isOpen, onClose, user, guildId, connectionId, isConnectionItem = false}) => {
		const {t, i18n} = useLingui();
		ParticipantVolumeStore;

		const member = GuildMemberStore.getMember(guildId ?? '', user.id);

		const isCurrentUser = user.id === AuthenticationStore.currentUserId;
		const canMuteMembers = guildId ? PermissionStore.can(Permissions.MUTE_MEMBERS, {guildId}) : false;
		const canMoveMembers = guildId ? PermissionStore.can(Permissions.MOVE_MEMBERS, {guildId}) : false;
		const canKickMembers = guildId ? PermissionStore.can(Permissions.KICK_MEMBERS, {guildId}) : false;
		const canBanMembers = guildId ? PermissionStore.can(Permissions.BAN_MEMBERS, {guildId}) : false;

		const voiceState = MediaEngineStore.getVoiceStateByConnectionId(connectionId ?? '');

		const isSelfMuted = voiceState?.self_mute ?? false;
		const isSelfDeafened = voiceState?.self_deaf ?? false;
		const isGuildMuted = voiceState?.mute ?? false;
		const isGuildDeafened = voiceState?.deaf ?? false;

		const handleUserProfile = () => {
			onClose();
			UserProfileActionCreators.openUserProfile(user.id, guildId);
		};

		const handleMessageUser = async () => {
			onClose();
			try {
				await PrivateChannelActionCreators.openDMChannel(user.id);
			} catch (error) {
				console.error('Failed to open DM channel:', error);
			}
		};

		const handleCopyUserId = async () => {
			await TextCopyActionCreators.copy(i18n, user.id);
			onClose();
		};

		const handleSelfMute = () => {
			if (isCurrentUser) {
				VoiceStateActionCreators.toggleSelfMute(null);
			}
			onClose();
		};

		const handleSelfDeafen = () => {
			if (isCurrentUser) {
				VoiceStateActionCreators.toggleSelfDeaf(null);
			}
			onClose();
		};

		const handleGuildMute = async () => {
			if (canMuteMembers && !isCurrentUser) {
				try {
					await GuildMemberActionCreators.update(guildId!, user.id, {
						mute: !isGuildMuted,
					});
				} catch (error) {
					console.error('Failed to toggle guild mute:', error);
				}
			}
			onClose();
		};

		const handleGuildDeafen = async () => {
			if (canMuteMembers && !isCurrentUser) {
				try {
					await GuildMemberActionCreators.update(guildId!, user.id, {
						deaf: !isGuildDeafened,
					});
				} catch (error) {
					console.error('Failed to toggle guild deafen:', error);
				}
			}
			onClose();
		};

		const handleDisconnect = async () => {
			if (canMoveMembers && !isCurrentUser) {
				try {
					await GuildMemberActionCreators.update(guildId!, user.id, {
						channel_id: null,
						connection_id: connectionId,
					});
				} catch (error) {
					console.error('Failed to disconnect participant:', error);
				}
			}
			onClose();
		};

		const handleSelfDisconnect = async () => {
			if (!isCurrentUser || !guildId) {
				onClose();
				return;
			}
			try {
				const socket = ConnectionStore.socket;
				const cid = connectionId ?? MediaEngineStore.connectionId ?? null;
				if (socket && cid) {
					socket.updateVoiceState({
						guild_id: guildId,
						channel_id: null,
						self_mute: true,
						self_deaf: true,
						self_video: false,
						self_stream: false,
						connection_id: cid,
					});
				}
			} catch (error) {
				console.error('Failed to disconnect self device:', error);
			} finally {
				onClose();
			}
		};

		const handleKick = async () => {
			if (canKickMembers && !isCurrentUser) {
				try {
					await GuildMemberActionCreators.kick(guildId!, user.id);
				} catch (error) {
					console.error('Failed to kick member:', error);
				}
			}
			onClose();
		};

		const handleBan = async () => {
			if (canBanMembers && !isCurrentUser) {
				try {
					await GuildActionCreators.banMember(guildId!, user.id);
				} catch (error) {
					console.error('Failed to ban member:', error);
				}
			}
			onClose();
		};

		const handleLocalMute = () => {
			if (!isCurrentUser) {
				const isLocalMuted = ParticipantVolumeStore.isLocalMuted(user.id);
				ParticipantVolumeStore.setLocalMute(user.id, !isLocalMuted);
			}
			onClose();
		};

		const handleLocalDeafen = () => {
			if (!isCurrentUser) {
				const isLocalMuted = ParticipantVolumeStore.isLocalMuted(user.id);
				ParticipantVolumeStore.setLocalMute(user.id, !isLocalMuted);
			}
			onClose();
		};

		const menuGroups: Array<MenuGroupType> = [];

		const userProfileItems = [
			{
				icon: <UserIcon weight="fill" className={sharedStyles.icon} />,
				label: t`View Profile`,
				onClick: handleUserProfile,
			},
		];

		if (!isCurrentUser) {
			userProfileItems.push({
				icon: <EnvelopeIcon weight="fill" className={sharedStyles.icon} />,
				label: t`Message`,
				onClick: handleMessageUser,
			});
		}

		menuGroups.push({
			items: userProfileItems,
		});

		const voiceControlItems = [];

		if (isCurrentUser) {
			voiceControlItems.push(
				{
					icon: isSelfMuted ? (
						<MicrophoneIcon weight="fill" className={sharedStyles.icon} />
					) : (
						<MicrophoneSlashIcon weight="fill" className={sharedStyles.icon} />
					),
					label: isSelfMuted ? t`Unmute` : t`Mute`,
					onClick: handleSelfMute,
				},
				{
					icon: isSelfDeafened ? (
						<SpeakerHighIcon weight="fill" className={sharedStyles.icon} />
					) : (
						<SpeakerSlashIcon weight="fill" className={sharedStyles.icon} />
					),
					label: isSelfDeafened ? t`Undeafen` : t`Deafen`,
					onClick: handleSelfDeafen,
				},
			);
			if (guildId && connectionId) {
				voiceControlItems.push({
					icon: <PhoneDisconnectIcon weight="fill" className={sharedStyles.icon} />,
					label: isConnectionItem ? t`Disconnect Device` : t`Disconnect`,
					onClick: handleSelfDisconnect,
				});
			}
		} else {
			const isLocalMuted = ParticipantVolumeStore.isLocalMuted(user.id);

			voiceControlItems.push(
				{
					icon: <MicrophoneSlashIcon weight="fill" className={sharedStyles.icon} />,
					label: isLocalMuted ? t`Unmute` : t`Local Mute`,
					onClick: handleLocalMute,
				},
				{
					icon: <SpeakerSlashIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Local Deafen`,
					onClick: handleLocalDeafen,
				},
			);
		}

		menuGroups.push({
			items: voiceControlItems,
		});

		if (!isCurrentUser) {
			const participantVolume = ParticipantVolumeStore.getVolume(user.id);

			menuGroups.push({
				items: [
					{
						label: t`User Volume`,
						value: participantVolume,
						minValue: 0,
						maxValue: 200,
						onChange: (value: number) => {
							ParticipantVolumeStore.setVolume(user.id, value);
							MediaEngineStore.applyLocalAudioPreferencesForUser(user.id);
						},
						onFormat: (value: number) => `${Math.round(value)}%`,
						factoryDefaultValue: 100,
					},
				],
			});
		}

		if (guildId && member && (canMuteMembers || canMoveMembers || canKickMembers || canBanMembers)) {
			const moderationItems = [];

			if (canMuteMembers && !isCurrentUser) {
				moderationItems.push(
					{
						icon: <MicrophoneSlashIcon weight="fill" className={sharedStyles.icon} />,
						label: isGuildMuted ? t`Unmute User` : t`Mute User`,
						onClick: handleGuildMute,
					},
					{
						icon: <SpeakerSlashIcon weight="fill" className={sharedStyles.icon} />,
						label: isGuildDeafened ? t`Undeafen User` : t`Deafen User`,
						onClick: handleGuildDeafen,
					},
				);
			}

			if (canMoveMembers && !isCurrentUser) {
				moderationItems.push({
					icon: <PhoneXIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Disconnect`,
					onClick: handleDisconnect,
				});
			}

			if (canKickMembers && !isCurrentUser) {
				moderationItems.push({
					icon: <BellSlashIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Kick`,
					onClick: handleKick,
					danger: true,
				});
			}

			if (canBanMembers && !isCurrentUser) {
				moderationItems.push({
					icon: <BellSlashIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Ban`,
					onClick: handleBan,
					danger: true,
				});
			}

			if (moderationItems.length > 0) {
				menuGroups.push({
					items: moderationItems,
				});
			}
		}

		menuGroups.push({
			items: [
				{
					icon: <CopyIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Copy User ID`,
					onClick: handleCopyUserId,
				},
			],
		});

		return <MenuBottomSheet isOpen={isOpen} onClose={onClose} groups={menuGroups} />;
	},
);
