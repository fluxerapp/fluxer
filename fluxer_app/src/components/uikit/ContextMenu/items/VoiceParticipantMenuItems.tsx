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
	EyeIcon,
	EyeSlashIcon,
	GearIcon,
	MicrophoneSlashIcon,
	PhoneXIcon,
	ProjectorScreenIcon,
	SpeakerSlashIcon,
	VideoCameraSlashIcon,
	VideoIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as GuildMemberActionCreators from '~/actions/GuildMemberActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as SoundActionCreators from '~/actions/SoundActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as VoiceCallLayoutActionCreators from '~/actions/VoiceCallLayoutActionCreators';
import * as VoiceStateActionCreators from '~/actions/VoiceStateActionCreators';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import CallMediaPrefsStore from '~/stores/CallMediaPrefsStore';
import ConnectionStore from '~/stores/ConnectionStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import ParticipantVolumeStore from '~/stores/ParticipantVolumeStore';
import UserStore from '~/stores/UserStore';
import VoiceCallLayoutStore from '~/stores/VoiceCallLayoutStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import type {VoiceState} from '~/stores/voice/VoiceStateManager';
import {SoundType} from '~/utils/SoundUtils';
import {MenuItem} from '../MenuItem';
import {MenuItemCheckbox} from '../MenuItemCheckbox';
import {MenuItemSlider} from '../MenuItemSlider';
import styles from './MenuItems.module.css';

interface SelfMuteMenuItemProps {
	onClose: () => void;
	connectionId?: string;
	isDeviceSpecific?: boolean;
	label?: string;
}

export const SelfMuteMenuItem: React.FC<SelfMuteMenuItemProps> = observer(
	({connectionId, isDeviceSpecific = false, label}) => {
		const {t} = useLingui();
		const voiceState = connectionId
			? MediaEngineStore.getVoiceStateByConnectionId(connectionId)
			: MediaEngineStore.getCurrentUserVoiceState();
		const isSelfMuted = voiceState?.self_mute ?? false;
		const handleToggle = React.useCallback(() => {
			if (isDeviceSpecific && connectionId) {
				VoiceStateActionCreators.toggleSelfMuteForConnection(connectionId);
			} else {
				VoiceStateActionCreators.toggleSelfMute(null);
			}
		}, [connectionId, isDeviceSpecific]);
		return (
			<MenuItemCheckbox
				icon={<MicrophoneSlashIcon weight="fill" className={styles.icon} />}
				checked={isSelfMuted}
				onChange={handleToggle}
			>
				{label ?? t`Mute`}
			</MenuItemCheckbox>
		);
	},
);

interface SelfDeafenMenuItemProps {
	onClose: () => void;
	connectionId?: string;
	isDeviceSpecific?: boolean;
	label?: string;
}

export const SelfDeafenMenuItem: React.FC<SelfDeafenMenuItemProps> = observer(
	({connectionId, isDeviceSpecific = false, label}) => {
		const {t} = useLingui();
		const voiceState = connectionId
			? MediaEngineStore.getVoiceStateByConnectionId(connectionId)
			: MediaEngineStore.getCurrentUserVoiceState();
		const isSelfDeafened = voiceState?.self_deaf ?? false;
		const handleToggle = React.useCallback(() => {
			if (isDeviceSpecific && connectionId) {
				VoiceStateActionCreators.toggleSelfDeafenForConnection(connectionId);
			} else {
				VoiceStateActionCreators.toggleSelfDeaf(null);
			}
		}, [connectionId, isDeviceSpecific]);
		return (
			<MenuItemCheckbox
				icon={<SpeakerSlashIcon weight="fill" className={styles.icon} />}
				checked={isSelfDeafened}
				onChange={handleToggle}
			>
				{label ?? t`Deafen`}
			</MenuItemCheckbox>
		);
	},
);

interface VoiceVideoSettingsMenuItemProps {
	onClose: () => void;
}

export const VoiceVideoSettingsMenuItem: React.FC<VoiceVideoSettingsMenuItemProps> = observer(({onClose}) => {
	const {t} = useLingui();
	const handleClick = React.useCallback(() => {
		onClose();
		ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="voice_video" />));
	}, [onClose]);
	return (
		<MenuItem icon={<GearIcon weight="fill" className={styles.icon} />} onClick={handleClick}>
			{t`Voice & Video Settings`}
		</MenuItem>
	);
});

interface SelfTurnOffCameraMenuItemProps {
	onClose: () => void;
}
export const SelfTurnOffCameraMenuItem: React.FC<SelfTurnOffCameraMenuItemProps> = observer(({onClose}) => {
	const {t} = useLingui();
	const connectionId = MediaEngineStore.connectionId;
	const voiceState = connectionId ? MediaEngineStore.getVoiceStateByConnectionId(connectionId) : null;
	const isCameraOn = voiceState?.self_video ?? false;
	const handleClick = React.useCallback(() => {
		if (connectionId) VoiceStateActionCreators.turnOffCameraForConnection(connectionId);
		onClose();
	}, [connectionId, onClose]);
	if (!isCameraOn) return null;
	return (
		<MenuItem icon={<VideoCameraSlashIcon weight="fill" className={styles.icon} />} onClick={handleClick}>
			{t`Turn Off Camera`}
		</MenuItem>
	);
});

interface SelfTurnOffStreamMenuItemProps {
	onClose: () => void;
}
export const SelfTurnOffStreamMenuItem: React.FC<SelfTurnOffStreamMenuItemProps> = observer(({onClose}) => {
	const {t} = useLingui();
	const connectionId = MediaEngineStore.connectionId;
	const voiceState = connectionId ? MediaEngineStore.getVoiceStateByConnectionId(connectionId) : null;
	const isStreaming = voiceState?.self_stream ?? false;
	const handleClick = React.useCallback(() => {
		if (connectionId) VoiceStateActionCreators.turnOffStreamForConnection(connectionId);
		onClose();
	}, [connectionId, onClose]);
	if (!isStreaming) return null;
	return (
		<MenuItem icon={<ProjectorScreenIcon weight="fill" className={styles.icon} />} onClick={handleClick}>
			{t`Turn Off Stream`}
		</MenuItem>
	);
});

interface ParticipantVolumeSliderProps {
	userId: string;
}

export const ParticipantVolumeSlider: React.FC<ParticipantVolumeSliderProps> = observer(({userId}) => {
	const {t} = useLingui();
	const participantVolume = ParticipantVolumeStore.getVolume(userId);
	const handleChange = React.useCallback(
		(value: number) => {
			ParticipantVolumeStore.setVolume(userId, value);
			MediaEngineStore.applyLocalAudioPreferencesForUser(userId);
		},
		[userId],
	);
	return (
		<MenuItemSlider
			label={t`User Volume`}
			value={participantVolume}
			minValue={0}
			maxValue={200}
			onChange={handleChange}
			onFormat={(value) => `${Math.round(value)}%`}
		/>
	);
});

interface LocalMuteParticipantMenuItemProps {
	userId: string;
	onClose: () => void;
}

export const LocalMuteParticipantMenuItem: React.FC<LocalMuteParticipantMenuItemProps> = observer(({userId}) => {
	const {t} = useLingui();
	const isLocalMuted = ParticipantVolumeStore.isLocalMuted(userId);
	const handleToggle = React.useCallback(
		(checked: boolean) => {
			ParticipantVolumeStore.setLocalMute(userId, checked);
			MediaEngineStore.applyLocalAudioPreferencesForUser(userId);
		},
		[userId],
	);
	return (
		<MenuItemCheckbox
			icon={<SpeakerSlashIcon weight="fill" className={styles.icon} />}
			checked={isLocalMuted}
			onChange={handleToggle}
		>
			{t`Mute`}
		</MenuItemCheckbox>
	);
});

interface LocalDisableVideoMenuItemProps {
	userId: string;
	connectionId: string;
	onClose: () => void;
}

export const LocalDisableVideoMenuItem: React.FC<LocalDisableVideoMenuItemProps> = observer(
	({userId, connectionId, onClose}) => {
		const {t} = useLingui();
		const callId = MediaEngineStore.connectionId ?? '';
		const identity = `user_${userId}_${connectionId}`;
		const disabled = callId ? CallMediaPrefsStore.isVideoDisabled(callId, identity) : false;
		const handleToggle = React.useCallback(
			(checked: boolean) => {
				const id = MediaEngineStore.connectionId ?? '';
				if (!id) return;
				MediaEngineStore.setLocalVideoDisabled(identity, checked);
				onClose();
			},
			[identity, onClose],
		);
		return (
			<MenuItemCheckbox
				icon={<VideoCameraSlashIcon weight="fill" className={styles.icon} />}
				checked={disabled}
				onChange={handleToggle}
			>
				{t`Disable Video (Local)`}
			</MenuItemCheckbox>
		);
	},
);

interface GuildMuteMenuItemProps {
	userId: string;
	guildId: string;
	onClose: () => void;
}

export const GuildMuteMenuItem: React.FC<GuildMuteMenuItemProps> = observer(function GuildMuteMenuItem({
	userId,
	guildId,
}) {
	const {t} = useLingui();
	const member = GuildMemberStore.getMember(guildId, userId);
	const isGuildMuted = member?.mute ?? false;
	const isTimedOut = member?.isTimedOut() ?? false;
	const handleToggle = React.useCallback(
		async (checked: boolean) => {
			try {
				await GuildMemberActionCreators.update(guildId, userId, {mute: checked});
				if (checked) SoundActionCreators.playSound(SoundType.Mute);
				else SoundActionCreators.playSound(SoundType.Unmute);
			} catch {}
		},
		[guildId, userId],
	);
	return (
		<MenuItemCheckbox
			icon={<MicrophoneSlashIcon weight="fill" className={styles.icon} />}
			checked={!!isGuildMuted}
			onChange={handleToggle}
			danger
			disabled={isTimedOut}
			description={isTimedOut ? t`Disabled while the member is timed out.` : undefined}
		>
			{t`Community Mute`}
		</MenuItemCheckbox>
	);
});

interface GuildDeafenMenuItemProps {
	userId: string;
	guildId: string;
	onClose: () => void;
}

export const GuildDeafenMenuItem: React.FC<GuildDeafenMenuItemProps> = observer(function GuildDeafenMenuItem({
	userId,
	guildId,
}) {
	const {t} = useLingui();
	const member = GuildMemberStore.getMember(guildId, userId);
	const isGuildDeafened = member?.deaf ?? false;
	const handleToggle = React.useCallback(
		async (checked: boolean) => {
			try {
				await GuildMemberActionCreators.update(guildId, userId, {deaf: checked});
				if (checked) SoundActionCreators.playSound(SoundType.Deaf);
				else SoundActionCreators.playSound(SoundType.Undeaf);
			} catch {}
		},
		[guildId, userId],
	);
	return (
		<MenuItemCheckbox
			icon={<SpeakerSlashIcon weight="fill" className={styles.icon} />}
			checked={!!isGuildDeafened}
			onChange={handleToggle}
			danger
		>
			{t`Community Deafen`}
		</MenuItemCheckbox>
	);
});

interface DisconnectParticipantMenuItemProps {
	userId: string;
	guildId: string;
	participantName: string;
	connectionId?: string;
	onClose: () => void;
	label?: string;
}

export const DisconnectParticipantMenuItem: React.FC<DisconnectParticipantMenuItemProps> = observer(
	function DisconnectParticipantMenuItem({userId, guildId, connectionId, onClose, label}) {
		const {t} = useLingui();
		const currentUser = UserStore.currentUser;
		const isSelf = currentUser?.id === userId;
		const handleClick = React.useCallback(async () => {
			onClose();
			if (isSelf) {
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
			} else {
				try {
					await GuildMemberActionCreators.update(guildId, userId, {channel_id: null, connection_id: connectionId});
				} catch {}
			}
		}, [guildId, userId, connectionId, onClose, isSelf]);
		const defaultLabel = connectionId ? t`Disconnect Device` : t`Disconnect`;
		return (
			<MenuItem icon={<PhoneXIcon weight="fill" className={styles.icon} />} onClick={handleClick} danger>
				{label ?? defaultLabel}
			</MenuItem>
		);
	},
);

interface TurnOffDeviceCameraMenuItemProps {
	onClose: () => void;
	connectionId: string;
}

export const TurnOffDeviceCameraMenuItem: React.FC<TurnOffDeviceCameraMenuItemProps> = observer(
	({connectionId, onClose}) => {
		const {t} = useLingui();
		const voiceState = MediaEngineStore.getVoiceStateByConnectionId(connectionId);
		const isCameraOn = voiceState?.self_video ?? false;
		const handleClick = React.useCallback(() => {
			VoiceStateActionCreators.turnOffCameraForConnection(connectionId);
			onClose();
		}, [connectionId, onClose]);
		if (!isCameraOn) return null;
		return (
			<MenuItem icon={<VideoCameraSlashIcon weight="fill" className={styles.icon} />} onClick={handleClick}>
				{t`Turn Off Device Camera`}
			</MenuItem>
		);
	},
);

interface TurnOffDeviceStreamMenuItemProps {
	onClose: () => void;
	connectionId: string;
}

export const TurnOffDeviceStreamMenuItem: React.FC<TurnOffDeviceStreamMenuItemProps> = observer(
	({connectionId, onClose}) => {
		const {t} = useLingui();
		const voiceState = MediaEngineStore.getVoiceStateByConnectionId(connectionId);
		const isStreaming = voiceState?.self_stream ?? false;
		const handleClick = React.useCallback(() => {
			VoiceStateActionCreators.turnOffStreamForConnection(connectionId);
			onClose();
		}, [connectionId, onClose]);
		if (!isStreaming) return null;
		return (
			<MenuItem icon={<ProjectorScreenIcon weight="fill" className={styles.icon} />} onClick={handleClick}>
				{t`Turn Off Device Stream`}
			</MenuItem>
		);
	},
);

interface CopyDeviceIdMenuItemProps {
	onClose: () => void;
	connectionId: string;
}

export const CopyDeviceIdMenuItem: React.FC<CopyDeviceIdMenuItemProps> = observer(({connectionId, onClose}) => {
	const {t, i18n} = useLingui();
	const handleClick = React.useCallback(() => {
		TextCopyActionCreators.copy(i18n, connectionId, true).catch(() => {});
		onClose();
	}, [connectionId, onClose, i18n]);
	return (
		<MenuItem icon={<CopyIcon weight="fill" className={styles.icon} />} onClick={handleClick}>
			{t`Copy Device ID`}
		</MenuItem>
	);
});

interface BulkMuteDevicesMenuItemProps {
	userVoiceStates: Array<{connectionId: string; voiceState: VoiceState}>;
	onClose: () => void;
}

export const BulkMuteDevicesMenuItem: React.FC<BulkMuteDevicesMenuItemProps> = observer(
	({userVoiceStates, onClose}) => {
		const {t} = useLingui();
		const allMuted = React.useMemo(
			() => userVoiceStates.every(({voiceState}) => voiceState.self_mute),
			[userVoiceStates],
		);
		const handleClick = React.useCallback(() => {
			const connectionIds = userVoiceStates.map(({connectionId}) => connectionId);
			const targetMute = !allMuted;
			VoiceStateActionCreators.bulkMuteConnections(connectionIds, targetMute);
			if (targetMute) SoundActionCreators.playSound(SoundType.Mute);
			else SoundActionCreators.playSound(SoundType.Unmute);
			onClose();
		}, [userVoiceStates, allMuted, onClose]);
		return (
			<MenuItem icon={<MicrophoneSlashIcon weight="fill" className={styles.icon} />} onClick={handleClick}>
				{allMuted ? t`Unmute All Devices` : t`Mute All Devices`}
			</MenuItem>
		);
	},
);

interface BulkDeafenDevicesMenuItemProps {
	userVoiceStates: Array<{connectionId: string; voiceState: VoiceState}>;
	onClose: () => void;
}

export const BulkDeafenDevicesMenuItem: React.FC<BulkDeafenDevicesMenuItemProps> = observer(
	({userVoiceStates, onClose}) => {
		const {t} = useLingui();
		const allDeafened = React.useMemo(
			() => userVoiceStates.every(({voiceState}) => voiceState.self_deaf),
			[userVoiceStates],
		);
		const handleClick = React.useCallback(() => {
			const connectionIds = userVoiceStates.map(({connectionId}) => connectionId);
			const targetDeafen = !allDeafened;
			VoiceStateActionCreators.bulkDeafenConnections(connectionIds, targetDeafen);
			if (targetDeafen) SoundActionCreators.playSound(SoundType.Deaf);
			else SoundActionCreators.playSound(SoundType.Undeaf);
			onClose();
		}, [userVoiceStates, allDeafened, onClose]);
		return (
			<MenuItem icon={<SpeakerSlashIcon weight="fill" className={styles.icon} />} onClick={handleClick}>
				{allDeafened ? t`Undeafen All Devices` : t`Deafen All Devices`}
			</MenuItem>
		);
	},
);

interface BulkCameraDevicesMenuItemProps {
	userVoiceStates: Array<{connectionId: string; voiceState: VoiceState}>;
	onClose: () => void;
}

export const BulkCameraDevicesMenuItem: React.FC<BulkCameraDevicesMenuItemProps> = observer(
	({userVoiceStates, onClose}) => {
		const {t} = useLingui();
		const handleClick = React.useCallback(() => {
			const connectionIds = userVoiceStates.map(({connectionId}) => connectionId);
			VoiceStateActionCreators.bulkTurnOffCameras(connectionIds);
			onClose();
		}, [userVoiceStates, onClose]);
		return (
			<MenuItem icon={<VideoIcon weight="fill" className={styles.icon} />} onClick={handleClick}>
				{t`Turn Off All Device Cameras`}
			</MenuItem>
		);
	},
);

interface BulkDisconnectDevicesMenuItemProps {
	userVoiceStates: Array<{connectionId: string; voiceState: VoiceState}>;
	onClose: () => void;
}

export const BulkDisconnectDevicesMenuItem: React.FC<BulkDisconnectDevicesMenuItemProps> = observer(
	({userVoiceStates, onClose}) => {
		const {t} = useLingui();
		const handleClick = React.useCallback(async () => {
			await VoiceStateActionCreators.bulkDisconnect(userVoiceStates.map(({connectionId}) => connectionId));
			onClose();
		}, [userVoiceStates, onClose]);
		return (
			<MenuItem icon={<PhoneXIcon weight="fill" className={styles.icon} />} onClick={handleClick} danger>
				{t`Disconnect All Devices`}
			</MenuItem>
		);
	},
);

interface FocusParticipantMenuItemProps {
	userId: string;
	connectionId: string;
	onClose: () => void;
}

export const FocusParticipantMenuItem: React.FC<FocusParticipantMenuItemProps> = observer(
	({userId, connectionId, onClose}) => {
		const {t} = useLingui();
		const identity = `user_${userId}_${connectionId}`;
		const isFocused = VoiceCallLayoutStore.pinnedParticipantIdentity === identity;
		const hasMultipleConnections = React.useMemo(() => {
			const allStates = MediaEngineStore.getAllVoiceStates();
			let count = 0;
			Object.values(allStates).forEach((guildData: any) => {
				Object.values(guildData).forEach((channelData: any) => {
					Object.values(channelData).forEach((vs: any) => {
						if ((vs as any)?.user_id === userId) count++;
					});
				});
			});
			return count > 1;
		}, [userId]);
		const handleClick = React.useCallback(() => {
			if (isFocused) {
				VoiceCallLayoutActionCreators.setPinnedParticipant(null);
				VoiceCallLayoutActionCreators.setLayoutMode('grid');
				VoiceCallLayoutActionCreators.markUserOverride();
			} else {
				VoiceCallLayoutActionCreators.setLayoutMode('focus');
				VoiceCallLayoutActionCreators.setPinnedParticipant(identity);
				VoiceCallLayoutActionCreators.markUserOverride();
			}
			onClose();
		}, [identity, onClose, isFocused]);
		return (
			<MenuItem
				icon={
					isFocused ? (
						<EyeSlashIcon weight="fill" className={styles.icon} />
					) : (
						<EyeIcon weight="fill" className={styles.icon} />
					)
				}
				onClick={handleClick}
			>
				{isFocused ? t`Unfocus` : hasMultipleConnections ? t`Focus This Device` : t`Focus This Person`}
			</MenuItem>
		);
	},
);
