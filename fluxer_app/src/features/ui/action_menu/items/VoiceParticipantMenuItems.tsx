// SPDX-License-Identifier: AGPL-3.0-or-later

import * as VoiceStateCommands from '@app/features/devtools/commands/VoiceStateCommands';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import {TURN_OFF_CAMERA_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {
	BulkTurnOffCameraIcon,
	CopyIdIcon,
	DisconnectIcon,
	FocusIcon,
	GuildDeafenIcon,
	GuildMuteIcon,
	LocalDisableVideoIcon,
	LocalMuteIcon,
	SelfDeafenIcon,
	SelfMuteIcon,
	SettingsIcon,
	TurnOffCameraIcon,
	TurnOffStreamIcon,
	UnfocusIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import styles from '@app/features/ui/action_menu/items/MenuItems.module.css';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSlider} from '@app/features/ui/action_menu/MenuItemSlider';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import Users from '@app/features/user/state/Users';
import * as VoiceCallLayoutCommands from '@app/features/voice/commands/VoiceCallLayoutCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import CallMediaPrefs from '@app/features/voice/state/CallMediaPrefs';
import EntranceSoundListenerPrefs from '@app/features/voice/state/EntranceSoundListenerPrefs';
import ParticipantVolume from '@app/features/voice/state/ParticipantVolume';
import VoiceCallLayout from '@app/features/voice/state/VoiceCallLayout';
import {
	getVoiceVideoSettingsLabel,
	VOICE_COMMUNITY_DEAFEN_DESCRIPTOR,
	VOICE_COMMUNITY_MUTE_DESCRIPTOR,
	VOICE_DEAFEN_ALL_DEVICES_DESCRIPTOR,
	VOICE_DEAFEN_DESCRIPTOR,
	VOICE_DISCONNECT_ALL_DEVICES_DESCRIPTOR,
	VOICE_DISCONNECT_DESCRIPTOR,
	VOICE_DISCONNECT_DEVICE_DESCRIPTOR,
	VOICE_MUTE_ALL_DEVICES_DESCRIPTOR,
	VOICE_UNDEAFEN_ALL_DEVICES_DESCRIPTOR,
	VOICE_UNMUTE_ALL_DEVICES_DESCRIPTOR,
	VOICE_USER_VOLUME_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {buildVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {Track} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	context: 'voice-control-action',
	comment: "Voice menu checkbox label. Mutes the current user's microphone, not what they hear.",
});
const TURN_OFF_STREAM_DESCRIPTOR = msg({
	message: 'Turn off stream',
	comment: "Voice menu action that stops the current user's screen share or stream.",
});
const MUTE_2_DESCRIPTOR = msg({
	message: 'Mute',
	context: 'playback-control-action',
	comment: 'Voice menu checkbox label. Locally mutes another participant only for the current user.',
});
const DISABLE_VIDEO_LOCALLY_DESCRIPTOR = msg({
	message: 'Disable video locally',
	comment: "Voice menu checkbox label. Hides another participant's video only for the current user.",
});
const TURN_OFF_DEVICE_CAMERA_DESCRIPTOR = msg({
	message: 'Turn off device camera',
	comment: 'Voice menu action that turns off the camera on one device/session.',
});
const TURN_OFF_DEVICE_STREAM_DESCRIPTOR = msg({
	message: 'Turn off device stream',
	comment: 'Voice menu action that stops screen sharing or streaming on one device/session.',
});
const COPY_DEVICE_ID_DESCRIPTOR = msg({
	message: 'Copy device ID',
	comment: 'Developer voice menu action that copies a voice device/session ID.',
});
const TURN_OFF_ALL_DEVICE_CAMERAS_DESCRIPTOR = msg({
	message: 'Turn off all device cameras',
	comment: "Voice menu action that turns off every active device/session's camera.",
});
const UNFOCUS_DESCRIPTOR = msg({
	message: 'Unfocus',
	comment: 'Voice layout menu action that removes the focused participant or stream.',
});
const FOCUS_THIS_DEVICE_DESCRIPTOR = msg({
	message: 'Focus this device',
	comment: 'Voice layout menu action that focuses one device/session for a participant.',
});
const FOCUS_THIS_PERSON_DESCRIPTOR = msg({
	message: 'Focus this person',
	comment: 'Voice layout menu action that focuses one participant in the call view.',
});
const logger = new Logger('VoiceParticipantMenuItems');

interface SelfMuteMenuItemProps {
	onClose: () => void;
	connectionId?: string;
	isDeviceSpecific?: boolean;
	label?: string;
}

export const SelfMuteMenuItem: React.FC<SelfMuteMenuItemProps> = observer(
	({connectionId, isDeviceSpecific = false, label}) => {
		const {i18n} = useLingui();
		const voiceState = connectionId
			? MediaEngine.getVoiceStateByConnectionId(connectionId)
			: MediaEngine.getCurrentUserVoiceState();
		const isSelfMuted = voiceState?.self_mute ?? false;
		const handleToggle = useCallback(() => {
			if (isDeviceSpecific && connectionId) {
				VoiceStateCommands.toggleSelfMuteForConnection(connectionId);
			} else {
				VoiceStateCommands.toggleSelfMute(null);
			}
		}, [connectionId, isDeviceSpecific]);
		return (
			<CheckboxItem
				icon={
					<SelfMuteIcon
						className={styles.icon}
						data-flx="ui.action-menu.items.voice-participant-menu-items.self-mute-menu-item.icon"
					/>
				}
				checked={isSelfMuted}
				onCheckedChange={handleToggle}
				data-flx="ui.action-menu.items.voice-participant-menu-items.self-mute-menu-item.checkbox-item"
			>
				{label ?? i18n._(MUTE_DESCRIPTOR)}
			</CheckboxItem>
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
		const {i18n} = useLingui();
		const voiceState = connectionId
			? MediaEngine.getVoiceStateByConnectionId(connectionId)
			: MediaEngine.getCurrentUserVoiceState();
		const isSelfDeafened = voiceState?.self_deaf ?? false;
		const handleToggle = useCallback(() => {
			if (isDeviceSpecific && connectionId) {
				VoiceStateCommands.toggleSelfDeafenForConnection(connectionId);
			} else {
				VoiceStateCommands.toggleSelfDeaf(null);
			}
		}, [connectionId, isDeviceSpecific]);
		return (
			<CheckboxItem
				icon={
					<SelfDeafenIcon
						className={styles.icon}
						data-flx="ui.action-menu.items.voice-participant-menu-items.self-deafen-menu-item.icon"
					/>
				}
				checked={isSelfDeafened}
				onCheckedChange={handleToggle}
				data-flx="ui.action-menu.items.voice-participant-menu-items.self-deafen-menu-item.checkbox-item"
			>
				{label ?? i18n._(VOICE_DEAFEN_DESCRIPTOR)}
			</CheckboxItem>
		);
	},
);

interface VoiceVideoSettingsMenuItemProps {
	onClose: () => void;
}

export const VoiceVideoSettingsMenuItem: React.FC<VoiceVideoSettingsMenuItemProps> = observer(({onClose}) => {
	const {i18n} = useLingui();
	const handleClick = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<UserSettingsModal
					initialTab="voice_video"
					data-flx="ui.action-menu.items.voice-participant-menu-items.handle-click.user-settings-modal"
				/>
			)),
		);
	}, [onClose]);
	return (
		<MenuItem
			icon={
				<SettingsIcon
					className={styles.icon}
					data-flx="ui.action-menu.items.voice-participant-menu-items.voice-video-settings-menu-item.icon"
				/>
			}
			onClick={handleClick}
			data-flx="ui.action-menu.items.voice-participant-menu-items.voice-video-settings-menu-item.menu-item.click"
		>
			{getVoiceVideoSettingsLabel(i18n)}
		</MenuItem>
	);
});

interface SelfTurnOffCameraMenuItemProps {
	onClose: () => void;
}

export const SelfTurnOffCameraMenuItem: React.FC<SelfTurnOffCameraMenuItemProps> = observer(({onClose}) => {
	const {i18n} = useLingui();
	const connectionId = MediaEngine.connectionId;
	const voiceState = connectionId ? MediaEngine.getVoiceStateByConnectionId(connectionId) : null;
	const isCameraOn = voiceState?.self_video ?? false;
	const handleClick = useCallback(() => {
		if (connectionId) VoiceStateCommands.turnOffCameraForConnection(connectionId);
		onClose();
	}, [connectionId, onClose]);
	if (!isCameraOn) return null;
	return (
		<MenuItem
			icon={
				<TurnOffCameraIcon
					className={styles.icon}
					data-flx="ui.action-menu.items.voice-participant-menu-items.self-turn-off-camera-menu-item.icon"
				/>
			}
			onClick={handleClick}
			data-flx="ui.action-menu.items.voice-participant-menu-items.self-turn-off-camera-menu-item.menu-item.click"
		>
			{i18n._(TURN_OFF_CAMERA_DESCRIPTOR)}
		</MenuItem>
	);
});

interface SelfTurnOffStreamMenuItemProps {
	onClose: () => void;
}

export const SelfTurnOffStreamMenuItem: React.FC<SelfTurnOffStreamMenuItemProps> = observer(({onClose}) => {
	const {i18n} = useLingui();
	const connectionId = MediaEngine.connectionId;
	const voiceState = connectionId ? MediaEngine.getVoiceStateByConnectionId(connectionId) : null;
	const isStreaming = voiceState?.self_stream ?? false;
	const handleClick = useCallback(() => {
		if (connectionId) VoiceStateCommands.turnOffStreamForConnection(connectionId);
		onClose();
	}, [connectionId, onClose]);
	if (!isStreaming) return null;
	return (
		<MenuItem
			icon={
				<TurnOffStreamIcon
					className={styles.icon}
					data-flx="ui.action-menu.items.voice-participant-menu-items.self-turn-off-stream-menu-item.icon"
				/>
			}
			onClick={handleClick}
			data-flx="ui.action-menu.items.voice-participant-menu-items.self-turn-off-stream-menu-item.menu-item.click"
		>
			{i18n._(TURN_OFF_STREAM_DESCRIPTOR)}
		</MenuItem>
	);
});

interface ParticipantVolumeSliderProps {
	userId: string;
}

export const ParticipantVolumeSlider: React.FC<ParticipantVolumeSliderProps> = observer(({userId}) => {
	const {i18n} = useLingui();
	const participantVolume = ParticipantVolume.getVolume(userId);
	const handleChange = useCallback(
		(value: number) => {
			ParticipantVolume.setVolume(userId, value);
			MediaEngine.applyLocalAudioPreferencesForUser(userId);
		},
		[userId],
	);
	return (
		<MenuItemSlider
			label={i18n._(VOICE_USER_VOLUME_DESCRIPTOR)}
			value={participantVolume}
			minValue={0}
			maxValue={200}
			onChange={handleChange}
			data-flx="ui.action-menu.items.voice-participant-menu-items.participant-volume-slider.menu-item-slider.change"
		/>
	);
});

const ENTRANCE_SOUND_SUBMENU_DESCRIPTOR = msg({
	message: 'Entrance sound',
	comment: 'Voice menu submenu label that contains per-user entrance sound mute and volume controls.',
});
const MUTE_ENTRANCE_SOUND_DESCRIPTOR = msg({
	message: 'Mute entrance sound',
	comment:
		"Voice menu checkbox label. Locally silences this user's custom entrance sound when they join, only for the current user.",
});
const ENTRANCE_SOUND_VOLUME_DESCRIPTOR = msg({
	message: 'Entrance sound volume',
	comment: "Voice menu slider label adjusting playback volume of this user's entrance sound, locally only.",
});

interface EntranceSoundListenerSubmenuProps {
	userId: string;
}

export const EntranceSoundListenerSubmenu: React.FC<EntranceSoundListenerSubmenuProps> = observer(({userId}) => {
	const {i18n} = useLingui();
	const isMuted = EntranceSoundListenerPrefs.isMuted(userId);
	const volume = EntranceSoundListenerPrefs.getVolume(userId);
	const handleMuteToggle = useCallback(
		(checked: boolean) => {
			EntranceSoundListenerPrefs.setMuted(userId, checked);
		},
		[userId],
	);
	const handleVolumeChange = useCallback(
		(value: number) => {
			EntranceSoundListenerPrefs.setVolume(userId, value);
		},
		[userId],
	);
	return (
		<MenuItemSubmenu
			label={i18n._(ENTRANCE_SOUND_SUBMENU_DESCRIPTOR)}
			render={() => (
				<>
					<CheckboxItem
						checked={isMuted}
						onCheckedChange={handleMuteToggle}
						data-flx="ui.action-menu.items.voice-participant-menu-items.entrance-sound-mute-checkbox"
					>
						{i18n._(MUTE_ENTRANCE_SOUND_DESCRIPTOR)}
					</CheckboxItem>
					<MenuItemSlider
						label={i18n._(ENTRANCE_SOUND_VOLUME_DESCRIPTOR)}
						value={volume}
						minValue={0}
						maxValue={200}
						onChange={handleVolumeChange}
						data-flx="ui.action-menu.items.voice-participant-menu-items.entrance-sound-volume-slider"
					/>
				</>
			)}
			data-flx="ui.action-menu.items.voice-participant-menu-items.entrance-sound-submenu"
		/>
	);
});

interface LocalMuteParticipantMenuItemProps {
	userId: string;
	onClose: () => void;
}

export const LocalMuteParticipantMenuItem: React.FC<LocalMuteParticipantMenuItemProps> = observer(({userId}) => {
	const {i18n} = useLingui();
	const isLocalMuted = ParticipantVolume.isLocalMuted(userId);
	const handleToggle = useCallback(
		(checked: boolean) => {
			ParticipantVolume.setLocalMute(userId, checked);
			MediaEngine.applyLocalAudioPreferencesForUser(userId);
		},
		[userId],
	);
	return (
		<CheckboxItem
			icon={
				<LocalMuteIcon
					className={styles.icon}
					data-flx="ui.action-menu.items.voice-participant-menu-items.local-mute-participant-menu-item.icon"
				/>
			}
			checked={isLocalMuted}
			onCheckedChange={handleToggle}
			data-flx="ui.action-menu.items.voice-participant-menu-items.local-mute-participant-menu-item.checkbox-item"
		>
			{i18n._(MUTE_2_DESCRIPTOR)}
		</CheckboxItem>
	);
});

interface LocalDisableVideoMenuItemProps {
	userId: string;
	connectionId: string;
}

export const LocalDisableVideoMenuItem: React.FC<LocalDisableVideoMenuItemProps> = observer(
	({userId, connectionId}) => {
		const {i18n} = useLingui();
		const callId = MediaEngine.connectionId ?? '';
		const identity = buildVoiceParticipantIdentity(userId, connectionId);
		const disabled = callId ? CallMediaPrefs.isVideoDisabled(callId, identity) : false;
		const handleToggle = useCallback(
			(checked: boolean) => {
				const id = MediaEngine.connectionId ?? '';
				if (!id) {
					const error = new Error('Unable to toggle local video without an active connection');
					logger.error('Local disable video toggle invoked without connection id', {
						userId,
						connectionId,
						identity,
					});
					throw error;
				}
				MediaEngine.setLocalVideoDisabled(identity, checked);
			},
			[identity, connectionId, userId],
		);
		return (
			<CheckboxItem
				icon={
					<LocalDisableVideoIcon
						className={styles.icon}
						data-flx="ui.action-menu.items.voice-participant-menu-items.local-disable-video-menu-item.icon"
					/>
				}
				checked={disabled}
				onCheckedChange={handleToggle}
				data-flx="ui.action-menu.items.voice-participant-menu-items.local-disable-video-menu-item.checkbox-item"
			>
				{i18n._(DISABLE_VIDEO_LOCALLY_DESCRIPTOR)}
			</CheckboxItem>
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
	const {i18n} = useLingui();
	const member = GuildMembers.getMember(guildId, userId);
	const isGuildMuted = member?.mute ?? false;
	const isTimedOut = member?.isTimedOut() ?? false;
	const handleToggle = useCallback(
		async (checked: boolean) => {
			try {
				await GuildMemberCommands.update(guildId, userId, {mute: checked});
				if (checked) SoundCommands.playSound(SoundType.Mute);
				else SoundCommands.playSound(SoundType.Unmute);
			} catch {}
		},
		[guildId, userId],
	);
	return (
		<CheckboxItem
			icon={
				<GuildMuteIcon
					className={styles.icon}
					data-flx="ui.action-menu.items.voice-participant-menu-items.guild-mute-menu-item.icon"
				/>
			}
			checked={!!isGuildMuted}
			onCheckedChange={handleToggle}
			danger
			disabled={isTimedOut}
			data-flx="ui.action-menu.items.voice-participant-menu-items.guild-mute-menu-item.checkbox-item"
		>
			{i18n._(VOICE_COMMUNITY_MUTE_DESCRIPTOR)}
		</CheckboxItem>
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
	const {i18n} = useLingui();
	const member = GuildMembers.getMember(guildId, userId);
	const isGuildDeafened = member?.deaf ?? false;
	const handleToggle = useCallback(
		async (checked: boolean) => {
			try {
				await GuildMemberCommands.update(guildId, userId, {deaf: checked});
				if (checked) SoundCommands.playSound(SoundType.Deaf);
				else SoundCommands.playSound(SoundType.Undeaf);
			} catch {}
		},
		[guildId, userId],
	);
	return (
		<CheckboxItem
			icon={
				<GuildDeafenIcon
					className={styles.icon}
					data-flx="ui.action-menu.items.voice-participant-menu-items.guild-deafen-menu-item.icon"
				/>
			}
			checked={!!isGuildDeafened}
			onCheckedChange={handleToggle}
			danger
			data-flx="ui.action-menu.items.voice-participant-menu-items.guild-deafen-menu-item.checkbox-item"
		>
			{i18n._(VOICE_COMMUNITY_DEAFEN_DESCRIPTOR)}
		</CheckboxItem>
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
		const {i18n} = useLingui();
		const currentUser = Users.currentUser;
		const isSelf = currentUser?.id === userId;
		const handleClick = useCallback(async () => {
			onClose();
			if (isSelf) {
				const isCurrentDevice = !connectionId || connectionId === MediaEngine.connectionId;
				if (isCurrentDevice) {
					await MediaEngine.disconnectFromVoiceChannel('user');
				} else {
					const cid = connectionId ?? MediaEngine.connectionId ?? null;
					if (cid) {
						MediaEngine.disconnectRemoteDevice(guildId, cid);
					}
				}
			} else {
				try {
					await GuildMemberCommands.update(guildId, userId, {channel_id: null, connection_id: connectionId});
				} catch {}
			}
		}, [guildId, userId, connectionId, onClose, isSelf]);
		const defaultLabel = i18n._(connectionId ? VOICE_DISCONNECT_DEVICE_DESCRIPTOR : VOICE_DISCONNECT_DESCRIPTOR);
		return (
			<MenuItem
				icon={
					<DisconnectIcon
						className={styles.icon}
						data-flx="ui.action-menu.items.voice-participant-menu-items.disconnect-participant-menu-item.icon"
					/>
				}
				onClick={handleClick}
				danger
				data-flx="ui.action-menu.items.voice-participant-menu-items.disconnect-participant-menu-item.menu-item.click"
			>
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
		const {i18n} = useLingui();
		const voiceState = MediaEngine.getVoiceStateByConnectionId(connectionId);
		const isCameraOn = voiceState?.self_video ?? false;
		const handleClick = useCallback(() => {
			VoiceStateCommands.turnOffCameraForConnection(connectionId);
			onClose();
		}, [connectionId, onClose]);
		if (!isCameraOn) return null;
		return (
			<MenuItem
				icon={
					<TurnOffCameraIcon
						className={styles.icon}
						data-flx="ui.action-menu.items.voice-participant-menu-items.turn-off-device-camera-menu-item.icon"
					/>
				}
				onClick={handleClick}
				data-flx="ui.action-menu.items.voice-participant-menu-items.turn-off-device-camera-menu-item.menu-item.click"
			>
				{i18n._(TURN_OFF_DEVICE_CAMERA_DESCRIPTOR)}
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
		const {i18n} = useLingui();
		const voiceState = MediaEngine.getVoiceStateByConnectionId(connectionId);
		const isStreaming = voiceState?.self_stream ?? false;
		const handleClick = useCallback(() => {
			VoiceStateCommands.turnOffStreamForConnection(connectionId);
			onClose();
		}, [connectionId, onClose]);
		if (!isStreaming) return null;
		return (
			<MenuItem
				icon={
					<TurnOffStreamIcon
						className={styles.icon}
						data-flx="ui.action-menu.items.voice-participant-menu-items.turn-off-device-stream-menu-item.icon"
					/>
				}
				onClick={handleClick}
				data-flx="ui.action-menu.items.voice-participant-menu-items.turn-off-device-stream-menu-item.menu-item.click"
			>
				{i18n._(TURN_OFF_DEVICE_STREAM_DESCRIPTOR)}
			</MenuItem>
		);
	},
);

interface CopyDeviceIdMenuItemProps {
	onClose: () => void;
	connectionId: string;
}

export const CopyDeviceIdMenuItem: React.FC<CopyDeviceIdMenuItemProps> = observer(({connectionId, onClose}) => {
	const {i18n} = useLingui();
	const handleClick = useCallback(() => {
		TextCopyCommands.copy(i18n, connectionId, true).catch(() => {});
		onClose();
	}, [connectionId, onClose, i18n]);
	return (
		<MenuItem
			icon={
				<CopyIdIcon
					className={styles.icon}
					data-flx="ui.action-menu.items.voice-participant-menu-items.copy-device-id-menu-item.icon"
				/>
			}
			onClick={handleClick}
			data-flx="ui.action-menu.items.voice-participant-menu-items.copy-device-id-menu-item.menu-item.click"
		>
			{i18n._(COPY_DEVICE_ID_DESCRIPTOR)}
		</MenuItem>
	);
});

interface BulkMuteDevicesMenuItemProps {
	userVoiceStates: Array<{connectionId: string; voiceState: VoiceState}>;
	onClose: () => void;
}

export const BulkMuteDevicesMenuItem: React.FC<BulkMuteDevicesMenuItemProps> = observer(
	({userVoiceStates, onClose}) => {
		const {i18n} = useLingui();
		const allMuted = useMemo(() => userVoiceStates.every(({voiceState}) => voiceState.self_mute), [userVoiceStates]);
		const handleClick = useCallback(() => {
			const connectionIds = userVoiceStates.map(({connectionId}) => connectionId);
			const targetMute = !allMuted;
			VoiceStateCommands.bulkMuteConnections(connectionIds, targetMute);
			if (targetMute) SoundCommands.playSound(SoundType.Mute);
			else SoundCommands.playSound(SoundType.Unmute);
			onClose();
		}, [userVoiceStates, allMuted, onClose]);
		return (
			<MenuItem
				icon={
					<GuildMuteIcon
						className={styles.icon}
						data-flx="ui.action-menu.items.voice-participant-menu-items.bulk-mute-devices-menu-item.icon"
					/>
				}
				onClick={handleClick}
				data-flx="ui.action-menu.items.voice-participant-menu-items.bulk-mute-devices-menu-item.menu-item.click"
			>
				{i18n._(allMuted ? VOICE_UNMUTE_ALL_DEVICES_DESCRIPTOR : VOICE_MUTE_ALL_DEVICES_DESCRIPTOR)}
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
		const {i18n} = useLingui();
		const allDeafened = useMemo(() => userVoiceStates.every(({voiceState}) => voiceState.self_deaf), [userVoiceStates]);
		const handleClick = useCallback(() => {
			const connectionIds = userVoiceStates.map(({connectionId}) => connectionId);
			const targetDeafen = !allDeafened;
			VoiceStateCommands.bulkDeafenConnections(connectionIds, targetDeafen);
			if (targetDeafen) SoundCommands.playSound(SoundType.Deaf);
			else SoundCommands.playSound(SoundType.Undeaf);
			onClose();
		}, [userVoiceStates, allDeafened, onClose]);
		return (
			<MenuItem
				icon={
					<GuildDeafenIcon
						className={styles.icon}
						data-flx="ui.action-menu.items.voice-participant-menu-items.bulk-deafen-devices-menu-item.icon"
					/>
				}
				onClick={handleClick}
				data-flx="ui.action-menu.items.voice-participant-menu-items.bulk-deafen-devices-menu-item.menu-item.click"
			>
				{i18n._(allDeafened ? VOICE_UNDEAFEN_ALL_DEVICES_DESCRIPTOR : VOICE_DEAFEN_ALL_DEVICES_DESCRIPTOR)}
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
		const {i18n} = useLingui();
		const handleClick = useCallback(() => {
			const connectionIds = userVoiceStates.map(({connectionId}) => connectionId);
			VoiceStateCommands.bulkTurnOffCameras(connectionIds);
			onClose();
		}, [userVoiceStates, onClose]);
		return (
			<MenuItem
				icon={
					<BulkTurnOffCameraIcon
						className={styles.icon}
						data-flx="ui.action-menu.items.voice-participant-menu-items.bulk-camera-devices-menu-item.icon"
					/>
				}
				onClick={handleClick}
				data-flx="ui.action-menu.items.voice-participant-menu-items.bulk-camera-devices-menu-item.menu-item.click"
			>
				{i18n._(TURN_OFF_ALL_DEVICE_CAMERAS_DESCRIPTOR)}
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
		const {i18n} = useLingui();
		const handleClick = useCallback(async () => {
			await VoiceStateCommands.bulkDisconnect(userVoiceStates.map(({connectionId}) => connectionId));
			onClose();
		}, [userVoiceStates, onClose]);
		return (
			<MenuItem
				icon={
					<DisconnectIcon
						className={styles.icon}
						data-flx="ui.action-menu.items.voice-participant-menu-items.bulk-disconnect-devices-menu-item.icon"
					/>
				}
				onClick={handleClick}
				danger
				data-flx="ui.action-menu.items.voice-participant-menu-items.bulk-disconnect-devices-menu-item.menu-item.click"
			>
				{i18n._(VOICE_DISCONNECT_ALL_DEVICES_DESCRIPTOR)}
			</MenuItem>
		);
	},
);

interface FocusParticipantMenuItemProps {
	userId: string;
	connectionId: string;
	isScreenShare?: boolean;
	onClose: () => void;
}

export const FocusParticipantMenuItem: React.FC<FocusParticipantMenuItemProps> = observer(
	({userId, connectionId, isScreenShare = false, onClose}) => {
		const {i18n} = useLingui();
		const identity = buildVoiceParticipantIdentity(userId, connectionId);
		const pinnedParticipantSource = VoiceCallLayout.pinnedParticipantSource;
		const isFocused =
			VoiceCallLayout.pinnedParticipantIdentity === identity &&
			(pinnedParticipantSource == null ||
				pinnedParticipantSource === (isScreenShare ? Track.Source.ScreenShare : Track.Source.Camera));
		const hasMultipleConnections = useMemo(() => {
			const allStates = MediaEngine.getAllVoiceStates();
			let count = 0;
			Object.values(allStates).forEach((guildData) => {
				Object.values(guildData).forEach((channelData) => {
					Object.values(channelData).forEach((vs: VoiceState) => {
						if (vs.user_id === userId) count++;
					});
				});
			});
			return count > 1;
		}, [userId]);
		const handleClick = useCallback(() => {
			if (isFocused) {
				VoiceCallLayoutCommands.setPinnedParticipant(null);
				VoiceCallLayoutCommands.setLayoutMode('grid');
				VoiceCallLayoutCommands.markUserOverride();
			} else {
				VoiceCallLayoutCommands.setLayoutMode('focus');
				VoiceCallLayoutCommands.setPinnedParticipant(
					identity,
					isScreenShare ? Track.Source.ScreenShare : Track.Source.Camera,
				);
				VoiceCallLayoutCommands.markUserOverride();
			}
			onClose();
		}, [identity, onClose, isFocused, isScreenShare]);
		const focusLabel = (() => {
			if (isFocused) {
				return i18n._(UNFOCUS_DESCRIPTOR);
			}
			if (hasMultipleConnections) {
				return i18n._(FOCUS_THIS_DEVICE_DESCRIPTOR);
			}
			return i18n._(FOCUS_THIS_PERSON_DESCRIPTOR);
		})();
		return (
			<MenuItem
				icon={
					isFocused ? (
						<UnfocusIcon
							className={styles.icon}
							data-flx="ui.action-menu.items.voice-participant-menu-items.focus-participant-menu-item.icon"
						/>
					) : (
						<FocusIcon
							className={styles.icon}
							data-flx="ui.action-menu.items.voice-participant-menu-items.focus-participant-menu-item.icon--2"
						/>
					)
				}
				onClick={handleClick}
				data-flx="ui.action-menu.items.voice-participant-menu-items.focus-participant-menu-item.menu-item.click"
			>
				{focusLabel}
			</MenuItem>
		);
	},
);
