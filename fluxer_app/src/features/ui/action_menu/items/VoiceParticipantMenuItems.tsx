import * as VoiceStateCommands from '@app/features/devtools/commands/VoiceStateCommands';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {LocalMuteIcon, SelfDeafenIcon, SelfMuteIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import styles from '@app/features/ui/action_menu/items/MenuItems.module.css';
import {MenuItemSlider} from '@app/features/ui/action_menu/MenuItemSlider';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import EntranceSoundListenerPrefs from '@app/features/voice/state/EntranceSoundListenerPrefs';
import ParticipantVolume from '@app/features/voice/state/ParticipantVolume';
import {VOICE_DEAFEN_DESCRIPTOR, VOICE_USER_VOLUME_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	context: 'voice-control-action',
	comment: "Voice menu checkbox label. Mutes the current user's microphone, not what they hear.",
});
const MUTE_2_DESCRIPTOR = msg({
	message: 'Mute',
	context: 'playback-control-action',
	comment: 'Voice menu checkbox label. Locally mutes another participant only for the current user.',
});

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
