// SPDX-License-Identifier: AGPL-3.0-or-later

import * as VoiceStateCommands from '@app/features/devtools/commands/VoiceStateCommands';
import {CAMERA_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Keybind from '@app/features/input/state/InputKeybind';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {PRIORITIZE_SPEAKERS_DESCRIPTOR} from '@app/features/ui/action_menu/items/voice_participant_menu_data/shared';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import {MenuItemSlider} from '@app/features/ui/action_menu/MenuItemSlider';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {AudioLevelMeter} from '@app/features/user/components/modals/tabs/components/AudioLevelMeter';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import Users from '@app/features/user/state/Users';
import * as CallCommands from '@app/features/voice/commands/CallCommands';
import * as VoiceCallLayoutCommands from '@app/features/voice/commands/VoiceCallLayoutCommands';
import * as VoiceDebugEventSinkCommands from '@app/features/voice/commands/VoiceDebugEventSinkCommands';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import {CameraPreviewModalInRoom} from '@app/features/voice/components/modals/CameraPreviewModal';
import {HideOwnCameraConfirmModal} from '@app/features/voice/components/modals/HideOwnCameraConfirmModal';
import styles from '@app/features/voice/components/VoiceSettingsMenus.module.css';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {
	getLocalPublicationMediaStreamTrack,
	getPrimaryLocalMicrophonePublication,
} from '@app/features/voice/engine/VoiceTrackPublicationUtils';
import CallState from '@app/features/voice/state/CallState';
import VoiceCallLayout from '@app/features/voice/state/VoiceCallLayout';
import VoicePrompts from '@app/features/voice/state/VoicePrompts';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {hasDeviceLabels, resolveEffectiveDeviceId} from '@app/features/voice/utils/VoiceDeviceManager';
import {
	formatFallbackCameraLabel,
	formatVoiceAudioDeviceLabel,
	getVoiceDeafenedByModeratorsStatusLabel,
	getVoiceVideoSettingsLabel,
	VOICE_DEAFEN_DESCRIPTOR,
	VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR,
	VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR,
	VOICE_INPUT_DEVICE_DESCRIPTOR,
	VOICE_INPUT_VOLUME_DESCRIPTOR,
	VOICE_OUTPUT_DEVICE_DESCRIPTOR,
	VOICE_OUTPUT_VOLUME_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {getActiveVoiceProcessingMode, type VoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';
import {VOICE_VOLUME_MAX_PERCENT} from '@app/features/voice/utils/VoiceVolumeUtils';
import {AUTOMATIC_VOICE_REGION_ID} from '@fluxer/constants/src/ChannelConstants';
import type {RtcRegionResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	CameraIcon,
	ChartBarIcon,
	EyeIcon,
	GearIcon,
	GridFourIcon,
	HandTapIcon,
	SpeakerSlashIcon,
	UsersIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const CUSTOM_DESCRIPTOR = msg({
	message: 'Custom',
	comment: 'Voice input processing profile where the user configures processing manually.',
	context: 'voice-processing-profile',
});
const INPUT_PROFILE_DESCRIPTOR = msg({
	message: 'Input profile',
	comment: 'Voice settings menu label for microphone processing profile.',
});
const MICROPHONE_2_DESCRIPTOR = msg({
	message: 'Microphone',
	comment: 'Fallback microphone label in the voice settings menu when the OS does not report a device name.',
});
const SPEAKER_2_DESCRIPTOR = msg({
	message: 'Speaker',
	comment: 'Fallback speaker label in the voice settings menu when the OS does not report a device name.',
});
const DEFAULT_3_DESCRIPTOR = msg({
	message: 'Default',
	comment:
		'Default device option label in the voice settings menu (alternate call site for the same concept as DEFAULT_DESCRIPTOR).',
});
const AUTOMATIC_DESCRIPTOR = msg({
	message: 'Automatic',
	comment:
		'Voice region option label in the voice settings menu meaning let Fluxer pick the best region automatically.',
});
const VOICE_REGION_DESCRIPTOR = msg({
	message: 'Voice region',
	comment: 'Section header in the voice settings menu for the voice region picker.',
});
const INPUT_LEVEL_DESCRIPTOR = msg({
	message: 'Input level',
	comment: 'Read-only microphone input level shown in the voice device menu.',
});
const VOICE_SETTINGS_DESCRIPTOR = msg({
	message: 'Voice settings',
	comment: 'Menu action that opens voice settings.',
});
const VIDEO_SETTINGS_DESCRIPTOR = msg({
	message: 'Video settings',
	comment: 'Menu action that opens video settings.',
});
const MIRROR_CAMERA_DESCRIPTOR = msg({
	message: 'Mirror camera',
	comment: 'Camera settings menu checkbox for flipping the local camera preview horizontally.',
});
const logger = new Logger('VoiceSettingsMenus');

const INPUT_LEVEL_MIN_DB = -60;
const INPUT_LEVEL_MAX_DB = 0;

type VoiceVideoSettingsSection = 'audio' | 'video';

export function openVoiceVideoSettings(onClose: () => void, section?: VoiceVideoSettingsSection): void {
	ModalCommands.pushAfterBottomSheetClose(
		onClose,
		modal(() => (
			<UserSettingsModal
				initialTab="voice_video"
				initialSubtab={section}
				data-flx="voice.voice-settings-menus.open-voice-video-settings.user-settings-modal"
			/>
		)),
	);
}

interface VoiceAudioDeviceSubmenuProps {
	devices: Array<MediaDeviceInfo>;
	deviceType: 'input' | 'output';
}

const VoiceAudioDeviceSubmenu: React.FC<VoiceAudioDeviceSubmenuProps> = observer(({devices, deviceType}) => {
	const {i18n} = useLingui();
	const isInput = deviceType === 'input';
	const storedDeviceId = isInput ? VoiceSettings.inputDeviceId : VoiceSettings.outputDeviceId;
	const effectiveDeviceId = resolveEffectiveDeviceId(storedDeviceId, devices);
	const effectiveDevice = devices.find((device) => device.deviceId === effectiveDeviceId) ?? null;
	const fallbackLabel = i18n._(isInput ? MICROPHONE_2_DESCRIPTOR : SPEAKER_2_DESCRIPTOR);
	const currentDeviceLabel = effectiveDevice
		? formatVoiceAudioDeviceLabel(i18n, effectiveDevice, fallbackLabel)
		: i18n._(DEFAULT_3_DESCRIPTOR);
	const selectDevice = (deviceId: string): void => {
		if (isInput) {
			VoiceSettingsCommands.update({inputDeviceId: deviceId});
			return;
		}
		VoiceSettingsCommands.update({outputDeviceId: deviceId});
	};
	return (
		<MenuItemSubmenu
			label={i18n._(isInput ? VOICE_INPUT_DEVICE_DESCRIPTOR : VOICE_OUTPUT_DEVICE_DESCRIPTOR)}
			hint={currentDeviceLabel}
			render={() => (
				<>
					{hasDeviceLabels(devices) ? (
						devices.map((device) => (
							<MenuItemRadio
								key={device.deviceId}
								selected={effectiveDeviceId === device.deviceId}
								onSelect={() => selectDevice(device.deviceId)}
								data-flx="voice.voice-settings-menus.audio-device-submenu.option"
							>
								{formatVoiceAudioDeviceLabel(i18n, device, fallbackLabel)}
							</MenuItemRadio>
						))
					) : (
						<MenuItemRadio
							selected={storedDeviceId === 'default'}
							onSelect={() => selectDevice('default')}
							data-flx="voice.voice-settings-menus.audio-device-submenu.default"
						>
							{i18n._(DEFAULT_3_DESCRIPTOR)}
						</MenuItemRadio>
					)}
				</>
			)}
			data-flx="voice.voice-settings-menus.audio-device-submenu"
		/>
	);
});

const VoiceInputProfileSubmenu: React.FC = observer(() => {
	const {i18n} = useLingui();
	const processingMode = getActiveVoiceProcessingMode(VoiceSettings);
	const processingModeLabels: Record<VoiceProcessingMode, string> = {
		voice: i18n._(VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR),
		studio: i18n._(VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR),
		custom: i18n._(CUSTOM_DESCRIPTOR),
	};
	return (
		<MenuItemSubmenu
			label={i18n._(INPUT_PROFILE_DESCRIPTOR)}
			hint={processingModeLabels[processingMode]}
			render={() => (
				<>
					{(['voice', 'studio', 'custom'] as const).map((mode) => (
						<MenuItemRadio
							key={mode}
							selected={processingMode === mode}
							onSelect={() => VoiceSettingsCommands.setActiveInputVoiceProcessingMode(mode)}
							data-flx="voice.voice-settings-menus.input-profile-submenu.option"
						>
							{processingModeLabels[mode]}
						</MenuItemRadio>
					))}
				</>
			)}
			data-flx="voice.voice-settings-menus.input-profile-submenu"
		/>
	);
});

const VoiceInputLevelItem: React.FC = () => {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const room = MediaEngine.room;
	const micPublication = room ? getPrimaryLocalMicrophonePublication(room.localParticipant) : null;
	const micTrack = micPublication ? getLocalPublicationMediaStreamTrack(micPublication) : null;
	const [level, setLevel] = useState(0);
	useEffect(() => {
		if (!micTrack) {
			setLevel(0);
			return;
		}
		const AudioContextClass =
			window.AudioContext ?? (window as unknown as {webkitAudioContext?: typeof AudioContext}).webkitAudioContext;
		if (!AudioContextClass) {
			return;
		}
		const audioContext = new AudioContextClass();
		const source = audioContext.createMediaStreamSource(new MediaStream([micTrack]));
		const analyser = audioContext.createAnalyser();
		analyser.fftSize = 2048;
		analyser.smoothingTimeConstant = 0.2;
		source.connect(analyser);
		const timeDomain = new Float32Array(analyser.fftSize);
		let frame = 0;
		let disposed = false;
		void audioContext.resume().catch(() => undefined);
		const sampleLevel = () => {
			if (disposed) {
				return;
			}
			analyser.getFloatTimeDomainData(timeDomain);
			let sumOfSquares = 0;
			for (let i = 0; i < timeDomain.length; i++) {
				sumOfSquares += timeDomain[i] * timeDomain[i];
			}
			const rms = Math.sqrt(sumOfSquares / timeDomain.length);
			const decibels = 20 * Math.log10(Math.max(rms, 1e-10));
			const normalized = Math.max(
				0,
				Math.min(1, (decibels - INPUT_LEVEL_MIN_DB) / (INPUT_LEVEL_MAX_DB - INPUT_LEVEL_MIN_DB)),
			);
			setLevel(normalized);
			frame = requestAnimationFrame(sampleLevel);
		};
		frame = requestAnimationFrame(sampleLevel);
		return () => {
			disposed = true;
			cancelAnimationFrame(frame);
			source.disconnect();
			analyser.disconnect();
			void audioContext.close().catch(() => undefined);
		};
	}, [micTrack]);
	const label = i18n._(INPUT_LEVEL_DESCRIPTOR);
	return (
		<div className={styles.inputLevelItem} data-flx="voice.voice-settings-menus.input-level">
			<div
				className={styles.inputLevelLabel}
				data-flx="voice.voice-settings-menus.voice-input-level-item.input-level-label"
			>
				{label}
			</div>
			<AudioLevelMeter
				level={level}
				className={styles.inputLevelMeter}
				role="meter"
				aria-label={label}
				aria-valuemin={0}
				aria-valuemax={100}
				aria-valuenow={Math.round(level * 100)}
				data-flx="voice.voice-settings-menus.voice-input-level-item.input-level-meter"
			/>
		</div>
	);
};

const VoiceInputVolumeItems: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<>
			<MenuItemSlider
				label={i18n._(VOICE_INPUT_VOLUME_DESCRIPTOR)}
				value={VoiceSettings.inputVolume}
				minValue={0}
				maxValue={VOICE_VOLUME_MAX_PERCENT}
				onChange={(value) => VoiceSettingsCommands.update({inputVolume: value})}
				onFormat={(value) => `${Math.round(value)}%`}
				data-flx="voice.voice-settings-menus.input-volume"
			/>
			<VoiceInputLevelItem data-flx="voice.voice-settings-menus.voice-input-volume-items.voice-input-level-item" />
		</>
	);
});

const VoiceOutputVolumeItem: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<MenuItemSlider
			label={i18n._(VOICE_OUTPUT_VOLUME_DESCRIPTOR)}
			value={VoiceSettings.outputVolume}
			minValue={0}
			maxValue={VOICE_VOLUME_MAX_PERCENT}
			onChange={(value) => VoiceSettingsCommands.update({outputVolume: value})}
			onFormat={(value) => `${Math.round(value)}%`}
			data-flx="voice.voice-settings-menus.output-volume"
		/>
	);
});

const VoiceSettingsMenuItem: React.FC<{onClose: () => void}> = ({onClose}) => {
	const {i18n} = useLingui();
	return (
		<MenuItem
			onClick={() => openVoiceVideoSettings(onClose, 'audio')}
			data-flx="voice.voice-settings-menus.voice-settings-item"
		>
			<span
				className={styles.leadingActionLabel}
				data-flx="voice.voice-settings-menus.voice-settings-menu-item.leading-action-label"
			>
				<GearIcon
					weight="fill"
					className={styles.icon}
					data-flx="voice.voice-settings-menus.voice-settings-menu-item.icon"
				/>
				{i18n._(VOICE_SETTINGS_DESCRIPTOR)}
			</span>
		</MenuItem>
	);
};

interface VoiceAudioSettingsMenuProps {
	inputDevices: Array<MediaDeviceInfo>;
	outputDevices: Array<MediaDeviceInfo>;
	onClose: () => void;
}

export const VoiceAudioSettingsMenu: React.FC<VoiceAudioSettingsMenuProps> = observer(
	({inputDevices, outputDevices, onClose}) => {
		const {i18n} = useLingui();
		const voiceState = MediaEngine.getCurrentUserVoiceState();
		const isGuildDeafened = voiceState?.deaf ?? false;
		const isDeafened = (voiceState?.self_deaf ?? false) || isGuildDeafened;
		const deafenMenuLabel = isGuildDeafened
			? getVoiceDeafenedByModeratorsStatusLabel(i18n, true)
			: i18n._(VOICE_DEAFEN_DESCRIPTOR);
		const isPushToTalk = Keybind.isPushToTalkEffective();
		const handleTogglePushToTalk = useCallback(
			(checked: boolean) => {
				if (!checked) {
					Keybind.setTransmitMode('voice_activity');
					MediaEngine.handlePushToTalkModeChange();
					return;
				}
				Keybind.setTransmitMode('voice_push_to_talk');
				MediaEngine.handlePushToTalkModeChange();
				if (!Keybind.hasPushToTalkKeybind()) {
					openVoiceVideoSettings(onClose, 'audio');
				}
			},
			[onClose],
		);
		return (
			<>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-audio-settings-menu.devices">
					<VoiceAudioDeviceSubmenu
						devices={inputDevices}
						deviceType="input"
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.voice-audio-device-submenu"
					/>
					<VoiceInputProfileSubmenu data-flx="voice.voice-settings-menus.voice-audio-settings-menu.voice-input-profile-submenu" />
					<VoiceAudioDeviceSubmenu
						devices={outputDevices}
						deviceType="output"
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.voice-audio-device-submenu--2"
					/>
				</MenuGroup>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-audio-settings-menu.levels">
					<VoiceInputVolumeItems data-flx="voice.voice-settings-menus.voice-audio-settings-menu.voice-input-volume-items" />
					<VoiceOutputVolumeItem data-flx="voice.voice-settings-menus.voice-audio-settings-menu.voice-output-volume-item" />
				</MenuGroup>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-audio-settings-menu.actions">
					<CheckboxItem
						icon={
							<HandTapIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-settings-menus.voice-audio-settings-menu.icon"
							/>
						}
						checked={isPushToTalk}
						onCheckedChange={handleTogglePushToTalk}
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.push-to-talk"
					>
						<Trans>Push-to-talk</Trans>
					</CheckboxItem>
					<CheckboxItem
						icon={
							<SpeakerSlashIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-settings-menus.voice-audio-settings-menu.icon--2"
							/>
						}
						checked={isDeafened}
						disabled={isGuildDeafened}
						onCheckedChange={() => VoiceStateCommands.toggleSelfDeaf(null)}
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.deafen"
					>
						{deafenMenuLabel}
					</CheckboxItem>
					<VoiceSettingsMenuItem
						onClose={onClose}
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.voice-settings-menu-item"
					/>
				</MenuGroup>
			</>
		);
	},
);

interface VoiceDeviceSettingsMenuProps {
	devices: Array<MediaDeviceInfo>;
	deviceType: 'input' | 'output';
	onClose: () => void;
}

export const VoiceDeviceSettingsMenu: React.FC<VoiceDeviceSettingsMenuProps> = observer(
	({devices, deviceType, onClose}) => {
		const isInput = deviceType === 'input';
		return (
			<>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-device-settings-menu.menu-group">
					<VoiceAudioDeviceSubmenu
						devices={devices}
						deviceType={deviceType}
						data-flx="voice.voice-settings-menus.voice-device-settings-menu.voice-audio-device-submenu"
					/>
					{isInput && (
						<VoiceInputProfileSubmenu data-flx="voice.voice-settings-menus.voice-device-settings-menu.voice-input-profile-submenu" />
					)}
				</MenuGroup>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-device-settings-menu.menu-group--2">
					{isInput ? (
						<VoiceInputVolumeItems data-flx="voice.voice-settings-menus.voice-device-settings-menu.voice-input-volume-items" />
					) : (
						<VoiceOutputVolumeItem data-flx="voice.voice-settings-menus.voice-device-settings-menu.voice-output-volume-item" />
					)}
				</MenuGroup>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-device-settings-menu.menu-group--3">
					<VoiceSettingsMenuItem
						onClose={onClose}
						data-flx="voice.voice-settings-menus.voice-device-settings-menu.voice-settings-menu-item"
					/>
				</MenuGroup>
			</>
		);
	},
);

interface VoiceInputSettingsMenuProps {
	inputDevices: Array<MediaDeviceInfo>;
	onClose: () => void;
}

export const VoiceInputSettingsMenu: React.FC<VoiceInputSettingsMenuProps> = observer(({inputDevices, onClose}) => {
	return (
		<VoiceDeviceSettingsMenu
			devices={inputDevices}
			deviceType="input"
			onClose={onClose}
			data-flx="voice.voice-settings-menus.voice-input-settings-menu.voice-device-settings-menu"
		/>
	);
});

interface VoiceOutputSettingsMenuProps {
	outputDevices: Array<MediaDeviceInfo>;
	onClose: () => void;
}

export const VoiceOutputSettingsMenu: React.FC<VoiceOutputSettingsMenuProps> = observer(({outputDevices, onClose}) => {
	return (
		<VoiceDeviceSettingsMenu
			devices={outputDevices}
			deviceType="output"
			onClose={onClose}
			data-flx="voice.voice-settings-menus.voice-output-settings-menu.voice-device-settings-menu"
		/>
	);
});

interface VoiceCameraDeviceSubmenuProps {
	videoDevices: Array<MediaDeviceInfo>;
}

const VoiceCameraDeviceSubmenu: React.FC<VoiceCameraDeviceSubmenuProps> = observer(({videoDevices}) => {
	const {i18n} = useLingui();
	const effectiveVideoDeviceId = resolveEffectiveDeviceId(VoiceSettings.videoDeviceId, videoDevices);
	const effectiveVideoDevice = videoDevices.find((device) => device.deviceId === effectiveVideoDeviceId) ?? null;
	return (
		<MenuItemSubmenu
			label={i18n._(CAMERA_DESCRIPTOR)}
			hint={effectiveVideoDevice?.label || i18n._(DEFAULT_3_DESCRIPTOR)}
			render={() =>
				hasDeviceLabels(videoDevices) ? (
					videoDevices.map((device) => (
						<MenuItemRadio
							key={device.deviceId}
							icon={
								<CameraIcon
									weight="fill"
									className={styles.icon}
									data-flx="voice.voice-settings-menus.voice-camera-device-submenu.icon"
								/>
							}
							selected={effectiveVideoDeviceId === device.deviceId}
							onSelect={() => VoiceSettingsCommands.update({videoDeviceId: device.deviceId})}
							data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-item-radio.update"
						>
							{device.deviceId === 'default'
								? i18n._(DEFAULT_3_DESCRIPTOR)
								: device.label || formatFallbackCameraLabel(i18n)}
						</MenuItemRadio>
					))
				) : (
					<MenuItemRadio
						key="default"
						icon={
							<CameraIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-settings-menus.voice-camera-device-submenu.icon--2"
							/>
						}
						selected={VoiceSettings.videoDeviceId === 'default'}
						onSelect={() => VoiceSettingsCommands.update({videoDeviceId: 'default'})}
						data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-item-radio"
					>
						{i18n._(DEFAULT_3_DESCRIPTOR)}
					</MenuItemRadio>
				)
			}
			data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-item-submenu"
		/>
	);
});

interface VoiceCameraSettingsMenuProps {
	videoDevices: Array<MediaDeviceInfo>;
	onClose: () => void;
}

export const VoiceCameraSettingsMenu: React.FC<VoiceCameraSettingsMenuProps> = observer(({videoDevices, onClose}) => {
	const {i18n} = useLingui();
	return (
		<>
			<MenuGroup data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-group">
				<VoiceCameraDeviceSubmenu
					videoDevices={videoDevices}
					data-flx="voice.voice-settings-menus.voice-camera-settings-menu.voice-camera-device-submenu"
				/>
				<CheckboxItem
					checked={VoiceSettings.mirrorCamera}
					onCheckedChange={(checked) => VoiceSettingsCommands.update({mirrorCamera: checked})}
					data-flx="voice.voice-settings-menus.voice-camera-settings-menu.checkbox-item.mirror-camera"
				>
					{i18n._(MIRROR_CAMERA_DESCRIPTOR)}
				</CheckboxItem>
			</MenuGroup>
			<MenuGroup data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-group--2">
				<MenuItem
					onClick={() => {
						ModalCommands.pushAfterBottomSheetClose(
							onClose,
							modal(() => (
								<CameraPreviewModalInRoom data-flx="voice.voice-settings-menus.voice-camera-settings-menu.camera-preview-modal-in-room" />
							)),
						);
					}}
					data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-item.close"
				>
					<span
						className={styles.leadingActionLabel}
						data-flx="voice.voice-settings-menus.voice-camera-settings-menu.leading-action-label"
					>
						<EyeIcon
							weight="fill"
							className={styles.icon}
							data-flx="voice.voice-settings-menus.voice-camera-settings-menu.icon"
						/>
						<Trans>Preview camera</Trans>
					</span>
				</MenuItem>
			</MenuGroup>
			<MenuGroup data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-group--3">
				<MenuItem
					onClick={() => openVoiceVideoSettings(onClose, 'video')}
					data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-item.close--2"
				>
					<span
						className={styles.leadingActionLabel}
						data-flx="voice.voice-settings-menus.voice-camera-settings-menu.leading-action-label--2"
					>
						<GearIcon
							weight="fill"
							className={styles.icon}
							data-flx="voice.voice-settings-menus.voice-camera-settings-menu.icon--2"
						/>
						{i18n._(VIDEO_SETTINGS_DESCRIPTOR)}
					</span>
				</MenuItem>
			</MenuGroup>
		</>
	);
});

interface VoiceMoreOptionsMenuProps {
	onClose: () => void;
}

export const VoiceMoreOptionsMenu: React.FC<VoiceMoreOptionsMenuProps> = observer(({onClose}) => {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const voiceSettings = VoiceSettings;
	const layoutMode = VoiceCallLayout.layoutMode;
	const isGrid = layoutMode === 'grid';
	const connectedChannelId = MediaEngine.channelId;
	const canOpenDebugEventSink =
		connectedChannelId != null &&
		(Users.currentUser?.isStaff() ?? false) &&
		VoiceDebugEventSinkCommands.canOpenVoiceDebugEventSinkPopout();
	const isDmVoiceCall = connectedChannelId != null && (MediaEngine.guildId ?? null) === null;
	const currentRegion =
		isDmVoiceCall && connectedChannelId
			? (CallState.getCall(connectedChannelId)?.region ?? AUTOMATIC_VOICE_REGION_ID)
			: null;
	const [regions, setRegions] = useState<Array<RtcRegionResponse>>([]);
	const [isChangingRegion, setIsChangingRegion] = useState(false);
	useEffect(() => {
		if (!isDmVoiceCall || !connectedChannelId) {
			setRegions([]);
			return undefined;
		}
		let cancelled = false;
		void CallCommands.fetchCallRegions(connectedChannelId)
			.then((fetchedRegions) => {
				if (!cancelled) {
					setRegions(fetchedRegions);
				}
			})
			.catch((error) => {
				logger.error('Failed to fetch DM call regions for more options menu:', error);
				if (!cancelled) {
					setRegions([]);
				}
			});
		return () => {
			cancelled = true;
		};
	}, [connectedChannelId, isDmVoiceCall]);
	const getRegionDisplayName = useCallback(
		(regionId: string, regionName: string): string => {
			if (regionId === AUTOMATIC_VOICE_REGION_ID) {
				return i18n._(AUTOMATIC_DESCRIPTOR);
			}
			if (regionName && regionName !== regionId) {
				return regionName;
			}
			return regionId
				.split('-')
				.map((part) => {
					const lower = part.toLowerCase();
					if (lower === 'us') return 'US';
					if (lower === 'eu') return 'EU';
					return `${lower.slice(0, 1).toUpperCase()}${lower.slice(1)}`;
				})
				.join(' ');
		},
		[i18n],
	);
	const regionHint = useMemo(() => {
		if (!currentRegion || currentRegion === AUTOMATIC_VOICE_REGION_ID) return i18n._(AUTOMATIC_DESCRIPTOR);
		const matchedRegion = regions.find((region) => region.id === currentRegion);
		if (matchedRegion) {
			return getRegionDisplayName(matchedRegion.id, matchedRegion.name);
		}
		return currentRegion;
	}, [currentRegion, getRegionDisplayName, regions, i18n.locale]);
	const handleRegionSelect = useCallback(
		(regionId: string) => {
			if (!connectedChannelId || isChangingRegion || currentRegion === regionId) {
				return;
			}
			setIsChangingRegion(true);
			void CallCommands.updateCallRegion(connectedChannelId, regionId)
				.catch((error) => {
					logger.error('Failed to update DM call region from more options menu:', error);
				})
				.finally(() => {
					setIsChangingRegion(false);
				});
		},
		[connectedChannelId, currentRegion, isChangingRegion],
	);
	return (
		<>
			<MenuGroup data-flx="voice.voice-settings-menus.voice-more-options-menu.menu-group">
				{isDmVoiceCall && (
					<MenuItemSubmenu
						label={i18n._(VOICE_REGION_DESCRIPTOR)}
						hint={regionHint}
						disabled={isChangingRegion}
						render={() => (
							<>
								<MenuItemRadio
									key={AUTOMATIC_VOICE_REGION_ID}
									selected={currentRegion === AUTOMATIC_VOICE_REGION_ID}
									disabled={isChangingRegion}
									onSelect={() => handleRegionSelect(AUTOMATIC_VOICE_REGION_ID)}
									data-flx="voice.voice-settings-menus.voice-more-options-menu.menu-item-radio.region-select"
								>
									{i18n._(AUTOMATIC_DESCRIPTOR)}
								</MenuItemRadio>
								{regions
									.filter((region) => region.id !== AUTOMATIC_VOICE_REGION_ID)
									.sort((a, b) => getRegionDisplayName(a.id, a.name).localeCompare(getRegionDisplayName(b.id, b.name)))
									.map((region) => {
										const label = getRegionDisplayName(region.id, region.name);
										return (
											<MenuItemRadio
												key={region.id}
												selected={currentRegion === region.id}
												disabled={isChangingRegion}
												onSelect={() => handleRegionSelect(region.id)}
												data-flx="voice.voice-settings-menus.voice-more-options-menu.menu-item-radio.region-select--2"
											>
												{label}
											</MenuItemRadio>
										);
									})}
							</>
						)}
						data-flx="voice.voice-settings-menus.voice-more-options-menu.menu-item-submenu"
					/>
				)}
				{!isDmVoiceCall && (
					<CheckboxItem
						icon={
							<GridFourIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-settings-menus.voice-more-options-menu.icon"
							/>
						}
						checked={isGrid}
						onCheckedChange={(checked) => {
							if (checked) VoiceCallLayoutCommands.setLayoutMode('grid');
							else VoiceCallLayoutCommands.setLayoutMode('focus');
							VoiceCallLayoutCommands.markUserOverride();
						}}
						data-flx="voice.voice-settings-menus.voice-more-options-menu.checkbox-item"
					>
						<Trans>Grid view</Trans>
					</CheckboxItem>
				)}
				<CheckboxItem
					icon={
						<UsersIcon
							weight="fill"
							className={styles.icon}
							data-flx="voice.voice-settings-menus.voice-more-options-menu.icon--2"
						/>
					}
					checked={voiceSettings.showMyOwnCamera}
					onCheckedChange={(checked) => {
						if (!checked) {
							if (VoicePrompts.getSkipHideOwnCameraConfirm()) {
								VoiceSettingsCommands.update({showMyOwnCamera: false});
							} else {
								ModalCommands.pushAfterBottomSheetClose(
									onClose,
									modal(() => (
										<HideOwnCameraConfirmModal data-flx="voice.voice-settings-menus.voice-more-options-menu.hide-own-camera-confirm-modal" />
									)),
								);
							}
						} else {
							VoiceSettingsCommands.update({showMyOwnCamera: true});
						}
					}}
					data-flx="voice.voice-settings-menus.voice-more-options-menu.checkbox-item--2"
				>
					<Trans>Show my own camera</Trans>
				</CheckboxItem>
				<CheckboxItem
					icon={
						<UsersIcon
							weight="fill"
							className={styles.icon}
							data-flx="voice.voice-settings-menus.voice-more-options-menu.icon--3"
						/>
					}
					checked={voiceSettings.showNonVideoParticipants}
					onCheckedChange={(checked) => VoiceSettingsCommands.update({showNonVideoParticipants: checked})}
					data-flx="voice.voice-settings-menus.voice-more-options-menu.checkbox-item--3"
				>
					<Trans>Show non-video participants</Trans>
				</CheckboxItem>
				<CheckboxItem
					icon={
						<HandTapIcon
							weight="fill"
							className={styles.icon}
							data-flx="voice.voice-settings-menus.voice-more-options-menu.icon.prioritize-speakers"
						/>
					}
					checked={voiceSettings.prioritizeSpeakingParticipants}
					onCheckedChange={(checked) => VoiceSettings.setPrioritizeSpeakingParticipants(checked)}
					data-flx="voice.voice-settings-menus.voice-more-options-menu.checkbox-item.prioritize-speakers"
				>
					{i18n._(PRIORITIZE_SPEAKERS_DESCRIPTOR)}
				</CheckboxItem>
				{canOpenDebugEventSink && (
					<MenuItem
						icon={
							<ChartBarIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-settings-menus.voice-more-options-menu.icon.debug-event-sink"
							/>
						}
						onClick={() => {
							void VoiceDebugEventSinkCommands.openVoiceDebugEventSinkPopout();
						}}
						data-flx="voice.voice-settings-menus.voice-more-options-menu.menu-item.debug-event-sink"
					>
						<Trans>Open event sink</Trans>
					</MenuItem>
				)}
			</MenuGroup>
			<MenuGroup data-flx="voice.voice-settings-menus.voice-more-options-menu.menu-group--2">
				<MenuItem
					icon={
						<GearIcon
							weight="fill"
							className={styles.icon}
							data-flx="voice.voice-settings-menus.voice-more-options-menu.icon--4"
						/>
					}
					onClick={() => {
						ModalCommands.pushAfterBottomSheetClose(
							onClose,
							modal(() => (
								<UserSettingsModal
									initialTab="voice_video"
									data-flx="voice.voice-settings-menus.voice-more-options-menu.user-settings-modal"
								/>
							)),
						);
					}}
					data-flx="voice.voice-settings-menus.voice-more-options-menu.menu-item.close"
				>
					{getVoiceVideoSettingsLabel(i18n)}
				</MenuItem>
			</MenuGroup>
		</>
	);
});
