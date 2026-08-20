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
	VOICE_AUTOMATIC_GAIN_CONTROL_DESCRIPTOR,
	VOICE_CAMERA_SETTINGS_DESCRIPTOR,
	VOICE_DEAFEN_DESCRIPTOR,
	VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR,
	VOICE_ECHO_CANCELLATION_DESCRIPTOR,
	VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR,
	VOICE_INPUT_DEVICE_DESCRIPTOR,
	VOICE_INPUT_SETTINGS_DESCRIPTOR,
	VOICE_INPUT_VOLUME_DESCRIPTOR,
	VOICE_NOISE_SUPPRESSION_DESCRIPTOR,
	VOICE_OUTPUT_DEVICE_DESCRIPTOR,
	VOICE_OUTPUT_SETTINGS_DESCRIPTOR,
	VOICE_OUTPUT_VOLUME_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {getActiveVoiceProcessingMode, type VoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';
import {VOICE_VOLUME_MAX_PERCENT} from '@app/features/voice/utils/VoiceVolumeUtils';
import {AUTOMATIC_VOICE_REGION_ID} from '@fluxer/constants/src/ChannelConstants';
import type {RtcRegionResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	ChartBarIcon,
	GearIcon,
	GridFourIcon,
	HandTapIcon,
	MicrophoneIcon,
	SpeakerSimpleSlashIcon,
	SpeakerSlashIcon,
	UsersIcon,
	VideoIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const CUSTOM_DESCRIPTOR = msg({
	message: 'Custom',
	comment: 'Voice input processing profile where the user configures processing manually.',
	context: 'voice-processing-profile',
});
const ENHANCED_DESCRIPTOR = msg({
	message: 'Enhanced',
	comment: 'Noise suppression option using enhanced filtering.',
	context: 'noise-suppression-option',
});
const STANDARD_DESCRIPTOR = msg({
	message: 'Standard',
	comment: 'Noise suppression option using standard browser filtering.',
	context: 'noise-suppression-option',
});
const OFF_DESCRIPTOR = msg({
	message: 'Off',
	comment: 'Noise suppression option that disables suppression.',
	context: 'noise-suppression-option',
});
const MICROPHONE_DESCRIPTOR = msg({
	message: 'Microphone',
	comment: 'Fallback microphone label in the voice settings menu when the OS does not report a device name.',
});
const DEFAULT_DESCRIPTOR = msg({
	message: 'Default',
	comment: 'Default microphone device option.',
	context: 'voice-input-device',
});
const SPEAKER_DESCRIPTOR = msg({
	message: 'Speaker',
	comment: 'Fallback speaker label in the voice settings menu when the OS does not report a device name.',
});
const DEFAULT_2_DESCRIPTOR = msg({
	message: 'Default',
	comment: 'Default speaker or output device option.',
	context: 'voice-output-device',
});
const INPUT_PROFILE_DESCRIPTOR = msg({
	message: 'Voice processing',
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
const MIRROR_CAMERA_DESCRIPTOR = msg({
	message: 'Mirror camera',
	comment: 'Camera settings menu checkbox for flipping the local camera preview horizontally.',
});
const logger = new Logger('VoiceSettingsMenus');

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

interface VoiceAudioSettingsMenuProps {
	inputDevices: Array<MediaDeviceInfo>;
	outputDevices: Array<MediaDeviceInfo>;
	onClose: () => void;
}

export const VoiceAudioSettingsMenu: React.FC<VoiceAudioSettingsMenuProps> = observer(
	({inputDevices, outputDevices, onClose}) => {
		const {i18n} = useLingui();
		const voiceSettings = VoiceSettings;
		const voiceState = MediaEngine.getCurrentUserVoiceState();
		const isGuildDeafened = voiceState?.deaf ?? false;
		const isDeafened = (voiceState?.self_deaf ?? false) || isGuildDeafened;
		const deafenMenuLabel = isGuildDeafened
			? getVoiceDeafenedByModeratorsStatusLabel(i18n, true)
			: i18n._(VOICE_DEAFEN_DESCRIPTOR);
		const isPushToTalk = Keybind.transmitMode === 'voice_push_to_talk';
		const handleToggleDeafen = useCallback((_checked: boolean) => {
			VoiceStateCommands.toggleSelfDeaf(null);
		}, []);
		const handleTogglePushToTalk = useCallback((checked: boolean) => {
			Keybind.setTransmitMode(checked ? 'voice_push_to_talk' : 'voice_activity');
			MediaEngine.handlePushToTalkModeChange();
		}, []);
		const effectiveInputDeviceId = resolveEffectiveDeviceId(voiceSettings.inputDeviceId, inputDevices);
		const effectiveOutputDeviceId = resolveEffectiveDeviceId(voiceSettings.outputDeviceId, outputDevices);
		const inputHasLabels = hasDeviceLabels(inputDevices);
		const outputHasLabels = hasDeviceLabels(outputDevices);
		const processingMode = getActiveVoiceProcessingMode(voiceSettings);
		const isCustomMode = processingMode === 'custom';
		const deepFilterEnabled = voiceSettings.deepFilterNoiseSuppression;
		const browserNsEnabled = voiceSettings.noiseSuppression;
		const processingModeLabels: Record<VoiceProcessingMode, string> = {
			voice: i18n._(VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR),
			studio: i18n._(VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR),
			custom: i18n._(CUSTOM_DESCRIPTOR),
		};
		type NoiseSuppressionChoice = 'enhanced' | 'standard' | 'none';
		const noiseSuppressionChoice: NoiseSuppressionChoice = deepFilterEnabled
			? 'enhanced'
			: browserNsEnabled
				? 'standard'
				: 'none';
		const noiseSuppressionLabels: Record<NoiseSuppressionChoice, string> = {
			enhanced: i18n._(ENHANCED_DESCRIPTOR),
			standard: i18n._(STANDARD_DESCRIPTOR),
			none: i18n._(OFF_DESCRIPTOR),
		};
		const setNoiseSuppression = (choice: NoiseSuppressionChoice) => {
			switch (choice) {
				case 'enhanced':
					VoiceSettingsCommands.update({deepFilterNoiseSuppression: true, noiseSuppression: false});
					return;
				case 'standard':
					VoiceSettingsCommands.update({deepFilterNoiseSuppression: false, noiseSuppression: true});
					return;
				case 'none':
					VoiceSettingsCommands.update({deepFilterNoiseSuppression: false, noiseSuppression: false});
					return;
			}
		};
		return (
			<>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-group">
					<MenuItemSubmenu
						label={i18n._(VOICE_INPUT_DEVICE_DESCRIPTOR)}
						render={() => (
							<>
								{inputHasLabels ? (
									inputDevices.map((device) => (
										<MenuItemRadio
											key={device.deviceId}
											selected={effectiveInputDeviceId === device.deviceId}
											onSelect={() => {
												VoiceSettingsCommands.update({inputDeviceId: device.deviceId});
											}}
											data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item-radio.update"
										>
											{formatVoiceAudioDeviceLabel(i18n, device, i18n._(MICROPHONE_DESCRIPTOR))}
										</MenuItemRadio>
									))
								) : (
									<MenuItemRadio
										key="default"
										selected={true}
										onSelect={() => {}}
										data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item-radio"
									>
										{i18n._(DEFAULT_DESCRIPTOR)}
									</MenuItemRadio>
								)}
							</>
						)}
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item-submenu"
					/>
					<MenuItemSubmenu
						label={i18n._(VOICE_OUTPUT_DEVICE_DESCRIPTOR)}
						render={() => (
							<>
								{outputHasLabels ? (
									outputDevices.map((device) => (
										<MenuItemRadio
											key={device.deviceId}
											selected={effectiveOutputDeviceId === device.deviceId}
											onSelect={() => {
												VoiceSettingsCommands.update({outputDeviceId: device.deviceId});
											}}
											data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item-radio.update--2"
										>
											{formatVoiceAudioDeviceLabel(i18n, device, i18n._(SPEAKER_DESCRIPTOR))}
										</MenuItemRadio>
									))
								) : (
									<MenuItemRadio
										key="default"
										selected={true}
										onSelect={() => {}}
										data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item-radio--2"
									>
										{i18n._(DEFAULT_2_DESCRIPTOR)}
									</MenuItemRadio>
								)}
							</>
						)}
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item-submenu--2"
					/>
				</MenuGroup>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-group--2">
					<MenuItemSlider
						label={i18n._(VOICE_INPUT_VOLUME_DESCRIPTOR)}
						value={voiceSettings.inputVolume}
						minValue={0}
						maxValue={VOICE_VOLUME_MAX_PERCENT}
						onChange={(value) => VoiceSettingsCommands.update({inputVolume: value})}
						onFormat={(value) => `${Math.round(value)}%`}
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item-slider.update"
					/>
					<MenuItemSlider
						label={i18n._(VOICE_OUTPUT_VOLUME_DESCRIPTOR)}
						value={voiceSettings.outputVolume}
						minValue={0}
						maxValue={VOICE_VOLUME_MAX_PERCENT}
						onChange={(value) => VoiceSettingsCommands.update({outputVolume: value})}
						onFormat={(value) => `${Math.round(value)}%`}
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item-slider.update--2"
					/>
				</MenuGroup>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-group--3">
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
										data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item-radio.update--3"
									>
										{processingModeLabels[mode]}
									</MenuItemRadio>
								))}
								{isCustomMode ? (
									<MenuGroup data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-group--4">
										<MenuItemSubmenu
											label={i18n._(VOICE_NOISE_SUPPRESSION_DESCRIPTOR)}
											hint={noiseSuppressionLabels[noiseSuppressionChoice]}
											render={() => (
												<>
													{(['enhanced', 'standard', 'none'] as const).map((choice) => (
														<MenuItemRadio
															key={choice}
															selected={noiseSuppressionChoice === choice}
															onSelect={() => setNoiseSuppression(choice)}
															data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item-radio.set-noise-suppression"
														>
															{noiseSuppressionLabels[choice]}
														</MenuItemRadio>
													))}
												</>
											)}
											data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item-submenu--4"
										/>
										<CheckboxItem
											icon={
												<SpeakerSimpleSlashIcon
													weight="fill"
													className={styles.icon}
													data-flx="voice.voice-settings-menus.voice-audio-settings-menu.icon"
												/>
											}
											checked={voiceSettings.echoCancellation}
											onCheckedChange={(checked) => VoiceSettingsCommands.update({echoCancellation: checked})}
											data-flx="voice.voice-settings-menus.voice-audio-settings-menu.checkbox-item"
										>
											{i18n._(VOICE_ECHO_CANCELLATION_DESCRIPTOR)}
										</CheckboxItem>
										<CheckboxItem
											icon={
												<MicrophoneIcon
													weight="fill"
													className={styles.icon}
													data-flx="voice.voice-settings-menus.voice-audio-settings-menu.icon--2"
												/>
											}
											checked={voiceSettings.autoGainControl}
											onCheckedChange={(checked) => VoiceSettingsCommands.update({autoGainControl: checked})}
											data-flx="voice.voice-settings-menus.voice-audio-settings-menu.checkbox-item--2"
										>
											{i18n._(VOICE_AUTOMATIC_GAIN_CONTROL_DESCRIPTOR)}
										</CheckboxItem>
									</MenuGroup>
								) : null}
							</>
						)}
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item-submenu--3"
					/>
				</MenuGroup>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-group--5">
					<CheckboxItem
						icon={
							<HandTapIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-settings-menus.voice-audio-settings-menu.icon--3"
							/>
						}
						checked={isPushToTalk}
						onCheckedChange={handleTogglePushToTalk}
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.checkbox-item--3"
					>
						<Trans>Push-to-talk</Trans>
					</CheckboxItem>
					<CheckboxItem
						icon={
							<SpeakerSlashIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-settings-menus.voice-audio-settings-menu.icon--4"
							/>
						}
						checked={isDeafened}
						disabled={isGuildDeafened}
						onCheckedChange={handleToggleDeafen}
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.checkbox-item--4"
					>
						{deafenMenuLabel}
					</CheckboxItem>
				</MenuGroup>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-group--6">
					<MenuItem
						icon={
							<GearIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-settings-menus.voice-audio-settings-menu.icon--5"
							/>
						}
						onClick={() => openVoiceVideoSettings(onClose, 'audio')}
						data-flx="voice.voice-settings-menus.voice-audio-settings-menu.menu-item.close"
					>
						{getVoiceVideoSettingsLabel(i18n)}
					</MenuItem>
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
		const {i18n} = useLingui();
		const voiceSettings = VoiceSettings;
		const isInput = deviceType === 'input';
		const deviceIdKey = isInput ? 'inputDeviceId' : 'outputDeviceId';
		const volumeKey = isInput ? 'inputVolume' : 'outputVolume';
		const storedDeviceId = voiceSettings[deviceIdKey];
		const currentVolume = voiceSettings[volumeKey];
		const effectiveDeviceId = resolveEffectiveDeviceId(storedDeviceId, devices);
		const devicesHaveLabels = hasDeviceLabels(devices);
		const menuLabel = i18n._(isInput ? VOICE_INPUT_DEVICE_DESCRIPTOR : VOICE_OUTPUT_DEVICE_DESCRIPTOR);
		const volumeLabel = i18n._(isInput ? VOICE_INPUT_VOLUME_DESCRIPTOR : VOICE_OUTPUT_VOLUME_DESCRIPTOR);
		const fallbackDeviceName = i18n._(isInput ? MICROPHONE_2_DESCRIPTOR : SPEAKER_2_DESCRIPTOR);
		return (
			<>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-device-settings-menu.menu-group">
					<MenuItemSubmenu
						label={menuLabel}
						render={() => (
							<>
								{devicesHaveLabels ? (
									devices.map((device) => (
										<MenuItemRadio
											key={device.deviceId}
											selected={effectiveDeviceId === device.deviceId}
											onSelect={() => {
												VoiceSettingsCommands.update({[deviceIdKey]: device.deviceId});
											}}
											data-flx="voice.voice-settings-menus.voice-device-settings-menu.menu-item-radio.update"
										>
											{formatVoiceAudioDeviceLabel(i18n, device, fallbackDeviceName)}
										</MenuItemRadio>
									))
								) : (
									<MenuItemRadio
										key="default"
										selected={true}
										onSelect={() => {}}
										data-flx="voice.voice-settings-menus.voice-device-settings-menu.menu-item-radio"
									>
										{i18n._(DEFAULT_3_DESCRIPTOR)}
									</MenuItemRadio>
								)}
							</>
						)}
						data-flx="voice.voice-settings-menus.voice-device-settings-menu.menu-item-submenu"
					/>
				</MenuGroup>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-device-settings-menu.menu-group--2">
					<MenuItemSlider
						label={volumeLabel}
						value={currentVolume}
						minValue={0}
						maxValue={VOICE_VOLUME_MAX_PERCENT}
						onChange={(value) => VoiceSettingsCommands.update({[volumeKey]: value})}
						onFormat={(value) => `${Math.round(value)}%`}
						data-flx="voice.voice-settings-menus.voice-device-settings-menu.menu-item-slider.update"
					/>
				</MenuGroup>
				<MenuGroup data-flx="voice.voice-settings-menus.voice-device-settings-menu.menu-group--3">
					<MenuItem
						icon={
							<GearIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-settings-menus.voice-device-settings-menu.icon"
							/>
						}
						onClick={() => openVoiceVideoSettings(onClose, 'audio')}
						data-flx="voice.voice-settings-menus.voice-device-settings-menu.menu-item.close"
					>
						{i18n._(isInput ? VOICE_INPUT_SETTINGS_DESCRIPTOR : VOICE_OUTPUT_SETTINGS_DESCRIPTOR)}
					</MenuItem>
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

interface VoiceCameraSettingsMenuProps {
	videoDevices: Array<MediaDeviceInfo>;
	onClose: () => void;
}

export const VoiceCameraSettingsMenu: React.FC<VoiceCameraSettingsMenuProps> = observer(({videoDevices, onClose}) => {
	const {i18n} = useLingui();
	const voiceSettings = VoiceSettings;
	const effectiveVideoDeviceId = resolveEffectiveDeviceId(voiceSettings.videoDeviceId, videoDevices);
	const devicesHaveLabels = hasDeviceLabels(videoDevices);
	return (
		<>
			<MenuGroup data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-group">
				<MenuItemSubmenu
					label={i18n._(CAMERA_DESCRIPTOR)}
					render={() => (
						<>
							{devicesHaveLabels ? (
								videoDevices.map((device) => (
									<MenuItemRadio
										key={device.deviceId}
										selected={effectiveVideoDeviceId === device.deviceId}
										onSelect={() => {
											VoiceSettingsCommands.update({videoDeviceId: device.deviceId});
										}}
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
									selected={true}
									onSelect={() => {}}
									data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-item-radio"
								>
									{i18n._(DEFAULT_3_DESCRIPTOR)}
								</MenuItemRadio>
							)}
						</>
					)}
					data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-item-submenu"
				/>
				<CheckboxItem
					checked={voiceSettings.mirrorCamera}
					onCheckedChange={(checked) => VoiceSettingsCommands.update({mirrorCamera: checked})}
					data-flx="voice.voice-settings-menus.voice-camera-settings-menu.checkbox-item.mirror-camera"
				>
					{i18n._(MIRROR_CAMERA_DESCRIPTOR)}
				</CheckboxItem>
			</MenuGroup>
			<MenuGroup data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-group--2">
				<MenuItem
					icon={
						<VideoIcon
							weight="fill"
							className={styles.icon}
							data-flx="voice.voice-settings-menus.voice-camera-settings-menu.icon"
						/>
					}
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
					<Trans>Preview camera</Trans>
				</MenuItem>
			</MenuGroup>
			<MenuGroup data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-group--3">
				<MenuItem
					icon={
						<GearIcon
							weight="fill"
							className={styles.icon}
							data-flx="voice.voice-settings-menus.voice-camera-settings-menu.icon--2"
						/>
					}
					onClick={() => openVoiceVideoSettings(onClose, 'video')}
					data-flx="voice.voice-settings-menus.voice-camera-settings-menu.menu-item.close--2"
				>
					{i18n._(VOICE_CAMERA_SETTINGS_DESCRIPTOR)}
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
	const canControlDebugLogging = connectedChannelId != null && (Users.currentUser?.isStaff() ?? false);
	const canOpenDebugEventSink =
		canControlDebugLogging && VoiceDebugEventSinkCommands.canOpenVoiceDebugEventSinkPopout();
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
				{canControlDebugLogging && (
					<CheckboxItem
						icon={
							<ChartBarIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-settings-menus.voice-more-options-menu.icon.debug-logging"
							/>
						}
						checked={MediaEngine.voiceDebugLoggingActive}
						disabled={MediaEngine.voiceDebugLoggingToggleInFlight}
						onCheckedChange={(checked) => {
							void MediaEngine.setVoiceDebugLoggingEnabled(checked);
						}}
						data-flx="voice.voice-settings-menus.voice-more-options-menu.checkbox-item.debug-logging"
					>
						<Trans>Start debug logging session</Trans>
					</CheckboxItem>
				)}
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
