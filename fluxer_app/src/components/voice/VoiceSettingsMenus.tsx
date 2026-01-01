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

import {t} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	CameraIcon,
	GearIcon,
	GridFourIcon,
	MicrophoneIcon,
	SpeakerHighIcon,
	SpeakerSimpleSlashIcon,
	SpeakerSlashIcon,
	UsersIcon,
	VideoIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as VoiceCallLayoutActionCreators from '~/actions/VoiceCallLayoutActionCreators';
import * as VoiceSettingsActionCreators from '~/actions/VoiceSettingsActionCreators';
import * as VoiceStateActionCreators from '~/actions/VoiceStateActionCreators';
import {CameraPreviewModalInRoom} from '~/components/modals/CameraPreviewModal';
import {HideOwnCameraConfirmModal} from '~/components/modals/HideOwnCameraConfirmModal';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import {MenuItemCheckbox} from '~/components/uikit/ContextMenu/MenuItemCheckbox';
import {MenuItemRadio} from '~/components/uikit/ContextMenu/MenuItemRadio';
import {MenuItemSlider} from '~/components/uikit/ContextMenu/MenuItemSlider';
import {MenuItemSubmenu} from '~/components/uikit/ContextMenu/MenuItemSubmenu';
import VoiceCallLayoutStore from '~/stores/VoiceCallLayoutStore';
import VoicePromptsStore from '~/stores/VoicePromptsStore';
import VoiceSettingsStore from '~/stores/VoiceSettingsStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import {hasDeviceLabels, resolveEffectiveDeviceId} from '~/utils/VoiceDeviceManager';
import styles from './VoiceSettingsMenus.module.css';

interface VoiceAudioSettingsMenuProps {
	inputDevices: Array<MediaDeviceInfo>;
	outputDevices: Array<MediaDeviceInfo>;
	onClose: () => void;
}

export const VoiceAudioSettingsMenu: React.FC<VoiceAudioSettingsMenuProps> = observer(
	({inputDevices, outputDevices, onClose}) => {
		const {i18n} = useLingui();
		const tt = t(i18n);

		const voiceSettings = VoiceSettingsStore;
		const voiceState = MediaEngineStore.getCurrentUserVoiceState();
		const isDeafened = voiceState?.self_deaf ?? false;

		const handleToggleDeafen = React.useCallback((_checked: boolean) => {
			VoiceStateActionCreators.toggleSelfDeaf(null);
		}, []);

		const effectiveInputDeviceId = resolveEffectiveDeviceId(voiceSettings.inputDeviceId, inputDevices);
		const effectiveOutputDeviceId = resolveEffectiveDeviceId(voiceSettings.outputDeviceId, outputDevices);
		const inputHasLabels = hasDeviceLabels(inputDevices);
		const outputHasLabels = hasDeviceLabels(outputDevices);

		return (
			<>
				<MenuGroup>
					<MenuItemSubmenu
						label={tt`Input Device`}
						icon={<MicrophoneIcon weight="fill" className={styles.icon} />}
						render={() => (
							<>
								{inputHasLabels ? (
									inputDevices.map((device) => (
										<MenuItemRadio
											key={device.deviceId}
											selected={effectiveInputDeviceId === device.deviceId}
											onSelect={() => {
												VoiceSettingsActionCreators.update({inputDeviceId: device.deviceId});
											}}
										>
											{device.label || tt`Microphone ${device.deviceId.slice(0, 8)}`}
										</MenuItemRadio>
									))
								) : (
									<MenuItemRadio key="default" selected={true} onSelect={() => {}}>
										{tt`Default`}
									</MenuItemRadio>
								)}
							</>
						)}
					/>

					<MenuItemSubmenu
						label={tt`Output Device`}
						icon={<SpeakerHighIcon weight="fill" className={styles.icon} />}
						render={() => (
							<>
								{outputHasLabels ? (
									outputDevices.map((device) => (
										<MenuItemRadio
											key={device.deviceId}
											selected={effectiveOutputDeviceId === device.deviceId}
											onSelect={() => {
												VoiceSettingsActionCreators.update({outputDeviceId: device.deviceId});
											}}
										>
											{device.label || tt`Speaker ${device.deviceId.slice(0, 8)}`}
										</MenuItemRadio>
									))
								) : (
									<MenuItemRadio key="default" selected={true} onSelect={() => {}}>
										{tt`Default`}
									</MenuItemRadio>
								)}
							</>
						)}
					/>
				</MenuGroup>

				<MenuGroup>
					<MenuItemSlider
						label={tt`Input Volume`}
						value={voiceSettings.inputVolume}
						minValue={0}
						maxValue={100}
						onChange={(value) => VoiceSettingsActionCreators.update({inputVolume: value})}
						onFormat={(value) => `${Math.round(value)}%`}
					/>
					<MenuItemSlider
						label={tt`Output Volume`}
						value={voiceSettings.outputVolume}
						minValue={0}
						maxValue={100}
						onChange={(value) => VoiceSettingsActionCreators.update({outputVolume: value})}
						onFormat={(value) => `${Math.round(value)}%`}
					/>
				</MenuGroup>

				<MenuGroup>
					<MenuItemCheckbox
						icon={<SpeakerSimpleSlashIcon weight="fill" className={styles.icon} />}
						checked={voiceSettings.echoCancellation}
						onChange={(checked) => VoiceSettingsActionCreators.update({echoCancellation: checked})}
					>
						<Trans>Echo Cancellation</Trans>
					</MenuItemCheckbox>

					<MenuItemCheckbox
						icon={<SpeakerSimpleSlashIcon weight="fill" className={styles.icon} />}
						checked={voiceSettings.noiseSuppression}
						onChange={(checked) => VoiceSettingsActionCreators.update({noiseSuppression: checked})}
					>
						<Trans>Noise Suppression</Trans>
					</MenuItemCheckbox>

					<MenuItemCheckbox
						icon={<MicrophoneIcon weight="fill" className={styles.icon} />}
						checked={voiceSettings.autoGainControl}
						onChange={(checked) => VoiceSettingsActionCreators.update({autoGainControl: checked})}
					>
						<Trans>Auto Gain Control</Trans>
					</MenuItemCheckbox>
				</MenuGroup>

				<MenuGroup>
					<MenuItemCheckbox
						icon={<SpeakerSlashIcon weight="fill" className={styles.icon} />}
						checked={isDeafened}
						onChange={handleToggleDeafen}
					>
						<Trans>Deafen</Trans>
					</MenuItemCheckbox>
				</MenuGroup>

				<MenuGroup>
					<MenuItem
						icon={<GearIcon weight="fill" className={styles.icon} />}
						onClick={() => {
							onClose();
							ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="voice_video" />));
						}}
					>
						<Trans>Voice Settings</Trans>
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
		const tt = t(i18n);

		const voiceSettings = VoiceSettingsStore;

		const isInput = deviceType === 'input';
		const deviceIdKey = isInput ? 'inputDeviceId' : 'outputDeviceId';
		const volumeKey = isInput ? 'inputVolume' : 'outputVolume';

		const storedDeviceId = voiceSettings[deviceIdKey];
		const currentVolume = voiceSettings[volumeKey];

		const effectiveDeviceId = resolveEffectiveDeviceId(storedDeviceId, devices);
		const devicesHaveLabels = hasDeviceLabels(devices);

		const menuLabel = isInput ? tt`Input Device` : tt`Output Device`;
		const volumeLabel = isInput ? tt`Input Volume` : tt`Output Volume`;
		const Icon = isInput ? MicrophoneIcon : SpeakerHighIcon;

		const defaultDeviceName = isInput
			? (deviceId: string) => tt`Microphone ${deviceId.slice(0, 8)}`
			: (deviceId: string) => tt`Speaker ${deviceId.slice(0, 8)}`;

		return (
			<>
				<MenuGroup>
					<MenuItemSubmenu
						label={menuLabel}
						icon={<Icon weight="fill" className={styles.icon} />}
						render={() => (
							<>
								{devicesHaveLabels ? (
									devices.map((device) => (
										<MenuItemRadio
											key={device.deviceId}
											selected={effectiveDeviceId === device.deviceId}
											onSelect={() => {
												VoiceSettingsActionCreators.update({[deviceIdKey]: device.deviceId});
											}}
										>
											{device.label || defaultDeviceName(device.deviceId)}
										</MenuItemRadio>
									))
								) : (
									<MenuItemRadio key="default" selected={true} onSelect={() => {}}>
										{tt`Default`}
									</MenuItemRadio>
								)}
							</>
						)}
					/>
				</MenuGroup>

				<MenuGroup>
					<MenuItemSlider
						label={volumeLabel}
						value={currentVolume}
						minValue={0}
						maxValue={100}
						onChange={(value) => VoiceSettingsActionCreators.update({[volumeKey]: value})}
						onFormat={(value) => `${Math.round(value)}%`}
					/>
				</MenuGroup>

				<MenuGroup>
					<MenuItem
						icon={<GearIcon weight="fill" className={styles.icon} />}
						onClick={() => {
							onClose();
							ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="voice_video" />));
						}}
					>
						<Trans>Voice Settings</Trans>
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
	return <VoiceDeviceSettingsMenu devices={inputDevices} deviceType="input" onClose={onClose} />;
});

interface VoiceOutputSettingsMenuProps {
	outputDevices: Array<MediaDeviceInfo>;
	onClose: () => void;
}

export const VoiceOutputSettingsMenu: React.FC<VoiceOutputSettingsMenuProps> = observer(({outputDevices, onClose}) => {
	return <VoiceDeviceSettingsMenu devices={outputDevices} deviceType="output" onClose={onClose} />;
});

interface VoiceCameraSettingsMenuProps {
	videoDevices: Array<MediaDeviceInfo>;
	onClose: () => void;
}

export const VoiceCameraSettingsMenu: React.FC<VoiceCameraSettingsMenuProps> = observer(({videoDevices, onClose}) => {
	const {i18n} = useLingui();
	const tt = t(i18n);

	const voiceSettings = VoiceSettingsStore;
	const effectiveVideoDeviceId = resolveEffectiveDeviceId(voiceSettings.videoDeviceId, videoDevices);
	const devicesHaveLabels = hasDeviceLabels(videoDevices);

	return (
		<>
			<MenuGroup>
				<MenuItemSubmenu
					label={tt`Camera`}
					icon={<CameraIcon weight="fill" className={styles.icon} />}
					render={() => (
						<>
							{devicesHaveLabels ? (
								videoDevices.map((device) => (
									<MenuItemRadio
										key={device.deviceId}
										selected={effectiveVideoDeviceId === device.deviceId}
										onSelect={() => {
											VoiceSettingsActionCreators.update({videoDeviceId: device.deviceId});
										}}
									>
										{device.label || tt`Camera ${device.deviceId.slice(0, 8)}`}
									</MenuItemRadio>
								))
							) : (
								<MenuItemRadio key="default" selected={true} onSelect={() => {}}>
									{tt`Default`}
								</MenuItemRadio>
							)}
						</>
					)}
				/>
			</MenuGroup>

			<MenuGroup>
				<MenuItem
					icon={<VideoIcon weight="fill" className={styles.icon} />}
					onClick={() => {
						onClose();
						ModalActionCreators.push(modal(() => <CameraPreviewModalInRoom />));
					}}
				>
					<Trans>Preview Camera</Trans>
				</MenuItem>
			</MenuGroup>

			<MenuGroup>
				<MenuItem
					icon={<GearIcon weight="fill" className={styles.icon} />}
					onClick={() => {
						onClose();
						ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="voice_video" />));
					}}
				>
					<Trans>Video Settings</Trans>
				</MenuItem>
			</MenuGroup>
		</>
	);
});

interface VoiceMoreOptionsMenuProps {
	onClose: () => void;
}

export const VoiceMoreOptionsMenu: React.FC<VoiceMoreOptionsMenuProps> = observer(({onClose}) => {
	const voiceSettings = VoiceSettingsStore;
	const layoutMode = VoiceCallLayoutStore.layoutMode;
	const isGrid = layoutMode === 'grid';

	return (
		<>
			<MenuGroup>
				<MenuItemCheckbox
					icon={<GridFourIcon weight="fill" className={styles.icon} />}
					checked={isGrid}
					onChange={(checked) => {
						if (checked) VoiceCallLayoutActionCreators.setLayoutMode('grid');
						else VoiceCallLayoutActionCreators.setLayoutMode('focus');
						VoiceCallLayoutActionCreators.markUserOverride();
					}}
				>
					<Trans>Grid View</Trans>
				</MenuItemCheckbox>

				<MenuItemCheckbox
					icon={<UsersIcon weight="fill" className={styles.icon} />}
					checked={voiceSettings.showMyOwnCamera}
					onChange={(checked) => {
						if (!checked) {
							if (VoicePromptsStore.getSkipHideOwnCameraConfirm()) {
								VoiceSettingsActionCreators.update({showMyOwnCamera: false});
							} else {
								ModalActionCreators.push(modal(() => <HideOwnCameraConfirmModal />));
							}
						} else {
							VoiceSettingsActionCreators.update({showMyOwnCamera: true});
						}
					}}
				>
					<Trans>Show My Own Camera</Trans>
				</MenuItemCheckbox>

				<MenuItemCheckbox
					icon={<UsersIcon weight="fill" className={styles.icon} />}
					checked={voiceSettings.showNonVideoParticipants}
					onChange={(checked) => VoiceSettingsActionCreators.update({showNonVideoParticipants: checked})}
				>
					<Trans>Show Non-Video Participants</Trans>
				</MenuItemCheckbox>
			</MenuGroup>

			<MenuGroup>
				<MenuItem
					icon={<GearIcon weight="fill" className={styles.icon} />}
					onClick={() => {
						onClose();
						ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="voice_video" />));
					}}
				>
					<Trans>Voice & Video Settings</Trans>
				</MenuItem>
			</MenuGroup>
		</>
	);
});
