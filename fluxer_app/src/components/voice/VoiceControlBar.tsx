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
import {useLocalParticipant} from '@livekit/components-react';
import {
	CameraIcon,
	CameraSlashIcon,
	CaretDownIcon,
	DotsThreeIcon,
	MicrophoneIcon,
	MicrophoneSlashIcon,
	MonitorIcon,
	PhoneXIcon,
	SpeakerHighIcon,
	SpeakerSlashIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {ScreenSharePresets} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as VoiceStateActionCreators from '~/actions/VoiceStateActionCreators';
import {
	VoiceAudioSettingsBottomSheet,
	VoiceCameraSettingsBottomSheet,
	VoiceMoreOptionsBottomSheet,
} from '~/components/bottomsheets/VoiceSettingsBottomSheets';
import {CameraPreviewModalInRoom} from '~/components/modals/CameraPreviewModal';
import {ScreenShareSettingsModal} from '~/components/modals/ScreenShareSettingsModal';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {TooltipWithKeybind} from '~/components/uikit/KeybindHint/KeybindHint';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useAudioSettingsMenu} from '~/hooks/useAudioSettingsMenu';
import {useMediaDevices} from '~/hooks/useMediaDevices';
import KeybindStore from '~/stores/KeybindStore';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import VoiceSettingsStore from '~/stores/VoiceSettingsStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import {formatKeyCombo} from '~/utils/KeybindUtils';
import {executeScreenShareOperation} from '~/utils/ScreenShareUtils';
import styles from './VoiceControlBar.module.css';
import {VoiceCameraSettingsMenu, VoiceMoreOptionsMenu} from './VoiceSettingsMenus';

const SCREEN_SHARE_PRESETS = {
	low: ScreenSharePresets.h360fps15,
	medium: ScreenSharePresets.h720fps30,
	high: ScreenSharePresets.h1080fps30,
	ultra: ScreenSharePresets.h1080fps30,
	'4k': ScreenSharePresets.original,
} as const;

const VoiceControlBarInner = observer(function VoiceControlBarInner() {
	const {t} = useLingui();
	const {localParticipant, isCameraEnabled, isMicrophoneEnabled, isScreenShareEnabled} = useLocalParticipant();

	const voiceState = MediaEngineStore.getCurrentUserVoiceState();
	const localSelfMute = LocalVoiceStateStore.selfMute;
	const localSelfDeaf = LocalVoiceStateStore.selfDeaf;

	const voiceSettings = VoiceSettingsStore;
	const isMobile = MobileLayoutStore.isMobileLayout();

	const {inputDevices, outputDevices, videoDevices} = useMediaDevices();

	const [audioSettingsOpen, setAudioSettingsOpen] = useState(false);
	const [cameraSettingsOpen, setCameraSettingsOpen] = useState(false);
	const [moreOptionsOpen, setMoreOptionsOpen] = useState(false);

	const isMuted = voiceState ? voiceState.self_mute : localSelfMute;
	const isDeafened = voiceState ? voiceState.self_deaf : localSelfDeaf;
	const isGuildMuted = voiceState?.mute ?? false;
	const isGuildDeafened = voiceState?.deaf ?? false;

	const muteReason = MediaEngineStore.getMuteReason(voiceState);
	const effectiveMuted = muteReason !== null || isMuted;

	const isPushToTalkEffective = KeybindStore.isPushToTalkEffective();
	const pushToTalkCombo = KeybindStore.getByAction('push_to_talk').combo;
	const pushToTalkHint = isPushToTalkEffective ? formatKeyCombo(pushToTalkCombo) : '';

	const {renderAudioSettingsMenu, handleAudioSettingsContextMenu} = useAudioSettingsMenu({
		inputDevices,
		outputDevices,
		isMobile,
		onOpenMobile: () => setAudioSettingsOpen(true),
	});

	useEffect(() => {
		if (!localParticipant) return;

		const switchMicrophone = async () => {
			if (isMicrophoneEnabled && voiceSettings.inputDeviceId) {
				try {
					await localParticipant.setMicrophoneEnabled(true, {
						deviceId: voiceSettings.inputDeviceId,
						echoCancellation: voiceSettings.echoCancellation,
						noiseSuppression: voiceSettings.noiseSuppression,
						autoGainControl: voiceSettings.autoGainControl,
					});
				} catch (error) {
					console.error('Failed to switch microphone:', error);
				}
			}
		};

		const switchCamera = async () => {
			if (isCameraEnabled && voiceSettings.videoDeviceId) {
				try {
					await localParticipant.setCameraEnabled(true, {
						deviceId: voiceSettings.videoDeviceId,
					});
				} catch (error) {
					console.error('Failed to switch camera:', error);
				}
			}
		};

		switchMicrophone();
		switchCamera();
	}, [
		voiceSettings.inputDeviceId,
		voiceSettings.videoDeviceId,
		voiceSettings.echoCancellation,
		voiceSettings.noiseSuppression,
		voiceSettings.autoGainControl,
		localParticipant,
		isMicrophoneEnabled,
		isCameraEnabled,
	]);

	const handleToggleMute = useCallback(() => {
		VoiceStateActionCreators.toggleSelfMute(null);
	}, []);

	const handleToggleDeafen = useCallback(() => {
		VoiceStateActionCreators.toggleSelfDeaf(null);
	}, []);

	const handleToggleVideo = useCallback(async () => {
		if (!localParticipant) return;

		try {
			if (isCameraEnabled) {
				await MediaEngineStore.setCameraEnabled(false);
			} else {
				ModalActionCreators.push(
					modal(() => (
						<CameraPreviewModalInRoom
							onEnabled={async () => {
								await MediaEngineStore.setCameraEnabled(true, {
									deviceId: VoiceSettingsStore.getVideoDeviceId() || undefined,
								});
							}}
						/>
					)),
				);
			}
		} catch (error) {
			console.error('Failed to toggle camera:', error);
		}
	}, [localParticipant, isCameraEnabled]);

	const getScreenShareOptions = useCallback(
		(resolution: 'low' | 'medium' | 'high' | 'ultra' | '4k', frameRate: number) => {
			const preset = SCREEN_SHARE_PRESETS[resolution];
			return {
				captureOptions: {
					audio: true,
					selfBrowserSurface: 'include' as const,
					resolution: preset.resolution,
					frameRate,
				},
				publishOptions: {
					screenShareEncoding: preset.encoding,
				},
			};
		},
		[],
	);

	const handleScreenShare = useCallback(async () => {
		if (!localParticipant) return;

		try {
			if (isScreenShareEnabled) {
				await MediaEngineStore.setScreenShareEnabled(false);
			} else {
				ModalActionCreators.push(
					modal(() => (
						<ScreenShareSettingsModal
							onStartShare={async (resolution, frameRate, includeAudio) => {
								await executeScreenShareOperation(async () => {
									const {captureOptions, publishOptions} = getScreenShareOptions(resolution, frameRate);
									await MediaEngineStore.setScreenShareEnabled(
										true,
										{
											...captureOptions,
											audio: includeAudio,
										},
										publishOptions,
									);
								});
							}}
						/>
					)),
				);
			}
		} catch (error) {
			console.error('Failed to toggle screen share:', error);
		}
	}, [localParticipant, isScreenShareEnabled, getScreenShareOptions]);

	const handleDisconnect = useCallback(async () => {
		await MediaEngineStore.disconnectFromVoiceChannel('user');
	}, []);

	const handleAudioSettingsClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			if (isMobile) {
				setAudioSettingsOpen(true);
			} else {
				ContextMenuActionCreators.openFromEvent(event, renderAudioSettingsMenu);
			}
		},
		[isMobile, renderAudioSettingsMenu],
	);

	const handleCameraSettingsClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			if (isMobile) {
				setCameraSettingsOpen(true);
			} else {
				ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
					<VoiceCameraSettingsMenu videoDevices={videoDevices} onClose={onClose} />
				));
			}
		},
		[videoDevices, isMobile],
	);

	const handleMoreOptionsClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			if (isMobile) {
				setMoreOptionsOpen(true);
			} else {
				ContextMenuActionCreators.openFromEvent(event, ({onClose}) => <VoiceMoreOptionsMenu onClose={onClose} />);
			}
		},
		[isMobile],
	);

	const getMuteTooltipLabel = useCallback(() => {
		if (isGuildMuted) return t`Community Muted`;

		switch (muteReason) {
			case 'push_to_talk':
				return t`Push-to-talk enabled — hold ${pushToTalkHint} to speak`;
			default:
				return effectiveMuted ? t`Unmute` : t`Mute`;
		}
	}, [effectiveMuted, isGuildMuted, muteReason, pushToTalkHint, t]);

	const getDeafenTooltipLabel = useCallback(() => {
		if (isGuildDeafened) return t`Community Deafened`;

		switch (isDeafened) {
			case true:
				return t`Undeafen`;
			default:
				return t`Deafen`;
		}
	}, [isDeafened, isGuildDeafened, t]);

	return (
		<div className={styles.container}>
			<div className={styles.buttonContainer}>
				<Tooltip
					text={() => (
						<TooltipWithKeybind label={getMuteTooltipLabel()} action={isGuildMuted ? undefined : 'toggle_mute'} />
					)}
				>
					<FocusRing offset={-2}>
						<div>
							<button
								type="button"
								className={clsx(
									styles.button,
									effectiveMuted || isGuildMuted ? styles.buttonMuted : styles.buttonUnmuted,
									isGuildMuted && 'disabled',
								)}
								onClick={isGuildMuted ? undefined : handleToggleMute}
								onContextMenu={handleAudioSettingsContextMenu}
								disabled={isGuildMuted}
							>
								{effectiveMuted || isGuildMuted ? (
									<MicrophoneSlashIcon weight="fill" className={styles.icon} />
								) : (
									<MicrophoneIcon weight="fill" className={styles.icon} />
								)}
							</button>
						</div>
					</FocusRing>
				</Tooltip>

				<Tooltip text={t`Audio Settings`}>
					<FocusRing offset={-2}>
						<button type="button" className={styles.settingsButton} onClick={handleAudioSettingsClick}>
							<CaretDownIcon weight="bold" className={styles.iconSmall} />
						</button>
					</FocusRing>
				</Tooltip>
			</div>

			<Tooltip
				text={() => (
					<TooltipWithKeybind label={getDeafenTooltipLabel()} action={isGuildDeafened ? undefined : 'toggle_deafen'} />
				)}
			>
				<FocusRing offset={-2}>
					<div>
						<button
							type="button"
							className={clsx(
								styles.button,
								isDeafened || isGuildDeafened ? styles.buttonDeafened : styles.buttonUndeafened,
								isGuildDeafened && 'disabled',
							)}
							onClick={isGuildDeafened ? undefined : handleToggleDeafen}
							onContextMenu={handleAudioSettingsContextMenu}
							disabled={isGuildDeafened}
						>
							{isDeafened || isGuildDeafened ? (
								<SpeakerSlashIcon weight="fill" className={styles.icon} />
							) : (
								<SpeakerHighIcon weight="fill" className={styles.icon} />
							)}
						</button>
					</div>
				</FocusRing>
			</Tooltip>

			<div className={styles.buttonContainer}>
				<Tooltip text={isCameraEnabled ? t`Turn Off Camera` : t`Turn On Camera`}>
					<FocusRing offset={-2}>
						<button
							type="button"
							className={clsx(styles.button, isCameraEnabled ? styles.buttonCameraOn : styles.buttonCameraOff)}
							onClick={handleToggleVideo}
						>
							{isCameraEnabled ? (
								<CameraIcon weight="fill" className={styles.icon} />
							) : (
								<CameraSlashIcon weight="fill" className={styles.icon} />
							)}
						</button>
					</FocusRing>
				</Tooltip>

				<Tooltip text={t`Camera Settings`}>
					<FocusRing offset={-2}>
						<button type="button" className={styles.settingsButton} onClick={handleCameraSettingsClick}>
							<CaretDownIcon weight="bold" className={styles.iconSmall} />
						</button>
					</FocusRing>
				</Tooltip>
			</div>

			<Tooltip text={isScreenShareEnabled ? t`Stop Sharing` : t`Share Your Screen`}>
				<FocusRing offset={-2}>
					<button
						type="button"
						className={clsx(
							styles.button,
							isScreenShareEnabled ? styles.buttonScreenShareOn : styles.buttonScreenShareOff,
						)}
						onClick={handleScreenShare}
					>
						<MonitorIcon weight="fill" className={styles.icon} />
					</button>
				</FocusRing>
			</Tooltip>

			<Tooltip text={t`More Options`}>
				<FocusRing offset={-2}>
					<button
						type="button"
						className={clsx(styles.button, styles.buttonMoreOptions)}
						onClick={handleMoreOptionsClick}
					>
						<DotsThreeIcon weight="bold" className={styles.icon} />
					</button>
				</FocusRing>
			</Tooltip>

			<Tooltip text={t`Disconnect`}>
				<FocusRing offset={-2}>
					<button type="button" className={clsx(styles.button, styles.buttonDisconnect)} onClick={handleDisconnect}>
						<PhoneXIcon weight="fill" className={styles.icon} />
					</button>
				</FocusRing>
			</Tooltip>

			{isMobile && (
				<>
					<VoiceAudioSettingsBottomSheet isOpen={audioSettingsOpen} onClose={() => setAudioSettingsOpen(false)} />
					<VoiceCameraSettingsBottomSheet isOpen={cameraSettingsOpen} onClose={() => setCameraSettingsOpen(false)} />
					<VoiceMoreOptionsBottomSheet isOpen={moreOptionsOpen} onClose={() => setMoreOptionsOpen(false)} />
				</>
			)}
		</div>
	);
});

export const VoiceControlBar = observer(() => {
	const room = MediaEngineStore.room;
	if (!room) return null;
	return <VoiceControlBarInner />;
});
