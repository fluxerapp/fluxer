// SPDX-License-Identifier: AGPL-3.0-or-later

import {TURN_OFF_CAMERA_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Permission from '@app/features/permissions/state/Permission';
import NativePermission from '@app/features/permissions/system/state/NativePermission';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {ActiveScreenShareMenu} from '@app/features/voice/components/ActiveScreenShareMenu';
import {
	CameraPreviewModalInRoom,
	CameraPreviewModalStandalone,
} from '@app/features/voice/components/modals/CameraPreviewModal';
import {
	openScreenSharePickerModal,
	preloadScreenSharePickerSources,
} from '@app/features/voice/components/modals/ScreenSharePickerModal';
import styles from '@app/features/voice/components/VoiceConnectionStatus.module.css';
import {VoiceCameraSettingsMenu} from '@app/features/voice/components/VoiceSettingsMenus';
import {selectLocalParticipantControlsViewState} from '@app/features/voice/components/voice_connection_status/LocalParticipantControlsStateMachine';
import {logger} from '@app/features/voice/components/voice_connection_status/shared';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR} from '@app/features/voice/engine/media_engine_facade/shared';
import type {LivekitParticipantSnapshot} from '@app/features/voice/engine/VoiceParticipantStateMachine';
import {useCameraUserCapBlocked} from '@app/features/voice/hooks/useCameraUserCapBlocked';
import {useMediaDevices} from '@app/features/voice/hooks/useMediaDevices';
import ActiveScreenShareSource from '@app/features/voice/state/ActiveScreenShareSource';
import {resolveDisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import {
	VOICE_SHARE_SCREEN_DESCRIPTOR,
	VOICE_TURN_ON_CAMERA_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {VOICE_CHANNEL_CAMERA_USER_LIMIT} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CameraIcon, CameraSlashIcon, MonitorPlayIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type MouseEvent as ReactMouseEvent, useCallback} from 'react';

const WAITING_FOR_CONNECTION_DESCRIPTOR = msg({
	message: 'Waiting for connection...',
	comment:
		'Tooltip explaining that video / screen share controls are gated while the voice connection is not yet ready.',
});
const NO_CAMERA_PERMISSION_DESCRIPTOR = msg({
	message: "You can't turn on your camera in this channel",
	comment: 'Tooltip shown when the user lacks the Video permission in the voice channel. Tone stays plain.',
});
const NO_SCREEN_SHARE_PERMISSION_DESCRIPTOR = msg({
	message: "You can't share your screen in this channel",
	comment: 'Tooltip shown when the user lacks the Screen Share permission in the voice channel. Tone stays plain.',
});
const CONFIGURE_OR_END_SCREEN_SHARE_DESCRIPTOR = msg({
	message: 'Configure or end screen share',
	comment: 'Tooltip / aria label on the screen-share control while a share is active. Opens the configure / end menu.',
});
export const LocalParticipantControls = observer(() => {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const {videoDevices} = useMediaDevices();
	const room = MediaEngine.room;
	const localParticipant = room?.localParticipant;
	const participants = MediaEngine.participants;
	let localParticipantSnapshot: LivekitParticipantSnapshot | null = null;
	for (const participantIdentity in participants) {
		const participant = participants[participantIdentity];
		if (!participant?.isLocal) continue;
		localParticipantSnapshot = participant;
		break;
	}
	const isCameraEnabled = localParticipantSnapshot?.isCameraEnabled ?? false;
	const isScreenShareEnabled = localParticipantSnapshot?.isScreenShareEnabled ?? false;
	const isNativeConnected = !room && MediaEngine.connected;
	const isConnected = Boolean((room && localParticipant) || isNativeConnected);
	const displayShareEnvironment = resolveDisplayShareEnvironment(isDesktop(), NativePermission.isLinuxWaylandDesktop);
	const channelId = MediaEngine.channelId;
	const guildId = MediaEngine.guildId;
	const canStream = !guildId || !channelId || Permission.can(Permissions.STREAM, {channelId});
	const isCameraUserCapReached = useCameraUserCapBlocked(isCameraEnabled);
	const controlState = selectLocalParticipantControlsViewState({
		isConnected,
		canStream,
		isCameraEnabled,
		isCameraUserCapReached,
		isScreenShareEnabled,
	});
	const cameraControlState = controlState.camera;
	const screenShareControlState = controlState.screenShare;
	const handleToggleCamera = useCallback(async () => {
		if (!isConnected) return;
		try {
			if (isCameraEnabled) {
				await MediaEngine.setCameraEnabled(false);
			} else {
				ModalCommands.push(
					modal(() =>
						localParticipant ? (
							<CameraPreviewModalInRoom data-flx="voice.voice-connection-status.handle-toggle-camera.camera-preview-modal-in-room" />
						) : (
							<CameraPreviewModalStandalone
								isCameraEnabled={false}
								onEnableCamera={() => MediaEngine.setCameraEnabled(true)}
								data-flx="voice.voice-connection-status.handle-toggle-camera.camera-preview-modal-standalone"
							/>
						),
					),
				);
			}
		} catch (error) {
			logger.error('Failed to toggle camera:', error);
		}
	}, [isCameraEnabled, isConnected, localParticipant]);
	const openCameraSettingsMenu = useCallback(
		(event: ReactMouseEvent<HTMLElement>) => {
			if (!isConnected) return;
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<VoiceCameraSettingsMenu
					videoDevices={videoDevices}
					onClose={onClose}
					data-flx="voice.voice-connection-status.open-camera-settings-menu.voice-camera-settings-menu"
				/>
			));
		},
		[isConnected, videoDevices],
	);
	const openScreenShareMenu = useCallback(
		(event: ReactMouseEvent<HTMLElement>) => {
			if (!isConnected || !isScreenShareEnabled) return;
			event.preventDefault();
			event.stopPropagation();
			const shareContext = ActiveScreenShareSource.getSourceId()?.startsWith('window:') ? 'app' : 'display';
			const shareContextResolved = ActiveScreenShareSource.getSourceId() != null;
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<ActiveScreenShareMenu
					onClose={onClose}
					displayShareEnvironment={displayShareEnvironment}
					shareContext={shareContext}
					shareContextResolved={shareContextResolved}
					iconClassName={styles.icon}
					data-flx="voice.voice-connection-status.open-screen-share-menu.active-screen-share-menu"
				/>
			));
		},
		[displayShareEnvironment, isConnected, isScreenShareEnabled],
	);
	const handleScreenShare = useCallback(async () => {
		if (!isConnected) return;
		try {
			if (isScreenShareEnabled) {
				return;
			}
			await openScreenSharePickerModal();
		} catch (error) {
			logger.error('Failed to toggle screen share:', error);
		}
	}, [isScreenShareEnabled, isConnected]);
	const handleScreenSharePreload = useCallback(() => {
		if (!isConnected || !screenShareControlState.canPreloadPicker) {
			return;
		}
		void preloadScreenSharePickerSources();
	}, [screenShareControlState.canPreloadPicker, isConnected]);
	const cameraLabel = (() => {
		switch (cameraControlState.labelKey) {
			case 'waitingForConnection':
				return i18n._(WAITING_FOR_CONNECTION_DESCRIPTOR);
			case 'noCameraPermission':
				return i18n._(NO_CAMERA_PERMISSION_DESCRIPTOR);
			case 'cameraUserCapReached':
				return i18n._(VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR, {
					voiceChannelCameraUserLimit: VOICE_CHANNEL_CAMERA_USER_LIMIT,
				});
			case 'turnOffCamera':
				return i18n._(TURN_OFF_CAMERA_DESCRIPTOR);
			case 'turnOnCamera':
				return i18n._(VOICE_TURN_ON_CAMERA_DESCRIPTOR);
		}
	})();
	const screenShareLabel = (() => {
		switch (screenShareControlState.labelKey) {
			case 'waitingForConnection':
				return i18n._(WAITING_FOR_CONNECTION_DESCRIPTOR);
			case 'noScreenSharePermission':
				return i18n._(NO_SCREEN_SHARE_PERMISSION_DESCRIPTOR);
			case 'configureOrEndScreenShare':
				return i18n._(CONFIGURE_OR_END_SCREEN_SHARE_DESCRIPTOR);
			case 'shareScreen':
				return i18n._(VOICE_SHARE_SCREEN_DESCRIPTOR);
		}
	})();
	return (
		<>
			<Tooltip text={cameraLabel} data-flx="voice.voice-connection-status.local-participant-controls.tooltip">
				<FocusRing
					offset={-2}
					enabled={!cameraControlState.disabled}
					data-flx="voice.voice-connection-status.local-participant-controls.focus-ring"
				>
					<button
						type="button"
						className={clsx(styles.mediaButton, isCameraEnabled && styles.cameraActive)}
						onClick={cameraControlState.disabled ? undefined : handleToggleCamera}
						onContextMenu={cameraControlState.canOpenSettings ? openCameraSettingsMenu : undefined}
						disabled={cameraControlState.disabled}
						aria-label={cameraLabel}
						aria-pressed={isCameraEnabled}
						data-flx="voice.voice-connection-status.local-participant-controls.media-button.undefined"
					>
						{isCameraEnabled ? (
							<CameraIcon
								weight="fill"
								className={styles.mediaIcon}
								data-flx="voice.voice-connection-status.local-participant-controls.media-icon"
							/>
						) : (
							<CameraSlashIcon
								weight="fill"
								className={styles.mediaIcon}
								data-flx="voice.voice-connection-status.local-participant-controls.media-icon--2"
							/>
						)}
					</button>
				</FocusRing>
			</Tooltip>
			<Tooltip text={screenShareLabel} data-flx="voice.voice-connection-status.local-participant-controls.tooltip--2">
				<FocusRing
					offset={-2}
					enabled={!screenShareControlState.disabled}
					data-flx="voice.voice-connection-status.local-participant-controls.focus-ring--2"
				>
					<button
						type="button"
						className={clsx(styles.mediaButton, isScreenShareEnabled && styles.screenShareActive)}
						onClick={
							screenShareControlState.disabled
								? undefined
								: (event) => {
										if (screenShareControlState.clickAction === 'openMenu') {
											openScreenShareMenu(event);
											return;
										}
										if (screenShareControlState.clickAction === 'openPicker') {
											void handleScreenShare();
										}
									}
						}
						onContextMenu={screenShareControlState.canOpenMenu ? openScreenShareMenu : undefined}
						onFocus={screenShareControlState.canPreloadPicker ? handleScreenSharePreload : undefined}
						onPointerEnter={screenShareControlState.canPreloadPicker ? handleScreenSharePreload : undefined}
						disabled={screenShareControlState.disabled}
						aria-label={screenShareLabel}
						aria-pressed={isScreenShareEnabled}
						data-flx="voice.voice-connection-status.local-participant-controls.media-button.undefined--2"
					>
						<MonitorPlayIcon
							weight="fill"
							className={styles.mediaIcon}
							data-flx="voice.voice-connection-status.local-participant-controls.media-icon--3"
						/>
					</button>
				</FocusRing>
			</Tooltip>
		</>
	);
});
