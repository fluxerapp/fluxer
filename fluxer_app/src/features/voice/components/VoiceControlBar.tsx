// SPDX-License-Identifier: AGPL-3.0-or-later

import * as VoiceStateCommands from '@app/features/devtools/commands/VoiceStateCommands';
import {MORE_OPTIONS_DESCRIPTOR, TURN_OFF_CAMERA_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Keybind from '@app/features/input/state/InputKeybind';
import {formatKeyCombo} from '@app/features/input/utils/KeybindUtils';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import Permission from '@app/features/permissions/state/Permission';
import NativePermission from '@app/features/permissions/system/state/NativePermission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {TooltipWithKeybind} from '@app/features/ui/keybind_hint/KeybindHint';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {
	VoiceAudioSettingsBottomSheet,
	VoiceCameraSettingsBottomSheet,
	VoiceMoreOptionsBottomSheet,
} from '@app/features/voice/components/bottomsheets/VoiceSettingsBottomSheets';
import {
	CameraPreviewModalInRoom,
	CameraPreviewModalStandalone,
} from '@app/features/voice/components/modals/CameraPreviewModal';
import {
	openScreenSharePickerModal,
	openScreenShareSourceSwitcherModal,
	preloadScreenSharePickerSources,
} from '@app/features/voice/components/modals/ScreenSharePickerModal';
import {getStreamKey} from '@app/features/voice/components/StreamKeys';
import {StreamSettingsMenuContent} from '@app/features/voice/components/StreamSettingsMenuContent';
import styles from '@app/features/voice/components/VoiceControlBar.module.css';
import {
	transitionVoiceControlBarState,
	type VoiceControlBarCameraLabel,
	type VoiceControlBarDeafenLabel,
	type VoiceControlBarMuteLabel,
	type VoiceControlBarScreenShareLabel,
} from '@app/features/voice/components/VoiceControlBarStateMachine';
import {
	openVoiceVideoSettings,
	VoiceCameraSettingsMenu,
	VoiceInputSettingsMenu,
	VoiceMoreOptionsMenu,
	VoiceOutputSettingsMenu,
} from '@app/features/voice/components/VoiceSettingsMenus';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR} from '@app/features/voice/engine/media_engine_facade/shared';
import {getEffectiveAudioState} from '@app/features/voice/engine/VoiceEffectiveAudioState';
import {selectVoiceMediaGraphViewerStreamKeys} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import type {LivekitParticipantSnapshot} from '@app/features/voice/engine/VoiceParticipantStateMachine';
import {stopWatchingStreamKey} from '@app/features/voice/engine/VoiceStreamWatchState';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {useCameraUserCapBlocked} from '@app/features/voice/hooks/useCameraUserCapBlocked';
import {useMediaDevices} from '@app/features/voice/hooks/useMediaDevices';
import ActiveScreenShareSource from '@app/features/voice/state/ActiveScreenShareSource';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import VoiceCallLayout from '@app/features/voice/state/VoiceCallLayout';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {resolveDisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import {
	getVoiceDeafenedByModeratorsStatusLabel,
	VOICE_CAMERA_SETTINGS_DESCRIPTOR,
	VOICE_DEAFEN_DESCRIPTOR,
	VOICE_DISCONNECT_DESCRIPTOR,
	VOICE_INPUT_SETTINGS_DESCRIPTOR,
	VOICE_MUTED_BY_MODERATORS_DESCRIPTOR,
	VOICE_NO_SPEAK_PERMISSION_DESCRIPTOR,
	VOICE_OUTPUT_SETTINGS_DESCRIPTOR,
	VOICE_SCREEN_SHARE_SETTINGS_DESCRIPTOR,
	VOICE_SHARE_SCREEN_DESCRIPTOR,
	VOICE_TURN_ON_CAMERA_DESCRIPTOR,
	VOICE_UNDEAFEN_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {parseVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {VOICE_CHANNEL_CAMERA_USER_LIMIT} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useMaybeRoomContext} from '@livekit/components-react';
import {
	CameraIcon,
	CameraSlashIcon,
	CaretDownIcon,
	DotsThreeIcon,
	EyeSlashIcon,
	GearIcon,
	MicrophoneIcon,
	MicrophoneSlashIcon,
	MonitorPlayIcon,
	PhoneXIcon,
	SpeakerHighIcon,
	SpeakerSlashIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {type LocalParticipant, type Room, RoomEvent} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const CHANGE_SOURCE_DESCRIPTOR = msg({
	message: 'Change source',
	comment: 'Voice control menu action to choose a different screen-share source.',
});
const END_SCREEN_SHARE_DESCRIPTOR = msg({
	message: 'End screen share',
	comment: 'Voice control menu action to stop sharing the screen.',
});
const PUSH_TO_TALK_HOLD_HINT_DESCRIPTOR = msg({
	message: 'Push-to-talk is on. Hold {pushToTalkHint} to speak.',
	comment:
		'Tooltip on the mic button in the voice control bar when push-to-talk is enabled. {pushToTalkHint} is the formatted shortcut key combo.',
});
const UNMUTE_DESCRIPTOR = msg({
	message: 'Unmute',
	comment: "Voice control button label to turn the user's microphone back on.",
	context: 'voice-control-action',
});
const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	comment: "Voice control button label to turn the user's microphone off.",
	context: 'voice-control-action',
});
const NO_CAMERA_PERMISSION_DESCRIPTOR = msg({
	message: "You can't turn on your camera in this channel",
	comment:
		'Tooltip in the voice control bar when the user lacks the Video permission in this channel. Tone stays plain.',
});
const CAMERAS_DISABLED_LIMIT_DESCRIPTOR = msg({
	message: 'Cameras are off above {voiceChannelCameraUserLimit} participants',
	comment:
		'Tooltip in the voice control bar when cameras are disabled because the participant count exceeds the limit. {voiceChannelCameraUserLimit} is the configured threshold.',
});
const NO_SCREEN_SHARE_PERMISSION_DESCRIPTOR = msg({
	message: "You can't share your screen in this channel",
	comment:
		'Tooltip in the voice control bar when the user lacks the Screen Share permission in this channel. Tone stays plain.',
});
const AUDIO_SETTINGS_DESCRIPTOR = msg({
	message: 'Audio settings',
	comment: 'Voice control button label for microphone and speaker settings.',
	context: 'voice-control-button',
});
const STOP_WATCHING_STREAM_DESCRIPTOR = msg({
	message: 'Stop watching stream',
	comment: 'Tooltip / button label in the voice control bar that stops watching a remote screen share.',
});
const logger = new Logger('VoiceControlBar');

interface VoiceControlLocalMediaState {
	localParticipant: LocalParticipant | null;
	isCameraEnabled: boolean;
	isScreenShareEnabled: boolean;
	isConnected: boolean;
}

type VoiceControlMenuRenderer = (props: {onClose: () => void}) => React.ReactNode;

const LOCAL_MEDIA_UPDATE_EVENTS: ReadonlyArray<RoomEvent> = [
	RoomEvent.LocalTrackPublished,
	RoomEvent.LocalTrackUnpublished,
	RoomEvent.TrackMuted,
	RoomEvent.TrackUnmuted,
	RoomEvent.ConnectionStateChanged,
];

function getLiveKitLocalMediaState(room: Room): VoiceControlLocalMediaState {
	const {localParticipant} = room;
	return {
		localParticipant,
		isCameraEnabled: localParticipant.isCameraEnabled,
		isScreenShareEnabled: localParticipant.isScreenShareEnabled,
		isConnected: true,
	};
}

function useVoiceControlLocalMediaState(): VoiceControlLocalMediaState {
	useMediaEngineVersion();
	const room = useMaybeRoomContext();
	const [liveKitState, setLiveKitState] = useState<VoiceControlLocalMediaState | null>(() =>
		room ? getLiveKitLocalMediaState(room) : null,
	);

	useEffect(() => {
		if (!room) {
			setLiveKitState(null);
			return;
		}
		const update = () => setLiveKitState(getLiveKitLocalMediaState(room));
		update();
		for (const event of LOCAL_MEDIA_UPDATE_EVENTS) {
			room.on(event, update);
		}
		return () => {
			for (const event of LOCAL_MEDIA_UPDATE_EVENTS) {
				room.off(event, update);
			}
		};
	}, [room]);

	if (room) {
		return liveKitState ?? getLiveKitLocalMediaState(room);
	}

	let localParticipantSnapshot: LivekitParticipantSnapshot | null = null;
	for (const participantIdentity in MediaEngine.participants) {
		const participant = MediaEngine.participants[participantIdentity];
		if (!participant?.isLocal) continue;
		localParticipantSnapshot = participant;
		break;
	}
	const isConnected = MediaEngine.connected;
	return {
		localParticipant: null,
		isCameraEnabled: localParticipantSnapshot?.isCameraEnabled ?? false,
		isScreenShareEnabled: localParticipantSnapshot?.isScreenShareEnabled ?? false,
		isConnected,
	};
}

const VoiceControlBarInner = observer(function VoiceControlBarInner() {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const {localParticipant, isCameraEnabled, isScreenShareEnabled, isConnected} = useVoiceControlLocalMediaState();
	const voiceState = MediaEngine.getCurrentUserVoiceState();
	const localSelfMute = LocalVoiceState.getSelfMute();
	const localSelfDeaf = LocalVoiceState.getSelfDeaf();
	const voiceSettings = VoiceSettings;
	const previousVideoDeviceIdRef = useRef(voiceSettings.videoDeviceId);
	const channelId = MediaEngine.channelId;
	const guildId = MediaEngine.guildId;
	const isMobile = MobileLayout.isMobileLayout();
	const {inputDevices, outputDevices, videoDevices} = useMediaDevices();
	const [audioSettingsOpen, setAudioSettingsOpen] = useState(false);
	const [cameraSettingsOpen, setCameraSettingsOpen] = useState(false);
	const [moreOptionsOpen, setMoreOptionsOpen] = useState(false);
	const isMuted = localSelfMute;
	const isDeafened = localSelfDeaf;
	const isGuildMuted = voiceState?.mute ?? false;
	const isGuildDeafened = voiceState?.deaf ?? false;
	const canStream = !guildId || !channelId || Permission.can(Permissions.STREAM, {channelId});
	const screenShareDisabled = !canStream && !isScreenShareEnabled;
	const muteReason = MediaEngine.getMuteReason(voiceState);
	const isPermissionMuted = muteReason === 'permission';
	const isPushToTalkEffective = Keybind.isPushToTalkEffective();
	const effectiveAudioState = getEffectiveAudioState({
		selfMute: localSelfMute,
		selfDeaf: localSelfDeaf,
		serverMute: isGuildMuted || isPermissionMuted,
		serverDeaf: voiceState?.deaf,
	});
	const effectiveMuted = effectiveAudioState.effectiveMute || muteReason !== null || isMuted || isPermissionMuted;
	const pushToTalkCombo = Keybind.getByAction('voice_push_to_talk').combo;
	const pushToTalkHint = formatKeyCombo(pushToTalkCombo);
	const displayShareEnvironment = resolveDisplayShareEnvironment(isDesktop(), NativePermission.isLinuxWaylandDesktop);
	const disconnectLabel = i18n._(VOICE_DISCONNECT_DESCRIPTOR);
	const renderInputSettingsMenu = useCallback(
		({onClose}: {onClose: () => void}) => (
			<VoiceInputSettingsMenu
				inputDevices={inputDevices}
				onClose={onClose}
				data-flx="voice.voice-control-bar.render-input-settings-menu.voice-input-settings-menu"
			/>
		),
		[inputDevices],
	);
	const renderOutputSettingsMenu = useCallback(
		({onClose}: {onClose: () => void}) => (
			<VoiceOutputSettingsMenu
				outputDevices={outputDevices}
				onClose={onClose}
				data-flx="voice.voice-control-bar.render-output-settings-menu.voice-output-settings-menu"
			/>
		),
		[outputDevices],
	);
	const renderCameraSettingsMenu = useCallback(
		({onClose}: {onClose: () => void}) => (
			<VoiceCameraSettingsMenu
				videoDevices={videoDevices}
				onClose={onClose}
				data-flx="voice.voice-control-bar.render-camera-settings-menu.voice-camera-settings-menu"
			/>
		),
		[videoDevices],
	);
	const openAnchoredMenu = useCallback((event: React.MouseEvent<HTMLElement>, renderMenu: VoiceControlMenuRenderer) => {
		ContextMenuCommands.openAboveElementBottomLeft(event, renderMenu);
	}, []);
	const openPointerMenu = useCallback((event: React.MouseEvent<HTMLElement>, renderMenu: VoiceControlMenuRenderer) => {
		ContextMenuCommands.openFromEvent(event, renderMenu);
	}, []);
	useEffect(() => {
		const previousVideoDeviceId = previousVideoDeviceIdRef.current;
		previousVideoDeviceIdRef.current = voiceSettings.videoDeviceId;
		if (!isConnected) return;
		if (!isCameraEnabled) return;
		if (!voiceSettings.videoDeviceId) return;
		if (voiceSettings.videoDeviceId === previousVideoDeviceId) return;
		const deviceId = voiceSettings.videoDeviceId === 'default' ? undefined : voiceSettings.videoDeviceId;
		const switchCamera = async () => {
			try {
				await MediaEngine.updateActiveCameraCapture({deviceId});
			} catch (error) {
				logger.error('Failed to switch camera:', error);
			}
		};
		void switchCamera();
	}, [voiceSettings.videoDeviceId, isConnected, isCameraEnabled]);
	const handleToggleMute = useCallback(() => {
		VoiceStateCommands.toggleSelfMute(null);
	}, []);
	const handleToggleDeafen = useCallback(() => {
		VoiceStateCommands.toggleSelfDeaf(null);
	}, []);
	const handleToggleVideo = useCallback(async () => {
		if (!isConnected) return;
		try {
			if (isCameraEnabled) {
				await MediaEngine.setCameraEnabled(false);
			} else {
				const voiceStates = MediaEngine.getAllVoiceStatesInChannel(
					MediaEngine.guildId ?? '',
					MediaEngine.channelId ?? '',
				);
				const participantCount = Object.keys(voiceStates).length;
				if (participantCount > VOICE_CHANNEL_CAMERA_USER_LIMIT) {
					return;
				}
				ModalCommands.push(
					modal(() =>
						localParticipant ? (
							<CameraPreviewModalInRoom data-flx="voice.voice-control-bar.handle-toggle-video.camera-preview-modal-in-room" />
						) : (
							<CameraPreviewModalStandalone
								isCameraEnabled={false}
								onEnableCamera={() => MediaEngine.setCameraEnabled(true)}
								data-flx="voice.voice-control-bar.handle-toggle-video.camera-preview-modal-standalone"
							/>
						),
					),
				);
			}
		} catch (error) {
			logger.error('Failed to toggle camera:', error);
		}
	}, [localParticipant, isCameraEnabled, isConnected]);
	const renderScreenShareMenu = useCallback(
		({onClose}: {onClose: () => void}) => (
			<>
				{isScreenShareEnabled && (
					<MenuGroup data-flx="voice.voice-control-bar.render-screen-share-menu.menu-group">
						<MenuItem
							icon={
								<MonitorPlayIcon
									weight="fill"
									className={styles.icon}
									data-flx="voice.voice-control-bar.render-screen-share-menu.icon"
								/>
							}
							onClick={async () => {
								onClose();
								try {
									await openScreenShareSourceSwitcherModal();
								} catch (error) {
									logger.error('Failed to open screen share source switcher:', error);
								}
							}}
							data-flx="voice.voice-control-bar.render-screen-share-menu.menu-item.close"
						>
							{i18n._(CHANGE_SOURCE_DESCRIPTOR)}
						</MenuItem>
						<MenuItem
							icon={
								<MonitorPlayIcon
									weight="fill"
									className={styles.icon}
									data-flx="voice.voice-control-bar.render-screen-share-menu.icon--2"
								/>
							}
							danger
							onClick={async () => {
								onClose();
								await MediaEngine.setScreenShareEnabled(false);
							}}
							data-flx="voice.voice-control-bar.render-screen-share-menu.menu-item.close--2"
						>
							{i18n._(END_SCREEN_SHARE_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
				)}
				<StreamSettingsMenuContent
					applyToLiveStream={isScreenShareEnabled}
					displayShareEnvironment={displayShareEnvironment}
					shareContext={ActiveScreenShareSource.getSourceId()?.startsWith('window:') ? 'app' : 'display'}
					shareContextResolved={ActiveScreenShareSource.getSourceId() != null}
					data-flx="voice.voice-control-bar.render-screen-share-menu.stream-settings-menu-content"
				/>
				<MenuGroup data-flx="voice.voice-control-bar.render-screen-share-menu.menu-group--2">
					<MenuItem
						icon={
							<GearIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-control-bar.render-screen-share-menu.gear-icon"
							/>
						}
						onClick={() => openVoiceVideoSettings(onClose, 'video')}
						data-flx="voice.voice-control-bar.render-screen-share-menu.menu-item.open-video-settings"
					>
						{i18n._(VOICE_SCREEN_SHARE_SETTINGS_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
			</>
		),
		[displayShareEnvironment, isScreenShareEnabled, i18n],
	);
	const openScreenShareMenu = useCallback(
		(event: React.MouseEvent<HTMLElement>, options?: {anchorToButton?: boolean}) => {
			if (!isConnected || screenShareDisabled) return;
			if (options?.anchorToButton) {
				openAnchoredMenu(event, renderScreenShareMenu);
				return;
			}
			openPointerMenu(event, renderScreenShareMenu);
		},
		[isConnected, openAnchoredMenu, openPointerMenu, renderScreenShareMenu, screenShareDisabled],
	);
	const handleScreenShare = useCallback(async () => {
		if (!isConnected) return;
		try {
			if (isScreenShareEnabled) {
				await MediaEngine.setScreenShareEnabled(false);
				return;
			}
			await openScreenSharePickerModal();
		} catch (error) {
			logger.error('Failed to toggle screen share:', error);
		}
	}, [isScreenShareEnabled, isConnected]);
	const handleScreenSharePreload = useCallback(() => {
		if (!isConnected || isScreenShareEnabled) {
			return;
		}
		void preloadScreenSharePickerSources();
	}, [isScreenShareEnabled, isConnected]);
	const handleDisconnect = useCallback(async () => {
		try {
			await MediaEngine.disconnectFromVoiceChannel('user');
		} catch (error) {
			logger.error('Failed to disconnect from voice channel', error);
		}
	}, []);
	const viewerStreamKeys = selectVoiceMediaGraphViewerStreamKeys(voiceMediaGraphStore.graph);
	const focusedScreenShareStreamKey = useMemo(() => {
		if (VoiceCallLayout.pinnedParticipantSource !== VoiceTrackSource.ScreenShare) return null;
		const identity = VoiceCallLayout.pinnedParticipantIdentity;
		const channelId = MediaEngine.channelId;
		if (!identity || !channelId) return null;
		const parsedIdentity = parseVoiceParticipantIdentity(identity);
		if (!parsedIdentity.connectionId) return null;
		return getStreamKey(MediaEngine.guildId, channelId, parsedIdentity.connectionId);
	}, [
		MediaEngine.channelId,
		MediaEngine.guildId,
		VoiceCallLayout.pinnedParticipantIdentity,
		VoiceCallLayout.pinnedParticipantSource,
	]);
	const isFocusedStreamOnLocalDevice = useMemo(() => {
		if (VoiceCallLayout.pinnedParticipantSource !== VoiceTrackSource.ScreenShare) return false;
		const identity = VoiceCallLayout.pinnedParticipantIdentity;
		if (!identity) return false;
		const parsedIdentity = parseVoiceParticipantIdentity(identity);
		if (!parsedIdentity.connectionId) return false;
		return parsedIdentity.connectionId === MediaEngine.connectionId;
	}, [MediaEngine.connectionId, VoiceCallLayout.pinnedParticipantIdentity, VoiceCallLayout.pinnedParticipantSource]);
	const canStopWatchingFocusedStream =
		focusedScreenShareStreamKey != null &&
		viewerStreamKeys.includes(focusedScreenShareStreamKey) &&
		!isFocusedStreamOnLocalDevice;
	const handleStopWatching = useCallback(() => {
		if (!focusedScreenShareStreamKey) return;
		if (
			stopWatchingStreamKey(focusedScreenShareStreamKey, {
				guildId: MediaEngine.guildId,
				channelId: MediaEngine.channelId,
			})
		) {
			SoundCommands.playSound(SoundType.ViewerLeave);
		}
	}, [focusedScreenShareStreamKey, MediaEngine.channelId, MediaEngine.guildId]);
	const handleInputSettingsClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			if (isMobile) {
				setAudioSettingsOpen(true);
			} else {
				openAnchoredMenu(event, renderInputSettingsMenu);
			}
		},
		[isMobile, openAnchoredMenu, renderInputSettingsMenu],
	);
	const handleOutputSettingsClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			if (isMobile) {
				setAudioSettingsOpen(true);
			} else {
				openAnchoredMenu(event, renderOutputSettingsMenu);
			}
		},
		[isMobile, openAnchoredMenu, renderOutputSettingsMenu],
	);
	const handleMuteContextMenu = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			event.preventDefault();
			event.stopPropagation();
			if (isMobile) {
				setAudioSettingsOpen(true);
				return;
			}
			openPointerMenu(event, renderInputSettingsMenu);
		},
		[isMobile, openPointerMenu, renderInputSettingsMenu],
	);
	const handleDeafenContextMenu = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			event.preventDefault();
			event.stopPropagation();
			if (isMobile) {
				setAudioSettingsOpen(true);
				return;
			}
			openPointerMenu(event, renderOutputSettingsMenu);
		},
		[isMobile, openPointerMenu, renderOutputSettingsMenu],
	);
	const handleCameraSettingsClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			if (isMobile) {
				setCameraSettingsOpen(true);
			} else {
				openAnchoredMenu(event, renderCameraSettingsMenu);
			}
		},
		[isMobile, openAnchoredMenu, renderCameraSettingsMenu],
	);
	const handleCameraSettingsContextMenu = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			event.preventDefault();
			event.stopPropagation();
			if (isMobile) {
				setCameraSettingsOpen(true);
				return;
			}
			openPointerMenu(event, renderCameraSettingsMenu);
		},
		[isMobile, openPointerMenu, renderCameraSettingsMenu],
	);
	const handleMoreOptionsClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			if (isMobile) {
				setMoreOptionsOpen(true);
			} else {
				openAnchoredMenu(event, ({onClose}) => (
					<VoiceMoreOptionsMenu
						onClose={onClose}
						data-flx="voice.voice-control-bar.handle-more-options-click.voice-more-options-menu"
					/>
				));
			}
		},
		[isMobile, openAnchoredMenu],
	);
	const isCameraLimitReached = useMemo(() => {
		if (isCameraEnabled) return false;
		const voiceStates = MediaEngine.getAllVoiceStatesInChannel(MediaEngine.guildId ?? '', MediaEngine.channelId ?? '');
		return Object.keys(voiceStates).length > VOICE_CHANNEL_CAMERA_USER_LIMIT;
	}, [isCameraEnabled]);
	const isCameraUserCapReached = useCameraUserCapBlocked(isCameraEnabled);
	const controlState = transitionVoiceControlBarState({
		isDeafened,
		isGuildMuted,
		isGuildDeafened,
		isPermissionMuted,
		isPushToTalkEffective,
		effectiveMuted,
		canStream,
		isCameraEnabled,
		isCameraLimitReached,
		isCameraUserCapReached,
		isScreenShareEnabled,
		screenShareDisabled,
	});
	const getMuteTooltipLabel = useCallback(
		(label: VoiceControlBarMuteLabel) => {
			switch (label) {
				case 'moderatorDeafened':
					return getVoiceDeafenedByModeratorsStatusLabel(i18n, true);
				case 'moderatorMuted':
					return i18n._(VOICE_MUTED_BY_MODERATORS_DESCRIPTOR);
				case 'permissionMuted':
					return i18n._(VOICE_NO_SPEAK_PERMISSION_DESCRIPTOR);
				case 'pushToTalkHoldHint':
					return i18n._(PUSH_TO_TALK_HOLD_HINT_DESCRIPTOR, {pushToTalkHint});
				case 'unmute':
					return i18n._(UNMUTE_DESCRIPTOR);
				default:
					return i18n._(MUTE_DESCRIPTOR);
			}
		},
		[i18n, pushToTalkHint],
	);
	const getDeafenTooltipLabel = useCallback(
		(label: VoiceControlBarDeafenLabel) => {
			switch (label) {
				case 'moderatorDeafened':
					return getVoiceDeafenedByModeratorsStatusLabel(i18n, true);
				case 'undeafen':
					return i18n._(VOICE_UNDEAFEN_DESCRIPTOR);
				default:
					return i18n._(VOICE_DEAFEN_DESCRIPTOR);
			}
		},
		[i18n],
	);
	const getCameraTooltipLabel = useCallback(
		(label: VoiceControlBarCameraLabel) => {
			switch (label) {
				case 'noPermission':
					return i18n._(NO_CAMERA_PERMISSION_DESCRIPTOR);
				case 'limitReached':
					return i18n._(CAMERAS_DISABLED_LIMIT_DESCRIPTOR, {
						voiceChannelCameraUserLimit: VOICE_CHANNEL_CAMERA_USER_LIMIT,
					});
				case 'userCapReached':
					return i18n._(VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR, {
						voiceChannelCameraUserLimit: VOICE_CHANNEL_CAMERA_USER_LIMIT,
					});
				case 'turnOff':
					return i18n._(TURN_OFF_CAMERA_DESCRIPTOR);
				default:
					return i18n._(VOICE_TURN_ON_CAMERA_DESCRIPTOR);
			}
		},
		[i18n],
	);
	const getScreenShareTooltipLabel = useCallback(
		(label: VoiceControlBarScreenShareLabel) => {
			switch (label) {
				case 'noPermission':
					return i18n._(NO_SCREEN_SHARE_PERMISSION_DESCRIPTOR);
				case 'end':
					return i18n._(END_SCREEN_SHARE_DESCRIPTOR);
				default:
					return i18n._(VOICE_SHARE_SCREEN_DESCRIPTOR);
			}
		},
		[i18n],
	);
	const muteControlLabel = getMuteTooltipLabel(controlState.mute.label);
	const deafenControlLabel = getDeafenTooltipLabel(controlState.deafen.label);
	const cameraControlLabel = getCameraTooltipLabel(controlState.camera.label);
	const screenShareControlLabel = getScreenShareTooltipLabel(controlState.screenShare.label);
	const isMuteToggleLocked = controlState.mute.disabled;
	const isMutePressed = controlState.mute.pressed;
	const isDeafenLocked = controlState.deafen.disabled;
	const isDeafenPressed = controlState.deafen.pressed;
	const cameraDisabled = controlState.camera.disabled;
	const resolvedScreenShareDisabled = controlState.screenShare.disabled;
	const inputSettingsLabel = isMobile ? i18n._(AUDIO_SETTINGS_DESCRIPTOR) : i18n._(VOICE_INPUT_SETTINGS_DESCRIPTOR);
	const outputSettingsLabel = isMobile ? i18n._(AUDIO_SETTINGS_DESCRIPTOR) : i18n._(VOICE_OUTPUT_SETTINGS_DESCRIPTOR);
	const cameraSettingsLabel = i18n._(VOICE_CAMERA_SETTINGS_DESCRIPTOR);
	const screenShareSettingsLabel = i18n._(VOICE_SCREEN_SHARE_SETTINGS_DESCRIPTOR);
	return (
		<div className={styles.container} data-flx="voice.voice-control-bar.voice-control-bar-inner.container">
			<div
				className={styles.buttonContainer}
				data-flx="voice.voice-control-bar.voice-control-bar-inner.button-container"
			>
				<Tooltip
					text={() => (
						<TooltipWithKeybind
							label={muteControlLabel}
							action={isMuteToggleLocked ? undefined : 'voice_toggle_mute'}
							data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip-with-keybind"
						/>
					)}
					data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip"
				>
					<FocusRing offset={-2} data-flx="voice.voice-control-bar.voice-control-bar-inner.focus-ring">
						<div data-flx="voice.voice-control-bar.voice-control-bar-inner.div">
							<button
								type="button"
								className={clsx(
									styles.button,
									isMutePressed ? styles.buttonMuted : styles.buttonUnmuted,
									isMuteToggleLocked && 'disabled',
								)}
								onClick={isMuteToggleLocked ? undefined : handleToggleMute}
								onContextMenu={handleMuteContextMenu}
								disabled={isMuteToggleLocked}
								aria-label={muteControlLabel}
								aria-pressed={isMutePressed}
								data-flx="voice.voice-control-bar.voice-control-bar-inner.button.undefined"
							>
								{isMutePressed ? (
									<MicrophoneSlashIcon
										weight="fill"
										className={styles.icon}
										data-flx="voice.voice-control-bar.voice-control-bar-inner.icon"
									/>
								) : (
									<MicrophoneIcon
										weight="fill"
										className={styles.icon}
										data-flx="voice.voice-control-bar.voice-control-bar-inner.icon--2"
									/>
								)}
							</button>
						</div>
					</FocusRing>
				</Tooltip>
				<Tooltip text={inputSettingsLabel} data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip--2">
					<FocusRing offset={-2} data-flx="voice.voice-control-bar.voice-control-bar-inner.focus-ring--2">
						<button
							type="button"
							className={styles.settingsButton}
							onClick={handleInputSettingsClick}
							aria-label={inputSettingsLabel}
							aria-haspopup={isMobile ? 'dialog' : 'menu'}
							data-flx="voice.voice-control-bar.voice-control-bar-inner.settings-button.input-settings-click"
						>
							<CaretDownIcon
								weight="bold"
								className={styles.iconSmall}
								data-flx="voice.voice-control-bar.voice-control-bar-inner.icon-small"
							/>
						</button>
					</FocusRing>
				</Tooltip>
			</div>
			<div
				className={styles.buttonContainer}
				data-flx="voice.voice-control-bar.voice-control-bar-inner.button-container.output-settings"
			>
				<Tooltip
					text={() => (
						<TooltipWithKeybind
							label={deafenControlLabel}
							action={isDeafenLocked ? undefined : 'voice_toggle_deafen'}
							data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip-with-keybind--2"
						/>
					)}
					data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip--3"
				>
					<FocusRing offset={-2} data-flx="voice.voice-control-bar.voice-control-bar-inner.focus-ring--3">
						<div data-flx="voice.voice-control-bar.voice-control-bar-inner.div--2">
							<button
								type="button"
								className={clsx(
									styles.button,
									isDeafenPressed ? styles.buttonDeafened : styles.buttonUnmuted,
									isDeafenLocked && 'disabled',
								)}
								onClick={isDeafenLocked ? undefined : handleToggleDeafen}
								onContextMenu={handleDeafenContextMenu}
								disabled={isDeafenLocked}
								aria-label={deafenControlLabel}
								aria-pressed={isDeafenPressed}
								data-flx="voice.voice-control-bar.voice-control-bar-inner.button.undefined--2"
							>
								{isDeafenPressed ? (
									<SpeakerSlashIcon
										weight="fill"
										className={styles.icon}
										data-flx="voice.voice-control-bar.voice-control-bar-inner.icon--3"
									/>
								) : (
									<SpeakerHighIcon
										weight="fill"
										className={styles.icon}
										data-flx="voice.voice-control-bar.voice-control-bar-inner.icon--4"
									/>
								)}
							</button>
						</div>
					</FocusRing>
				</Tooltip>
				<Tooltip
					text={outputSettingsLabel}
					data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip.output-settings"
				>
					<FocusRing offset={-2} data-flx="voice.voice-control-bar.voice-control-bar-inner.focus-ring.output-settings">
						<button
							type="button"
							className={styles.settingsButton}
							onClick={handleOutputSettingsClick}
							aria-label={outputSettingsLabel}
							aria-haspopup={isMobile ? 'dialog' : 'menu'}
							data-flx="voice.voice-control-bar.voice-control-bar-inner.settings-button.output-settings-click"
						>
							<CaretDownIcon
								weight="bold"
								className={styles.iconSmall}
								data-flx="voice.voice-control-bar.voice-control-bar-inner.icon-small.output-settings"
							/>
						</button>
					</FocusRing>
				</Tooltip>
			</div>
			<div
				className={styles.buttonContainer}
				data-flx="voice.voice-control-bar.voice-control-bar-inner.button-container--2"
			>
				<Tooltip text={cameraControlLabel} data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip--4">
					<FocusRing offset={-2} data-flx="voice.voice-control-bar.voice-control-bar-inner.focus-ring--4">
						<button
							type="button"
							className={clsx(styles.button, isCameraEnabled ? styles.buttonCameraOn : styles.buttonCameraOff)}
							onClick={cameraDisabled ? undefined : handleToggleVideo}
							onContextMenu={cameraDisabled ? undefined : handleCameraSettingsContextMenu}
							disabled={cameraDisabled}
							aria-label={cameraControlLabel}
							aria-pressed={isCameraEnabled}
							data-flx="voice.voice-control-bar.voice-control-bar-inner.button.undefined--3"
						>
							{isCameraEnabled ? (
								<CameraIcon
									weight="fill"
									className={styles.icon}
									data-flx="voice.voice-control-bar.voice-control-bar-inner.icon--5"
								/>
							) : (
								<CameraSlashIcon
									weight="fill"
									className={styles.icon}
									data-flx="voice.voice-control-bar.voice-control-bar-inner.icon--6"
								/>
							)}
						</button>
					</FocusRing>
				</Tooltip>
				<Tooltip text={cameraSettingsLabel} data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip--5">
					<FocusRing offset={-2} data-flx="voice.voice-control-bar.voice-control-bar-inner.focus-ring--5">
						<button
							type="button"
							className={styles.settingsButton}
							onClick={handleCameraSettingsClick}
							aria-label={cameraSettingsLabel}
							aria-haspopup={isMobile ? 'dialog' : 'menu'}
							data-flx="voice.voice-control-bar.voice-control-bar-inner.settings-button.camera-settings-click"
						>
							<CaretDownIcon
								weight="bold"
								className={styles.iconSmall}
								data-flx="voice.voice-control-bar.voice-control-bar-inner.icon-small--2"
							/>
						</button>
					</FocusRing>
				</Tooltip>
			</div>
			<div
				className={styles.buttonContainer}
				data-flx="voice.voice-control-bar.voice-control-bar-inner.button-container.screen-share-settings"
			>
				<Tooltip text={screenShareControlLabel} data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip--6">
					<FocusRing offset={-2} data-flx="voice.voice-control-bar.voice-control-bar-inner.focus-ring--6">
						<button
							type="button"
							className={clsx(
								styles.button,
								isScreenShareEnabled ? styles.buttonScreenShareOn : styles.buttonScreenShareOff,
							)}
							onClick={resolvedScreenShareDisabled ? undefined : () => void handleScreenShare()}
							onContextMenu={resolvedScreenShareDisabled ? undefined : openScreenShareMenu}
							onFocus={resolvedScreenShareDisabled ? undefined : handleScreenSharePreload}
							onPointerEnter={resolvedScreenShareDisabled ? undefined : handleScreenSharePreload}
							disabled={resolvedScreenShareDisabled}
							aria-label={screenShareControlLabel}
							aria-pressed={isScreenShareEnabled}
							data-flx="voice.voice-control-bar.voice-control-bar-inner.button.undefined--4"
						>
							<MonitorPlayIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-control-bar.voice-control-bar-inner.icon--7"
							/>
						</button>
					</FocusRing>
				</Tooltip>
				<Tooltip
					text={screenShareSettingsLabel}
					data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip.screen-share-settings"
				>
					<FocusRing
						offset={-2}
						data-flx="voice.voice-control-bar.voice-control-bar-inner.focus-ring.screen-share-settings"
					>
						<button
							type="button"
							className={styles.settingsButton}
							onClick={(event) => openScreenShareMenu(event, {anchorToButton: true})}
							onFocus={resolvedScreenShareDisabled ? undefined : handleScreenSharePreload}
							onPointerEnter={resolvedScreenShareDisabled ? undefined : handleScreenSharePreload}
							disabled={resolvedScreenShareDisabled}
							aria-label={screenShareSettingsLabel}
							aria-haspopup="menu"
							data-flx="voice.voice-control-bar.voice-control-bar-inner.settings-button.screen-share-settings-click"
						>
							<CaretDownIcon
								weight="bold"
								className={styles.iconSmall}
								data-flx="voice.voice-control-bar.voice-control-bar-inner.icon-small.screen-share-settings"
							/>
						</button>
					</FocusRing>
				</Tooltip>
			</div>
			<Tooltip
				text={i18n._(MORE_OPTIONS_DESCRIPTOR)}
				data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip--7"
			>
				<FocusRing offset={-2} data-flx="voice.voice-control-bar.voice-control-bar-inner.focus-ring--7">
					<button
						type="button"
						className={clsx(styles.button, styles.buttonMoreOptions)}
						onClick={handleMoreOptionsClick}
						aria-label={i18n._(MORE_OPTIONS_DESCRIPTOR)}
						aria-haspopup={isMobile ? 'dialog' : 'menu'}
						data-flx="voice.voice-control-bar.voice-control-bar-inner.button.more-options-click"
					>
						<DotsThreeIcon
							weight="bold"
							className={styles.icon}
							data-flx="voice.voice-control-bar.voice-control-bar-inner.icon--8"
						/>
					</button>
				</FocusRing>
			</Tooltip>
			<span
				className={styles.buttonSeparator}
				aria-hidden="true"
				data-flx="voice.voice-control-bar.voice-control-bar-inner.button-separator"
			/>
			<Tooltip text={disconnectLabel} data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip--8">
				<FocusRing offset={-2} data-flx="voice.voice-control-bar.voice-control-bar-inner.focus-ring--8">
					<button
						type="button"
						className={clsx(styles.button, styles.buttonDisconnect)}
						onClick={handleDisconnect}
						aria-label={disconnectLabel}
						data-flx="voice.voice-control-bar.voice-control-bar-inner.button.disconnect"
					>
						<PhoneXIcon
							weight="fill"
							className={styles.icon}
							data-flx="voice.voice-control-bar.voice-control-bar-inner.icon--9"
						/>
					</button>
				</FocusRing>
			</Tooltip>
			{canStopWatchingFocusedStream && (
				<Tooltip
					text={i18n._(STOP_WATCHING_STREAM_DESCRIPTOR)}
					data-flx="voice.voice-control-bar.voice-control-bar-inner.tooltip--9"
				>
					<FocusRing offset={-2} data-flx="voice.voice-control-bar.voice-control-bar-inner.focus-ring--9">
						<button
							type="button"
							className={styles.stopWatchingButton}
							onClick={handleStopWatching}
							aria-label={i18n._(STOP_WATCHING_STREAM_DESCRIPTOR)}
							data-flx="voice.voice-control-bar.voice-control-bar-inner.stop-watching-button"
						>
							<EyeSlashIcon
								weight="fill"
								className={styles.icon}
								data-flx="voice.voice-control-bar.voice-control-bar-inner.icon--10"
							/>
						</button>
					</FocusRing>
				</Tooltip>
			)}
			{isMobile && (
				<>
					<VoiceAudioSettingsBottomSheet
						isOpen={audioSettingsOpen}
						onClose={() => setAudioSettingsOpen(false)}
						data-flx="voice.voice-control-bar.voice-control-bar-inner.voice-audio-settings-bottom-sheet"
					/>
					<VoiceCameraSettingsBottomSheet
						isOpen={cameraSettingsOpen}
						onClose={() => setCameraSettingsOpen(false)}
						data-flx="voice.voice-control-bar.voice-control-bar-inner.voice-camera-settings-bottom-sheet"
					/>
					<VoiceMoreOptionsBottomSheet
						isOpen={moreOptionsOpen}
						onClose={() => setMoreOptionsOpen(false)}
						data-flx="voice.voice-control-bar.voice-control-bar-inner.voice-more-options-bottom-sheet"
					/>
				</>
			)}
		</div>
	);
});
export const VoiceControlBar = observer(() => {
	const room = MediaEngine.room;
	const hasNativeVoiceSession = !room && MediaEngine.connected;
	if (!room && !hasNativeVoiceSession) return null;
	return <VoiceControlBarInner data-flx="voice.voice-control-bar.voice-control-bar-inner" />;
});
