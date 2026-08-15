// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {CAMERA_DESCRIPTOR, SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import BackgroundImageGalleryModal from '@app/features/theme/components/modals/BackgroundImageGalleryModal';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR, Slider} from '@app/features/ui/components/Slider';
import {Spinner} from '@app/features/ui/components/Spinner';
import {canResetSliderValue, SliderResetIconButton} from '@app/features/ui/components/slider/SliderResetIconButton';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {formatRoundedPercentage} from '@app/features/ui/utils/PercentageFormatting';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import styles from '@app/features/voice/components/modals/CameraPreviewModal.module.css';
import {
	NATIVE_CAMERA_PREVIEW_RETRY_DELAY_MS,
	selectNativeCameraPreviewFallback,
} from '@app/features/voice/components/modals/CameraPreviewSessionPolicy';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR} from '@app/features/voice/engine/media_engine_facade/shared';
import NativeVideoTileManager from '@app/features/voice/engine/native_voice_engine/NativeVideoTileManager';
import {useStoreVersion} from '@app/features/voice/engine/Store';
import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import {useCameraUserCapBlocked} from '@app/features/voice/hooks/useCameraUserCapBlocked';
import VoiceSettings, {
	BLUR_BACKGROUND_ID,
	CAMERA_EFFECT_STRENGTH_DEFAULT,
	CAMERA_EFFECT_STRENGTH_MAX,
	CAMERA_EFFECT_STRENGTH_MIN,
	NONE_BACKGROUND_ID,
} from '@app/features/voice/state/VoiceSettings';
import {applyBackgroundProcessor} from '@app/features/voice/utils/VideoBackgroundProcessor';
import {areVoiceBackgroundsAvailable} from '@app/features/voice/utils/VoiceBackgroundAvailability';
import {resolveEffectiveDeviceId, type VoiceDeviceState} from '@app/features/voice/utils/VoiceDeviceManager';
import {
	formatFallbackCameraLabel,
	VOICE_TURN_ON_CAMERA_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {VOICE_CHANNEL_CAMERA_USER_LIMIT} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {useMaybeRoomContext} from '@livekit/components-react';
import {CameraIcon, ImageIcon} from '@phosphor-icons/react';
import type {LocalParticipant, LocalVideoTrack, Room} from 'livekit-client';
import {createLocalVideoTrack, RoomEvent} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const FAILED_TO_START_CAMERA_PREVIEW_PLEASE_CHECK_YOUR_DESCRIPTOR = msg({
	message: 'Failed to start camera preview. Check your camera permissions.',
	comment:
		'Error text in the camera preview modal when the camera stream cannot be opened. Prompts user to check OS / browser permissions.',
});
const FAILED_TO_ENABLE_CAMERA_DESCRIPTOR = msg({
	message: 'Failed to enable camera.',
	comment: 'Toast / inline error in the camera preview modal when enabling the camera fails.',
});
const CAMERA_PREVIEW_DESCRIPTOR = msg({
	message: 'Camera preview',
	comment: 'Title of the camera preview modal.',
});
const DEFAULT_CAMERA_DESCRIPTOR = msg({
	message: 'Default',
	comment: 'Default camera device option.',
});
const MIRROR_CAMERA_DESCRIPTOR = msg({
	message: 'Mirror camera',
	comment: 'Switch label in the camera preview modal for flipping the local camera preview horizontally.',
});
const YOU_DON_T_HAVE_PERMISSION_TO_TURN_ON_DESCRIPTOR = msg({
	message: "You can't turn on your camera in this channel",
	comment:
		'Tooltip / error shown in the camera preview modal when the user lacks Video permission in the current channel. Tone stays plain.',
});
const EFFECTS_PREVIEW_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Effects preview unavailable — showing unprocessed camera',
	comment:
		'Inline notice in the camera preview modal when the native effects preview cannot start and the raw camera feed is shown instead.',
});
const BLUR_STRENGTH_DESCRIPTOR = msg({
	message: 'Blur strength',
	comment: 'Slider label in the camera preview modal controlling how strong the background blur effect is.',
});
const logger = new Logger('CameraPreviewModal');

interface CameraPreviewModalProps {
	onEnabled?: () => void;
	onEnableCamera?: () => void | Promise<void>;
	showEnableCameraButton?: boolean;
	localParticipant?: LocalParticipant;
	isCameraEnabled?: boolean;
}

interface VideoResolutionPreset {
	width: number;
	height: number;
	frameRate: number;
}

const TARGET_ASPECT_RATIO = 16 / 9;
const ASPECT_RATIO_TOLERANCE = 0.1;
const RESOLUTION_WAIT_TIMEOUT = 2000;
const RESOLUTION_CHECK_INTERVAL = 100;
const VIDEO_ELEMENT_WAIT_TIMEOUT = 5000;
const VIDEO_ELEMENT_CHECK_INTERVAL = 10;
const PLAYBACK_WAIT_TIMEOUT = 5000;
const PLAYBACK_CHECK_INTERVAL = 50;
const VIDEO_READY_STATE_HAS_CURRENT_DATA = 2;
const RESOLUTION_FIX_TRIGGER_DELAY = 800;
const RESOLUTION_FIX_SETTLE_DELAY = 1200;
const RESOLUTION_FIX_SWITCH_BACK_DELAY = 500;
const CAMERA_RESOLUTION_PRESETS: Record<'low' | 'medium' | 'high', VideoResolutionPreset> = {
	low: {width: 640, height: 360, frameRate: 24},
	medium: {width: 1280, height: 720, frameRate: 30},
	high: {width: 1920, height: 1080, frameRate: 30},
};

interface CameraPreviewConfig {
	videoDeviceId: string;
	backgroundImageId: string;
	mirrorCamera: boolean;
	cameraResolution: 'low' | 'medium' | 'high';
	videoFrameRate: number;
}

interface CameraPreviewParticipantState {
	localParticipant: LocalParticipant | undefined;
	isCameraEnabled: boolean;
}

const NO_CAMERA_PREVIEW_PARTICIPANT_STATE: CameraPreviewParticipantState = {
	localParticipant: undefined,
	isCameraEnabled: false,
};

const CAMERA_PREVIEW_LOCAL_PARTICIPANT_EVENTS: ReadonlyArray<RoomEvent> = [
	RoomEvent.ConnectionStateChanged,
	RoomEvent.LocalTrackPublished,
	RoomEvent.LocalTrackUnpublished,
	RoomEvent.TrackMuted,
	RoomEvent.TrackUnmuted,
];

function getCameraPreviewParticipantState(room: Room | undefined): CameraPreviewParticipantState {
	if (!room) return NO_CAMERA_PREVIEW_PARTICIPANT_STATE;
	const localParticipant = room.localParticipant;
	return {
		localParticipant,
		isCameraEnabled: localParticipant.isCameraEnabled,
	};
}

function useCameraPreviewParticipantState(): CameraPreviewParticipantState {
	const room = useMaybeRoomContext();
	const [state, setState] = useState<CameraPreviewParticipantState>(() => getCameraPreviewParticipantState(room));
	useEffect(() => {
		if (!room) {
			setState(NO_CAMERA_PREVIEW_PARTICIPANT_STATE);
			return;
		}
		const update = () => setState(getCameraPreviewParticipantState(room));
		update();
		for (const event of CAMERA_PREVIEW_LOCAL_PARTICIPANT_EVENTS) {
			room.on(event, update);
		}
		return () => {
			for (const event of CAMERA_PREVIEW_LOCAL_PARTICIPANT_EVENTS) {
				room.off(event, update);
			}
		};
	}, [room]);
	return state;
}

interface CameraPreviewProcessor {
	destroy: () => Promise<void>;
}

function isSameCameraPreviewConfig(previous: CameraPreviewConfig | null, next: CameraPreviewConfig): boolean {
	return previous != null && JSON.stringify(previous) === JSON.stringify(next);
}

function isNear16x9AspectRatio(resolution: {width: number; height: number}): boolean {
	const aspectRatio = resolution.width / resolution.height;
	return Math.abs(aspectRatio - TARGET_ASPECT_RATIO) < ASPECT_RATIO_TOLERANCE;
}

async function waitForVideoElement(
	videoRef: React.RefObject<HTMLVideoElement | null>,
	isCurrentInitialization: () => boolean,
): Promise<HTMLVideoElement | null> {
	let videoElement = videoRef.current;
	let attempts = 0;
	const maxAttempts = VIDEO_ELEMENT_WAIT_TIMEOUT / VIDEO_ELEMENT_CHECK_INTERVAL;
	while (!videoElement && isCurrentInitialization() && attempts < maxAttempts) {
		await new Promise((resolve) => setTimeout(resolve, VIDEO_ELEMENT_CHECK_INTERVAL));
		videoElement = videoRef.current;
		attempts++;
	}
	return videoElement;
}

function waitForPlaybackSettle(videoElement: HTMLVideoElement, isCurrentInitialization: () => boolean): Promise<void> {
	return new Promise<void>((resolve) => {
		let playbackAttempts = 0;
		const maxAttempts = PLAYBACK_WAIT_TIMEOUT / PLAYBACK_CHECK_INTERVAL;
		const checkPlayback = () => {
			if (!isCurrentInitialization()) {
				resolve();
				return;
			}
			const hasData = videoElement.srcObject && videoElement.readyState >= VIDEO_READY_STATE_HAS_CURRENT_DATA;
			if (hasData) {
				resolve();
			} else if (++playbackAttempts < maxAttempts) {
				setTimeout(checkPlayback, PLAYBACK_CHECK_INTERVAL);
			} else {
				resolve();
			}
		};
		checkPlayback();
	});
}

function waitForNegotiatedResolution(
	track: LocalVideoTrack,
	isCurrentInitialization: () => boolean,
): Promise<{width: number; height: number} | null> {
	return new Promise((resolve) => {
		let resolutionAttempts = 0;
		const maxAttempts = RESOLUTION_WAIT_TIMEOUT / RESOLUTION_CHECK_INTERVAL;
		const checkResolution = () => {
			if (!isCurrentInitialization()) {
				resolve(null);
				return;
			}
			const settings = track.mediaStreamTrack.getSettings();
			if (settings.width && settings.height) {
				resolve({width: settings.width, height: settings.height});
			} else if (++resolutionAttempts < maxAttempts) {
				setTimeout(checkResolution, RESOLUTION_CHECK_INTERVAL);
			} else {
				resolve(null);
			}
		};
		checkResolution();
	});
}

interface CameraPreviewTrackSetupArgs {
	videoElement: HTMLVideoElement;
	effectiveVideoDeviceId: string | null;
	cameraResolution: 'low' | 'medium' | 'high';
	videoFrameRate: number;
	isCurrentInitialization: () => boolean;
	trackRef: React.MutableRefObject<LocalVideoTrack | null>;
	processorRef: React.MutableRefObject<CameraPreviewProcessor | null>;
	onResolutionNegotiated: (resolution: {width: number; height: number} | null) => void;
}

async function setupPreviewTrackAndProcessor(args: CameraPreviewTrackSetupArgs): Promise<'cancelled' | 'ready'> {
	if (args.trackRef.current) {
		args.trackRef.current.stop();
		args.trackRef.current = null;
	}
	if (args.processorRef.current) {
		await args.processorRef.current.destroy();
		args.processorRef.current = null;
	}
	const resolutionPreset = CAMERA_RESOLUTION_PRESETS[args.cameraResolution];
	const track = await createLocalVideoTrack({
		deviceId:
			args.effectiveVideoDeviceId && args.effectiveVideoDeviceId !== 'default'
				? args.effectiveVideoDeviceId
				: undefined,
		resolution: {
			width: resolutionPreset.width,
			height: resolutionPreset.height,
			frameRate: args.videoFrameRate,
			aspectRatio: TARGET_ASPECT_RATIO,
		},
	});
	if (!args.isCurrentInitialization()) {
		track.stop();
		return 'cancelled';
	}
	args.trackRef.current = track;
	track.attach(args.videoElement);
	await waitForPlaybackSettle(args.videoElement, args.isCurrentInitialization);
	if (!args.isCurrentInitialization()) {
		track.stop();
		return 'cancelled';
	}
	const negotiatedResolution = await waitForNegotiatedResolution(track, args.isCurrentInitialization);
	if (!args.isCurrentInitialization()) {
		track.stop();
		return 'cancelled';
	}
	args.onResolutionNegotiated(negotiatedResolution);
	let processor: CameraPreviewProcessor | null = null;
	try {
		processor = await applyBackgroundProcessor(track);
	} catch (_webglError) {
		logger.warn('WebGL not supported for background processing, falling back to basic camera');
	}
	if (!args.isCurrentInitialization()) {
		await processor?.destroy().catch((destroyError) => {
			logger.warn('Failed to destroy camera preview processor after stale initialization', {
				error: destroyError,
			});
		});
		track.stop();
		return 'cancelled';
	}
	args.processorRef.current = processor;
	return 'ready';
}

function usePublishedNativeCameraPreviewStream(
	enabled: boolean,
	localParticipant: LocalParticipant | undefined,
): MediaStream | null {
	useStoreVersion(NativeVideoTileManager);
	if (!enabled) return null;
	return MediaEngine.getNativeCameraLocalPreviewStream(localParticipant ?? null);
}

interface NativeCameraPreviewSession {
	stream: MediaStream | null;
	failed: boolean;
}

function useNativeCameraPreviewSession(enabled: boolean): NativeCameraPreviewSession {
	useStoreVersion(NativeVideoTileManager);
	const [trackSid, setTrackSid] = useState<string | null>(null);
	const [failed, setFailed] = useState(false);
	const [retryNonce, setRetryNonce] = useState(0);
	const retryAttemptRef = useRef(0);
	const voiceSettings = VoiceSettings;
	const videoDeviceId = voiceSettings.videoDeviceId;
	const backgroundImageId = voiceSettings.backgroundImageId;
	const cameraResolution = voiceSettings.cameraResolution;
	const videoFrameRate = voiceSettings.videoFrameRate;
	const backgroundBlurStrength = voiceSettings.backgroundBlurStrength;
	useEffect(() => {
		retryAttemptRef.current = 0;
	}, [enabled, videoDeviceId, backgroundImageId, cameraResolution, videoFrameRate, backgroundBlurStrength]);
	useEffect(() => {
		if (!enabled) {
			return;
		}
		let cancelled = false;
		let retryTimeoutId: number | null = null;
		const scheduleRetry = () => {
			const decision = selectNativeCameraPreviewFallback({
				sessionFailed: true,
				backgroundEffectConfigured: backgroundImageId !== NONE_BACKGROUND_ID,
				retryAttempt: retryAttemptRef.current,
			});
			if (!decision.shouldScheduleRetry) return;
			retryAttemptRef.current += 1;
			retryTimeoutId = window.setTimeout(() => {
				retryTimeoutId = null;
				setRetryNonce((nonce) => nonce + 1);
			}, NATIVE_CAMERA_PREVIEW_RETRY_DELAY_MS);
		};
		setFailed(false);
		MediaEngine.startNativeCameraPreviewSession()
			.then((startedTrackSid) => {
				if (cancelled) return;
				setTrackSid(startedTrackSid);
				if (startedTrackSid) {
					retryAttemptRef.current = 0;
					return;
				}
				setFailed(true);
				scheduleRetry();
			})
			.catch((error) => {
				logger.warn('Failed to start native camera preview session', {error});
				if (cancelled) return;
				setTrackSid(null);
				setFailed(true);
				scheduleRetry();
			});
		return () => {
			cancelled = true;
			if (retryTimeoutId !== null) {
				window.clearTimeout(retryTimeoutId);
			}
		};
	}, [enabled, retryNonce, videoDeviceId, backgroundImageId, cameraResolution, videoFrameRate, backgroundBlurStrength]);
	useEffect(() => {
		if (!enabled) {
			return;
		}
		return () => {
			setTrackSid(null);
			void MediaEngine.stopNativeCameraPreviewSession();
		};
	}, [enabled]);
	const stream = enabled && trackSid ? (NativeVideoTileManager.tracks[trackSid]?.stream ?? null) : null;
	return {stream, failed};
}

interface CameraEffectStrengthSliderProps {
	label: string;
	value: number;
	onChange: (value: number) => void;
	resetLabel: string;
	dataFlx: string;
}

const CameraEffectStrengthSlider = ({label, value, onChange, resetLabel, dataFlx}: CameraEffectStrengthSliderProps) => {
	const [draftValue, setDraftValueState] = useState(value);
	const draftValueRef = useRef(value);
	const committedValueRef = useRef(value);
	const isInteractingRef = useRef(false);
	const setDraftValue = useCallback((nextValue: number) => {
		draftValueRef.current = nextValue;
		setDraftValueState(nextValue);
	}, []);
	const commitValue = useCallback(
		(nextValue: number) => {
			if (nextValue === committedValueRef.current) {
				setDraftValue(nextValue);
				return;
			}
			committedValueRef.current = nextValue;
			setDraftValue(nextValue);
			onChange(nextValue);
		},
		[onChange, setDraftValue],
	);
	const handlePointerInteractionChange = useCallback(
		(isInteracting: boolean) => {
			isInteractingRef.current = isInteracting;
			if (isInteracting) return;
			const nextValue = draftValueRef.current;
			if (nextValue !== committedValueRef.current) {
				commitValue(nextValue);
			}
		},
		[commitValue],
	);
	useEffect(() => {
		committedValueRef.current = value;
		if (isInteractingRef.current) return;
		setDraftValue(value);
	}, [setDraftValue, value]);
	return (
		<div className={styles.effectStrengthSlider} data-flx={dataFlx}>
			<div className={styles.effectStrengthLabelRow} data-flx={`${dataFlx}.label-row`}>
				<div className={styles.effectStrengthLabel} data-flx={`${dataFlx}.label`}>
					{label}
				</div>
				<SliderResetIconButton
					canReset={canResetSliderValue(draftValue, CAMERA_EFFECT_STRENGTH_DEFAULT)}
					onReset={() => commitValue(CAMERA_EFFECT_STRENGTH_DEFAULT)}
					ariaLabel={resetLabel}
					dataFlx={`${dataFlx}.reset`}
					data-flx="voice.camera-preview-modal.camera-effect-strength-slider.slider-reset-icon-button"
				/>
			</div>
			<Slider
				value={draftValue}
				defaultValue={value}
				factoryDefaultValue={CAMERA_EFFECT_STRENGTH_DEFAULT}
				minValue={CAMERA_EFFECT_STRENGTH_MIN}
				maxValue={CAMERA_EFFECT_STRENGTH_MAX}
				step={1}
				onValueRender={formatRoundedPercentage}
				asValueChanges={setDraftValue}
				onValueChange={commitValue}
				onPointerInteractionChange={handlePointerInteractionChange}
				data-flx={`${dataFlx}.slider`}
			/>
		</div>
	);
};

const CameraPreviewModalContent = observer((props: CameraPreviewModalProps) => {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const {localParticipant, onEnabled, onEnableCamera, isCameraEnabled, showEnableCameraButton = true} = props;
	const nativeCameraPublished = MediaEngine.isNativeCameraPublished();
	const nativePreviewStream = usePublishedNativeCameraPreviewStream(nativeCameraPublished, localParticipant);
	const hasPublishedNativePreview = nativePreviewStream != null;
	const cameraAlreadyOn = isCameraEnabled === true || nativeCameraPublished;
	const nativePreviewSessionEnabled =
		!nativeCameraPublished && !hasPublishedNativePreview && MediaEngine.isNativeCameraPreviewSessionAvailable();
	const nativePreviewSession = useNativeCameraPreviewSession(nativePreviewSessionEnabled);
	const usesNativePreviewSession = nativePreviewSessionEnabled && !nativePreviewSession.failed;
	const activeNativeStream = nativePreviewStream ?? (usesNativePreviewSession ? nativePreviewSession.stream : null);
	const voiceBackgroundsAvailable = areVoiceBackgroundsAvailable();
	const channelId = MediaEngine.channelId;
	const guildId = MediaEngine.guildId;
	const canStream = !localParticipant || !guildId || !channelId || Permission.can(Permissions.STREAM, {channelId});
	const cameraCapBlocked = useCameraUserCapBlocked(cameraAlreadyOn);
	const selectedBackgroundImageId = voiceBackgroundsAvailable ? VoiceSettings.backgroundImageId : NONE_BACKGROUND_ID;
	const backgroundEffectConfigured = selectedBackgroundImageId !== NONE_BACKGROUND_ID;
	const showEffectsUnavailableNotice = selectNativeCameraPreviewFallback({
		sessionFailed: nativePreviewSessionEnabled && nativePreviewSession.failed,
		backgroundEffectConfigured,
		retryAttempt: 0,
	}).showEffectsUnavailableNotice;
	const [videoDevices, setVideoDevices] = useState<Array<MediaDeviceInfo>>([]);
	const [status, setStatus] = useState<
		'idle' | 'initializing' | 'ready' | 'error' | 'fixing' | 'fix-settling' | 'fix-switching-back'
	>('initializing');
	const [error, setError] = useState<string | null>(null);
	const [backgroundOverrideId, setBackgroundOverrideId] = useState<string | null>(null);
	const videoRef = useRef<HTMLVideoElement>(null);
	const trackRef = useRef<LocalVideoTrack | null>(null);
	const processorRef = useRef<CameraPreviewProcessor | null>(null);
	const isMountedRef = useRef(true);
	const isIOSRef = useRef(/iPad|iPhone|iPod/.test(navigator.userAgent) && !('MSStream' in window));
	const prevConfigRef = useRef<CameraPreviewConfig | null>(null);
	const needsResolutionFixRef = useRef(false);
	const isApplyingFixRef = useRef(false);
	const initializationTimeoutRef = useRef<NodeJS.Timeout | null>(null);
	const settleTimeoutRef = useRef<NodeJS.Timeout | null>(null);
	const switchBackTimeoutRef = useRef<NodeJS.Timeout | null>(null);
	const initializationGenerationRef = useRef(0);
	const handleDeviceUpdate = useCallback((state: VoiceDeviceState) => {
		if (!isMountedRef.current) return;
		const videoInputs = state.videoDevices.filter((device) => device.deviceId.trim().length > 0);
		setVideoDevices(videoInputs);
		const voiceSettings = VoiceSettings;
		const currentDeviceId = voiceSettings.videoDeviceId;
		const currentDeviceExists =
			currentDeviceId === 'default' || videoInputs.some((device) => device.deviceId === currentDeviceId);
		if (videoInputs.length > 0 && !currentDeviceExists) {
			VoiceSettingsCommands.update({videoDeviceId: 'default'});
		}
	}, []);
	const applyResolutionFix = useCallback(() => {
		if (!isMountedRef.current || isApplyingFixRef.current) {
			return;
		}
		if (!voiceBackgroundsAvailable) {
			setStatus('ready');
			return;
		}
		isApplyingFixRef.current = true;
		needsResolutionFixRef.current = false;
		const currentBg = VoiceSettings.backgroundImageId;
		const overrideBg = currentBg === NONE_BACKGROUND_ID ? BLUR_BACKGROUND_ID : NONE_BACKGROUND_ID;
		setStatus('fixing');
		setBackgroundOverrideId(overrideBg);
		settleTimeoutRef.current = setTimeout(() => {
			setStatus('fix-switching-back');
			setBackgroundOverrideId(null);
			switchBackTimeoutRef.current = setTimeout(() => {
				if (isMountedRef.current) {
					isApplyingFixRef.current = false;
					setStatus('ready');
				}
			}, RESOLUTION_FIX_SWITCH_BACK_DELAY);
		}, RESOLUTION_FIX_SETTLE_DELAY);
	}, [voiceBackgroundsAvailable]);
	const initializeCamera = useCallback(async () => {
		const generation = ++initializationGenerationRef.current;
		const isCurrentInitialization = () => isMountedRef.current && initializationGenerationRef.current === generation;
		const voiceSettings = VoiceSettings;
		const isMobile = MobileLayout.isMobileLayout() || isIOSRef.current;
		if (isMobile) {
			if (isCurrentInitialization()) {
				setStatus('ready');
			}
			return;
		}
		if (hasPublishedNativePreview || usesNativePreviewSession || nativeCameraPublished) {
			if (isCurrentInitialization()) {
				setError(null);
			}
			return;
		}
		if (!isCurrentInitialization()) {
			return;
		}
		const videoElement = await waitForVideoElement(videoRef, isCurrentInitialization);
		if (!isCurrentInitialization()) {
			return;
		}
		if (!videoElement) {
			setStatus('error');
			setError(i18n._(FAILED_TO_START_CAMERA_PREVIEW_PLEASE_CHECK_YOUR_DESCRIPTOR));
			return;
		}
		try {
			const effectiveVideoDeviceId = resolveEffectiveDeviceId(voiceSettings.videoDeviceId, videoDevices);
			const backgroundImageId =
				backgroundOverrideId ?? (voiceBackgroundsAvailable ? voiceSettings.backgroundImageId : NONE_BACKGROUND_ID);
			const currentConfig: CameraPreviewConfig = {
				videoDeviceId: effectiveVideoDeviceId ?? 'default',
				backgroundImageId,
				mirrorCamera: voiceSettings.mirrorCamera,
				cameraResolution: voiceSettings.cameraResolution,
				videoFrameRate: voiceSettings.videoFrameRate,
			};
			if (trackRef.current && isSameCameraPreviewConfig(prevConfigRef.current, currentConfig)) {
				return;
			}
			prevConfigRef.current = currentConfig;
			if (isCurrentInitialization()) {
				setStatus(isApplyingFixRef.current ? 'fixing' : 'initializing');
				setError(null);
			}
			videoElement.muted = true;
			videoElement.autoplay = true;
			videoElement.playsInline = true;
			const setupResult = await setupPreviewTrackAndProcessor({
				videoElement,
				effectiveVideoDeviceId,
				cameraResolution: voiceSettings.cameraResolution,
				videoFrameRate: voiceSettings.videoFrameRate,
				isCurrentInitialization,
				trackRef,
				processorRef,
				onResolutionNegotiated: (resolution) => {
					if (resolution && !isApplyingFixRef.current) {
						needsResolutionFixRef.current = !isNear16x9AspectRatio(resolution);
					}
				},
			});
			if (setupResult === 'cancelled') {
				return;
			}
			setStatus('ready');
			if (voiceBackgroundsAvailable && needsResolutionFixRef.current && !isApplyingFixRef.current) {
				initializationTimeoutRef.current = setTimeout(() => applyResolutionFix(), RESOLUTION_FIX_TRIGGER_DELAY);
			}
		} catch (err) {
			if (isCurrentInitialization()) {
				const message =
					err instanceof Error ? err.message : i18n._(FAILED_TO_START_CAMERA_PREVIEW_PLEASE_CHECK_YOUR_DESCRIPTOR);
				setStatus('error');
				setError(message);
				showGenericErrorModal({
					title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
					message: () => i18n._(FAILED_TO_START_CAMERA_PREVIEW_PLEASE_CHECK_YOUR_DESCRIPTOR),
					dataFlx: 'voice.camera-preview-modal.start-preview-error-modal',
				});
			}
		}
	}, [
		applyResolutionFix,
		backgroundOverrideId,
		hasPublishedNativePreview,
		usesNativePreviewSession,
		nativeCameraPublished,
		i18n,
		videoDevices,
		voiceBackgroundsAvailable,
	]);
	const handleDeviceChange = useCallback((deviceId: string) => {
		VoiceSettingsCommands.update({videoDeviceId: deviceId});
	}, []);
	const handleOpenBackgroundGallery = useCallback(() => {
		if (!voiceBackgroundsAvailable) {
			return;
		}
		ModalCommands.push(
			modal(() => (
				<BackgroundImageGalleryModal data-flx="voice.camera-preview-modal.handle-open-background-gallery.background-image-gallery-modal" />
			)),
		);
	}, [voiceBackgroundsAvailable]);
	const handleEnableCamera = useCallback(async () => {
		try {
			await MediaEngine.stopNativeCameraPreviewSession();
			if (!localParticipant) {
				await onEnableCamera?.();
				onEnabled?.();
				ModalCommands.pop();
				return;
			}
			const voiceSettings = VoiceSettings;
			const effectiveVideoDeviceId = resolveEffectiveDeviceId(voiceSettings.videoDeviceId, videoDevices);
			const enableCamera =
				onEnableCamera ??
				(() =>
					MediaEngine.setCameraEnabled(true, {
						deviceId:
							effectiveVideoDeviceId && effectiveVideoDeviceId !== 'default' ? effectiveVideoDeviceId : undefined,
					}));
			await enableCamera();
			onEnabled?.();
			ModalCommands.pop();
		} catch (_err) {
			showGenericErrorModal({
				title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
				message: () => i18n._(FAILED_TO_ENABLE_CAMERA_DESCRIPTOR),
				dataFlx: 'voice.camera-preview-modal.enable-camera-error-modal',
			});
		}
	}, [i18n, localParticipant, onEnabled, onEnableCamera, videoDevices]);
	useEffect(() => {
		isMountedRef.current = true;
		const unsubscribeDevices = VoiceDevicePermissionState.subscribe(handleDeviceUpdate);
		void VoiceDevicePermissionState.ensureDevices({requestPermissions: true}).catch((error) => {
			logger.warn('Failed to enumerate camera preview devices', {error});
		});
		return () => {
			isMountedRef.current = false;
			initializationGenerationRef.current++;
			if (initializationTimeoutRef.current) clearTimeout(initializationTimeoutRef.current);
			if (settleTimeoutRef.current) clearTimeout(settleTimeoutRef.current);
			if (switchBackTimeoutRef.current) clearTimeout(switchBackTimeoutRef.current);
			isApplyingFixRef.current = false;
			if (trackRef.current) {
				trackRef.current.stop();
				trackRef.current = null;
			}
			if (processorRef.current) {
				processorRef.current.destroy().catch((error) => {
					logger.warn('Failed to destroy camera preview processor during modal cleanup', {error});
				});
				processorRef.current = null;
			}
			if (videoRef.current) {
				try {
					if (videoRef.current.srcObject) {
						videoRef.current.srcObject = null;
					}
				} catch {}
			}
			unsubscribeDevices?.();
		};
	}, [handleDeviceUpdate]);
	useEffect(() => {
		if (!activeNativeStream) return;
		initializationGenerationRef.current++;
		if (trackRef.current) {
			trackRef.current.stop();
			trackRef.current = null;
		}
		if (processorRef.current) {
			processorRef.current.destroy().catch((error) => {
				logger.warn('Failed to destroy browser camera preview processor after native preview became available', {
					error,
				});
			});
			processorRef.current = null;
		}
		const videoElement = videoRef.current;
		if (!videoElement) return;
		videoElement.muted = true;
		videoElement.autoplay = true;
		videoElement.playsInline = true;
		videoElement.srcObject = activeNativeStream;
		setStatus('ready');
		setError(null);
		const playResult = videoElement.play();
		if (playResult && typeof playResult.catch === 'function') {
			playResult.catch((error) => {
				logger.debug('Native camera preview play() rejected', {error});
			});
		}
		return () => {
			if (videoRef.current?.srcObject === activeNativeStream) {
				videoRef.current.srcObject = null;
			}
		};
	}, [activeNativeStream]);
	useEffect(() => {
		if (hasPublishedNativePreview || usesNativePreviewSession || nativeCameraPublished) {
			return;
		}
		const voiceSettings = VoiceSettings;
		const backgroundImageId =
			backgroundOverrideId ?? (voiceBackgroundsAvailable ? voiceSettings.backgroundImageId : NONE_BACKGROUND_ID);
		const currentConfig: CameraPreviewConfig = {
			videoDeviceId: voiceSettings.videoDeviceId,
			backgroundImageId,
			mirrorCamera: voiceSettings.mirrorCamera,
			cameraResolution: voiceSettings.cameraResolution,
			videoFrameRate: voiceSettings.videoFrameRate,
		};
		if (!isSameCameraPreviewConfig(prevConfigRef.current, currentConfig)) {
			initializeCamera();
		}
	}, [
		initializeCamera,
		backgroundOverrideId,
		hasPublishedNativePreview,
		usesNativePreviewSession,
		nativeCameraPublished,
		VoiceSettings.videoDeviceId,
		VoiceSettings.backgroundImageId,
		VoiceSettings.mirrorCamera,
		VoiceSettings.cameraResolution,
		VoiceSettings.videoFrameRate,
		voiceBackgroundsAvailable,
	]);
	const voiceSettings = VoiceSettings;
	const effectiveVideoDeviceId = resolveEffectiveDeviceId(voiceSettings.videoDeviceId, videoDevices) ?? 'default';
	const previewVideoClassName =
		activeNativeStream && voiceSettings.mirrorCamera ? `${styles.video} ${styles.videoMirrored}` : styles.video;
	const videoDeviceOptions =
		videoDevices.length > 0
			? videoDevices.map((device) => ({
					value: device.deviceId,
					label:
						device.deviceId === 'default'
							? i18n._(DEFAULT_CAMERA_DESCRIPTOR)
							: device.label || formatFallbackCameraLabel(i18n),
				}))
			: [{value: 'default', label: i18n._(DEFAULT_CAMERA_DESCRIPTOR)}];
	return (
		<Modal.Root size="medium" data-flx="voice.camera-preview-modal.camera-preview-modal-content.modal-root">
			<Modal.Header
				title={i18n._(CAMERA_PREVIEW_DESCRIPTOR)}
				data-flx="voice.camera-preview-modal.camera-preview-modal-content.modal-header"
			/>
			<Modal.Content data-flx="voice.camera-preview-modal.camera-preview-modal-content.modal-content">
				<div className={styles.content} data-flx="voice.camera-preview-modal.camera-preview-modal-content.content">
					<div data-flx="voice.camera-preview-modal.camera-preview-modal-content.div">
						<Combobox
							label={i18n._(CAMERA_DESCRIPTOR)}
							value={effectiveVideoDeviceId}
							options={videoDeviceOptions}
							onChange={handleDeviceChange}
							data-flx="voice.camera-preview-modal.camera-preview-modal-content.select.device-change"
						/>
					</div>
					{voiceBackgroundsAvailable && (
						<div
							className={styles.backgroundSection}
							data-flx="voice.camera-preview-modal.camera-preview-modal-content.background-section"
						>
							<div
								className={styles.backgroundLabel}
								data-flx="voice.camera-preview-modal.camera-preview-modal-content.background-label"
							>
								<Trans>Background</Trans>
							</div>
							<Button
								variant="primary"
								onClick={handleOpenBackgroundGallery}
								leftIcon={
									<ImageIcon
										size={remFromPx(16)}
										data-flx="voice.camera-preview-modal.camera-preview-modal-content.image-icon"
									/>
								}
								data-flx="voice.camera-preview-modal.camera-preview-modal-content.button.open-background-gallery"
							>
								<Trans>Change background</Trans>
							</Button>
							{selectedBackgroundImageId === BLUR_BACKGROUND_ID && (
								<CameraEffectStrengthSlider
									label={i18n._(BLUR_STRENGTH_DESCRIPTOR)}
									value={voiceSettings.backgroundBlurStrength}
									onChange={(value) => VoiceSettingsCommands.update({backgroundBlurStrength: value})}
									resetLabel={i18n._(RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR)}
									dataFlx="voice.camera-preview-modal.camera-preview-modal-content.blur-strength"
									data-flx="voice.camera-preview-modal.camera-preview-modal-content.camera-effect-strength-slider.update"
								/>
							)}
						</div>
					)}
					{showEffectsUnavailableNotice && (
						<div
							className={styles.effectsNotice}
							data-flx="voice.camera-preview-modal.camera-preview-modal-content.effects-notice"
						>
							{i18n._(EFFECTS_PREVIEW_UNAVAILABLE_DESCRIPTOR)}
						</div>
					)}
					<div
						className={styles.videoContainer}
						data-flx="voice.camera-preview-modal.camera-preview-modal-content.video-container"
					>
						<video
							ref={videoRef}
							autoPlay
							playsInline
							muted
							className={previewVideoClassName}
							aria-label={i18n._(CAMERA_PREVIEW_DESCRIPTOR)}
							data-flx="voice.camera-preview-modal.camera-preview-modal-content.video"
						/>
						{(status === 'initializing' || status === 'fixing' || status === 'fix-switching-back') && (
							<div
								className={styles.overlay}
								data-flx="voice.camera-preview-modal.camera-preview-modal-content.overlay"
							>
								<Spinner data-flx="voice.camera-preview-modal.camera-preview-modal-content.spinner" />
								<div
									className={styles.overlayText}
									data-flx="voice.camera-preview-modal.camera-preview-modal-content.overlay-text"
								>
									<div
										className={styles.overlayTextMedium}
										data-flx="voice.camera-preview-modal.camera-preview-modal-content.overlay-text-medium"
									>
										{status === 'fixing' ? (
											<Trans>Optimizing camera...</Trans>
										) : status === 'fix-switching-back' ? (
											<Trans>Finalizing camera...</Trans>
										) : (
											<Trans>Initializing camera...</Trans>
										)}
									</div>
								</div>
							</div>
						)}
						{status === 'error' && (
							<div
								className={styles.errorOverlay}
								data-flx="voice.camera-preview-modal.camera-preview-modal-content.error-overlay"
							>
								<div
									className={styles.errorText}
									data-flx="voice.camera-preview-modal.camera-preview-modal-content.error-text"
								>
									<div
										className={styles.errorTitle}
										data-flx="voice.camera-preview-modal.camera-preview-modal-content.error-title"
									>
										<Trans>Camera error</Trans>
									</div>
									<div
										className={styles.errorDetail}
										data-flx="voice.camera-preview-modal.camera-preview-modal-content.error-detail"
									>
										{error}
									</div>
								</div>
							</div>
						)}
					</div>
				</div>
			</Modal.Content>
			<Modal.Footer
				className={styles.footer}
				data-flx="voice.camera-preview-modal.camera-preview-modal-content.modal-footer"
			>
				<div
					className={styles.footerStart}
					data-flx="voice.camera-preview-modal.camera-preview-modal-content.footer-start"
				>
					<Switch
						compact
						className={styles.footerMirrorSwitch}
						label={i18n._(MIRROR_CAMERA_DESCRIPTOR)}
						value={voiceSettings.mirrorCamera}
						onChange={(value) => VoiceSettingsCommands.update({mirrorCamera: value})}
						data-flx="voice.camera-preview-modal.camera-preview-modal-content.footer-mirror-switch.update"
					/>
				</div>
				<Button
					variant="secondary"
					onClick={() => ModalCommands.pop()}
					data-flx="voice.camera-preview-modal.camera-preview-modal-content.button.pop"
				>
					<Trans>Cancel</Trans>
				</Button>
				{showEnableCameraButton &&
					!cameraAlreadyOn &&
					(canStream && !cameraCapBlocked ? (
						<Button
							onClick={handleEnableCamera}
							leftIcon={
								<CameraIcon
									size={remFromPx(16)}
									data-flx="voice.camera-preview-modal.camera-preview-modal-content.camera-icon"
								/>
							}
							data-flx="voice.camera-preview-modal.camera-preview-modal-content.button.enable-camera"
						>
							{i18n._(VOICE_TURN_ON_CAMERA_DESCRIPTOR)}
						</Button>
					) : (
						<Tooltip
							text={
								canStream
									? i18n._(VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR, {
											voiceChannelCameraUserLimit: VOICE_CHANNEL_CAMERA_USER_LIMIT,
										})
									: i18n._(YOU_DON_T_HAVE_PERMISSION_TO_TURN_ON_DESCRIPTOR)
							}
							data-flx="voice.camera-preview-modal.camera-preview-modal-content.tooltip"
						>
							<Button
								onClick={undefined}
								leftIcon={
									<CameraIcon
										size={remFromPx(16)}
										data-flx="voice.camera-preview-modal.camera-preview-modal-content.camera-icon--2"
									/>
								}
								disabled
								data-flx="voice.camera-preview-modal.camera-preview-modal-content.button.undefined"
							>
								{i18n._(VOICE_TURN_ON_CAMERA_DESCRIPTOR)}
							</Button>
						</Tooltip>
					))}
			</Modal.Footer>
		</Modal.Root>
	);
});
export const CameraPreviewModalInRoom: React.FC<Omit<CameraPreviewModalProps, 'localParticipant' | 'isCameraEnabled'>> =
	observer((props) => {
		const {localParticipant, isCameraEnabled} = useCameraPreviewParticipantState();
		return (
			<CameraPreviewModalContent
				localParticipant={localParticipant}
				isCameraEnabled={isCameraEnabled}
				data-flx="voice.camera-preview-modal.camera-preview-modal-in-room.camera-preview-modal-content"
				{...props}
			/>
		);
	});
export const CameraPreviewModalStandalone: React.FC<CameraPreviewModalProps> = observer((props) => {
	return (
		<CameraPreviewModalContent
			localParticipant={undefined}
			isCameraEnabled={false}
			data-flx="voice.camera-preview-modal.camera-preview-modal-standalone.camera-preview-modal-content"
			{...props}
		/>
	);
});
