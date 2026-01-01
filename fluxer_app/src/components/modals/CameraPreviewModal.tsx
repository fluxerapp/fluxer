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

import {Trans, useLingui} from '@lingui/react/macro';
import {useLocalParticipant} from '@livekit/components-react';
import {BackgroundProcessor} from '@livekit/track-processors';
import {CameraIcon, ImageIcon} from '@phosphor-icons/react';
import type {LocalParticipant, LocalVideoTrack} from 'livekit-client';
import {createLocalVideoTrack} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as VoiceSettingsActionCreators from '~/actions/VoiceSettingsActionCreators';
import {Select} from '~/components/form/Select';
import BackgroundImageGalleryModal from '~/components/modals/BackgroundImageGalleryModal';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {Spinner} from '~/components/uikit/Spinner';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import VoiceSettingsStore, {BLUR_BACKGROUND_ID, NONE_BACKGROUND_ID} from '~/stores/VoiceSettingsStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import VoiceDevicePermissionStore, {type VoiceDeviceState} from '~/stores/voice/VoiceDevicePermissionStore';
import * as BackgroundImageDB from '~/utils/BackgroundImageDB';
import styles from './CameraPreviewModal.module.css';

interface CameraPreviewModalProps {
	onEnabled?: () => void;
	onEnableCamera?: () => void;
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

const MEDIAPIPE_TASKS_VISION_WASM_BASE = `https://fluxerstatic.com/libs/mediapipe/tasks-vision/0.10.14/wasm`;
const MEDIAPIPE_SEGMENTER_MODEL_PATH =
	'https://fluxerstatic.com/libs/mediapipe/image_segmenter/selfie_segmenter/float16/latest/selfie_segmenter.tflite';

const CAMERA_RESOLUTION_PRESETS: Record<'low' | 'medium' | 'high', VideoResolutionPreset> = {
	low: {width: 640, height: 360, frameRate: 24},
	medium: {width: 1280, height: 720, frameRate: 30},
	high: {width: 1920, height: 1080, frameRate: 30},
};

const CameraPreviewModalContent = observer((props: CameraPreviewModalProps) => {
	const {t} = useLingui();
	const {localParticipant, onEnabled, onEnableCamera, isCameraEnabled, showEnableCameraButton = true} = props;

	const [videoDevices, setVideoDevices] = useState<Array<MediaDeviceInfo>>([]);
	const [status, setStatus] = useState<
		'idle' | 'initializing' | 'ready' | 'error' | 'fixing' | 'fix-settling' | 'fix-switching-back'
	>('initializing');
	const [resolution, setResolution] = useState<{width: number; height: number} | null>(null);
	const [error, setError] = useState<string | null>(null);

	const videoRef = useRef<HTMLVideoElement>(null);
	const trackRef = useRef<LocalVideoTrack | null>(null);
	const processorRef = useRef<ReturnType<typeof BackgroundProcessor> | null>(null);
	const isMountedRef = useRef(true);
	const isIOSRef = useRef(/iPad|iPhone|iPod/.test(navigator.userAgent) && !(window as any).MSStream);
	const prevConfigRef = useRef<{
		videoDeviceId: string;
		backgroundImageId: string;
		cameraResolution: 'low' | 'medium' | 'high';
		videoFrameRate: number;
	} | null>(null);
	const originalBackgroundIdRef = useRef<string | null>(VoiceSettingsStore.backgroundImageId);
	const needsResolutionFixRef = useRef(false);
	const isApplyingFixRef = useRef(false);
	const initializationTimeoutRef = useRef<NodeJS.Timeout | null>(null);
	const fixTimeoutRef = useRef<NodeJS.Timeout | null>(null);
	const settleTimeoutRef = useRef<NodeJS.Timeout | null>(null);
	const switchBackTimeoutRef = useRef<NodeJS.Timeout | null>(null);

	const handleDeviceUpdate = useCallback((state: VoiceDeviceState) => {
		if (!isMountedRef.current) return;

		const videoInputs = state.videoDevices.filter((device) => device.deviceId !== 'default');
		setVideoDevices(videoInputs);

		const voiceSettings = VoiceSettingsStore;
		if (voiceSettings.videoDeviceId === 'default' && videoInputs.length > 0) {
			VoiceSettingsActionCreators.update({videoDeviceId: videoInputs[0].deviceId});
		}
	}, []);

	const applyResolutionFix = useCallback(() => {
		if (!isMountedRef.current || isApplyingFixRef.current) {
			return;
		}

		isApplyingFixRef.current = true;
		needsResolutionFixRef.current = false;

		const voiceSettings = VoiceSettingsStore;
		const currentBg = voiceSettings.backgroundImageId;

		const tempBg = currentBg === NONE_BACKGROUND_ID ? BLUR_BACKGROUND_ID : NONE_BACKGROUND_ID;

		setStatus('fixing');

		VoiceSettingsActionCreators.update({backgroundImageId: tempBg});

		settleTimeoutRef.current = setTimeout(() => {
			setStatus('fix-switching-back');
			VoiceSettingsActionCreators.update({backgroundImageId: originalBackgroundIdRef.current!});

			switchBackTimeoutRef.current = setTimeout(() => {
				if (isMountedRef.current) {
					isApplyingFixRef.current = false;
					setStatus('ready');
				}
			}, 500);
		}, 1200);
	}, []);

	const initializeCamera = useCallback(async () => {
		const voiceSettings = VoiceSettingsStore;
		const isMobile = MobileLayoutStore.isMobileLayout() || isIOSRef.current;

		if (isMobile) {
			if (isMountedRef.current) {
				setStatus('ready');
			}
			return;
		}

		if (!isMountedRef.current) {
			return;
		}

		let videoElement = videoRef.current;
		let attempts = 0;
		const maxAttempts = VIDEO_ELEMENT_WAIT_TIMEOUT / VIDEO_ELEMENT_CHECK_INTERVAL;

		while (!videoElement && attempts < maxAttempts) {
			await new Promise((resolve) => setTimeout(resolve, VIDEO_ELEMENT_CHECK_INTERVAL));
			videoElement = videoRef.current;
			attempts++;
		}

		if (!videoElement) {
			if (isMountedRef.current) {
				setStatus('error');
				setError('Video element not available');
			}
			return;
		}

		try {
			const currentConfig = {
				videoDeviceId: voiceSettings.videoDeviceId,
				backgroundImageId: voiceSettings.backgroundImageId,
				cameraResolution: voiceSettings.cameraResolution,
				videoFrameRate: voiceSettings.videoFrameRate,
			};

			if (prevConfigRef.current && JSON.stringify(prevConfigRef.current) === JSON.stringify(currentConfig)) {
				return;
			}

			prevConfigRef.current = currentConfig;

			if (!originalBackgroundIdRef.current) {
				originalBackgroundIdRef.current = voiceSettings.backgroundImageId;
			}

			if (isMountedRef.current) {
				setStatus(isApplyingFixRef.current ? 'fixing' : 'initializing');
				setError(null);
			}

			videoElement.muted = true;
			videoElement.autoplay = true;
			videoElement.playsInline = true;

			if (trackRef.current) {
				trackRef.current.stop();
				trackRef.current = null;
			}

			if (processorRef.current) {
				await processorRef.current.destroy();
				processorRef.current = null;
			}

			const resolutionPreset = CAMERA_RESOLUTION_PRESETS[voiceSettings.cameraResolution];

			const track = await createLocalVideoTrack({
				deviceId:
					voiceSettings.videoDeviceId && voiceSettings.videoDeviceId !== 'default'
						? voiceSettings.videoDeviceId
						: undefined,
				resolution: {
					width: resolutionPreset.width,
					height: resolutionPreset.height,
					frameRate: voiceSettings.videoFrameRate,
					aspectRatio: TARGET_ASPECT_RATIO,
				},
			});

			if (!isMountedRef.current) {
				track.stop();
				return;
			}

			trackRef.current = track;
			track.attach(videoElement);

			await new Promise<void>((resolve) => {
				let playbackAttempts = 0;
				const checkPlayback = () => {
					const hasData = videoElement!.srcObject && videoElement!.readyState >= 2;
					if (hasData) {
						resolve();
					} else if (++playbackAttempts < 100) {
						setTimeout(checkPlayback, 50);
					} else {
						resolve();
					}
				};
				checkPlayback();
			});

			if (!isMountedRef.current) {
				track.stop();
				return;
			}
			let negotiatedResolution: {width: number; height: number} | null = null;

			await new Promise<void>((resolve) => {
				let resolutionAttempts = 0;
				const checkResolution = () => {
					const settings = track.mediaStreamTrack.getSettings();
					if (settings.width && settings.height) {
						negotiatedResolution = {width: settings.width, height: settings.height};
						if (isMountedRef.current) {
							setResolution(negotiatedResolution);
						}
						resolve();
					} else if (++resolutionAttempts < RESOLUTION_WAIT_TIMEOUT / RESOLUTION_CHECK_INTERVAL) {
						setTimeout(checkResolution, RESOLUTION_CHECK_INTERVAL);
					} else {
						resolve();
					}
				};
				checkResolution();
			});

			if (!isMountedRef.current) {
				track.stop();
				return;
			}

			if (negotiatedResolution && !isApplyingFixRef.current) {
				const {width, height} = negotiatedResolution;
				const aspectRatio = width / height;
				const isValid16x9 = Math.abs(aspectRatio - TARGET_ASPECT_RATIO) < ASPECT_RATIO_TOLERANCE;
				needsResolutionFixRef.current = !isValid16x9;
			}

			const isNone = voiceSettings.backgroundImageId === NONE_BACKGROUND_ID;
			const isBlur = voiceSettings.backgroundImageId === BLUR_BACKGROUND_ID;

			try {
				if (isBlur) {
					processorRef.current = BackgroundProcessor({
						mode: 'background-blur',
						blurRadius: 20,
						assetPaths: {
							tasksVisionFileSet: MEDIAPIPE_TASKS_VISION_WASM_BASE,
							modelAssetPath: MEDIAPIPE_SEGMENTER_MODEL_PATH,
						},
					});
					await track.setProcessor(processorRef.current);
				} else if (!isNone) {
					const backgroundImage = voiceSettings.backgroundImages?.find(
						(img) => img.id === voiceSettings.backgroundImageId,
					);
					if (backgroundImage) {
						const imageUrl = await BackgroundImageDB.getBackgroundImageURL(backgroundImage.id);
						if (imageUrl) {
							processorRef.current = BackgroundProcessor({
								mode: 'virtual-background',
								imagePath: imageUrl,
								assetPaths: {
									tasksVisionFileSet: MEDIAPIPE_TASKS_VISION_WASM_BASE,
									modelAssetPath: MEDIAPIPE_SEGMENTER_MODEL_PATH,
								},
							});
							await track.setProcessor(processorRef.current);
						}
					}
				}
			} catch (_webglError) {
				console.warn('WebGL not supported for background processing, falling back to basic camera');
			}

			if (!isMountedRef.current) {
				track.stop();
				return;
			}

			if (isMountedRef.current) {
				setStatus('ready');

				if (needsResolutionFixRef.current && !isApplyingFixRef.current) {
					initializationTimeoutRef.current = setTimeout(() => applyResolutionFix(), 800);
				}
			}
		} catch (err) {
			if (isMountedRef.current) {
				const message = err instanceof Error ? err.message : 'Unknown error';
				setStatus('error');
				setError(message);
				ToastActionCreators.createToast({
					type: 'error',
					children: t`Failed to start camera preview. Please check your camera permissions.`,
				});
			}
		}
	}, [applyResolutionFix]);

	const handleDeviceChange = useCallback((deviceId: string) => {
		VoiceSettingsActionCreators.update({videoDeviceId: deviceId});
	}, []);

	const handleOpenBackgroundGallery = useCallback(() => {
		ModalActionCreators.push(modal(() => <BackgroundImageGalleryModal />));
	}, []);

	const handleEnableCamera = useCallback(async () => {
		if (!localParticipant) {
			onEnabled?.();
			onEnableCamera?.();
			ModalActionCreators.pop();
			return;
		}

		try {
			const voiceSettings = VoiceSettingsStore;
			await localParticipant.setCameraEnabled(true, {
				deviceId: voiceSettings.videoDeviceId !== 'default' ? voiceSettings.videoDeviceId : undefined,
			});

			LocalVoiceStateStore.updateSelfVideo(true);
			MediaEngineStore.syncLocalVoiceStateWithServer({self_video: true});

			onEnabled?.();
			onEnableCamera?.();
			ModalActionCreators.pop();
		} catch (_err) {
			ToastActionCreators.createToast({
				type: 'error',
				children: t`Failed to enable camera.`,
			});
		}
	}, [localParticipant, onEnabled, onEnableCamera]);

	useEffect(() => {
		isMountedRef.current = true;
		const unsubscribeDevices = VoiceDevicePermissionStore.subscribe(handleDeviceUpdate);
		void VoiceDevicePermissionStore.ensureDevices({requestPermissions: true}).catch(() => {});
		initializeCamera();

		return () => {
			isMountedRef.current = false;

			if (initializationTimeoutRef.current) clearTimeout(initializationTimeoutRef.current);
			if (fixTimeoutRef.current) clearTimeout(fixTimeoutRef.current);
			if (settleTimeoutRef.current) clearTimeout(settleTimeoutRef.current);
			if (switchBackTimeoutRef.current) clearTimeout(switchBackTimeoutRef.current);

			if (trackRef.current) {
				trackRef.current.stop();
				trackRef.current = null;
			}

			if (processorRef.current) {
				processorRef.current.destroy().catch(() => {});
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
	}, [handleDeviceUpdate, initializeCamera]);

	useEffect(() => {
		const voiceSettings = VoiceSettingsStore;
		const currentConfig = {
			videoDeviceId: voiceSettings.videoDeviceId,
			backgroundImageId: voiceSettings.backgroundImageId,
			cameraResolution: voiceSettings.cameraResolution,
			videoFrameRate: voiceSettings.videoFrameRate,
		};

		const configChanged =
			!prevConfigRef.current || JSON.stringify(prevConfigRef.current) !== JSON.stringify(currentConfig);
		if (configChanged) {
			initializeCamera();
		}
	}, [
		initializeCamera,
		VoiceSettingsStore.videoDeviceId,
		VoiceSettingsStore.backgroundImageId,
		VoiceSettingsStore.cameraResolution,
		VoiceSettingsStore.videoFrameRate,
	]);

	const voiceSettings = VoiceSettingsStore;

	const videoDeviceOptions = videoDevices.map((device) => ({
		value: device.deviceId,
		label: device.label || t`Camera ${device.deviceId.slice(0, 8)}`,
	}));

	const isValidAspectRatio = resolution
		? Math.abs(resolution.width / resolution.height - TARGET_ASPECT_RATIO) < ASPECT_RATIO_TOLERANCE
		: null;

	const resolutionDisplay = resolution
		? {
				display: `${resolution.width}×${resolution.height}`,
				aspectRatio: (resolution.width / resolution.height).toFixed(3),
				frameRate: voiceSettings.videoFrameRate,
			}
		: null;

	return (
		<Modal.Root size="medium">
			<Modal.Header title={t`Camera Preview`} />
			<Modal.Content>
				<div className={styles.content}>
					<div>
						<Select
							label={t`Camera`}
							value={voiceSettings.videoDeviceId}
							options={videoDeviceOptions}
							onChange={handleDeviceChange}
						/>
					</div>

					<div className={styles.backgroundSection}>
						<div className={styles.backgroundLabel}>
							<Trans>Background</Trans>
						</div>
						<Button variant="primary" onClick={handleOpenBackgroundGallery} leftIcon={<ImageIcon size={16} />}>
							<Trans>Change Background</Trans>
						</Button>
					</div>

					<div className={styles.videoContainer}>
						<video ref={videoRef} autoPlay playsInline muted className={styles.video} aria-label={t`Camera preview`} />

						{(status === 'initializing' || status === 'fixing' || status === 'fix-switching-back') && (
							<div className={styles.overlay}>
								<Spinner />
								<div className={styles.overlayText}>
									<div className={styles.overlayTextMedium}>
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
							<div className={styles.errorOverlay}>
								<div className={styles.errorText}>
									<div className={styles.errorTitle}>
										<Trans>Camera error</Trans>
									</div>
									<div className={styles.errorDetail}>{error}</div>
								</div>
							</div>
						)}

						<div className={styles.liveLabel}>
							<Trans>Live Preview</Trans>
						</div>

						{status === 'ready' && resolutionDisplay && (
							<div className={styles.resolutionInfo}>
								<div className={styles.resolutionDetails}>
									<div>{resolutionDisplay.display}</div>
									<div className={styles.resolutionRow}>
										<span>AR: {resolutionDisplay.aspectRatio}</span>
										<span>{resolutionDisplay.frameRate}fps</span>
										{isValidAspectRatio === false && (
											<Tooltip text={t`Not 16:9 aspect ratio`}>
												<span className={styles.warningIcon}>⚠</span>
											</Tooltip>
										)}
									</div>
								</div>
							</div>
						)}
					</div>
				</div>
			</Modal.Content>
			<Modal.Footer>
				<Button variant="secondary" onClick={() => ModalActionCreators.pop()}>
					<Trans>Cancel</Trans>
				</Button>
				{showEnableCameraButton && !isCameraEnabled && (
					<Button onClick={handleEnableCamera} leftIcon={<CameraIcon size={16} />}>
						<Trans>Turn On Camera</Trans>
					</Button>
				)}
			</Modal.Footer>
		</Modal.Root>
	);
});

const CameraPreviewModalInRoom: React.FC<Omit<CameraPreviewModalProps, 'localParticipant' | 'isCameraEnabled'>> =
	observer((props) => {
		const {localParticipant, isCameraEnabled} = useLocalParticipant();
		return (
			<CameraPreviewModalContent localParticipant={localParticipant} isCameraEnabled={isCameraEnabled} {...props} />
		);
	});

const CameraPreviewModalStandalone: React.FC<CameraPreviewModalProps> = observer((props) => {
	return <CameraPreviewModalContent localParticipant={undefined} isCameraEnabled={false} {...props} />;
});

export {CameraPreviewModalInRoom, CameraPreviewModalStandalone};
