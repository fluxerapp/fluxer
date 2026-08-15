// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {
	CLOSE_DESCRIPTOR,
	MORE_OPTIONS_DESCRIPTOR,
	PAUSE_DESCRIPTOR,
	PLAY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {PanZoomSurface} from '@app/features/messaging/components/modals/media_modal/pan_zoom/PanZoomSurface';
import type {ZoomState} from '@app/features/messaging/components/modals/media_modal/shared';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {
	clampMediaTime,
	getBufferedPercentage,
	getEffectiveMediaDuration,
	getSeekPercentageFromClientX,
	resolveDoubleTapSeekDirection,
	type SeekDirection,
	type SeekTapPoint,
} from '@app/features/voice/components/media_player/utils/MediaSeekUtils';
import styles from '@app/features/voice/components/modals/MobileVideoViewer.module.css';
import {useInAppMediaSoundCapture} from '@app/features/voice/hooks/useInAppMediaSoundCapture';
import VideoVolume from '@app/features/voice/state/VideoVolume';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {DotsThreeIcon, PauseIcon, PlayIcon, SpeakerHighIcon, SpeakerXIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const TOGGLE_CONTROLS_DESCRIPTOR = msg({
	message: 'Toggle controls',
	comment: 'Aria label on the tap-to-toggle area of the mobile video viewer. Shows or hides the playback controls.',
});
const UNMUTE_DESCRIPTOR = msg({
	message: 'Unmute',
	comment: 'Mute toggle button label in the mobile video viewer (currently muted).',
});
const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	comment: 'Mute toggle button label in the mobile video viewer (currently unmuted).',
});
const VIDEO_PROGRESS_DESCRIPTOR = msg({
	message: 'Video progress',
	comment: 'Aria label on the video scrubber / progress slider in the mobile video viewer.',
});
const DOUBLE_TAP_SEEK_SECONDS = 10;

interface SeekFeedbackState {
	id: number;
	direction: SeekDirection;
	seconds: number;
}

interface MobileVideoViewerProps {
	src: string;
	initialTime?: number;
	loop?: boolean;
	onClose: () => void;
	onMenuOpen?: () => void;
}

function formatTime(time: number): string {
	if (!Number.isFinite(time)) return '0:00';
	const minutes = Math.floor(time / 60);
	const seconds = Math.floor(time % 60);
	return `${minutes}:${seconds.toString().padStart(2, '0')}`;
}

export const MobileVideoViewer = observer(function MobileVideoViewer({
	src,
	initialTime,
	loop = true,
	onClose,
	onMenuOpen,
}: MobileVideoViewerProps) {
	const {i18n} = useLingui();
	const videoRef = useRef<HTMLVideoElement>(null);
	useInAppMediaSoundCapture(videoRef);
	const [isPlaying, setIsPlaying] = useState(false);
	const [zoomState, setZoomState] = useState<ZoomState>('fit');
	const [zoomResetKey, setZoomResetKey] = useState(0);
	const [currentTime, setCurrentTime] = useState(0);
	const [duration, setDuration] = useState(0);
	const [bufferedProgress, setBufferedProgress] = useState(0);
	const [hudVisible, setHudVisible] = useState(true);
	const [isScrubbing, setIsScrubbing] = useState(false);
	const [scrubProgress, setScrubProgress] = useState<number | null>(null);
	const [seekFeedback, setSeekFeedback] = useState<SeekFeedbackState | null>(null);
	const hudTimerRef = useRef<NodeJS.Timeout | null>(null);
	const seekFeedbackTimerRef = useRef<NodeJS.Timeout | null>(null);
	const progressRectRef = useRef<DOMRect | null>(null);
	const activeProgressPointerIdRef = useRef<number | null>(null);
	const pendingTapPointRef = useRef<SeekTapPoint | null>(null);
	const previousTapPointRef = useRef<SeekTapPoint | null>(null);
	const [hasInitialized, setHasInitialized] = useState(false);
	const safelyPlayVideo = useCallback((video: HTMLVideoElement) => {
		const playPromise = video.play();
		void playPromise?.catch(() => {});
	}, []);
	const progress = duration > 0 ? currentTime / duration : 0;
	const displayProgress = scrubProgress ?? progress;
	const displayCurrentTime = duration > 0 ? displayProgress * duration : currentTime;
	const clearHudTimer = useCallback(() => {
		if (!hudTimerRef.current) return;
		clearTimeout(hudTimerRef.current);
		hudTimerRef.current = null;
	}, []);
	const scheduleHudHide = useCallback(() => {
		clearHudTimer();
		hudTimerRef.current = setTimeout(() => {
			if (isPlaying) {
				setHudVisible(false);
			}
			hudTimerRef.current = null;
		}, 3000);
	}, [clearHudTimer, isPlaying]);
	const commitSeekToTime = useCallback(
		(time: number) => {
			const video = videoRef.current;
			if (!video) return;
			const effectiveDuration = getEffectiveMediaDuration(video, duration);
			if (!effectiveDuration) return;
			const nextTime = clampMediaTime(time, effectiveDuration);
			video.currentTime = nextTime;
			setCurrentTime(nextTime);
			setScrubProgress(null);
		},
		[duration],
	);
	const commitSeekToProgress = useCallback(
		(nextProgress: number) => {
			if (duration <= 0) return;
			commitSeekToTime(nextProgress * duration);
		},
		[commitSeekToTime, duration],
	);
	const showSeekFeedback = useCallback((direction: SeekDirection) => {
		if (seekFeedbackTimerRef.current) {
			clearTimeout(seekFeedbackTimerRef.current);
		}
		setSeekFeedback({
			id: Date.now(),
			direction,
			seconds: DOUBLE_TAP_SEEK_SECONDS,
		});
		seekFeedbackTimerRef.current = setTimeout(() => {
			setSeekFeedback(null);
			seekFeedbackTimerRef.current = null;
		}, 650);
	}, []);
	const seekRelative = useCallback(
		(deltaSeconds: number) => {
			const video = videoRef.current;
			if (!video) return;
			commitSeekToTime(video.currentTime + deltaSeconds);
		},
		[commitSeekToTime],
	);
	const handleTapSurface = useCallback(() => {
		const tapPoint = pendingTapPointRef.current;
		pendingTapPointRef.current = null;
		const direction = tapPoint ? resolveDoubleTapSeekDirection(previousTapPointRef.current, tapPoint) : null;
		if (direction) {
			previousTapPointRef.current = null;
			seekRelative(direction === 'forward' ? DOUBLE_TAP_SEEK_SECONDS : -DOUBLE_TAP_SEEK_SECONDS);
			showSeekFeedback(direction);
			setHudVisible(true);
			scheduleHudHide();
			return;
		}
		previousTapPointRef.current = tapPoint;
		setHudVisible((prev) => !prev);
		if (!hudVisible) {
			scheduleHudHide();
		}
	}, [hudVisible, scheduleHudHide, seekRelative, showSeekFeedback]);
	const handlePlayPause = useCallback(
		(e: React.MouseEvent) => {
			e.stopPropagation();
			const video = videoRef.current;
			if (!video) return;
			if (video.paused) {
				safelyPlayVideo(video);
				setZoomResetKey((key) => key + 1);
			} else {
				video.pause();
			}
		},
		[safelyPlayVideo],
	);
	const handleToggleMute = useCallback((e: React.MouseEvent) => {
		e.stopPropagation();
		VideoVolume.toggleMute();
	}, []);
	const getProgressFromClientX = useCallback((clientX: number): number => {
		const rect = progressRectRef.current;
		if (!rect) return 0;
		return getSeekPercentageFromClientX(clientX, rect) / 100;
	}, []);
	const handleProgressPointerDown = useCallback(
		(e: React.PointerEvent<HTMLDivElement>) => {
			if (e.pointerType === 'mouse' && e.button !== 0) return;
			if (duration <= 0) return;
			e.preventDefault();
			e.stopPropagation();
			clearHudTimer();
			progressRectRef.current = e.currentTarget.getBoundingClientRect();
			activeProgressPointerIdRef.current = e.pointerId;
			e.currentTarget.setPointerCapture?.(e.pointerId);
			setHudVisible(true);
			setIsScrubbing(true);
			setScrubProgress(getProgressFromClientX(e.clientX));
		},
		[clearHudTimer, duration, getProgressFromClientX],
	);
	const handleProgressPointerMove = useCallback(
		(e: React.PointerEvent<HTMLDivElement>) => {
			if (activeProgressPointerIdRef.current !== e.pointerId) return;
			e.preventDefault();
			e.stopPropagation();
			setScrubProgress(getProgressFromClientX(e.clientX));
		},
		[getProgressFromClientX],
	);
	const finishProgressPointer = useCallback(
		(e: React.PointerEvent<HTMLDivElement>) => {
			if (activeProgressPointerIdRef.current !== e.pointerId) return;
			e.preventDefault();
			e.stopPropagation();
			const nextProgress = getProgressFromClientX(e.clientX);
			commitSeekToProgress(nextProgress);
			activeProgressPointerIdRef.current = null;
			progressRectRef.current = null;
			e.currentTarget.releasePointerCapture?.(e.pointerId);
			setIsScrubbing(false);
			setScrubProgress(null);
			scheduleHudHide();
		},
		[commitSeekToProgress, getProgressFromClientX, scheduleHudHide],
	);
	const handleProgressPointerCancel = useCallback(
		(e: React.PointerEvent<HTMLDivElement>) => {
			if (activeProgressPointerIdRef.current !== e.pointerId) return;
			e.preventDefault();
			e.stopPropagation();
			commitSeekToProgress(scrubProgress ?? progress);
			activeProgressPointerIdRef.current = null;
			progressRectRef.current = null;
			e.currentTarget.releasePointerCapture?.(e.pointerId);
			setIsScrubbing(false);
			setScrubProgress(null);
			scheduleHudHide();
		},
		[commitSeekToProgress, progress, scheduleHudHide, scrubProgress],
	);
	const handleProgressKeyDown = useCallback(
		(e: React.KeyboardEvent<HTMLDivElement>) => {
			if (duration <= 0) return;
			let nextTime = displayCurrentTime;
			if (e.key === 'ArrowLeft') {
				nextTime -= 5;
			} else if (e.key === 'ArrowRight') {
				nextTime += 5;
			} else if (e.key === 'Home') {
				nextTime = 0;
			} else if (e.key === 'End') {
				nextTime = duration;
			} else {
				return;
			}
			e.preventDefault();
			e.stopPropagation();
			commitSeekToTime(nextTime);
			setHudVisible(true);
			scheduleHudHide();
		},
		[commitSeekToTime, displayCurrentTime, duration, scheduleHudHide],
	);
	const handleVideoPointerUp = useCallback((e: React.PointerEvent<HTMLVideoElement>) => {
		const rect = e.currentTarget.getBoundingClientRect();
		pendingTapPointRef.current = {
			x: e.clientX - rect.left,
			width: rect.width,
			time: performance.now(),
		};
	}, []);
	useEffect(() => {
		const video = videoRef.current;
		if (!video) return;
		const handlePlay = () => {
			setIsPlaying(true);
			scheduleHudHide();
		};
		const handlePause = () => {
			setIsPlaying(false);
			setHudVisible(true);
			clearHudTimer();
		};
		const updateBuffered = () => setBufferedProgress(getBufferedPercentage(video) / 100);
		const handleTimeUpdate = () => {
			setCurrentTime(video.currentTime);
			updateBuffered();
		};
		const handleLoadedMetadata = () => {
			const nextDuration = getEffectiveMediaDuration(video);
			setDuration(nextDuration);
			updateBuffered();
			if (initialTime !== undefined && !hasInitialized) {
				const startTime = clampMediaTime(initialTime, nextDuration);
				video.currentTime = startTime;
				setCurrentTime(startTime);
				setHasInitialized(true);
				safelyPlayVideo(video);
			}
		};
		const handleDurationChange = () => {
			setDuration(getEffectiveMediaDuration(video));
			updateBuffered();
		};
		video.addEventListener('play', handlePlay);
		video.addEventListener('pause', handlePause);
		video.addEventListener('timeupdate', handleTimeUpdate);
		video.addEventListener('loadedmetadata', handleLoadedMetadata);
		video.addEventListener('durationchange', handleDurationChange);
		video.addEventListener('progress', updateBuffered);
		video.addEventListener('canplay', updateBuffered);
		return () => {
			video.removeEventListener('play', handlePlay);
			video.removeEventListener('pause', handlePause);
			video.removeEventListener('timeupdate', handleTimeUpdate);
			video.removeEventListener('loadedmetadata', handleLoadedMetadata);
			video.removeEventListener('durationchange', handleDurationChange);
			video.removeEventListener('progress', updateBuffered);
			video.removeEventListener('canplay', updateBuffered);
		};
	}, [clearHudTimer, initialTime, hasInitialized, scheduleHudHide, safelyPlayVideo]);
	useEffect(() => {
		const video = videoRef.current;
		if (!video) return;
		video.muted = VideoVolume.isMuted;
	}, []);
	useEffect(() => {
		return () => {
			clearHudTimer();
			if (seekFeedbackTimerRef.current) {
				clearTimeout(seekFeedbackTimerRef.current);
				seekFeedbackTimerRef.current = null;
			}
		};
	}, [clearHudTimer]);
	const handleZoomStateChange = useCallback((state: ZoomState) => {
		setZoomState((previousState) => (previousState === state ? previousState : state));
	}, []);
	const handleClose = useCallback(
		(e: React.MouseEvent) => {
			e.stopPropagation();
			onClose();
		},
		[onClose],
	);
	const handleMenuOpen = useCallback(
		(e: React.MouseEvent) => {
			e.stopPropagation();
			onMenuOpen?.();
		},
		[onMenuOpen],
	);
	return (
		<div className={styles.container} data-flx="voice.mobile-video-viewer.container">
			<PanZoomSurface
				className={styles.zoomSurface}
				contentClassName={styles.zoomContent}
				zoomState={zoomState}
				maxScale={5}
				zoomedScale={2.25}
				disabled={isPlaying}
				panDisabled={isPlaying}
				doubleClickEnabled={!isPlaying}
				tapToToggleZoom={false}
				resetKey={zoomResetKey}
				onZoomStateChange={handleZoomStateChange}
				onTap={handleTapSurface}
				onKeyDown={(e) => {
					if (isKeyboardActivationKey(e.key)) {
						e.preventDefault();
						handleTapSurface();
					}
				}}
				role="button"
				tabIndex={0}
				aria-label={i18n._(TOGGLE_CONTROLS_DESCRIPTOR)}
				data-flx="voice.mobile-video-viewer.pan-zoom-surface"
			>
				<video
					ref={videoRef}
					className={styles.video}
					src={src}
					autoPlay={initialTime === undefined}
					playsInline
					loop={loop}
					muted={VideoVolume.isMuted}
					onPointerUp={handleVideoPointerUp}
					data-flx="voice.mobile-video-viewer.video"
				/>
			</PanZoomSurface>
			<AnimatePresence data-flx="voice.mobile-video-viewer.seek-feedback-presence">
				{seekFeedback && (
					<motion.div
						key={seekFeedback.id}
						className={clsx(
							styles.seekFeedback,
							seekFeedback.direction === 'backward' ? styles.seekFeedbackBackward : styles.seekFeedbackForward,
						)}
						initial={{opacity: 0, scale: Accessibility.useReducedMotion ? 1 : 0.86}}
						animate={{opacity: 1, scale: 1}}
						exit={{opacity: 0, scale: Accessibility.useReducedMotion ? 1 : 1.08}}
						transition={{duration: Accessibility.useReducedMotion ? 0 : 0.16}}
						aria-hidden="true"
						data-flx="voice.mobile-video-viewer.seek-feedback"
					>
						{seekFeedback.direction === 'backward' ? '-' : '+'}
						{seekFeedback.seconds}s
					</motion.div>
				)}
			</AnimatePresence>
			<AnimatePresence data-flx="voice.mobile-video-viewer.animate-presence">
				{hudVisible && zoomState === 'fit' && (
					<motion.div
						className={styles.hudOverlay}
						initial={{opacity: 0}}
						animate={{opacity: 1}}
						exit={{opacity: 0}}
						transition={{duration: Accessibility.useReducedMotion ? 0 : 0.15}}
						data-flx="voice.mobile-video-viewer.hud-overlay"
					>
						<div className={styles.topBar} data-flx="voice.mobile-video-viewer.top-bar">
							<button
								type="button"
								className={styles.topBarButton}
								onClick={handleClose}
								aria-label={i18n._(CLOSE_DESCRIPTOR)}
								data-flx="voice.mobile-video-viewer.top-bar-button.close"
							>
								<XIcon size={remFromPx(20)} weight="bold" data-flx="voice.mobile-video-viewer.x-icon" />
							</button>
							{onMenuOpen && (
								<button
									type="button"
									className={styles.topBarButton}
									onClick={handleMenuOpen}
									aria-label={i18n._(MORE_OPTIONS_DESCRIPTOR)}
									data-flx="voice.mobile-video-viewer.top-bar-button.menu-open"
								>
									<DotsThreeIcon
										size={remFromPx(20)}
										weight="bold"
										data-flx="voice.mobile-video-viewer.dots-three-icon"
									/>
								</button>
							)}
						</div>
						<div className={styles.bottomArea} data-flx="voice.mobile-video-viewer.bottom-area">
							<button
								type="button"
								className={styles.muteButton}
								onClick={handleToggleMute}
								aria-label={VideoVolume.isMuted ? i18n._(UNMUTE_DESCRIPTOR) : i18n._(MUTE_DESCRIPTOR)}
								data-flx="voice.mobile-video-viewer.mute-button.toggle-mute"
							>
								{VideoVolume.isMuted ? (
									<SpeakerXIcon
										size={remFromPx(18)}
										weight="fill"
										data-flx="voice.mobile-video-viewer.speaker-x-icon"
									/>
								) : (
									<SpeakerHighIcon
										size={remFromPx(18)}
										weight="fill"
										data-flx="voice.mobile-video-viewer.speaker-high-icon"
									/>
								)}
							</button>
							<div className={styles.controlsBar} data-flx="voice.mobile-video-viewer.controls-bar">
								<button
									type="button"
									className={styles.playPauseButton}
									onClick={handlePlayPause}
									aria-label={isPlaying ? i18n._(PAUSE_DESCRIPTOR) : i18n._(PLAY_DESCRIPTOR)}
									data-flx="voice.mobile-video-viewer.play-pause-button"
								>
									{isPlaying ? (
										<PauseIcon size={remFromPx(20)} weight="fill" data-flx="voice.mobile-video-viewer.pause-icon" />
									) : (
										<PlayIcon size={remFromPx(20)} weight="fill" data-flx="voice.mobile-video-viewer.play-icon" />
									)}
								</button>
								<div
									className={clsx(styles.progressBarWrapper, isScrubbing && styles.progressBarScrubbing)}
									onPointerDown={handleProgressPointerDown}
									onPointerMove={handleProgressPointerMove}
									onPointerUp={finishProgressPointer}
									onPointerCancel={handleProgressPointerCancel}
									onKeyDown={handleProgressKeyDown}
									role="slider"
									tabIndex={0}
									aria-valuenow={Math.round(displayProgress * 100)}
									aria-valuemin={0}
									aria-valuemax={100}
									aria-valuetext={formatTime(displayCurrentTime)}
									aria-label={i18n._(VIDEO_PROGRESS_DESCRIPTOR)}
									data-flx="voice.mobile-video-viewer.progress-bar-wrapper.progress-seek"
								>
									<div className={styles.progressTrack} data-flx="voice.mobile-video-viewer.progress-track">
										<div
											className={styles.progressBuffered}
											style={{width: `${bufferedProgress * 100}%`}}
											data-flx="voice.mobile-video-viewer.progress-buffered"
										/>
										<div
											className={styles.progressFill}
											style={{width: `${displayProgress * 100}%`}}
											data-flx="voice.mobile-video-viewer.progress-fill"
										/>
										<div
											className={styles.progressThumb}
											style={{left: `${displayProgress * 100}%`}}
											data-flx="voice.mobile-video-viewer.progress-thumb"
										/>
									</div>
								</div>
								<span className={styles.timeDisplay} data-flx="voice.mobile-video-viewer.time-display">
									{formatTime(displayCurrentTime)} / {formatTime(duration)}
								</span>
							</div>
						</div>
					</motion.div>
				)}
			</AnimatePresence>
		</div>
	);
});
