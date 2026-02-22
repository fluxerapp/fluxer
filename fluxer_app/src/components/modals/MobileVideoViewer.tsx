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

import styles from '@app/components/modals/MobileVideoViewer.module.css';
import AccessibilityStore from '@app/stores/AccessibilityStore';
import VideoVolumeStore from '@app/stores/VideoVolumeStore';
import {useLingui} from '@lingui/react/macro';
import {DotsThreeIcon, PauseIcon, PlayIcon, SpeakerHighIcon, SpeakerXIcon, XIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';
import {type ReactZoomPanPinchRef, TransformComponent, TransformWrapper} from 'react-zoom-pan-pinch';

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
	const {t} = useLingui();
	const videoRef = useRef<HTMLVideoElement>(null);
	const transformRef = useRef<ReactZoomPanPinchRef>(null);
	const [isPlaying, setIsPlaying] = useState(false);
	const [zoomScale, setZoomScale] = useState(1);
	const [currentTime, setCurrentTime] = useState(0);
	const [duration, setDuration] = useState(0);
	const [hudVisible, setHudVisible] = useState(true);
	const hudTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);
	const [hasInitialized, setHasInitialized] = useState(false);

	const safelyPlayVideo = useCallback((video: HTMLVideoElement) => {
		const playPromise = video.play();
		void playPromise?.catch(() => {});
	}, []);

	const progress = duration > 0 ? currentTime / duration : 0;

	const scheduleHudHide = useCallback(() => {
		if (hudTimerRef.current) {
			clearTimeout(hudTimerRef.current);
		}
		hudTimerRef.current = setTimeout(() => {
			if (isPlaying) {
				setHudVisible(false);
			}
			hudTimerRef.current = null;
		}, 3000);
	}, [isPlaying]);

	const handleTapSurface = useCallback(() => {
		setHudVisible((prev) => !prev);
		if (!hudVisible) {
			scheduleHudHide();
		}
	}, [hudVisible, scheduleHudHide]);

	const handleZoomChange = useCallback((_ref: ReactZoomPanPinchRef, state: {scale: number}) => {
		setZoomScale(state.scale);
	}, []);

	const handlePlayPause = useCallback(
		(e: React.MouseEvent) => {
			e.stopPropagation();
			const video = videoRef.current;
			if (!video) return;

			if (video.paused) {
				safelyPlayVideo(video);
				transformRef.current?.resetTransform();
			} else {
				video.pause();
			}
		},
		[safelyPlayVideo],
	);

	const handleToggleMute = useCallback((e: React.MouseEvent) => {
		e.stopPropagation();
		VideoVolumeStore.toggleMute();
	}, []);

	const handleProgressClick = useCallback(
		(e: React.MouseEvent<HTMLDivElement>) => {
			e.stopPropagation();
			const video = videoRef.current;
			if (!video || !duration) return;
			const rect = e.currentTarget.getBoundingClientRect();
			const x = e.clientX - rect.left;
			const percentage = Math.max(0, Math.min(1, x / rect.width));
			video.currentTime = percentage * duration;
		},
		[duration],
	);

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
			if (hudTimerRef.current) {
				clearTimeout(hudTimerRef.current);
				hudTimerRef.current = null;
			}
		};
		const handleTimeUpdate = () => setCurrentTime(video.currentTime);
		const handleLoadedMetadata = () => {
			setDuration(video.duration);
			if (initialTime && !hasInitialized) {
				video.currentTime = initialTime;
				setHasInitialized(true);
				safelyPlayVideo(video);
			}
		};
		const handleDurationChange = () => setDuration(video.duration);

		video.addEventListener('play', handlePlay);
		video.addEventListener('pause', handlePause);
		video.addEventListener('timeupdate', handleTimeUpdate);
		video.addEventListener('loadedmetadata', handleLoadedMetadata);
		video.addEventListener('durationchange', handleDurationChange);

		return () => {
			video.removeEventListener('play', handlePlay);
			video.removeEventListener('pause', handlePause);
			video.removeEventListener('timeupdate', handleTimeUpdate);
			video.removeEventListener('loadedmetadata', handleLoadedMetadata);
			video.removeEventListener('durationchange', handleDurationChange);
		};
	}, [initialTime, hasInitialized, scheduleHudHide, safelyPlayVideo]);

	useEffect(() => {
		const video = videoRef.current;
		if (!video) return;
		video.muted = VideoVolumeStore.isMuted;
	}, []);

	useEffect(() => {
		return () => {
			if (hudTimerRef.current) {
				clearTimeout(hudTimerRef.current);
			}
		};
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
		<div className={styles.container}>
			<TransformWrapper
				ref={transformRef}
				initialScale={1}
				minScale={1}
				maxScale={5}
				pinch={{step: 5}}
				doubleClick={{disabled: true}}
				panning={{disabled: isPlaying}}
				disabled={isPlaying}
				onTransformed={handleZoomChange}
				centerOnInit
				centerZoomedOut
			>
				<TransformComponent
					wrapperStyle={{width: '100%', height: '100%'}}
					contentStyle={{
						width: '100%',
						height: '100%',
						display: 'flex',
						alignItems: 'center',
						justifyContent: 'center',
					}}
				>
					<video
						ref={videoRef}
						className={styles.video}
						src={src}
						autoPlay={!initialTime}
						playsInline
						loop={loop}
						muted={VideoVolumeStore.isMuted}
					/>
				</TransformComponent>
			</TransformWrapper>

			<div
				className={styles.tapSurface}
				onClick={handleTapSurface}
				onKeyDown={(e) => {
					if (e.key === 'Enter' || e.key === ' ') {
						handleTapSurface();
					}
				}}
				role="button"
				tabIndex={0}
				aria-label={t`Toggle controls`}
			/>

			<AnimatePresence>
				{hudVisible && zoomScale <= 1 && (
					<motion.div
						className={styles.hudOverlay}
						initial={{opacity: 0}}
						animate={{opacity: 1}}
						exit={{opacity: 0}}
						transition={{duration: AccessibilityStore.useReducedMotion ? 0 : 0.15}}
					>
						<div className={styles.topBar}>
							<button type="button" className={styles.topBarButton} onClick={handleClose} aria-label={t`Close`}>
								<XIcon size={20} weight="bold" />
							</button>
							{onMenuOpen && (
								<button
									type="button"
									className={styles.topBarButton}
									onClick={handleMenuOpen}
									aria-label={t`More options`}
								>
									<DotsThreeIcon size={20} weight="bold" />
								</button>
							)}
						</div>

						<div className={styles.bottomArea}>
							<button
								type="button"
								className={styles.muteButton}
								onClick={handleToggleMute}
								aria-label={VideoVolumeStore.isMuted ? t`Unmute` : t`Mute`}
							>
								{VideoVolumeStore.isMuted ? (
									<SpeakerXIcon size={18} weight="fill" />
								) : (
									<SpeakerHighIcon size={18} weight="fill" />
								)}
							</button>

							<div className={styles.controlsBar}>
								<button
									type="button"
									className={styles.playPauseButton}
									onClick={handlePlayPause}
									aria-label={isPlaying ? t`Pause` : t`Play`}
								>
									{isPlaying ? <PauseIcon size={20} weight="fill" /> : <PlayIcon size={20} weight="fill" />}
								</button>

								<div
									className={styles.progressBarWrapper}
									onClick={handleProgressClick}
									onKeyDown={(e) => {
										if (e.key === 'ArrowLeft' || e.key === 'ArrowRight') {
											e.preventDefault();
											const video = videoRef.current;
											if (!video) return;
											const step = duration * 0.05;
											video.currentTime = Math.max(
												0,
												Math.min(duration, video.currentTime + (e.key === 'ArrowRight' ? step : -step)),
											);
										}
									}}
									role="slider"
									tabIndex={0}
									aria-valuenow={Math.round(progress * 100)}
									aria-valuemin={0}
									aria-valuemax={100}
									aria-label={t`Video progress`}
								>
									<div className={styles.progressTrack}>
										<div className={styles.progressFill} style={{width: `${progress * 100}%`}} />
									</div>
								</div>

								<span className={styles.timeDisplay}>
									{formatTime(currentTime)} / {formatTime(duration)}
								</span>
							</div>
						</div>
					</motion.div>
				)}
			</AnimatePresence>
		</div>
	);
});
