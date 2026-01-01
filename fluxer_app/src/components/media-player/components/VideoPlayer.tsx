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
import {CircleNotchIcon, PauseIcon, PlayIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import {thumbHashToDataURL} from 'thumbhash';
import VideoVolumeStore from '~/stores/VideoVolumeStore';
import * as ImageCacheUtils from '~/utils/ImageCacheUtils';
import {useControlsVisibility} from '../hooks/useControlsVisibility';
import {useMediaFullscreen} from '../hooks/useMediaFullscreen';
import {useMediaKeyboard} from '../hooks/useMediaKeyboard';
import {useMediaPiP} from '../hooks/useMediaPiP';
import {useMediaPlayer} from '../hooks/useMediaPlayer';
import {useMediaProgress} from '../hooks/useMediaProgress';
import {VIDEO_BREAKPOINTS, VIDEO_PLAYBACK_RATES} from '../utils/mediaConstants';
import {MediaFullscreenButton} from './MediaFullscreenButton';
import {MediaPipButton} from './MediaPipButton';
import {MediaPlayButton} from './MediaPlayButton';
import {MediaPlaybackRate} from './MediaPlaybackRate';
import {MediaProgressBar} from './MediaProgressBar';
import {MediaTimeDisplay} from './MediaTimeDisplay';
import {MediaVolumeControl} from './MediaVolumeControl';
import styles from './VideoPlayer.module.css';

export interface VideoPlayerProps {
	src: string;
	poster?: string;
	placeholder?: string;
	duration?: number;
	width?: number;
	height?: number;
	autoPlay?: boolean;
	loop?: boolean;
	fillContainer?: boolean;
	isMobile?: boolean;
	onInitialPlay?: () => void;
	onEnded?: () => void;
	className?: string;
	style?: React.CSSProperties;
}

export const VideoPlayer = observer(function VideoPlayer({
	src,
	poster,
	placeholder,
	duration: initialDuration,
	width,
	height,
	autoPlay = false,
	loop = false,
	fillContainer = false,
	isMobile = false,
	onInitialPlay,
	onEnded,
	className,
	style,
}: VideoPlayerProps) {
	const {t} = useLingui();
	const containerRef = useRef<HTMLDivElement>(null);
	const [hasPlayed, setHasPlayed] = useState(autoPlay);
	const [containerWidth, setContainerWidth] = useState(width || VIDEO_BREAKPOINTS.LARGE + 1);
	const [isInteracting, setIsInteracting] = useState(false);
	const [showPlayPauseIndicator, setShowPlayPauseIndicator] = useState<'play' | 'pause' | null>(null);
	const prevPlayingRef = useRef<boolean | null>(null);

	const [posterLoaded, setPosterLoaded] = useState(poster ? ImageCacheUtils.hasImage(poster) : false);

	const thumbHashURL = useMemo(() => {
		if (!placeholder) return undefined;
		try {
			const bytes = Uint8Array.from(atob(placeholder), (c) => c.charCodeAt(0));
			return thumbHashToDataURL(bytes);
		} catch {
			return undefined;
		}
	}, [placeholder]);

	useEffect(() => {
		if (!poster) return;
		if (ImageCacheUtils.hasImage(poster)) {
			setPosterLoaded(true);
			return;
		}
		ImageCacheUtils.loadImage(
			poster,
			() => setPosterLoaded(true),
			() => setPosterLoaded(false),
		);
	}, [poster]);

	const [volume, setVolumeState] = useState(VideoVolumeStore.volume);
	const [isMuted, setIsMutedState] = useState(VideoVolumeStore.isMuted);

	const {mediaRef, state, play, toggle, seekRelative, setPlaybackRate} = useMediaPlayer({
		autoPlay,
		loop,
		persistVolume: false,
		persistPlaybackRate: true,
		onEnded,
	});

	const {currentTime, duration, progress, buffered, seekToPercentage, startSeeking, endSeeking} = useMediaProgress({
		mediaRef,
		initialDuration,
	});

	useEffect(() => {
		const media = mediaRef.current;
		if (!media) return;
		media.volume = volume;
		media.muted = isMuted;
	}, [mediaRef, volume, isMuted]);

	const handleVolumeChange = useCallback(
		(newVolume: number) => {
			const clamped = Math.max(0, Math.min(1, newVolume));
			setVolumeState(clamped);
			VideoVolumeStore.setVolume(clamped);
			if (isMuted && clamped > 0) {
				setIsMutedState(false);
			}
		},
		[isMuted],
	);

	const handleToggleMute = useCallback(() => {
		setIsMutedState((prev) => !prev);
		VideoVolumeStore.toggleMute();
	}, []);

	const {isFullscreen, supportsFullscreen, toggleFullscreen} = useMediaFullscreen({
		containerRef,
		videoRef: mediaRef as React.RefObject<HTMLVideoElement | null>,
	});

	const {isPiP, supportsPiP, togglePiP} = useMediaPiP({
		videoRef: mediaRef as React.RefObject<HTMLVideoElement | null>,
	});

	const {controlsVisible, showControls, containerProps} = useControlsVisibility({
		isPlaying: state.isPlaying,
		isInteracting,
	});

	useEffect(() => {
		if (prevPlayingRef.current !== null && prevPlayingRef.current !== state.isPlaying && hasPlayed) {
			setShowPlayPauseIndicator(state.isPlaying ? 'play' : 'pause');
			const timer = setTimeout(() => setShowPlayPauseIndicator(null), 500);
			prevPlayingRef.current = state.isPlaying;
			return () => clearTimeout(timer);
		}
		prevPlayingRef.current = state.isPlaying;
		return undefined;
	}, [state.isPlaying, hasPlayed]);

	useMediaKeyboard({
		containerRef,
		enabled: true,
		onTogglePlay: toggle,
		onSeekBackward: (amount) => seekRelative(-amount),
		onSeekForward: (amount) => seekRelative(amount),
		onVolumeUp: () => handleVolumeChange(volume + 0.1),
		onVolumeDown: () => handleVolumeChange(volume - 0.1),
		onToggleMute: handleToggleMute,
		onToggleFullscreen: toggleFullscreen,
		onSeekPercentage: seekToPercentage,
	});

	const handleResize = useCallback(() => {
		if (containerRef.current) {
			setContainerWidth(containerRef.current.offsetWidth);
		}
	}, []);

	useEffect(() => {
		handleResize();
		window.addEventListener('resize', handleResize);
		return () => window.removeEventListener('resize', handleResize);
	}, [handleResize]);

	const hasAutoPlayedRef = useRef(autoPlay);
	useEffect(() => {
		if (hasPlayed && !hasAutoPlayedRef.current) {
			hasAutoPlayedRef.current = true;
			const timer = setTimeout(() => {
				play();
			}, 0);
			return () => clearTimeout(timer);
		}
		return undefined;
	}, [hasPlayed, play]);

	const handlePosterClick = useCallback(() => {
		setHasPlayed(true);
		onInitialPlay?.();
	}, [onInitialPlay]);

	const handleSeek = useCallback(
		(percentage: number) => {
			seekToPercentage(percentage);
		},
		[seekToPercentage],
	);

	const handleSeekStart = useCallback(() => {
		setIsInteracting(true);
		startSeeking();
	}, [startSeeking]);

	const handleSeekEnd = useCallback(() => {
		setIsInteracting(false);
		endSeeking();
	}, [endSeeking]);

	const handleVideoClick = useCallback(() => {
		if (isMobile) {
			showControls();
		} else {
			toggle();
		}
	}, [isMobile, showControls, toggle]);

	const isSmall = containerWidth < VIDEO_BREAKPOINTS.SMALL;
	const isMedium = containerWidth < VIDEO_BREAKPOINTS.MEDIUM;

	const containerStyle: React.CSSProperties = {
		...style,
		...(width && height && !fillContainer ? {aspectRatio: `${width} / ${height}`} : {}),
	};

	return (
		<div
			ref={containerRef}
			className={clsx(
				styles.container,
				fillContainer && styles.fillContainer,
				isFullscreen && styles.fullscreen,
				className,
			)}
			style={containerStyle}
			{...containerProps}
		>
			{/* biome-ignore lint/a11y/useMediaCaption: Video player doesn't require captions for now */}
			<video
				ref={mediaRef as React.RefObject<HTMLVideoElement>}
				className={clsx(styles.video, !hasPlayed && styles.videoHidden)}
				src={hasPlayed ? src : undefined}
				preload="none"
				playsInline
				onClick={handleVideoClick}
				aria-label={t`Video`}
			/>

			{!hasPlayed && !autoPlay && (
				<div
					className={styles.posterOverlay}
					onClick={handlePosterClick}
					onKeyDown={(e) => (e.key === 'Enter' || e.key === ' ') && handlePosterClick()}
					role="button"
					tabIndex={0}
				>
					<AnimatePresence>
						{thumbHashURL && !posterLoaded && (
							<motion.img
								key="thumbhash"
								initial={{opacity: 1}}
								exit={{opacity: 0}}
								transition={{duration: 0.2}}
								src={thumbHashURL}
								alt=""
								className={styles.thumbHashPlaceholder}
							/>
						)}
					</AnimatePresence>

					{poster && posterLoaded && <img src={poster} alt="" className={styles.posterImage} />}

					<button type="button" className={styles.playOverlayButton} aria-label={t`Play video`}>
						<PlayIcon size={24} weight="fill" />
					</button>
				</div>
			)}

			{state.isBuffering && hasPlayed && (
				<div className={styles.loadingOverlay}>
					<CircleNotchIcon size={48} weight="bold" className={styles.spinner} color="#fff" />
				</div>
			)}

			<AnimatePresence>
				{showPlayPauseIndicator && (
					<motion.div
						className={styles.playPauseIndicator}
						initial={{opacity: 0, scale: 0.5, x: '-50%', y: '-50%'}}
						animate={{opacity: 1, scale: 1, x: '-50%', y: '-50%'}}
						exit={{opacity: 0, scale: 1.2, x: '-50%', y: '-50%'}}
						transition={{duration: 0.2}}
					>
						{showPlayPauseIndicator === 'play' ? (
							<PlayIcon size={24} weight="fill" />
						) : (
							<PauseIcon size={24} weight="fill" />
						)}
					</motion.div>
				)}
			</AnimatePresence>

			<AnimatePresence>
				{(hasPlayed || autoPlay) && (
					<motion.div
						className={styles.controlsOverlay}
						initial={{y: '100%'}}
						animate={{y: controlsVisible ? 0 : '100%'}}
						exit={{y: '100%'}}
						transition={{duration: 0.2, ease: 'easeOut'}}
					>
						<MediaProgressBar
							progress={progress}
							buffered={buffered}
							currentTime={currentTime}
							duration={duration}
							onSeek={handleSeek}
							onSeekStart={handleSeekStart}
							onSeekEnd={handleSeekEnd}
							className={styles.progressBar}
							compact
						/>

						<div className={styles.controlsRow}>
							<div className={styles.controlsLeft}>
								<MediaPlayButton isPlaying={state.isPlaying} onToggle={toggle} size="small" />
								{!isSmall && (
									<MediaVolumeControl
										volume={volume}
										isMuted={isMuted}
										onVolumeChange={handleVolumeChange}
										onToggleMute={handleToggleMute}
										expandable
										compact
									/>
								)}
								{!isSmall && <MediaTimeDisplay currentTime={currentTime} duration={duration} size="small" />}
							</div>

							<div className={styles.controlsCenter} />

							<div className={styles.controlsRight}>
								{!isMedium && (
									<MediaPlaybackRate
										rate={state.playbackRate}
										onRateChange={setPlaybackRate}
										rates={VIDEO_PLAYBACK_RATES}
										size="small"
									/>
								)}
								{!isMedium && supportsPiP && (
									<MediaPipButton
										isPiP={isPiP}
										supportsPiP={supportsPiP}
										onToggle={togglePiP}
										iconSize={18}
										size="small"
									/>
								)}
								{supportsFullscreen && (
									<MediaFullscreenButton
										isFullscreen={isFullscreen}
										supportsFullscreen={supportsFullscreen}
										onToggle={toggleFullscreen}
										iconSize={18}
										size="small"
									/>
								)}
							</div>
						</div>
					</motion.div>
				)}
			</AnimatePresence>
		</div>
	);
});
