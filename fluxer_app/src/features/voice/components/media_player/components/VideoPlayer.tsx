// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {decodeThumbHashDataURL} from '@app/features/messaging/utils/ThumbHashUtils';
import {observeResize} from '@app/features/platform/utils/SharedResizeObserver';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {PortalHostContext} from '@app/features/ui/overlay/PortalHostContext';
import {MediaFullscreenButton} from '@app/features/voice/components/media_player/components/MediaFullscreenButton';
import {MediaPipButton} from '@app/features/voice/components/media_player/components/MediaPipButton';
import {MediaPlayButton} from '@app/features/voice/components/media_player/components/MediaPlayButton';
import {MediaPlaybackRate} from '@app/features/voice/components/media_player/components/MediaPlaybackRate';
import {MediaProgressBar} from '@app/features/voice/components/media_player/components/MediaProgressBar';
import {MediaTimeDisplay} from '@app/features/voice/components/media_player/components/MediaTimeDisplay';
import {MediaVerticalVolumeControl} from '@app/features/voice/components/media_player/components/MediaVerticalVolumeControl';
import {
	CONTROLS_AUTOHIDE_DELAY_MS,
	CONTROLS_AUTOHIDE_FULLSCREEN_DELAY_MS,
	useControlsVisibility,
} from '@app/features/voice/components/media_player/hooks/useControlsVisibility';
import {useHiddenPlaybackGuard} from '@app/features/voice/components/media_player/hooks/useHiddenPlaybackGuard';
import {useMediaFullscreen} from '@app/features/voice/components/media_player/hooks/useMediaFullscreen';
import {useMediaKeyboard} from '@app/features/voice/components/media_player/hooks/useMediaKeyboard';
import {useMediaPiP} from '@app/features/voice/components/media_player/hooks/useMediaPiP';
import {useMediaPlayer} from '@app/features/voice/components/media_player/hooks/useMediaPlayer';
import {useMediaProgress} from '@app/features/voice/components/media_player/hooks/useMediaProgress';
import {mediaGainFromVolumeSetting} from '@app/features/voice/components/media_player/hooks/useMediaVolume';
import {useVideoDecodability} from '@app/features/voice/components/media_player/hooks/useVideoDecodability';
import {
	SEEK_STEP,
	VIDEO_BREAKPOINTS,
	VIDEO_PLAYBACK_RATES,
	VOLUME_STEP,
} from '@app/features/voice/components/media_player/utils/MediaConstants';
import {
	armPendingSeekTarget,
	clampMediaTime,
	detachMediaElementSource,
} from '@app/features/voice/components/media_player/utils/MediaSeekUtils';
import styles from '@app/features/voice/components/media_player/VideoPlayer.module.css';
import {
	createVideoPlayerRenderSnapshot,
	selectVideoPlayerPlayPauseIndicator,
	selectVideoPlayerRenderModel,
	transitionVideoPlayerRenderSnapshot,
} from '@app/features/voice/components/media_player/VideoPlayerRenderStateMachine';
import VideoVolume from '@app/features/voice/state/VideoVolume';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PauseIcon, PlayIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useContext, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

const VIDEO_DESCRIPTOR = msg({
	message: 'Video',
	comment: 'Aria label on the inline video player container.',
});
const PLAY_VIDEO_DESCRIPTOR = msg({
	message: 'Play video',
	comment: 'Play-button label on the inline video player thumbnail before playback starts.',
});
const VIDEO_PLAYER_KEYSHORTCUTS = [
	'Space',
	'K',
	'J',
	'L',
	'ArrowLeft',
	'ArrowRight',
	'ArrowUp',
	'ArrowDown',
	'M',
	'F',
	'Home',
	'End',
	'0',
	'1',
	'2',
	'3',
	'4',
	'5',
	'6',
	'7',
	'8',
	'9',
].join(' ');

export {detachMediaElementSource} from '@app/features/voice/components/media_player/utils/MediaSeekUtils';

function useMediaSourceRelease(mediaRef: React.RefObject<HTMLMediaElement | null>): void {
	useLayoutEffect(() => {
		const media = mediaRef.current;
		return () => {
			detachMediaElementSource(media ?? mediaRef.current);
		};
	}, [mediaRef]);
}

const SEEK_PREVIEW_SEEK_EPSILON_SECONDS = 0.05;
const SEEK_PREVIEW_THUMBNAIL_MIN_WIDTH = 88;
const SEEK_PREVIEW_THUMBNAIL_MAX_WIDTH = 144;
const SEEK_PREVIEW_THUMBNAIL_WIDTH_RATIO = 0.48;

interface VideoPlayerProps {
	src: string;
	poster?: string;
	placeholder?: string;
	duration?: number;
	initialTime?: number;
	width?: number;
	height?: number;
	autoPlay?: boolean;
	loop?: boolean;
	fillContainer?: boolean;
	isMobile?: boolean;
	onInitialPlay?: () => void;
	onLoadedMetadata?: (metadata: VideoPlayerMetadata) => void;
	onEnded?: () => void;
	className?: string;
	style?: React.CSSProperties;
}

export interface VideoPlayerMetadata {
	width: number;
	height: number;
	duration: number;
}

interface VideoSeekPreviewThumbnailProps {
	src: string;
	poster?: string;
	time: number;
}

function VideoSeekPreviewThumbnail({src, poster, time}: VideoSeekPreviewThumbnailProps) {
	const videoRef = useRef<HTMLVideoElement>(null);
	const seekPreviewVideo = useCallback((targetTime: number) => {
		const video = videoRef.current;
		if (!video || video.readyState < 1) return;
		const nextTime = clampMediaTime(targetTime, video.duration);
		if (Math.abs(video.currentTime - nextTime) <= SEEK_PREVIEW_SEEK_EPSILON_SECONDS) return;
		video.currentTime = nextTime;
	}, []);
	useMediaSourceRelease(videoRef);
	useEffect(() => {
		seekPreviewVideo(time);
	}, [seekPreviewVideo, time]);
	const handleLoadedMetadata = useCallback(() => {
		seekPreviewVideo(time);
	}, [seekPreviewVideo, time]);
	return (
		<video
			ref={videoRef}
			className={styles.seekPreviewVideo}
			src={src}
			poster={poster}
			preload="metadata"
			muted
			playsInline
			disablePictureInPicture
			onLoadedMetadata={handleLoadedMetadata}
			data-flx="voice.media-player.video-player.seek-preview-video"
		>
			<track kind="captions" data-flx="voice.media-player.video-player.seek-preview-track" />
		</video>
	);
}

export const VideoPlayer = observer(function VideoPlayer({
	src,
	poster,
	placeholder,
	duration: initialDuration,
	initialTime,
	width,
	height,
	autoPlay = false,
	loop = false,
	fillContainer = false,
	isMobile = false,
	onInitialPlay,
	onLoadedMetadata,
	onEnded,
	className,
	style,
}: VideoPlayerProps) {
	const {i18n} = useLingui();
	const videoLabel = i18n._(VIDEO_DESCRIPTOR);
	const showSeekPreviewThumbnail = Accessibility.showVideoSeekPreviewThumbnails;
	const containerRef = useRef<HTMLDivElement>(null);
	const [hasPlayed, setHasPlayed] = useState(autoPlay);
	const [containerWidth, setContainerWidth] = useState(width || VIDEO_BREAKPOINTS.LARGE + 1);
	const [isInteracting, setIsInteracting] = useState(false);
	const [showPlayPauseIndicator, setShowPlayPauseIndicator] = useState<'play' | 'pause' | null>(null);
	const renderSnapshotRef = useRef(createVideoPlayerRenderSnapshot());
	const restoreFocusAfterFullscreenRef = useRef<HTMLElement | null>(null);
	const [isSeekHoldActive, setIsSeekHoldActive] = useState(false);
	const hasAppliedInitialTimeRef = useRef(false);
	const [wantsMetadata, setWantsMetadata] = useState(false);
	const [fullscreenPortalRoot, setFullscreenPortalRoot] = useState<HTMLElement | null>(null);
	const doubleClickPlaybackWasPausedRef = useRef<boolean | null>(null);
	const [loadedPosterSrc, setLoadedPosterSrc] = useState<string | null>(() => {
		if (poster && ImageCacheUtils.hasImage(poster)) return poster;
		return null;
	});
	const posterLoaded = poster != null && loadedPosterSrc === poster;
	const thumbHashURL = useMemo(() => {
		return decodeThumbHashDataURL(placeholder);
	}, [placeholder]);
	useEffect(() => {
		if (!poster) {
			return;
		}
		if (loadedPosterSrc === poster) return;
		if (ImageCacheUtils.hasImage(poster)) {
			setLoadedPosterSrc(poster);
			return;
		}
		let active = true;
		const cleanup = ImageCacheUtils.loadImage(
			poster,
			() => {
				if (active) setLoadedPosterSrc(poster);
			},
			() => {
				if (active) setLoadedPosterSrc((currentSource) => (currentSource === poster ? null : currentSource));
			},
		);
		return () => {
			active = false;
			cleanup();
		};
	}, [loadedPosterSrc, poster]);
	const [draftVolume, setDraftVolume] = useState<number | null>(null);
	const uncommittedVolumeRef = useRef<number | null>(null);
	const volume = draftVolume ?? VideoVolume.volume;
	const isMuted = draftVolume !== null ? draftVolume === 0 : VideoVolume.isMuted;
	const {mediaRef, state, play, pause, resumeHold, releaseHold, toggle, seekRelative, setPlaybackRate} = useMediaPlayer(
		{
			mediaKind: 'video',
			autoPlay,
			loop,
			persistPlaybackRate: true,
			onLoadedMetadata: (duration, media) => {
				if (initialTime !== undefined && !hasAppliedInitialTimeRef.current) {
					hasAppliedInitialTimeRef.current = true;
					const resumeTime = clampMediaTime(initialTime, duration);
					armPendingSeekTarget(media, resumeTime);
					media.currentTime = resumeTime;
				}
				if (!(media instanceof HTMLVideoElement)) return;
				const {videoWidth, videoHeight} = media;
				if (videoWidth <= 0 || videoHeight <= 0) return;
				onLoadedMetadata?.({
					width: videoWidth,
					height: videoHeight,
					duration: Number.isFinite(duration) ? duration : 0,
				});
			},
			onPlay: () => setIsSeekHoldActive(false),
			onEnded,
		},
	);
	useMediaSourceRelease(mediaRef);
	const transportIsPlaying = state.isPlaying || isSeekHoldActive;
	const holdPlaybackWhileDocumentHidden = useCallback(() => {
		pause('hidden-document');
	}, [pause]);
	const releasePlaybackWhenDocumentVisible = useCallback(() => {
		resumeHold('hidden-document');
	}, [resumeHold]);
	useHiddenPlaybackGuard({
		videoRef: mediaRef as React.RefObject<HTMLVideoElement | null>,
		onHoldPlayback: holdPlaybackWhileDocumentHidden,
		onReleasePlayback: releasePlaybackWhenDocumentVisible,
	});
	const {currentTime, duration, progress, buffered, seekToPercentage, startSeeking, endSeeking} = useMediaProgress({
		mediaRef,
		initialDuration,
	});
	useVideoDecodability({
		videoRef: mediaRef as React.RefObject<HTMLVideoElement | null>,
		isPlaying: state.isPlaying,
		src,
	});
	const [isSeekPreviewActive, setIsSeekPreviewActive] = useState(false);
	const [seekPreviewPercentage, setSeekPreviewPercentage] = useState<number | null>(null);
	const seekPreviewTime =
		seekPreviewPercentage !== null && duration > 0 ? (seekPreviewPercentage / 100) * duration : null;
	useEffect(() => {
		const media = mediaRef.current;
		if (!media) return;
		media.volume = mediaGainFromVolumeSetting(volume);
		media.muted = isMuted;
	}, [mediaRef, volume, isMuted]);
	const commitVolume = useCallback(() => {
		const uncommittedVolume = uncommittedVolumeRef.current;
		uncommittedVolumeRef.current = null;
		if (uncommittedVolume === null) return;
		VideoVolume.setVolume(uncommittedVolume);
		VideoVolume.setMuted(uncommittedVolume === 0);
		setDraftVolume(null);
	}, []);
	const handleVolumeChange = useCallback((newVolume: number) => {
		const clamped = Math.max(0, Math.min(1, newVolume));
		uncommittedVolumeRef.current = clamped;
		setDraftVolume(clamped);
	}, []);
	useEffect(() => {
		if (draftVolume === null) return;
		window.addEventListener('pointerup', commitVolume);
		window.addEventListener('pointercancel', commitVolume);
		window.addEventListener('keyup', commitVolume);
		window.addEventListener('blur', commitVolume);
		return () => {
			window.removeEventListener('pointerup', commitVolume);
			window.removeEventListener('pointercancel', commitVolume);
			window.removeEventListener('keyup', commitVolume);
			window.removeEventListener('blur', commitVolume);
		};
	}, [commitVolume, draftVolume]);
	useEffect(() => {
		return () => {
			commitVolume();
		};
	}, [commitVolume]);
	const handleToggleMute = useCallback(() => {
		uncommittedVolumeRef.current = null;
		setDraftVolume(null);
		VideoVolume.toggleMute();
	}, []);
	const {isFullscreen, supportsFullscreen, toggleFullscreen} = useMediaFullscreen({
		containerRef,
		videoRef: mediaRef as React.RefObject<HTMLVideoElement | null>,
	});
	const {isPiP, supportsPiP, togglePiP} = useMediaPiP({
		videoRef: mediaRef as React.RefObject<HTMLVideoElement | null>,
	});
	const {controlsVisible, showControls, containerProps} = useControlsVisibility({
		autohideDelay: isFullscreen ? CONTROLS_AUTOHIDE_FULLSCREEN_DELAY_MS : CONTROLS_AUTOHIDE_DELAY_MS,
		isPlaying: state.isPlaying,
		isInteracting,
	});
	const renderModel = useMemo(
		() =>
			selectVideoPlayerRenderModel({
				autoPlay,
				hasPlayed,
				wantsMetadata,
				isPlaying: state.isPlaying,
				isPaused: state.isPaused,
				isEnded: state.isEnded,
				hasError: state.error !== null,
			}),
		[autoPlay, hasPlayed, wantsMetadata, state.error, state.isEnded, state.isPaused, state.isPlaying],
	);
	useEffect(() => {
		const snapshot = transitionVideoPlayerRenderSnapshot(renderSnapshotRef.current, {
			type: 'video.observePlayback',
			signals: {
				hasPlayed,
				isPlaying: transportIsPlaying,
			},
		});
		renderSnapshotRef.current = snapshot;
		const indicator = selectVideoPlayerPlayPauseIndicator(snapshot);
		if (document.visibilityState === 'hidden') {
			return undefined;
		}
		if (indicator) {
			setShowPlayPauseIndicator(indicator);
			const timer = setTimeout(() => setShowPlayPauseIndicator(null), 500);
			return () => clearTimeout(timer);
		}
		return undefined;
	}, [transportIsPlaying, hasPlayed]);
	useMediaKeyboard({
		containerRef,
		enabled: true,
		captureDocumentKeys: isFullscreen,
		onTogglePlay: toggle,
		onSeekBackward: () => seekRelative(-SEEK_STEP),
		onSeekForward: () => seekRelative(SEEK_STEP),
		onVolumeUp: () => {
			handleVolumeChange(volume + VOLUME_STEP);
			commitVolume();
		},
		onVolumeDown: () => {
			handleVolumeChange(volume - VOLUME_STEP);
			commitVolume();
		},
		onToggleMute: handleToggleMute,
		onToggleFullscreen: toggleFullscreen,
		onSeekPercentage: seekToPercentage,
	});
	const inheritedPortalHost = useContext(PortalHostContext);
	useLayoutEffect(() => {
		setFullscreenPortalRoot(isFullscreen ? containerRef.current : null);
	}, [isFullscreen]);
	useEffect(() => {
		if (!isFullscreen) return;
		const container = containerRef.current;
		if (!container) return;
		const activeElement = document.activeElement;
		restoreFocusAfterFullscreenRef.current = activeElement instanceof HTMLElement ? activeElement : null;
		const focusTimer = window.setTimeout(() => {
			container.focus({preventScroll: true});
		}, 0);
		return () => {
			window.clearTimeout(focusTimer);
			const restoreTarget = restoreFocusAfterFullscreenRef.current;
			restoreFocusAfterFullscreenRef.current = null;
			if (restoreTarget && document.contains(restoreTarget)) {
				restoreTarget.focus({preventScroll: true});
			}
		};
	}, [isFullscreen]);
	const updateContainerWidth = useCallback((width: number) => {
		setContainerWidth((previousWidth) => (previousWidth === width ? previousWidth : width));
	}, []);
	const handleResize = useCallback(() => {
		const container = containerRef.current;
		if (!container) return;
		updateContainerWidth(container.offsetWidth);
	}, [updateContainerWidth]);
	const handleObservedResize = useCallback(
		(entry: ResizeObserverEntry) => {
			updateContainerWidth(entry.contentRect.width);
		},
		[updateContainerWidth],
	);
	useEffect(() => {
		const container = containerRef.current;
		if (!container) return;
		handleResize();
		const supportsResizeObserver = typeof ResizeObserver !== 'undefined';
		const unobserve = supportsResizeObserver ? observeResize(container, handleObservedResize) : undefined;
		if (!supportsResizeObserver) {
			window.addEventListener('resize', handleResize);
		}
		return () => {
			if (!supportsResizeObserver) {
				window.removeEventListener('resize', handleResize);
			}
			unobserve?.();
		};
	}, [handleResize, handleObservedResize]);
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
		containerRef.current?.focus({preventScroll: true});
		setHasPlayed(true);
		onInitialPlay?.();
	}, [onInitialPlay]);
	const handleSeek = useCallback(
		(percentage: number) => {
			seekToPercentage(percentage);
		},
		[seekToPercentage],
	);
	const handleSeekPreview = useCallback(
		(percentage: number) => {
			setSeekPreviewPercentage(showSeekPreviewThumbnail ? percentage : null);
			seekToPercentage(percentage);
		},
		[seekToPercentage, showSeekPreviewThumbnail],
	);
	const handleSeekStart = useCallback(() => {
		const media = mediaRef.current;
		const wasPlaying = media != null && !media.paused && !media.ended;
		if (wasPlaying) {
			setIsSeekHoldActive(true);
			pause('seek');
		}
		setIsInteracting(true);
		setIsSeekPreviewActive(showSeekPreviewThumbnail);
		startSeeking();
	}, [mediaRef, pause, showSeekPreviewThumbnail, startSeeking]);
	const handleSeekEnd = useCallback(() => {
		setIsInteracting(false);
		setIsSeekPreviewActive(false);
		setSeekPreviewPercentage(null);
		endSeeking();
		const media = mediaRef.current;
		const mediaDuration = media?.duration ?? 0;
		const isAtEnd =
			media != null && Number.isFinite(mediaDuration) && mediaDuration > 0 && media.currentTime >= mediaDuration;
		if (isAtEnd) {
			releaseHold('seek');
			setIsSeekHoldActive(false);
			return;
		}
		if (!resumeHold('seek')) {
			setIsSeekHoldActive(false);
		}
	}, [endSeeking, mediaRef, releaseHold, resumeHold]);
	const handleSeekHover = useCallback(
		(percentage: number) => {
			if (!showSeekPreviewThumbnail) return;
			setSeekPreviewPercentage(percentage);
		},
		[showSeekPreviewThumbnail],
	);
	const handleSeekHoverEnd = useCallback(() => {
		if (!isSeekPreviewActive) {
			setSeekPreviewPercentage(null);
		}
	}, [isSeekPreviewActive]);
	const handleVideoClick = useCallback(
		(e: React.MouseEvent<HTMLVideoElement>) => {
			containerRef.current?.focus({preventScroll: true});
			if (isMobile) {
				showControls();
				return;
			}
			if (e.detail > 1) {
				e.preventDefault();
				e.stopPropagation();
				return;
			}
			doubleClickPlaybackWasPausedRef.current = mediaRef.current?.paused ?? state.isPaused;
			toggle();
		},
		[isMobile, mediaRef, showControls, state.isPaused, toggle],
	);
	const restoreDoubleClickPlaybackState = useCallback(() => {
		const wasPaused = doubleClickPlaybackWasPausedRef.current;
		doubleClickPlaybackWasPausedRef.current = null;
		const media = mediaRef.current;
		if (wasPaused === null || !media) return;
		if (wasPaused && !media.paused) {
			media.pause();
		} else if (!wasPaused && media.paused) {
			void play();
		}
	}, [mediaRef, play]);
	const handleVideoDoubleClick = useCallback(
		(e: React.MouseEvent<HTMLVideoElement>) => {
			if (isMobile || !supportsFullscreen) return;
			e.preventDefault();
			e.stopPropagation();
			restoreDoubleClickPlaybackState();
			containerRef.current?.focus({preventScroll: true});
			void toggleFullscreen();
		},
		[isMobile, restoreDoubleClickPlaybackState, supportsFullscreen, toggleFullscreen],
	);
	const escalateToMetadata = useCallback(() => {
		setWantsMetadata(true);
	}, []);
	const handleFocusCapture = useCallback(() => {
		escalateToMetadata();
		showControls();
	}, [escalateToMetadata, showControls]);
	const isSmall = containerWidth < VIDEO_BREAKPOINTS.SMALL;
	const isMedium = containerWidth < VIDEO_BREAKPOINTS.MEDIUM;
	const seekPreviewThumbnailWidth = Math.round(
		Math.max(
			SEEK_PREVIEW_THUMBNAIL_MIN_WIDTH,
			Math.min(SEEK_PREVIEW_THUMBNAIL_MAX_WIDTH, containerWidth * SEEK_PREVIEW_THUMBNAIL_WIDTH_RATIO),
		),
	);
	const progressPreviewPortalRoot = fullscreenPortalRoot;
	const containerStyle: React.CSSProperties = {
		...style,
		...(width && height && !fillContainer
			? {aspectRatio: `${width} / ${height}`}
			: !fillContainer
				? {aspectRatio: '16 / 9'}
				: {}),
	};
	return (
		<PortalHostContext.Provider value={fullscreenPortalRoot ?? inheritedPortalHost}>
			<FocusRing offset={-2} data-flx="voice.media-player.video-player.focus-ring">
				<div
					ref={containerRef}
					className={clsx(
						styles.container,
						fillContainer && styles.fillContainer,
						isFullscreen && styles.fullscreen,
						className,
					)}
					style={containerStyle}
					role="group"
					aria-label={videoLabel}
					aria-keyshortcuts={VIDEO_PLAYER_KEYSHORTCUTS}
					tabIndex={-1}
					data-media-fullscreen-root="true"
					onFocusCapture={handleFocusCapture}
					onPointerEnter={escalateToMetadata}
					data-flx="voice.media-player.video-player.container"
					{...containerProps}
				>
					<video
						ref={mediaRef as React.RefObject<HTMLVideoElement>}
						className={clsx(styles.video, renderModel.shouldHideVideo && styles.videoHidden)}
						src={renderModel.shouldAttachSource ? src : undefined}
						preload={renderModel.preloadAttribute}
						crossOrigin="anonymous"
						playsInline
						data-embed-media="true"
						onClick={handleVideoClick}
						onDoubleClick={handleVideoDoubleClick}
						data-flx="voice.media-player.video-player.video"
					>
						<track kind="captions" data-flx="voice.media-player.video-player.track" />
					</video>
					{renderModel.shouldShowPosterOverlay && (
						<FocusRing offset={-2} data-flx="voice.media-player.video-player.focus-ring--2">
							<button
								type="button"
								className={styles.posterOverlay}
								onClick={handlePosterClick}
								aria-label={i18n._(PLAY_VIDEO_DESCRIPTOR)}
								data-flx="voice.media-player.video-player.poster-overlay.poster-click.button"
							>
								<AnimatePresence data-flx="voice.media-player.video-player.animate-presence">
									{thumbHashURL && !posterLoaded && (
										<motion.img
											key="thumbhash"
											initial={{opacity: 1}}
											exit={{opacity: Accessibility.useReducedMotion ? 1 : 0}}
											transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2}}
											src={thumbHashURL}
											alt=""
											aria-hidden={true}
											className={styles.thumbHashPlaceholder}
											data-flx="voice.media-player.video-player.thumb-hash-placeholder"
										/>
									)}
								</AnimatePresence>
								{poster && posterLoaded && (
									<img
										src={poster}
										alt=""
										className={styles.posterImage}
										data-flx="voice.media-player.video-player.poster-image"
									/>
								)}
								<span
									className={styles.playOverlayButton}
									aria-hidden="true"
									data-flx="voice.media-player.video-player.play-overlay-button"
								>
									<PlayIcon size={remFromPx(24)} weight="fill" data-flx="voice.media-player.video-player.play-icon" />
								</span>
							</button>
						</FocusRing>
					)}
					<AnimatePresence data-flx="voice.media-player.video-player.animate-presence--2">
						{showPlayPauseIndicator && (
							<motion.div
								className={styles.playPauseIndicator}
								initial={
									Accessibility.useReducedMotion
										? {opacity: 1, scale: 1, x: '-50%', y: '-50%'}
										: {opacity: 0, scale: 0.5, x: '-50%', y: '-50%'}
								}
								animate={{opacity: 1, scale: 1, x: '-50%', y: '-50%'}}
								exit={
									Accessibility.useReducedMotion
										? {opacity: 1, scale: 1, x: '-50%', y: '-50%'}
										: {opacity: 0, scale: 1.2, x: '-50%', y: '-50%'}
								}
								transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2}}
								data-flx="voice.media-player.video-player.play-pause-indicator"
							>
								{showPlayPauseIndicator === 'play' ? (
									<PlayIcon
										size={remFromPx(24)}
										weight="fill"
										data-flx="voice.media-player.video-player.play-icon--2"
									/>
								) : (
									<PauseIcon size={remFromPx(24)} weight="fill" data-flx="voice.media-player.video-player.pause-icon" />
								)}
							</motion.div>
						)}
					</AnimatePresence>
					<AnimatePresence data-flx="voice.media-player.video-player.animate-presence--3">
						{renderModel.shouldShowControlsOverlay && (
							<motion.div
								className={styles.controlsOverlay}
								initial={{y: Accessibility.useReducedMotion ? 0 : '100%'}}
								animate={{y: controlsVisible ? 0 : '100%'}}
								exit={{y: Accessibility.useReducedMotion ? 0 : '100%'}}
								transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2, ease: 'easeOut'}}
								data-flx="voice.media-player.video-player.controls-overlay"
							>
								<MediaProgressBar
									progress={progress}
									buffered={buffered}
									currentTime={currentTime}
									duration={duration}
									mediaRef={mediaRef}
									isPlaying={state.isPlaying}
									onSeek={handleSeek}
									onSeekPreview={handleSeekPreview}
									onSeekStart={handleSeekStart}
									onSeekEnd={handleSeekEnd}
									onSeekHover={handleSeekHover}
									onSeekHoverEnd={handleSeekHoverEnd}
									previewThumbnail={
										showSeekPreviewThumbnail && seekPreviewPercentage !== null && duration > 0 ? (
											<VideoSeekPreviewThumbnail
												src={src}
												poster={poster}
												time={seekPreviewTime ?? currentTime}
												data-flx="voice.media-player.video-player.video-seek-preview-thumbnail"
											/>
										) : null
									}
									previewThumbnailWidth={seekPreviewThumbnailWidth}
									previewPortalRoot={progressPreviewPortalRoot}
									className={styles.progressBar}
									compact
									data-flx="voice.media-player.video-player.progress-bar"
								/>
								<div className={styles.controlsRow} data-flx="voice.media-player.video-player.controls-row">
									<div className={styles.controlsLeft} data-flx="voice.media-player.video-player.controls-left">
										<MediaPlayButton
											isPlaying={transportIsPlaying}
											onToggle={toggle}
											size="small"
											data-flx="voice.media-player.video-player.media-play-button"
										/>
										<MediaVerticalVolumeControl
											volume={volume}
											isMuted={isMuted}
											onVolumeChange={handleVolumeChange}
											onToggleMute={handleToggleMute}
											iconSize={18}
											data-flx="voice.media-player.video-player.media-vertical-volume-control"
										/>
										{!isSmall && (
											<MediaTimeDisplay
												currentTime={currentTime}
												duration={duration}
												size="small"
												data-flx="voice.media-player.video-player.media-time-display"
											/>
										)}
									</div>
									<div className={styles.controlsCenter} data-flx="voice.media-player.video-player.controls-center" />
									<div className={styles.controlsRight} data-flx="voice.media-player.video-player.controls-right">
										{!isSmall && (
											<MediaPlaybackRate
												rate={state.playbackRate}
												onRateChange={setPlaybackRate}
												rates={VIDEO_PLAYBACK_RATES}
												size="small"
												data-flx="voice.media-player.video-player.media-playback-rate"
											/>
										)}
										{!isMedium && supportsPiP && (
											<MediaPipButton
												isPiP={isPiP}
												supportsPiP={supportsPiP}
												onToggle={togglePiP}
												iconSize={18}
												size="small"
												data-flx="voice.media-player.video-player.media-pip-button"
											/>
										)}
										{supportsFullscreen && (
											<MediaFullscreenButton
												isFullscreen={isFullscreen}
												supportsFullscreen={supportsFullscreen}
												onToggle={toggleFullscreen}
												iconSize={18}
												size="small"
												data-flx="voice.media-player.video-player.media-fullscreen-button"
											/>
										)}
									</div>
								</div>
							</motion.div>
						)}
					</AnimatePresence>
				</div>
			</FocusRing>
		</PortalHostContext.Provider>
	);
});
