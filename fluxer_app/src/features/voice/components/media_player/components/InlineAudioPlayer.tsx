// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {
	ADD_TO_FAVORITES_DESCRIPTOR,
	DOWNLOAD_DESCRIPTOR,
	PAUSE_DESCRIPTOR,
	PLAY_DESCRIPTOR,
	REMOVE_FROM_FAVORITES_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {MediaPlaybackRate} from '@app/features/voice/components/media_player/components/MediaPlaybackRate';
import {MediaProgressBar} from '@app/features/voice/components/media_player/components/MediaProgressBar';
import {MediaVolumeControl} from '@app/features/voice/components/media_player/components/MediaVolumeControl';
import {useMediaPlayer} from '@app/features/voice/components/media_player/hooks/useMediaPlayer';
import {useMediaProgress} from '@app/features/voice/components/media_player/hooks/useMediaProgress';
import {useMediaVolume} from '@app/features/voice/components/media_player/hooks/useMediaVolume';
import styles from '@app/features/voice/components/media_player/InlineAudioPlayer.module.css';
import {AUDIO_PLAYBACK_RATES} from '@app/features/voice/components/media_player/utils/MediaConstants';
import {formatDuration} from '@fluxer/date_utils/src/DateDuration';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {DownloadSimpleIcon, PauseIcon, PlayIcon, StarIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const AUDIO_PLAYER_DESCRIPTOR = msg({
	message: 'Audio player',
	comment: 'Aria label for the inline audio attachment player container.',
});
const AUDIO_PROGRESS_DESCRIPTOR = msg({
	message: 'Audio progress',
	comment: 'Aria label on the playback progress slider in the inline audio attachment player.',
});

interface InlineAudioPlayerProps {
	src: string;
	title?: string;
	fileSize?: number;
	duration?: number;
	extension?: string;
	isMobile?: boolean;
	isFavorited?: boolean;
	canFavorite?: boolean;
	onFavoriteClick?: (e: React.MouseEvent) => void;
	onDownloadClick?: (e: React.MouseEvent) => void;
	onContextMenu?: (e: React.MouseEvent) => void;
	className?: string;
}

function formatFileSize(bytes: number): string {
	if (bytes < 1024) return `${bytes} B`;
	if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
	return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
}

function formatTime(time: number): string {
	if (!Number.isFinite(time)) return '0:00';
	const minutes = Math.floor(time / 60);
	const seconds = Math.floor(time % 60);
	return `${minutes}:${seconds.toString().padStart(2, '0')}`;
}

function splitFilename(filename: string): {name: string; extension: string} {
	const lastDot = filename.lastIndexOf('.');
	if (lastDot === -1 || lastDot === 0) {
		return {name: filename, extension: ''};
	}
	return {
		name: filename.substring(0, lastDot),
		extension: filename.substring(lastDot),
	};
}

export function InlineAudioPlayer({
	src,
	title = 'Audio',
	fileSize,
	duration: initialDuration,
	extension,
	isMobile = false,
	isFavorited = false,
	canFavorite = false,
	onFavoriteClick,
	onDownloadClick,
	onContextMenu,
	className,
}: InlineAudioPlayerProps) {
	const {i18n} = useLingui();
	const trackRef = useRef<HTMLDivElement>(null);
	const [hasStarted, setHasStarted] = useState(false);
	const pendingPlayRef = useRef(false);
	const {mediaRef, state, play, toggle, setPlaybackRate} = useMediaPlayer({
		persistVolume: true,
	});
	const {
		currentTime,
		duration,
		progress,
		buffered,
		previewSeekToPercentage,
		seekToPercentage,
		startSeeking,
		endSeeking,
	} = useMediaProgress({
		mediaRef,
		initialDuration,
	});
	const {volume, isMuted, setVolume, toggleMute} = useMediaVolume({
		mediaRef,
	});
	const displayDuration = initialDuration ?? duration;
	const isLoading = hasStarted && state.isBuffering;
	const isActive = state.isPlaying || isLoading;
	const {name: fileName, extension: fileExtension} = extension
		? {name: title, extension: `.${extension}`}
		: splitFilename(title);
	const fileSizeString = fileSize ? formatFileSize(fileSize) : '';
	useEffect(() => {
		if (hasStarted && pendingPlayRef.current) {
			const timer = setTimeout(() => {
				if (pendingPlayRef.current) {
					pendingPlayRef.current = false;
					play();
				}
			}, 0);
			return () => clearTimeout(timer);
		}
		return undefined;
	}, [hasStarted, play]);
	const handlePlayToggle = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			if (!hasStarted) {
				pendingPlayRef.current = true;
				setHasStarted(true);
				return;
			}
			pendingPlayRef.current = false;
			toggle();
		},
		[hasStarted, toggle],
	);
	const handleProgressClick = useCallback(
		(e: React.MouseEvent<HTMLDivElement>) => {
			if (state.isBuffering) return;
			const rect = e.currentTarget.getBoundingClientRect();
			const x = e.clientX - rect.left;
			const percentage = (x / rect.width) * 100;
			seekToPercentage(Math.max(0, Math.min(100, percentage)));
		},
		[seekToPercentage, state.isBuffering],
	);
	const handleProgressKeyDown = useCallback(
		(e: React.KeyboardEvent<HTMLDivElement>) => {
			if (state.isBuffering) return;
			const step = 5;
			if (e.key === 'ArrowLeft') {
				e.preventDefault();
				seekToPercentage(Math.max(0, progress - step));
			} else if (e.key === 'ArrowRight') {
				e.preventDefault();
				seekToPercentage(Math.min(100, progress + step));
			}
		},
		[seekToPercentage, state.isBuffering, progress],
	);
	const handleSeek = useCallback(
		(percentage: number) => {
			seekToPercentage(percentage);
		},
		[seekToPercentage],
	);
	const handleSeekPreview = useCallback(
		(percentage: number) => {
			previewSeekToPercentage(percentage);
		},
		[previewSeekToPercentage],
	);
	if (isMobile) {
		return (
			<motion.div
				className={clsx(styles.mobileContainer, isActive && styles.mobileContainerActive, className)}
				onContextMenu={onContextMenu}
				role="group"
				aria-label={i18n._(AUDIO_PLAYER_DESCRIPTOR)}
				initial={false}
				animate={{
					backgroundColor: isActive ? 'var(--brand-primary)' : 'var(--background-secondary)',
				}}
				transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2, ease: 'easeOut'}}
				data-flx="voice.media-player.inline-audio-player.mobile-container.context-menu"
			>
				<audio
					ref={mediaRef as React.RefObject<HTMLAudioElement>}
					src={hasStarted ? src : undefined}
					preload="none"
					data-flx="voice.media-player.inline-audio-player.audio"
				>
					<track kind="captions" data-flx="voice.media-player.inline-audio-player.track" />
				</audio>
				<motion.button
					type="button"
					onClick={handlePlayToggle}
					className={styles.mobilePlayButton}
					aria-label={state.isPlaying ? i18n._(PAUSE_DESCRIPTOR) : i18n._(PLAY_DESCRIPTOR)}
					whileTap={Accessibility.useReducedMotion ? undefined : {y: 1}}
					initial={false}
					animate={{
						backgroundColor: isActive ? 'var(--text-on-brand-primary)' : 'var(--brand-primary)',
						color: isActive ? 'var(--brand-primary)' : 'var(--text-on-brand-primary)',
					}}
					transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2, ease: 'easeOut'}}
					data-flx="voice.media-player.inline-audio-player.mobile-play-button.play-toggle"
				>
					{isLoading && (
						<span className={styles.loadingSpinner} data-flx="voice.media-player.inline-audio-player.loading-spinner" />
					)}
					<AnimatePresence mode="wait" data-flx="voice.media-player.inline-audio-player.animate-presence">
						<motion.div
							key={state.isPlaying ? 'pause' : 'play'}
							className={styles.playButtonIcon}
							initial={Accessibility.useReducedMotion ? {scale: 1, opacity: 1} : {scale: 0.5, opacity: 0}}
							animate={{scale: 1, opacity: 1}}
							exit={Accessibility.useReducedMotion ? {scale: 1, opacity: 1} : {scale: 0.5, opacity: 0}}
							transition={{duration: Accessibility.useReducedMotion ? 0 : 0.1}}
							data-flx="voice.media-player.inline-audio-player.play-button-icon"
						>
							{state.isPlaying ? (
								<PauseIcon
									size={remFromPx(20)}
									weight="fill"
									data-flx="voice.media-player.inline-audio-player.pause-icon"
								/>
							) : (
								<PlayIcon
									size={remFromPx(20)}
									weight="fill"
									data-flx="voice.media-player.inline-audio-player.play-icon"
								/>
							)}
						</motion.div>
					</AnimatePresence>
				</motion.button>
				<div className={styles.mobileContent} data-flx="voice.media-player.inline-audio-player.mobile-content">
					<motion.div
						className={styles.mobileFileInfo}
						initial={false}
						animate={{
							color: isActive
								? 'color-mix(in srgb, var(--text-on-brand-primary) 90%, transparent)'
								: 'var(--text-primary)',
						}}
						transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2, ease: 'easeOut'}}
						data-flx="voice.media-player.inline-audio-player.mobile-file-info"
					>
						<span className={styles.mobileFileName} data-flx="voice.media-player.inline-audio-player.mobile-file-name">
							{fileName}
							{fileExtension}
						</span>
						{fileSizeString && (
							<span
								className={styles.mobileFileMeta}
								data-flx="voice.media-player.inline-audio-player.mobile-file-meta"
							>
								{' '}
								· {fileSizeString}
							</span>
						)}
					</motion.div>
					<div
						ref={trackRef}
						className={styles.mobileProgressContainer}
						onClick={handleProgressClick}
						onKeyDown={handleProgressKeyDown}
						role="slider"
						tabIndex={0}
						aria-valuenow={Math.round(progress)}
						aria-valuemin={0}
						aria-valuemax={100}
						aria-label={i18n._(AUDIO_PROGRESS_DESCRIPTOR)}
						data-flx="voice.media-player.inline-audio-player.mobile-progress-container.progress-click"
					>
						<div
							className={clsx(styles.mobileProgressTrack, isActive && styles.mobileProgressTrackActive)}
							data-flx="voice.media-player.inline-audio-player.mobile-progress-track"
						>
							<div
								className={clsx(styles.mobileProgressFill, isActive && styles.mobileProgressFillActive)}
								style={{width: `${progress}%`}}
								data-flx="voice.media-player.inline-audio-player.mobile-progress-fill"
							/>
						</div>
					</div>
				</div>
				<motion.span
					className={styles.mobileTimestamp}
					initial={false}
					animate={{
						color: isActive
							? 'color-mix(in srgb, var(--text-on-brand-primary) 90%, transparent)'
							: 'var(--text-secondary)',
					}}
					transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2, ease: 'easeOut'}}
					data-flx="voice.media-player.inline-audio-player.mobile-timestamp"
				>
					{state.isPlaying ? formatTime(currentTime) : formatTime(displayDuration)}
				</motion.span>
			</motion.div>
		);
	}
	return (
		<div
			className={clsx(styles.container, className)}
			onContextMenu={onContextMenu}
			role="group"
			aria-label={i18n._(AUDIO_PLAYER_DESCRIPTOR)}
			data-flx="voice.media-player.inline-audio-player.container.context-menu"
		>
			<audio
				ref={mediaRef as React.RefObject<HTMLAudioElement>}
				src={hasStarted ? src : undefined}
				preload="none"
				data-flx="voice.media-player.inline-audio-player.audio--2"
			>
				<track kind="captions" data-flx="voice.media-player.inline-audio-player.track--2" />
			</audio>
			<div className={styles.header} data-flx="voice.media-player.inline-audio-player.header">
				<FocusRing offset={-2} data-flx="voice.media-player.inline-audio-player.focus-ring">
					<button
						type="button"
						onClick={handlePlayToggle}
						className={styles.playButton}
						aria-label={state.isPlaying ? i18n._(PAUSE_DESCRIPTOR) : i18n._(PLAY_DESCRIPTOR)}
						data-flx="voice.media-player.inline-audio-player.play-button.play-toggle"
					>
						{isLoading && (
							<span
								className={styles.loadingSpinnerDesktop}
								data-flx="voice.media-player.inline-audio-player.loading-spinner-desktop"
							/>
						)}
						{state.isPlaying ? (
							<PauseIcon
								size={remFromPx(20)}
								weight="fill"
								data-flx="voice.media-player.inline-audio-player.pause-icon--2"
							/>
						) : (
							<PlayIcon
								size={remFromPx(20)}
								weight="fill"
								data-flx="voice.media-player.inline-audio-player.play-icon--2"
							/>
						)}
					</button>
				</FocusRing>
				<div className={styles.fileInfo} data-flx="voice.media-player.inline-audio-player.file-info">
					<p className={styles.fileName} data-flx="voice.media-player.inline-audio-player.file-name">
						<span
							className={styles.fileNameTruncate}
							data-flx="voice.media-player.inline-audio-player.file-name-truncate"
						>
							{fileName}
						</span>
						<span className={styles.fileExtension} data-flx="voice.media-player.inline-audio-player.file-extension">
							{fileExtension}
						</span>
					</p>
					<p className={styles.fileMeta} data-flx="voice.media-player.inline-audio-player.file-meta">
						{fileSizeString}
						{fileSizeString && displayDuration > 0 && ' · '}
						{displayDuration > 0 && formatDuration(displayDuration)}
					</p>
				</div>
			</div>
			<div className={styles.progressSection} data-flx="voice.media-player.inline-audio-player.progress-section">
				<MediaProgressBar
					progress={progress}
					buffered={buffered}
					currentTime={currentTime}
					duration={displayDuration}
					onSeek={handleSeek}
					onSeekPreview={handleSeekPreview}
					onSeekStart={startSeeking}
					onSeekEnd={endSeeking}
					compact
					className={styles.progressBar}
					data-flx="voice.media-player.inline-audio-player.progress-bar"
				/>
				<span className={styles.time} data-flx="voice.media-player.inline-audio-player.time">
					{formatDuration(currentTime)} / {formatDuration(displayDuration)}
				</span>
			</div>
			<div className={styles.controls} data-flx="voice.media-player.inline-audio-player.controls">
				<div className={styles.controlsLeft} data-flx="voice.media-player.inline-audio-player.controls-left">
					<MediaVolumeControl
						volume={volume}
						isMuted={isMuted}
						onVolumeChange={setVolume}
						onToggleMute={toggleMute}
						iconSize={18}
						expandable
						compact
						className={styles.volumeControl}
						data-flx="voice.media-player.inline-audio-player.volume-control"
					/>
				</div>
				<div className={styles.controlsRight} data-flx="voice.media-player.inline-audio-player.controls-right">
					<MediaPlaybackRate
						rate={state.playbackRate}
						onRateChange={setPlaybackRate}
						rates={AUDIO_PLAYBACK_RATES}
						size="small"
						data-flx="voice.media-player.inline-audio-player.media-playback-rate"
					/>
					{canFavorite && onFavoriteClick && (
						<Tooltip
							text={isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR)}
							position="top"
							openOnMountHover={false}
							data-flx="voice.media-player.inline-audio-player.tooltip"
						>
							<FocusRing offset={-2} data-flx="voice.media-player.inline-audio-player.focus-ring--2">
								<button
									type="button"
									onClick={(e) => onFavoriteClick(e)}
									className={styles.actionButton}
									aria-label={
										isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR)
									}
									data-flx="voice.media-player.inline-audio-player.action-button.favorite-click"
								>
									<StarIcon
										size={remFromPx(18)}
										weight={isFavorited ? 'fill' : 'regular'}
										data-flx="voice.media-player.inline-audio-player.star-icon"
									/>
								</button>
							</FocusRing>
						</Tooltip>
					)}
					{onDownloadClick && (
						<Tooltip
							text={i18n._(DOWNLOAD_DESCRIPTOR)}
							position="top"
							openOnMountHover={false}
							data-flx="voice.media-player.inline-audio-player.tooltip--2"
						>
							<FocusRing offset={-2} data-flx="voice.media-player.inline-audio-player.focus-ring--3">
								<button
									type="button"
									onClick={(e) => onDownloadClick(e)}
									className={styles.actionButton}
									aria-label={i18n._(DOWNLOAD_DESCRIPTOR)}
									data-flx="voice.media-player.inline-audio-player.action-button.download-click"
								>
									<DownloadSimpleIcon
										size={remFromPx(18)}
										weight="bold"
										data-flx="voice.media-player.inline-audio-player.download-simple-icon"
									/>
								</button>
							</FocusRing>
						</Tooltip>
					)}
				</div>
			</div>
		</div>
	);
}
