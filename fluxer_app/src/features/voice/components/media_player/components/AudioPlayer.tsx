// SPDX-License-Identifier: AGPL-3.0-or-later

import {PAUSE_DESCRIPTOR, PLAY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/voice/components/media_player/AudioPlayer.module.css';
import {MediaPlaybackRate} from '@app/features/voice/components/media_player/components/MediaPlaybackRate';
import {MediaProgressBar} from '@app/features/voice/components/media_player/components/MediaProgressBar';
import {MediaVolumeControl} from '@app/features/voice/components/media_player/components/MediaVolumeControl';
import {useMediaPlayer} from '@app/features/voice/components/media_player/hooks/useMediaPlayer';
import {useMediaProgress} from '@app/features/voice/components/media_player/hooks/useMediaProgress';
import {useMediaVolume} from '@app/features/voice/components/media_player/hooks/useMediaVolume';
import {useMetadataPreload} from '@app/features/voice/components/media_player/hooks/useMetadataPreload';
import {
	AUDIO_PLAYBACK_RATES,
	DEFAULT_SEEK_AMOUNT,
} from '@app/features/voice/components/media_player/utils/MediaConstants';
import {formatDuration} from '@fluxer/date_utils/src/DateDuration';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ClockClockwiseIcon, ClockCounterClockwiseIcon, PauseIcon, PlayIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const REWIND_SECONDS_DESCRIPTOR = msg({
	message: 'Rewind {defaultSeekAmount} seconds',
	comment:
		'Tooltip / aria label on the rewind button in the standalone audio player. {defaultSeekAmount} is an integer second count.',
});
const FORWARD_SECONDS_DESCRIPTOR = msg({
	message: 'Forward {defaultSeekAmount} seconds',
	comment:
		'Tooltip / aria label on the forward / fast-forward button in the standalone audio player. {defaultSeekAmount} is an integer second count.',
});

interface AudioPlayerProps {
	src: string;
	title?: string;
	duration?: number;
	autoPlay?: boolean;
	isMobile?: boolean;
	className?: string;
}

export function AudioPlayer({
	src,
	title,
	duration: initialDuration,
	autoPlay = false,
	isMobile = false,
	className,
}: AudioPlayerProps) {
	const {i18n} = useLingui();
	const containerRef = useRef<HTMLDivElement>(null);
	const [hasStarted, setHasStarted] = useState(autoPlay);
	const [prePlayCurrentTime, setPrePlayCurrentTime] = useState(0);
	const pendingPlayRef = useRef(false);
	const pendingSeekPercentageRef = useRef<number | null>(null);
	const {mediaRef, state, play, toggle, seekRelative, setPlaybackRate} = useMediaPlayer({
		mediaKind: 'audio',
		autoPlay,
		persistPlaybackRate: true,
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
	const {volume, isMuted, setVolume, toggleMute} = useMediaVolume({mediaRef});
	const {escalateToMetadata, sourceAttribute, preloadAttribute} = useMetadataPreload(src, hasStarted);
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
	useEffect(() => {
		if (!hasStarted || pendingSeekPercentageRef.current === null) {
			return;
		}
		const media = mediaRef.current;
		if (!media) {
			return;
		}
		const applyPendingSeek = () => {
			if (pendingSeekPercentageRef.current === null || !Number.isFinite(media.duration) || media.duration <= 0) {
				return;
			}
			const seekTime = (pendingSeekPercentageRef.current / 100) * media.duration;
			media.currentTime = seekTime;
			setPrePlayCurrentTime(seekTime);
			pendingSeekPercentageRef.current = null;
		};
		applyPendingSeek();
		media.addEventListener('loadedmetadata', applyPendingSeek);
		media.addEventListener('durationchange', applyPendingSeek);
		return () => {
			media.removeEventListener('loadedmetadata', applyPendingSeek);
			media.removeEventListener('durationchange', applyPendingSeek);
		};
	}, [hasStarted, mediaRef]);
	const handlePlayClick = useCallback(
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
	const handleSeekBackward = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			if (!hasStarted || duration <= 0) {
				if (!initialDuration || initialDuration <= 0) {
					return;
				}
				const nextTime = Math.max(0, prePlayCurrentTime - DEFAULT_SEEK_AMOUNT);
				setPrePlayCurrentTime(nextTime);
				pendingSeekPercentageRef.current = (nextTime / initialDuration) * 100;
				return;
			}
			seekRelative(-DEFAULT_SEEK_AMOUNT);
		},
		[duration, hasStarted, initialDuration, prePlayCurrentTime, seekRelative],
	);
	const handleSeekForward = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			if (!hasStarted || duration <= 0) {
				if (!initialDuration || initialDuration <= 0) {
					return;
				}
				const nextTime = Math.min(initialDuration, prePlayCurrentTime + DEFAULT_SEEK_AMOUNT);
				setPrePlayCurrentTime(nextTime);
				pendingSeekPercentageRef.current = (nextTime / initialDuration) * 100;
				return;
			}
			seekRelative(DEFAULT_SEEK_AMOUNT);
		},
		[duration, hasStarted, initialDuration, prePlayCurrentTime, seekRelative],
	);
	const handleSeek = useCallback(
		(percentage: number) => {
			if (!hasStarted || duration <= 0) {
				if (!initialDuration || initialDuration <= 0) {
					return;
				}
				const clampedPercentage = Math.max(0, Math.min(100, percentage));
				const nextTime = (clampedPercentage / 100) * initialDuration;
				setPrePlayCurrentTime(nextTime);
				pendingSeekPercentageRef.current = clampedPercentage;
				return;
			}
			seekToPercentage(percentage);
		},
		[duration, hasStarted, initialDuration, seekToPercentage],
	);
	const handleSeekPreview = useCallback(
		(percentage: number) => {
			if (!hasStarted || duration <= 0) {
				if (!initialDuration || initialDuration <= 0) {
					return;
				}
				const clampedPercentage = Math.max(0, Math.min(100, percentage));
				setPrePlayCurrentTime((clampedPercentage / 100) * initialDuration);
				return;
			}
			previewSeekToPercentage(percentage);
		},
		[duration, hasStarted, initialDuration, previewSeekToPercentage],
	);
	const playButtonSize = isMobile ? 28 : 24;
	const seekButtonSize = isMobile ? 24 : 20;
	const displayDuration = duration > 0 ? duration : (initialDuration ?? 0);
	const displayCurrentTime = !hasStarted && initialDuration ? prePlayCurrentTime : currentTime;
	const displayProgress =
		!hasStarted && initialDuration && initialDuration > 0 ? (prePlayCurrentTime / initialDuration) * 100 : progress;
	return (
		<div
			ref={containerRef}
			className={clsx(styles.container, isMobile && styles.mobile, className)}
			onPointerEnter={escalateToMetadata}
			data-flx="voice.media-player.audio-player.container"
		>
			{/* biome-ignore lint/a11y/useMediaCaption: voice/audio attachments have no caption track source */}
			<audio
				ref={mediaRef as React.RefObject<HTMLAudioElement>}
				src={sourceAttribute}
				preload={preloadAttribute}
				data-flx="voice.media-player.audio-player.audio"
			/>
			{title && (
				<h3 className={styles.fileName} data-flx="voice.media-player.audio-player.file-name">
					{title}
				</h3>
			)}
			<div className={styles.progressSection} data-flx="voice.media-player.audio-player.progress-section">
				<MediaProgressBar
					progress={displayProgress}
					buffered={buffered}
					currentTime={displayCurrentTime}
					duration={displayDuration}
					mediaRef={mediaRef}
					isPlaying={state.isPlaying}
					onSeek={handleSeek}
					onSeekPreview={handleSeekPreview}
					onSeekStart={startSeeking}
					onSeekEnd={endSeeking}
					className={styles.progressBar}
					data-flx="voice.media-player.audio-player.progress-bar"
				/>
				<span className={styles.timeDisplay} data-flx="voice.media-player.audio-player.time-display">
					{formatDuration(displayCurrentTime)} / {formatDuration(displayDuration)}
				</span>
			</div>
			<div className={styles.controls} data-flx="voice.media-player.audio-player.controls">
				<div className={styles.mainControls} data-flx="voice.media-player.audio-player.main-controls">
					<Tooltip
						text={i18n._(REWIND_SECONDS_DESCRIPTOR, {defaultSeekAmount: DEFAULT_SEEK_AMOUNT})}
						position="top"
						openOnMountHover={false}
						data-flx="voice.media-player.audio-player.tooltip"
					>
						<FocusRing offset={-2} data-flx="voice.media-player.audio-player.focus-ring">
							<button
								type="button"
								onClick={handleSeekBackward}
								className={styles.seekButton}
								aria-label={i18n._(REWIND_SECONDS_DESCRIPTOR, {defaultSeekAmount: DEFAULT_SEEK_AMOUNT})}
								data-flx="voice.media-player.audio-player.seek-button.seek-backward"
							>
								<ClockCounterClockwiseIcon
									size={seekButtonSize}
									weight="bold"
									data-flx="voice.media-player.audio-player.clock-counter-clockwise-icon"
								/>
							</button>
						</FocusRing>
					</Tooltip>
					<FocusRing offset={-2} data-flx="voice.media-player.audio-player.focus-ring--2">
						<button
							type="button"
							onClick={handlePlayClick}
							className={styles.playButton}
							aria-label={state.isPlaying ? i18n._(PAUSE_DESCRIPTOR) : i18n._(PLAY_DESCRIPTOR)}
							data-flx="voice.media-player.audio-player.play-button.play-click"
						>
							{state.isPlaying ? (
								<PauseIcon size={playButtonSize} weight="fill" data-flx="voice.media-player.audio-player.pause-icon" />
							) : (
								<PlayIcon size={playButtonSize} weight="fill" data-flx="voice.media-player.audio-player.play-icon" />
							)}
						</button>
					</FocusRing>
					<Tooltip
						text={i18n._(FORWARD_SECONDS_DESCRIPTOR, {defaultSeekAmount: DEFAULT_SEEK_AMOUNT})}
						position="top"
						openOnMountHover={false}
						data-flx="voice.media-player.audio-player.tooltip--2"
					>
						<FocusRing offset={-2} data-flx="voice.media-player.audio-player.focus-ring--3">
							<button
								type="button"
								onClick={handleSeekForward}
								className={styles.seekButton}
								aria-label={i18n._(FORWARD_SECONDS_DESCRIPTOR, {defaultSeekAmount: DEFAULT_SEEK_AMOUNT})}
								data-flx="voice.media-player.audio-player.seek-button.seek-forward"
							>
								<ClockClockwiseIcon
									size={seekButtonSize}
									weight="bold"
									data-flx="voice.media-player.audio-player.clock-clockwise-icon"
								/>
							</button>
						</FocusRing>
					</Tooltip>
				</div>
			</div>
			<div className={styles.secondaryControls} data-flx="voice.media-player.audio-player.secondary-controls">
				<MediaVolumeControl
					volume={volume}
					isMuted={isMuted}
					onVolumeChange={setVolume}
					onToggleMute={toggleMute}
					iconSize={18}
					className={styles.volumeControl}
					data-flx="voice.media-player.audio-player.volume-control"
				/>
				<MediaPlaybackRate
					rate={state.playbackRate}
					onRateChange={setPlaybackRate}
					rates={AUDIO_PLAYBACK_RATES}
					isAudio
					className={styles.playbackRate}
					data-flx="voice.media-player.audio-player.playback-rate"
				/>
			</div>
		</div>
	);
}
