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
import {ClockClockwiseIcon, ClockCounterClockwiseIcon, PauseIcon, PlayIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import AudioVolumeStore from '~/stores/AudioVolumeStore';
import {useMediaPlayer} from '../hooks/useMediaPlayer';
import {useMediaProgress} from '../hooks/useMediaProgress';
import {formatTime} from '../utils/formatTime';
import {AUDIO_PLAYBACK_RATES, DEFAULT_SEEK_AMOUNT} from '../utils/mediaConstants';
import styles from './AudioPlayer.module.css';
import {MediaPlaybackRate} from './MediaPlaybackRate';
import {MediaProgressBar} from './MediaProgressBar';
import {MediaVolumeControl} from './MediaVolumeControl';

export interface AudioPlayerProps {
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
	const {t} = useLingui();
	const containerRef = useRef<HTMLDivElement>(null);

	const [hasStarted, setHasStarted] = useState(autoPlay);

	const [volume, setVolumeState] = useState(AudioVolumeStore.volume);
	const [isMuted, setIsMutedState] = useState(AudioVolumeStore.isMuted);

	const {mediaRef, state, play, toggle, seekRelative, setPlaybackRate} = useMediaPlayer({
		autoPlay,
		persistVolume: false,
		persistPlaybackRate: true,
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

	const setVolume = useCallback(
		(newVolume: number) => {
			const clamped = Math.max(0, Math.min(1, newVolume));
			setVolumeState(clamped);
			AudioVolumeStore.setVolume(clamped);
			if (isMuted && clamped > 0) {
				setIsMutedState(false);
			}
		},
		[isMuted],
	);

	const toggleMute = useCallback(() => {
		setIsMutedState((prev) => !prev);
		AudioVolumeStore.toggleMute();
	}, []);

	const hasAutoPlayedRef = useRef(autoPlay);
	useEffect(() => {
		if (hasStarted && !hasAutoPlayedRef.current) {
			hasAutoPlayedRef.current = true;
			const timer = setTimeout(() => {
				play();
			}, 0);
			return () => clearTimeout(timer);
		}
		return undefined;
	}, [hasStarted, play]);

	const handlePlayClick = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			if (!hasStarted) {
				setHasStarted(true);
			} else {
				toggle();
			}
		},
		[hasStarted, toggle],
	);

	const handleSeekBackward = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			seekRelative(-DEFAULT_SEEK_AMOUNT);
		},
		[seekRelative],
	);

	const handleSeekForward = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			seekRelative(DEFAULT_SEEK_AMOUNT);
		},
		[seekRelative],
	);

	const handleSeek = useCallback(
		(percentage: number) => {
			seekToPercentage(percentage);
		},
		[seekToPercentage],
	);

	const playButtonSize = isMobile ? 28 : 24;
	const seekButtonSize = isMobile ? 24 : 20;

	return (
		<div ref={containerRef} className={clsx(styles.container, isMobile && styles.mobile, className)}>
			{/* biome-ignore lint/a11y/useMediaCaption: Audio player doesn't require captions */}
			<audio ref={mediaRef as React.RefObject<HTMLAudioElement>} src={hasStarted ? src : undefined} preload="none" />

			{title && <h3 className={styles.fileName}>{title}</h3>}

			<div className={styles.progressSection}>
				<MediaProgressBar
					progress={progress}
					buffered={buffered}
					currentTime={currentTime}
					duration={duration}
					onSeek={handleSeek}
					onSeekStart={startSeeking}
					onSeekEnd={endSeeking}
					className={styles.progressBar}
				/>
				<div className={styles.timeDisplay}>
					<span>{formatTime(currentTime)}</span>
					<span>{formatTime(duration)}</span>
				</div>
			</div>

			<div className={styles.controls}>
				<div className={styles.mainControls}>
					<Tooltip text={t`Rewind ${DEFAULT_SEEK_AMOUNT} seconds`} position="top">
						<button
							type="button"
							onClick={handleSeekBackward}
							className={styles.seekButton}
							aria-label={t`Rewind ${DEFAULT_SEEK_AMOUNT} seconds`}
						>
							<ClockCounterClockwiseIcon size={seekButtonSize} weight="bold" />
						</button>
					</Tooltip>

					<button
						type="button"
						onClick={handlePlayClick}
						className={styles.playButton}
						aria-label={state.isPlaying ? t`Pause` : t`Play`}
					>
						{state.isPlaying ? (
							<PauseIcon size={playButtonSize} weight="fill" />
						) : (
							<PlayIcon size={playButtonSize} weight="fill" />
						)}
					</button>

					<Tooltip text={t`Forward ${DEFAULT_SEEK_AMOUNT} seconds`} position="top">
						<button
							type="button"
							onClick={handleSeekForward}
							className={styles.seekButton}
							aria-label={t`Forward ${DEFAULT_SEEK_AMOUNT} seconds`}
						>
							<ClockClockwiseIcon size={seekButtonSize} weight="bold" />
						</button>
					</Tooltip>
				</div>
			</div>

			<div className={styles.secondaryControls}>
				<MediaVolumeControl
					volume={volume}
					isMuted={isMuted}
					onVolumeChange={setVolume}
					onToggleMute={toggleMute}
					iconSize={18}
					className={styles.volumeControl}
				/>

				<MediaPlaybackRate
					rate={state.playbackRate}
					onRateChange={setPlaybackRate}
					rates={AUDIO_PLAYBACK_RATES}
					isAudio
					className={styles.playbackRate}
				/>
			</div>
		</div>
	);
}
