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
import {DownloadSimpleIcon, FileAudioIcon, PauseIcon, PlayIcon, StarIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useMediaPlayer} from '../hooks/useMediaPlayer';
import {useMediaProgress} from '../hooks/useMediaProgress';
import {useMediaVolume} from '../hooks/useMediaVolume';
import {formatTime} from '../utils/formatTime';
import styles from './InlineAudioPlayer.module.css';
import {MediaProgressBar} from './MediaProgressBar';
import {MediaVolumeControl} from './MediaVolumeControl';

export interface InlineAudioPlayerProps {
	src: string;
	title?: string;
	fileSize?: number;
	duration?: number;
	extension?: string;
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
	isFavorited = false,
	canFavorite = false,
	onFavoriteClick,
	onDownloadClick,
	onContextMenu,
	className,
}: InlineAudioPlayerProps) {
	const {t} = useLingui();
	const containerRef = useRef<HTMLDivElement>(null);

	const [hasStarted, setHasStarted] = useState(false);

	const {mediaRef, state, play, toggle} = useMediaPlayer({
		persistVolume: true,
	});

	const {currentTime, duration, progress, buffered, seekToPercentage, startSeeking, endSeeking} = useMediaProgress({
		mediaRef,
		initialDuration,
	});

	const {volume, isMuted, setVolume, toggleMute} = useMediaVolume({
		mediaRef,
	});

	const hasAutoPlayedRef = useRef(false);
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

	const displayDuration = initialDuration || duration;

	const {name: fileName, extension: fileExtension} = extension
		? {name: title, extension: `.${extension}`}
		: splitFilename(title);

	const metaString = fileSize
		? `${formatFileSize(fileSize)}${displayDuration ? ` • ${formatTime(displayDuration)}` : ''}`
		: displayDuration
			? formatTime(displayDuration)
			: '';

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

	const handleSeek = useCallback(
		(percentage: number) => {
			seekToPercentage(percentage);
		},
		[seekToPercentage],
	);

	return (
		<div
			ref={containerRef}
			className={clsx(styles.container, className)}
			onContextMenu={onContextMenu}
			role="group"
			aria-label={t`Audio Player`}
		>
			{/* biome-ignore lint/a11y/useMediaCaption: Audio player doesn't require captions */}
			<audio ref={mediaRef as React.RefObject<HTMLAudioElement>} src={hasStarted ? src : undefined} preload="none" />

			<div className={styles.header}>
				<div className={styles.iconContainer}>
					<FileAudioIcon size={24} weight="regular" />
				</div>

				<div className={styles.fileInfo}>
					<p className={styles.fileName}>
						<span className={styles.fileNameTruncate}>{fileName}</span>
						<span className={styles.fileExtension}>{fileExtension}</span>
					</p>
					{metaString && <p className={styles.fileMeta}>{metaString}</p>}
				</div>

				<button
					type="button"
					onClick={handlePlayClick}
					className={styles.playButton}
					aria-label={state.isPlaying ? t`Pause` : t`Play`}
				>
					{state.isPlaying ? <PauseIcon size={20} weight="fill" /> : <PlayIcon size={20} weight="fill" />}
				</button>
			</div>

			<div className={styles.progressSection}>
				<MediaProgressBar
					progress={progress}
					buffered={buffered}
					currentTime={currentTime}
					duration={displayDuration}
					onSeek={handleSeek}
					onSeekStart={startSeeking}
					onSeekEnd={endSeeking}
					compact
					className={styles.progressBar}
				/>
				<span className={styles.time}>
					{formatTime(currentTime)} / {formatTime(displayDuration)}
				</span>
			</div>

			<div className={styles.controls}>
				<div className={styles.controlsLeft}>
					<MediaVolumeControl
						volume={volume}
						isMuted={isMuted}
						onVolumeChange={setVolume}
						onToggleMute={toggleMute}
						compact
						iconSize={18}
						className={styles.volumeControl}
					/>
				</div>

				<div className={styles.controlsRight}>
					{canFavorite && onFavoriteClick && (
						<Tooltip text={isFavorited ? t`Remove from favorites` : t`Add to favorites`} position="top">
							<button
								type="button"
								onClick={(e) => onFavoriteClick(e)}
								className={styles.actionButton}
								aria-label={isFavorited ? t`Remove from favorites` : t`Add to favorites`}
							>
								<StarIcon size={18} weight={isFavorited ? 'fill' : 'regular'} />
							</button>
						</Tooltip>
					)}

					{onDownloadClick && (
						<Tooltip text={t`Download`} position="top">
							<button
								type="button"
								onClick={(e) => onDownloadClick(e)}
								className={styles.actionButton}
								aria-label={t`Download`}
							>
								<DownloadSimpleIcon size={18} weight="bold" />
							</button>
						</Tooltip>
					)}
				</div>
			</div>
		</div>
	);
}
