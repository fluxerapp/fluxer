// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import type {BaseMediaProps} from '@app/features/channel/components/embeds/media/MediaTypes';
import styles from '@app/features/channel/components/embeds/media/VoiceMessagePlayer.module.css';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import {PAUSE_DESCRIPTOR, PLAY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {buildMediaProxyURL} from '@app/features/messaging/utils/MediaProxyUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MediaContextMenu} from '@app/features/ui/action_menu/MediaContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {MediaPlaybackRate} from '@app/features/voice/components/media_player/components/MediaPlaybackRate';
import {MediaVerticalVolumeControl} from '@app/features/voice/components/media_player/components/MediaVerticalVolumeControl';
import {useMediaPlayer} from '@app/features/voice/components/media_player/hooks/useMediaPlayer';
import {useMediaProgress} from '@app/features/voice/components/media_player/hooks/useMediaProgress';
import {useMediaVolume} from '@app/features/voice/components/media_player/hooks/useMediaVolume';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PauseIcon, PlayIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const VOICE_MESSAGE_PLAYER_DESCRIPTOR = msg({
	message: 'Voice message player',
	comment: 'Short label in the channel and chat voice message player. Keep it concise.',
});
const logger = new Logger('VoiceMessagePlayer');
const WAVEFORM_BAR_COUNT = 32;
const VOICE_PLAYBACK_RATE_STEPS = [0.75, 1, 1.5, 2] as const;
const voicePlaybackStops = new Map<object, () => void>();

export interface VoiceMessagePlayerProps extends BaseMediaProps {
	src: string;
	title?: string;
	duration?: number;
	fileSize?: number;
	waveform: string;
	mediaAttachments?: ReadonlyArray<MessageAttachment>;
	isPreview?: boolean;
	snapshotIndex?: number;
}

export function decodeWaveform(waveform: string): Array<number> {
	if (!waveform) return [];
	try {
		const decoded = atob(waveform);
		const values: Array<number> = [];
		for (let i = 0; i < decoded.length; i++) {
			values.push(decoded.charCodeAt(i) / 255);
		}
		return values;
	} catch (error) {
		logger.warn({error, waveform}, 'Unable to decode waveform');
		return [];
	}
}

export function resampleWaveform(samples: ReadonlyArray<number>, barCount: number): Array<number> {
	if (samples.length === barCount) return [...samples];
	if (samples.length < barCount) {
		return [...samples, ...new Array<number>(barCount - samples.length).fill(0)];
	}
	const ratio = samples.length / barCount;
	const resampled: Array<number> = [];
	let cursor = 0;
	while (resampled.length < barCount) {
		const boundary = Math.round((resampled.length + 1) * ratio);
		let total = 0;
		let taken = 0;
		for (let i = cursor; i < boundary && i < samples.length; i++) {
			total += samples[i];
			taken++;
		}
		resampled.push(taken > 0 ? total / taken : 0);
		cursor = boundary;
	}
	return resampled;
}

export function countPlayedBars(progressPercent: number, barCount: number): number {
	if (barCount <= 0) return 0;
	return Math.max(0, Math.min(barCount, Math.round((progressPercent / 100) * barCount)));
}

export function waveformBarHeightPercent(value: number): number {
	const clamped = Math.max(0, Math.min(1, value));
	return ((22 * clamped + 2) / 24) * 100;
}

function formatTime(time: number): string {
	if (!Number.isFinite(time)) return '0:00';
	const minutes = Math.floor(time / 60);
	const seconds = Math.floor(time % 60);
	return `${minutes}:${seconds.toString().padStart(2, '0')}`;
}

const VoiceMessagePlayer: React.FC<VoiceMessagePlayerProps> = observer(
	({
		src,
		title: _title,
		duration: initialDuration,
		channelId: _channelId,
		messageId: _messageId,
		attachmentId,
		message,
		contentHash,
		onDelete,
		waveform,
		snapshotIndex,
	}) => {
		const {i18n} = useLingui();
		const messageViewContext = useMaybeMessageViewContext();
		const effectiveSrc = buildMediaProxyURL(src);
		const [hasStarted, setHasStarted] = useState(false);
		const [wantsMetadata, setWantsMetadata] = useState(false);
		const [prePlayCurrentTime, setPrePlayCurrentTime] = useState(0);
		const pendingPlayRef = useRef(false);
		const pendingSeekPercentageRef = useRef<number | null>(null);
		const instanceKeyRef = useRef({});
		const {mediaRef, state, play, pause, toggle, setPlaybackRate} = useMediaPlayer({
			mediaKind: 'voice-message',
		});
		const {currentTime, duration, progress, seekToPercentage} = useMediaProgress({
			mediaRef,
			initialDuration: initialDuration ?? 0,
		});
		const {volume, isMuted, setVolume, toggleMute} = useMediaVolume({
			mediaRef,
		});
		const handleContextMenu = useCallback(
			(e: React.MouseEvent) => {
				if (!message) return;
				e.preventDefault();
				e.stopPropagation();
				ContextMenuCommands.openFromEvent(e, ({onClose}) => (
					<MediaContextMenu
						message={message}
						sourceChannel={messageViewContext?.channel}
						originalSrc={src}
						type="audio"
						contentHash={contentHash}
						attachmentId={attachmentId}
						snapshotIndex={snapshotIndex}
						onClose={onClose}
						onDelete={onDelete || (() => {})}
						data-flx="channel.embeds.media.voice-message-player.handle-context-menu.media-context-menu.audio"
					/>
				));
			},
			[message, messageViewContext?.channel, src, contentHash, attachmentId, onDelete, snapshotIndex],
		);
		const waveformBars = useMemo(() => resampleWaveform(decodeWaveform(waveform), WAVEFORM_BAR_COUNT), [waveform]);
		const displayDuration = duration > 0 ? duration : (initialDuration ?? 0);
		const isLoading = hasStarted && state.isBuffering;
		const isActive = state.isPlaying || isLoading;
		useEffect(() => {
			const instanceKey = instanceKeyRef.current;
			voicePlaybackStops.set(instanceKey, pause);
			return () => {
				voicePlaybackStops.delete(instanceKey);
			};
		}, [pause]);
		useEffect(() => {
			if (!state.isPlaying) return;
			const instanceKey = instanceKeyRef.current;
			for (const [key, stop] of voicePlaybackStops) {
				if (key !== instanceKey) {
					stop();
				}
			}
		}, [state.isPlaying]);
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
		const handleContainerMouseEnter = useCallback(() => {
			setWantsMetadata(true);
		}, []);
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
		const handleWaveformClick = useCallback(
			(e: React.MouseEvent<HTMLDivElement>) => {
				if (state.isBuffering) return;
				const rect = e.currentTarget.getBoundingClientRect();
				const x = e.clientX - rect.left;
				const percentage = (x / rect.width) * 100;
				if (!hasStarted || duration <= 0) {
					if (!displayDuration || displayDuration <= 0) {
						return;
					}
					const clampedPercentage = Math.max(0, Math.min(100, percentage));
					const nextTime = (clampedPercentage / 100) * displayDuration;
					setPrePlayCurrentTime(nextTime);
					pendingSeekPercentageRef.current = clampedPercentage;
					return;
				}
				seekToPercentage(Math.max(0, Math.min(100, percentage)));
			},
			[displayDuration, duration, hasStarted, seekToPercentage, state.isBuffering],
		);
		const handleWaveformKeyDown = useCallback(
			(e: React.KeyboardEvent<HTMLDivElement>) => {
				if (state.isBuffering) return;
				const step = 5;
				if (e.key === 'ArrowLeft') {
					e.preventDefault();
					if (!hasStarted || duration <= 0) {
						if (!displayDuration || displayDuration <= 0) {
							return;
						}
						const nextTime = Math.max(0, prePlayCurrentTime - displayDuration * (step / 100));
						setPrePlayCurrentTime(nextTime);
						pendingSeekPercentageRef.current = (nextTime / displayDuration) * 100;
						return;
					}
					seekToPercentage(Math.max(0, progress - step));
				} else if (e.key === 'ArrowRight') {
					e.preventDefault();
					if (!hasStarted || duration <= 0) {
						if (!displayDuration || displayDuration <= 0) {
							return;
						}
						const nextTime = Math.min(displayDuration, prePlayCurrentTime + displayDuration * (step / 100));
						setPrePlayCurrentTime(nextTime);
						pendingSeekPercentageRef.current = (nextTime / displayDuration) * 100;
						return;
					}
					seekToPercentage(Math.min(100, progress + step));
				}
			},
			[displayDuration, duration, hasStarted, prePlayCurrentTime, progress, seekToPercentage, state.isBuffering],
		);
		const displayCurrentTime = !hasStarted && displayDuration > 0 ? prePlayCurrentTime : currentTime;
		const displayProgress =
			!hasStarted && displayDuration > 0 ? (prePlayCurrentTime / displayDuration) * 100 : progress;
		const playedBarCount = countPlayedBars(displayProgress, waveformBars.length);
		const waveformElements = useMemo(
			() =>
				waveformBars.map((value, index) => (
					<span
						key={`waveform-${index}`}
						className={clsx(styles.waveformBar, index < playedBarCount && styles.waveformBarPast)}
						style={{height: `${waveformBarHeightPercent(value)}%`}}
						data-flx="channel.embeds.media.voice-message-player.waveform-elements.waveform-bar"
					/>
				)),
			[playedBarCount, waveformBars],
		);
		const hasBegunPlayback = hasStarted || displayCurrentTime > 0;
		const remainingSeconds = Math.max(0, Math.ceil(displayDuration - displayCurrentTime));
		const readoutText =
			displayDuration > 0 ? formatTime(hasBegunPlayback ? remainingSeconds : Math.ceil(displayDuration)) : '--:--';
		const isMobile = MobileLayout.enabled;
		return (
			<motion.div
				className={clsx(styles.container, isActive && styles.containerActive)}
				onContextMenu={handleContextMenu}
				onMouseEnter={handleContainerMouseEnter}
				role="region"
				aria-label={i18n._(VOICE_MESSAGE_PLAYER_DESCRIPTOR)}
				initial={false}
				animate={{
					backgroundColor: isActive ? 'var(--brand-primary)' : 'var(--background-secondary)',
				}}
				transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2, ease: 'easeOut'}}
				data-flx="channel.embeds.media.voice-message-player.container.context-menu"
			>
				<audio
					ref={mediaRef as React.RefObject<HTMLAudioElement>}
					src={hasStarted || wantsMetadata ? effectiveSrc : undefined}
					preload={wantsMetadata ? 'metadata' : 'none'}
					data-flx="channel.embeds.media.voice-message-player.audio"
				>
					<track kind="captions" data-flx="channel.embeds.media.voice-message-player.track" />
				</audio>
				<motion.button
					type="button"
					onClick={handlePlayToggle}
					className={styles.playButton}
					aria-label={state.isPlaying ? i18n._(PAUSE_DESCRIPTOR) : i18n._(PLAY_DESCRIPTOR)}
					whileTap={Accessibility.useReducedMotion ? undefined : {y: 1}}
					initial={false}
					animate={{
						backgroundColor: isActive ? 'var(--text-on-brand-primary)' : 'var(--brand-primary)',
						color: isActive ? 'var(--brand-primary)' : 'var(--text-on-brand-primary)',
					}}
					transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2, ease: 'easeOut'}}
					data-flx="channel.embeds.media.voice-message-player.play-button.play-toggle"
				>
					{isLoading && (
						<span
							className={styles.loadingSpinner}
							data-flx="channel.embeds.media.voice-message-player.loading-spinner"
						/>
					)}
					<AnimatePresence mode="wait" data-flx="channel.embeds.media.voice-message-player.animate-presence">
						<motion.div
							key={state.isPlaying ? 'pause' : 'play'}
							className={styles.playButtonIcon}
							initial={Accessibility.useReducedMotion ? {scale: 1, opacity: 1} : {scale: 0.5, opacity: 0}}
							animate={{scale: 1, opacity: 1}}
							exit={Accessibility.useReducedMotion ? {scale: 1, opacity: 1} : {scale: 0.5, opacity: 0}}
							transition={{duration: Accessibility.useReducedMotion ? 0 : 0.1}}
							data-flx="channel.embeds.media.voice-message-player.play-button-icon"
						>
							{state.isPlaying ? (
								<PauseIcon
									size={remFromPx(16)}
									weight="fill"
									data-flx="channel.embeds.media.voice-message-player.pause-icon"
								/>
							) : (
								<PlayIcon
									size={remFromPx(16)}
									weight="fill"
									data-flx="channel.embeds.media.voice-message-player.play-icon"
								/>
							)}
						</motion.div>
					</AnimatePresence>
				</motion.button>
				<div
					className={styles.waveformContainer}
					onClick={handleWaveformClick}
					onKeyDown={handleWaveformKeyDown}
					role="slider"
					tabIndex={0}
					aria-valuenow={Math.round(displayProgress)}
					aria-valuemin={0}
					aria-valuemax={100}
					aria-valuetext={
						displayDuration > 0 ? `${formatTime(displayCurrentTime)} / ${formatTime(displayDuration)}` : undefined
					}
					data-flx="channel.embeds.media.voice-message-player.waveform-container.waveform-click"
				>
					{waveformElements}
				</div>
				<motion.span
					className={styles.timestamp}
					initial={false}
					animate={{
						color: isActive
							? 'color-mix(in srgb, var(--text-on-brand-primary) 90%, transparent)'
							: 'var(--text-secondary)',
					}}
					transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2, ease: 'easeOut'}}
					data-flx="channel.embeds.media.voice-message-player.timestamp"
				>
					{readoutText}
				</motion.span>
				<MediaPlaybackRate
					rate={state.playbackRate}
					onRateChange={setPlaybackRate}
					rates={VOICE_PLAYBACK_RATE_STEPS}
					size="small"
					className={styles.speedControl}
					data-flx="channel.embeds.media.voice-message-player.speed-control"
				/>
				{!isMobile && (
					<MediaVerticalVolumeControl
						volume={volume}
						isMuted={isMuted}
						onVolumeChange={setVolume}
						onToggleMute={toggleMute}
						iconSize={16}
						className={styles.volumeControl}
						data-flx="channel.embeds.media.voice-message-player.volume-control"
					/>
				)}
			</motion.div>
		);
	},
);

export default VoiceMessagePlayer;
