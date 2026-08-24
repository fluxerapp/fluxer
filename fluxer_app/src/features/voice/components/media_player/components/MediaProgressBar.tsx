// SPDX-License-Identifier: AGPL-3.0-or-later

import {SliderTooltipPortal} from '@app/features/ui/components/slider/SliderTooltipPortal';
import {useSliderTooltip} from '@app/features/ui/components/slider/useSliderTooltip';
import styles from '@app/features/voice/components/media_player/MediaProgressBar.module.css';
import {
	type BufferedSpanFraction,
	clampPercentage,
	EMPTY_BUFFERED_SPANS,
	getEffectiveMediaDuration,
	getSeekPercentageFromClientX,
	stepPlayheadPrediction,
} from '@app/features/voice/components/media_player/utils/MediaSeekUtils';
import {formatDuration} from '@fluxer/date_utils/src/DateDuration';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

export const PLAYHEAD_CSS_PROPERTY = '--media-progress-value';

export interface MediaElementRef {
	readonly current: HTMLMediaElement | null;
}

const MEDIA_PROGRESS_DESCRIPTOR = msg({
	message: 'Media progress',
	comment: 'Aria label on the media player progress / scrubber slider.',
});
export interface MediaProgressBarProps {
	progress: number;
	buffered?: ReadonlyArray<BufferedSpanFraction>;
	currentTime?: number;
	duration?: number;
	isSeeking?: boolean;
	onSeek?: (percentage: number) => void;
	onSeekPreview?: (percentage: number) => void;
	onSeekStart?: () => void;
	onSeekEnd?: () => void;
	onSeekHover?: (percentage: number) => void;
	onSeekHoverEnd?: () => void;
	showPreview?: boolean;
	previewThumbnail?: React.ReactNode;
	previewThumbnailWidth?: number;
	previewPortalRoot?: HTMLElement | null;
	className?: string;
	compact?: boolean;
	ariaLabel?: string;
	mediaRef?: MediaElementRef;
	isPlaying?: boolean;
}

export function MediaProgressBar({
	progress,
	buffered = EMPTY_BUFFERED_SPANS,
	currentTime = 0,
	duration = 0,
	onSeek,
	onSeekPreview,
	onSeekStart,
	onSeekEnd,
	onSeekHover,
	onSeekHoverEnd,
	showPreview = true,
	previewThumbnail,
	previewThumbnailWidth,
	previewPortalRoot,
	className,
	compact = false,
	ariaLabel,
	mediaRef,
	isPlaying = false,
}: MediaProgressBarProps) {
	const {i18n} = useLingui();
	const containerRef = useRef<HTMLDivElement>(null);
	const tooltipAnchorRef = useRef<HTMLDivElement>(null);
	const [isDragging, setIsDragging] = useState(false);
	const [hoverPosition, setHoverPosition] = useState<number | null>(null);
	const [hoverTime, setHoverTime] = useState<string>('0:00');
	const shouldRenderTooltip = showPreview && hoverPosition !== null;
	const tooltipThumbnailStyle =
		previewThumbnailWidth != null
			? ({'--media-progress-tooltip-thumbnail-width': `${previewThumbnailWidth}px`} as React.CSSProperties)
			: undefined;
	const tooltip = useSliderTooltip({
		showTooltip: shouldRenderTooltip,
		value: hoverPosition ?? 0,
		isDragging,
		thumbRef: tooltipAnchorRef,
		portalRoot: previewPortalRoot,
	});
	const rafRef = useRef<number | null>(null);
	const cachedRectRef = useRef<DOMRect | null>(null);
	const pendingSeekRef = useRef<number | null>(null);
	const hoverRafRef = useRef<number | null>(null);
	const pendingHoverRef = useRef<number | null>(null);
	const activePointerIdRef = useRef<number | null>(null);
	const lastSeekPercentageRef = useRef(clampPercentage(progress));
	const cacheContainerRect = useCallback(() => {
		const container = containerRef.current;
		if (!container) return null;
		const rect = container.getBoundingClientRect();
		cachedRectRef.current = rect;
		return rect;
	}, []);
	const clearCachedRect = useCallback(() => {
		cachedRectRef.current = null;
	}, []);
	const getPercentageFromEvent = useCallback(
		(clientX: number): number => {
			const container = containerRef.current;
			if (!container) return 0;
			const rect = cachedRectRef.current ?? cacheContainerRect();
			if (!rect) return 0;
			return getSeekPercentageFromClientX(clientX, rect);
		},
		[cacheContainerRect],
	);
	const setHoverPositionFromPercentage = useCallback(
		(percentage: number) => {
			const clampedPercentage = clampPercentage(percentage);
			setHoverPosition(clampedPercentage);
			if (duration > 0) {
				setHoverTime(formatDuration((clampedPercentage / 100) * duration));
			} else {
				setHoverTime(formatDuration(0));
			}
		},
		[duration],
	);
	const scheduleSeekPreview = useCallback(
		(percentage: number) => {
			if (!onSeekPreview) return;
			pendingSeekRef.current = percentage;
			if (rafRef.current !== null) return;
			rafRef.current = requestAnimationFrame(() => {
				const nextPercentage = pendingSeekRef.current;
				pendingSeekRef.current = null;
				rafRef.current = null;
				if (nextPercentage !== null) {
					onSeekPreview(nextPercentage);
				}
			});
		},
		[onSeekPreview],
	);
	const scheduleSeekHover = useCallback(
		(percentage: number) => {
			if (!onSeekHover) return;
			pendingHoverRef.current = percentage;
			if (hoverRafRef.current !== null) return;
			hoverRafRef.current = requestAnimationFrame(() => {
				const nextPercentage = pendingHoverRef.current;
				pendingHoverRef.current = null;
				hoverRafRef.current = null;
				if (nextPercentage !== null) {
					onSeekHover(nextPercentage);
				}
			});
		},
		[onSeekHover],
	);
	const cancelPendingHover = useCallback(() => {
		if (hoverRafRef.current !== null) {
			cancelAnimationFrame(hoverRafRef.current);
			hoverRafRef.current = null;
		}
		pendingHoverRef.current = null;
	}, []);
	const flushSeekPreview = useCallback(() => {
		if (rafRef.current !== null) {
			cancelAnimationFrame(rafRef.current);
			rafRef.current = null;
		}
		const nextPercentage = pendingSeekRef.current;
		pendingSeekRef.current = null;
		if (nextPercentage !== null) {
			onSeekPreview?.(nextPercentage);
		}
	}, [onSeekPreview]);
	const previewSeek = useCallback(
		(percentage: number) => {
			const clampedPercentage = clampPercentage(percentage);
			lastSeekPercentageRef.current = clampedPercentage;
			setHoverPositionFromPercentage(clampedPercentage);
			scheduleSeekPreview(clampedPercentage);
		},
		[scheduleSeekPreview, setHoverPositionFromPercentage],
	);
	useEffect(() => {
		return () => {
			if (rafRef.current !== null) {
				cancelAnimationFrame(rafRef.current);
				rafRef.current = null;
			}
			pendingSeekRef.current = null;
			if (hoverRafRef.current !== null) {
				cancelAnimationFrame(hoverRafRef.current);
				hoverRafRef.current = null;
			}
			pendingHoverRef.current = null;
		};
	}, []);
	const handlePointerMove = useCallback(
		(e: React.PointerEvent<HTMLDivElement>) => {
			if (activePointerIdRef.current === e.pointerId) {
				e.preventDefault();
				previewSeek(getPercentageFromEvent(e.clientX));
				return;
			}
			if (e.pointerType !== 'touch') {
				const percentage = getPercentageFromEvent(e.clientX);
				setHoverPositionFromPercentage(percentage);
				scheduleSeekHover(percentage);
			}
		},
		[getPercentageFromEvent, previewSeek, scheduleSeekHover, setHoverPositionFromPercentage],
	);
	const handlePointerDown = useCallback(
		(e: React.PointerEvent<HTMLDivElement>) => {
			if (e.pointerType === 'mouse' && e.button !== 0) return;
			e.preventDefault();
			cacheContainerRect();
			activePointerIdRef.current = e.pointerId;
			e.currentTarget.setPointerCapture?.(e.pointerId);
			setIsDragging(true);
			onSeekStart?.();
			previewSeek(getPercentageFromEvent(e.clientX));
			flushSeekPreview();
		},
		[cacheContainerRect, flushSeekPreview, getPercentageFromEvent, onSeekStart, previewSeek],
	);
	const finishPointerSeek = useCallback(
		(e: React.PointerEvent<HTMLDivElement>, commit: boolean) => {
			if (activePointerIdRef.current !== e.pointerId) return;
			e.preventDefault();
			const percentage = getPercentageFromEvent(e.clientX);
			previewSeek(percentage);
			flushSeekPreview();
			if (commit) {
				onSeek?.(lastSeekPercentageRef.current);
			}
			activePointerIdRef.current = null;
			e.currentTarget.releasePointerCapture?.(e.pointerId);
			setIsDragging(false);
			if (e.pointerType === 'touch') {
				setHoverPosition(null);
			}
			clearCachedRect();
			onSeekEnd?.();
		},
		[clearCachedRect, flushSeekPreview, getPercentageFromEvent, onSeek, onSeekEnd, previewSeek],
	);
	const handlePointerLeave = useCallback(() => {
		if (!isDragging) {
			setHoverPosition(null);
			clearCachedRect();
			cancelPendingHover();
			onSeekHoverEnd?.();
		}
	}, [cancelPendingHover, clearCachedRect, isDragging, onSeekHoverEnd]);
	const handleKeyDown = useCallback(
		(e: React.KeyboardEvent) => {
			if (!duration) return;
			let newPercentage = clampPercentage(progress);
			const step = 5;
			switch (e.key) {
				case 'ArrowLeft':
					e.preventDefault();
					newPercentage = clampPercentage(progress - step);
					break;
				case 'ArrowRight':
					e.preventDefault();
					newPercentage = clampPercentage(progress + step);
					break;
				case 'Home':
					e.preventDefault();
					newPercentage = 0;
					break;
				case 'End':
					e.preventDefault();
					newPercentage = 100;
					break;
				default:
					return;
			}
			onSeekPreview?.(newPercentage);
			onSeek?.(newPercentage);
		},
		[progress, duration, onSeek, onSeekPreview],
	);
	const displayProgress = clampPercentage(isDragging && hoverPosition !== null ? hoverPosition : progress);
	const displayCurrentTime = duration > 0 ? (displayProgress / 100) * duration : currentTime;
	const committedProgressRef = useRef(displayProgress);
	const durationRef = useRef(duration);
	useLayoutEffect(() => {
		committedProgressRef.current = displayProgress;
		durationRef.current = duration;
		containerRef.current?.style.setProperty(PLAYHEAD_CSS_PROPERTY, `${displayProgress}%`);
	});
	const shouldPredictPlayhead = isPlaying && !isDragging && mediaRef != null;
	useEffect(() => {
		if (!shouldPredictPlayhead) return;
		const container = containerRef.current;
		const media = mediaRef?.current;
		if (!container || !media) return;
		let frame: number | null = null;
		let predictedSeconds: number | null = null;
		let lastFrameMs: number | null = null;
		const writeCommittedValue = () => {
			container.style.setProperty(PLAYHEAD_CSS_PROPERTY, `${committedProgressRef.current}%`);
		};
		const stop = () => {
			if (frame !== null) {
				cancelAnimationFrame(frame);
				frame = null;
			}
			predictedSeconds = null;
			lastFrameMs = null;
			writeCommittedValue();
		};
		const tick = () => {
			frame = null;
			if (document.hidden) {
				stop();
				return;
			}
			const nowMs = performance.now();
			const elapsedSeconds = lastFrameMs === null ? 0 : (nowMs - lastFrameMs) / 1000;
			lastFrameMs = nowMs;
			predictedSeconds = stepPlayheadPrediction({
				predictedSeconds,
				actualSeconds: media.currentTime,
				elapsedSeconds,
				playbackRate: media.playbackRate,
			});
			const totalSeconds = getEffectiveMediaDuration(media, durationRef.current);
			if (totalSeconds > 0) {
				container.style.setProperty(
					PLAYHEAD_CSS_PROPERTY,
					`${clampPercentage((predictedSeconds / totalSeconds) * 100)}%`,
				);
			}
			frame = requestAnimationFrame(tick);
		};
		const handleVisibilityChange = () => {
			if (document.hidden) {
				stop();
			} else if (frame === null) {
				frame = requestAnimationFrame(tick);
			}
		};
		document.addEventListener('visibilitychange', handleVisibilityChange);
		if (!document.hidden) {
			frame = requestAnimationFrame(tick);
		}
		return () => {
			document.removeEventListener('visibilitychange', handleVisibilityChange);
			stop();
		};
	}, [mediaRef, shouldPredictPlayhead]);
	const renderTooltipContent = useCallback(
		() => (
			<div className={styles.tooltipPreviewContent} data-flx="voice.media-player.media-progress-bar.tooltip-content">
				{previewThumbnail && (
					<div
						className={styles.tooltipThumbnail}
						style={tooltipThumbnailStyle}
						data-flx="voice.media-player.media-progress-bar.tooltip-thumbnail"
					>
						{previewThumbnail}
					</div>
				)}
				<div className={styles.tooltipTime} data-flx="voice.media-player.media-progress-bar.tooltip-time">
					{hoverTime}
				</div>
			</div>
		),
		[hoverTime, previewThumbnail, tooltipThumbnailStyle],
	);
	return (
		<div
			ref={containerRef}
			className={clsx(styles.container, compact && styles.compact, isDragging && styles.isDragging, className)}
			onPointerDown={handlePointerDown}
			onPointerMove={handlePointerMove}
			onPointerUp={(e) => finishPointerSeek(e, true)}
			onPointerCancel={(e) => finishPointerSeek(e, true)}
			onPointerLeave={handlePointerLeave}
			onKeyDown={handleKeyDown}
			role="slider"
			aria-label={ariaLabel || i18n._(MEDIA_PROGRESS_DESCRIPTOR)}
			aria-valuemin={0}
			aria-valuemax={100}
			aria-valuenow={Math.round(displayProgress)}
			aria-valuetext={formatDuration(displayCurrentTime)}
			tabIndex={0}
			data-playhead-predicted={shouldPredictPlayhead ? 'true' : undefined}
			data-flx="voice.media-player.media-progress-bar.container.key-down"
		>
			<div className={styles.track} data-flx="voice.media-player.media-progress-bar.track">
				{buffered.map((bufferedSpan, index) => (
					<div
						key={index}
						className={styles.buffered}
						style={{left: `${bufferedSpan.offset * 100}%`, width: `${bufferedSpan.span * 100}%`}}
						data-flx="voice.media-player.media-progress-bar.buffered"
					/>
				))}
				<div className={styles.fill} data-flx="voice.media-player.media-progress-bar.fill" />
			</div>
			<div className={styles.thumb} data-flx="voice.media-player.media-progress-bar.thumb" />
			{shouldRenderTooltip && (
				<div
					ref={tooltipAnchorRef}
					className={styles.tooltipAnchor}
					style={{left: `${hoverPosition ?? 0}%`}}
					aria-hidden="true"
					data-flx="voice.media-player.media-progress-bar.tooltip-anchor"
				/>
			)}
			<SliderTooltipPortal
				showTooltip={shouldRenderTooltip}
				shouldRender={showPreview}
				value={hoverPosition ?? 0}
				onValueRender={renderTooltipContent}
				tooltip={tooltip}
				data-flx="voice.media-player.media-progress-bar.slider-tooltip-portal"
			/>
		</div>
	);
}
