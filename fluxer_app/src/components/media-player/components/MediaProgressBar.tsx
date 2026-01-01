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

import {FloatingPortal} from '@floating-ui/react';
import {computePosition, offset} from '@floating-ui/react-dom';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';
import {useTooltipPortalRoot} from '~/components/uikit/Tooltip';
import {formatTime} from '../utils/formatTime';
import styles from './MediaProgressBar.module.css';

export interface MediaProgressBarProps {
	progress: number;
	buffered?: number;
	currentTime?: number;
	duration?: number;
	isSeeking?: boolean;
	onSeek?: (percentage: number) => void;
	onSeekStart?: () => void;
	onSeekEnd?: () => void;
	showPreview?: boolean;
	className?: string;
	compact?: boolean;
	ariaLabel?: string;
}

export function MediaProgressBar({
	progress,
	buffered = 0,
	currentTime = 0,
	duration = 0,
	onSeek,
	onSeekStart,
	onSeekEnd,
	showPreview = true,
	className,
	compact = false,
	ariaLabel,
}: MediaProgressBarProps) {
	const {t} = useLingui();
	const containerRef = useRef<HTMLDivElement>(null);
	const thumbRef = useRef<HTMLDivElement>(null);
	const tooltipRef = useRef<HTMLDivElement>(null);
	const tooltipPortalRoot = useTooltipPortalRoot();

	const [isDragging, setIsDragging] = useState(false);
	const [hoverPosition, setHoverPosition] = useState<number | null>(null);
	const [hoverTime, setHoverTime] = useState<string>('0:00');
	const [tooltipPosition, setTooltipPosition] = useState({x: 0, y: 0, isReady: false});

	const rafRef = useRef<number | null>(null);

	const getPercentageFromEvent = useCallback((clientX: number): number => {
		const container = containerRef.current;
		if (!container) return 0;

		const rect = container.getBoundingClientRect();
		const x = clientX - rect.left;
		return Math.max(0, Math.min(100, (x / rect.width) * 100));
	}, []);

	const updateTooltipPosition = useCallback(async () => {
		if (hoverPosition === null || !containerRef.current || !tooltipRef.current) {
			return;
		}

		const container = containerRef.current;
		const tooltip = tooltipRef.current;
		const rect = container.getBoundingClientRect();

		const virtualElement = {
			getBoundingClientRect: () => ({
				x: rect.left + (hoverPosition / 100) * rect.width,
				y: rect.top,
				top: rect.top,
				left: rect.left + (hoverPosition / 100) * rect.width,
				bottom: rect.bottom,
				right: rect.left + (hoverPosition / 100) * rect.width,
				width: 0,
				height: rect.height,
			}),
		};

		try {
			const {x, y} = await computePosition(virtualElement, tooltip, {
				placement: 'top',
				middleware: [offset(8)],
			});

			setTooltipPosition({x, y, isReady: true});
		} catch (error) {
			console.error('Error positioning tooltip:', error);
		}
	}, [hoverPosition]);

	useEffect(() => {
		if (hoverPosition !== null && tooltipRef.current) {
			updateTooltipPosition();
		}
	}, [hoverPosition, updateTooltipPosition]);

	const handleMouseMove = useCallback(
		(e: MouseEvent | React.MouseEvent) => {
			const percentage = getPercentageFromEvent(e.clientX);
			setHoverPosition(percentage);

			if (duration > 0) {
				const time = (percentage / 100) * duration;
				setHoverTime(formatTime(time));
			}

			if (isDragging && onSeek) {
				if (rafRef.current !== null) {
					cancelAnimationFrame(rafRef.current);
				}
				rafRef.current = requestAnimationFrame(() => {
					onSeek(percentage);
					rafRef.current = null;
				});
			}
		},
		[getPercentageFromEvent, duration, isDragging, onSeek],
	);

	const handleMouseDown = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			const percentage = getPercentageFromEvent(e.clientX);

			setIsDragging(true);
			onSeekStart?.();
			onSeek?.(percentage);
		},
		[getPercentageFromEvent, onSeekStart, onSeek],
	);

	const handleMouseUp = useCallback(() => {
		if (isDragging) {
			setIsDragging(false);
			onSeekEnd?.();
		}
	}, [isDragging, onSeekEnd]);

	const handleMouseLeave = useCallback(() => {
		if (!isDragging) {
			setHoverPosition(null);
			setTooltipPosition((prev) => ({...prev, isReady: false}));
		}
	}, [isDragging]);

	const handleTouchStart = useCallback(
		(e: React.TouchEvent) => {
			const touch = e.touches[0];
			const percentage = getPercentageFromEvent(touch.clientX);

			setIsDragging(true);
			onSeekStart?.();
			onSeek?.(percentage);
		},
		[getPercentageFromEvent, onSeekStart, onSeek],
	);

	const handleTouchMove = useCallback(
		(e: TouchEvent) => {
			const touch = e.touches[0];
			const percentage = getPercentageFromEvent(touch.clientX);

			setHoverPosition(percentage);
			onSeek?.(percentage);
		},
		[getPercentageFromEvent, onSeek],
	);

	const handleTouchEnd = useCallback(() => {
		setIsDragging(false);
		setHoverPosition(null);
		onSeekEnd?.();
	}, [onSeekEnd]);

	useEffect(() => {
		if (isDragging) {
			const handleGlobalMouseMove = (e: MouseEvent) => handleMouseMove(e);
			const handleGlobalMouseUp = () => handleMouseUp();
			const handleGlobalTouchMove = (e: TouchEvent) => handleTouchMove(e);
			const handleGlobalTouchEnd = () => handleTouchEnd();

			document.addEventListener('mousemove', handleGlobalMouseMove);
			document.addEventListener('mouseup', handleGlobalMouseUp);
			document.addEventListener('touchmove', handleGlobalTouchMove);
			document.addEventListener('touchend', handleGlobalTouchEnd);

			return () => {
				document.removeEventListener('mousemove', handleGlobalMouseMove);
				document.removeEventListener('mouseup', handleGlobalMouseUp);
				document.removeEventListener('touchmove', handleGlobalTouchMove);
				document.removeEventListener('touchend', handleGlobalTouchEnd);

				if (rafRef.current !== null) {
					cancelAnimationFrame(rafRef.current);
					rafRef.current = null;
				}
			};
		}
		return;
	}, [isDragging, handleMouseMove, handleMouseUp, handleTouchMove, handleTouchEnd]);

	const handleKeyDown = useCallback(
		(e: React.KeyboardEvent) => {
			if (!duration) return;

			let newPercentage = progress;
			const step = 5;

			switch (e.key) {
				case 'ArrowLeft':
					e.preventDefault();
					newPercentage = Math.max(0, progress - step);
					break;
				case 'ArrowRight':
					e.preventDefault();
					newPercentage = Math.min(100, progress + step);
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

			onSeek?.(newPercentage);
		},
		[progress, duration, onSeek],
	);

	const displayProgress = isDragging && hoverPosition !== null ? hoverPosition : progress;

	return (
		<div
			ref={containerRef}
			className={clsx(styles.container, compact && styles.compact, isDragging && styles.isDragging, className)}
			onMouseDown={handleMouseDown}
			onMouseMove={(e) => handleMouseMove(e)}
			onMouseLeave={handleMouseLeave}
			onTouchStart={handleTouchStart}
			onKeyDown={handleKeyDown}
			role="slider"
			aria-label={ariaLabel || t`Media progress`}
			aria-valuemin={0}
			aria-valuemax={100}
			aria-valuenow={Math.round(progress)}
			aria-valuetext={formatTime(currentTime)}
			tabIndex={0}
		>
			<div className={styles.track}>
				<div className={styles.buffered} style={{width: `${buffered}%`}} />
				<div className={styles.fill} style={{width: `${displayProgress}%`}} />
			</div>
			<div ref={thumbRef} className={styles.thumb} style={{left: `${displayProgress}%`}} />

			{showPreview && hoverPosition !== null && (
				<FloatingPortal root={tooltipPortalRoot}>
					<AnimatePresence mode="wait">
						<motion.div
							key="progress-tooltip"
							ref={tooltipRef}
							className={styles.tooltip}
							style={{
								position: 'fixed',
								left: tooltipPosition.x,
								top: tooltipPosition.y,
								visibility: tooltipPosition.isReady ? 'visible' : 'hidden',
							}}
							initial={{opacity: 0}}
							animate={{opacity: 1}}
							exit={{opacity: 0}}
							transition={{
								opacity: {duration: 0.1},
							}}
						>
							{hoverTime}
							<div className={styles.tooltipArrow} />
						</motion.div>
					</AnimatePresence>
				</FloatingPortal>
			)}
		</div>
	);
}
