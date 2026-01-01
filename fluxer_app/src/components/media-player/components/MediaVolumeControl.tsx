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
import {SpeakerHighIcon, SpeakerLowIcon, SpeakerNoneIcon, SpeakerXIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import KeyboardModeStore from '~/stores/KeyboardModeStore';
import styles from './MediaVolumeControl.module.css';

export interface MediaVolumeControlProps {
	volume: number;
	isMuted: boolean;
	onVolumeChange: (volume: number) => void;
	onToggleMute: () => void;
	expandable?: boolean;
	iconSize?: number;
	compact?: boolean;
	className?: string;
}

function getVolumeIcon(volume: number, isMuted: boolean) {
	if (isMuted || volume === 0) {
		return SpeakerXIcon;
	}
	if (volume < 0.33) {
		return SpeakerNoneIcon;
	}
	if (volume < 0.67) {
		return SpeakerLowIcon;
	}
	return SpeakerHighIcon;
}

export const MediaVolumeControl = observer(function MediaVolumeControl({
	volume,
	isMuted,
	onVolumeChange,
	onToggleMute,
	expandable = false,
	iconSize = 20,
	compact = false,
	className,
}: MediaVolumeControlProps) {
	const {t} = useLingui();
	const containerRef = useRef<HTMLDivElement>(null);
	const sliderRef = useRef<HTMLDivElement>(null);
	const [isDragging, setIsDragging] = useState(false);
	const [isHovered, setIsHovered] = useState(false);
	const [isFocused, setIsFocused] = useState(false);
	const rafRef = useRef<number | null>(null);

	const Icon = getVolumeIcon(volume, isMuted);
	const displayVolume = isMuted ? 0 : volume;

	const getVolumeFromEvent = useCallback((clientX: number): number => {
		const slider = sliderRef.current;
		if (!slider) return 0;

		const rect = slider.getBoundingClientRect();
		const x = clientX - rect.left;
		return Math.max(0, Math.min(1, x / rect.width));
	}, []);

	const handleMuteClick = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			onToggleMute();
		},
		[onToggleMute],
	);

	const handleSliderMouseDown = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			const newVolume = getVolumeFromEvent(e.clientX);
			setIsDragging(true);
			onVolumeChange(newVolume);
		},
		[getVolumeFromEvent, onVolumeChange],
	);

	const handleSliderMouseMove = useCallback(
		(e: MouseEvent) => {
			if (!isDragging) return;

			if (rafRef.current !== null) {
				cancelAnimationFrame(rafRef.current);
			}

			rafRef.current = requestAnimationFrame(() => {
				const newVolume = getVolumeFromEvent(e.clientX);
				onVolumeChange(newVolume);
				rafRef.current = null;
			});
		},
		[isDragging, getVolumeFromEvent, onVolumeChange],
	);

	const handleSliderMouseUp = useCallback(() => {
		setIsDragging(false);
	}, []);

	const handleSliderTouchStart = useCallback(
		(e: React.TouchEvent) => {
			e.stopPropagation();
			const touch = e.touches[0];
			const newVolume = getVolumeFromEvent(touch.clientX);
			setIsDragging(true);
			onVolumeChange(newVolume);
		},
		[getVolumeFromEvent, onVolumeChange],
	);

	const handleSliderTouchMove = useCallback(
		(e: TouchEvent) => {
			if (!isDragging) return;
			const touch = e.touches[0];
			const newVolume = getVolumeFromEvent(touch.clientX);
			onVolumeChange(newVolume);
		},
		[isDragging, getVolumeFromEvent, onVolumeChange],
	);

	const handleSliderTouchEnd = useCallback(() => {
		setIsDragging(false);
	}, []);

	const handleSliderKeyDown = useCallback(
		(e: React.KeyboardEvent) => {
			let newVolume = volume;
			const step = 0.1;

			switch (e.key) {
				case 'ArrowLeft':
				case 'ArrowDown':
					e.preventDefault();
					newVolume = Math.max(0, volume - step);
					break;
				case 'ArrowRight':
				case 'ArrowUp':
					e.preventDefault();
					newVolume = Math.min(1, volume + step);
					break;
				case 'Home':
					e.preventDefault();
					newVolume = 0;
					break;
				case 'End':
					e.preventDefault();
					newVolume = 1;
					break;
				default:
					return;
			}

			onVolumeChange(newVolume);
		},
		[volume, onVolumeChange],
	);

	useEffect(() => {
		if (isDragging) {
			document.addEventListener('mousemove', handleSliderMouseMove);
			document.addEventListener('mouseup', handleSliderMouseUp);
			document.addEventListener('touchmove', handleSliderTouchMove);
			document.addEventListener('touchend', handleSliderTouchEnd);

			return () => {
				document.removeEventListener('mousemove', handleSliderMouseMove);
				document.removeEventListener('mouseup', handleSliderMouseUp);
				document.removeEventListener('touchmove', handleSliderTouchMove);
				document.removeEventListener('touchend', handleSliderTouchEnd);

				if (rafRef.current !== null) {
					cancelAnimationFrame(rafRef.current);
					rafRef.current = null;
				}
			};
		}
		return;
	}, [isDragging, handleSliderMouseMove, handleSliderMouseUp, handleSliderTouchMove, handleSliderTouchEnd]);

	const muteLabel = isMuted ? t`Unmute` : t`Mute`;
	const isKeyboardMode = KeyboardModeStore.keyboardModeEnabled;
	const isExpanded = !expandable || isHovered || isDragging || (isFocused && isKeyboardMode);
	const sliderWidth = compact ? 62 : 72;

	const handleButtonKeyDown = useCallback(
		(e: React.KeyboardEvent) => {
			const step = 0.1;
			let newVolume = volume;

			switch (e.key) {
				case 'ArrowLeft':
				case 'ArrowDown':
					e.preventDefault();
					e.stopPropagation();
					newVolume = Math.max(0, volume - step);
					break;
				case 'ArrowRight':
				case 'ArrowUp':
					e.preventDefault();
					e.stopPropagation();
					newVolume = Math.min(1, volume + step);
					break;
				case 'm':
				case 'M':
					e.preventDefault();
					e.stopPropagation();
					onToggleMute();
					return;
				default:
					return;
			}

			onVolumeChange(newVolume);
		},
		[volume, onVolumeChange, onToggleMute],
	);

	const handleFocus = useCallback(() => setIsFocused(true), []);
	const handleBlur = useCallback(() => setIsFocused(false), []);

	const sliderElement = (
		<div
			ref={sliderRef}
			className={clsx(styles.slider, isDragging && styles.isDragging)}
			onMouseDown={handleSliderMouseDown}
			onTouchStart={handleSliderTouchStart}
			onKeyDown={handleSliderKeyDown}
			role="slider"
			aria-label={t`Volume`}
			aria-valuemin={0}
			aria-valuemax={100}
			aria-valuenow={Math.round(displayVolume * 100)}
			aria-valuetext={`${Math.round(displayVolume * 100)}%`}
			aria-orientation="horizontal"
			tabIndex={0}
		>
			<div className={styles.sliderTrack}>
				<div className={styles.sliderFill} style={{width: `${displayVolume * 100}%`}} />
			</div>
			<div
				className={styles.sliderThumb}
				style={{left: `calc(6px + ${displayVolume * 100}% - ${displayVolume * 12}px)`}}
			/>
		</div>
	);

	return (
		<div
			ref={containerRef}
			className={clsx(styles.container, compact && styles.compact, className)}
			onMouseEnter={() => setIsHovered(true)}
			onMouseLeave={() => !isDragging && setIsHovered(false)}
			role="group"
			aria-label={t`Volume Control`}
		>
			{isExpanded ? (
				<button
					type="button"
					onClick={handleMuteClick}
					onKeyDown={handleButtonKeyDown}
					onFocus={handleFocus}
					onBlur={handleBlur}
					className={styles.muteButton}
					aria-label={muteLabel}
				>
					<Icon size={iconSize} weight="fill" />
				</button>
			) : (
				<Tooltip text={muteLabel} position="top">
					<button
						type="button"
						onClick={handleMuteClick}
						onKeyDown={handleButtonKeyDown}
						onFocus={handleFocus}
						onBlur={handleBlur}
						className={styles.muteButton}
						aria-label={muteLabel}
					>
						<Icon size={iconSize} weight="fill" />
					</button>
				</Tooltip>
			)}

			{expandable ? (
				<motion.div
					className={styles.sliderWrapper}
					initial={false}
					animate={{
						width: isExpanded ? sliderWidth : 0,
						opacity: isExpanded ? 1 : 0,
					}}
					transition={{
						duration: 0.2,
						ease: [0.4, 0, 0.2, 1],
					}}
				>
					{sliderElement}
				</motion.div>
			) : (
				sliderElement
			)}
		</div>
	);
});
