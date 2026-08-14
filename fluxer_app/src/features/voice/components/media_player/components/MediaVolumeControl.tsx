// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {VOLUME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/voice/components/media_player/MediaVolumeControl.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {SpeakerHighIcon, SpeakerLowIcon, SpeakerNoneIcon, SpeakerXIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const UNMUTE_DESCRIPTOR = msg({
	message: 'Unmute',
	comment: 'Mute toggle button label in the media player volume control (currently muted).',
});
const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	comment: 'Mute toggle button label in the media player volume control (currently unmuted).',
});
const VOLUME_CONTROL_DESCRIPTOR = msg({
	message: 'Volume control',
	comment: 'Aria label for the volume control container in the media player.',
});

interface MediaVolumeControlProps {
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
	const {i18n} = useLingui();
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
			if (e.button !== 0) return;
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
	const muteLabel = isMuted ? i18n._(UNMUTE_DESCRIPTOR) : i18n._(MUTE_DESCRIPTOR);
	const isKeyboardMode = KeyboardMode.keyboardModeEnabled;
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
			aria-label={i18n._(VOLUME_DESCRIPTOR)}
			aria-valuemin={0}
			aria-valuemax={100}
			aria-valuenow={Math.round(displayVolume * 100)}
			aria-valuetext={`${Math.round(displayVolume * 100)}%`}
			aria-orientation="horizontal"
			tabIndex={0}
			data-flx="voice.media-player.media-volume-control.slider"
		>
			<div className={styles.sliderTrack} data-flx="voice.media-player.media-volume-control.slider-track">
				<div
					className={styles.sliderFill}
					style={{width: `${displayVolume * 100}%`}}
					data-flx="voice.media-player.media-volume-control.slider-fill"
				/>
			</div>
			<div
				className={styles.sliderThumb}
				style={{left: `calc(6px + ${displayVolume * 100}% - ${displayVolume * 12}px)`}}
				data-flx="voice.media-player.media-volume-control.slider-thumb"
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
			aria-label={i18n._(VOLUME_CONTROL_DESCRIPTOR)}
			data-flx="voice.media-player.media-volume-control.container"
		>
			{isExpanded ? (
				<FocusRing offset={-2} data-flx="voice.media-player.media-volume-control.focus-ring">
					<button
						type="button"
						onClick={handleMuteClick}
						onKeyDown={handleButtonKeyDown}
						onFocus={handleFocus}
						onBlur={handleBlur}
						className={styles.muteButton}
						aria-label={muteLabel}
						data-flx="voice.media-player.media-volume-control.mute-button.mute-click"
					>
						<Icon size={remFromPx(iconSize)} weight="fill" data-flx="voice.media-player.media-volume-control.icon" />
					</button>
				</FocusRing>
			) : (
				<Tooltip
					text={muteLabel}
					position="top"
					openOnMountHover={false}
					data-flx="voice.media-player.media-volume-control.tooltip"
				>
					<FocusRing offset={-2} data-flx="voice.media-player.media-volume-control.focus-ring--2">
						<button
							type="button"
							onClick={handleMuteClick}
							onKeyDown={handleButtonKeyDown}
							onFocus={handleFocus}
							onBlur={handleBlur}
							className={styles.muteButton}
							aria-label={muteLabel}
							data-flx="voice.media-player.media-volume-control.mute-button.mute-click--2"
						>
							<Icon
								size={remFromPx(iconSize)}
								weight="fill"
								data-flx="voice.media-player.media-volume-control.icon--2"
							/>
						</button>
					</FocusRing>
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
						duration: Accessibility.useReducedMotion ? 0 : 0.2,
						ease: [0.4, 0, 0.2, 1],
					}}
					data-flx="voice.media-player.media-volume-control.slider-wrapper"
				>
					{sliderElement}
				</motion.div>
			) : (
				sliderElement
			)}
		</div>
	);
});
