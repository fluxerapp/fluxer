// SPDX-License-Identifier: AGPL-3.0-or-later

import {VOLUME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getExtendedDocument} from '@app/features/platform/types/Browser';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Slider} from '@app/features/ui/components/Slider';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import {appZoomLayoutPx} from '@app/features/ui/utils/AppZoomUtils';
import styles from '@app/features/voice/components/media_player/MediaVerticalVolumeControl.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {SpeakerHighIcon, SpeakerLowIcon, SpeakerNoneIcon, SpeakerXIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';
import {createPortal} from 'react-dom';

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

function getActiveFullscreenElement(): Element | null {
	const doc = getExtendedDocument();
	return (
		document.fullscreenElement ||
		doc.webkitFullscreenElement ||
		doc.mozFullScreenElement ||
		doc.msFullscreenElement ||
		null
	);
}

interface MediaVerticalVolumeControlProps {
	volume: number;
	isMuted: boolean;
	onVolumeChange: (volume: number) => void;
	onToggleMute: () => void;
	iconSize?: number;
	className?: string;
	position?: 'above' | 'below';
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

const POPOUT_GAP = 8;

export function MediaVerticalVolumeControl({
	volume,
	isMuted,
	onVolumeChange,
	onToggleMute,
	iconSize = 18,
	className,
	position = 'above',
}: MediaVerticalVolumeControlProps) {
	const {i18n} = useLingui();
	const buttonRef = useRef<HTMLButtonElement>(null);
	const popoutRef = useRef<HTMLDivElement>(null);
	const [isHovered, setIsHovered] = useState(false);
	const [isDragging, setIsDragging] = useState(false);
	const closeTimerRef = useRef<NodeJS.Timeout | null>(null);
	const [popoutPosition, setPopoutPosition] = useState<{top: number; left: number; width: number} | null>(null);
	const portalHost = usePortalHost();
	const [portalTarget, setPortalTarget] = useState<Element | null>(() =>
		typeof document === 'undefined' ? null : document.body,
	);
	useEffect(() => {
		const updateTarget = () => {
			if (portalHost) {
				setPortalTarget(portalHost);
				return;
			}
			const fsElement = getActiveFullscreenElement();
			const button = buttonRef.current;
			if (fsElement && button && fsElement.contains(button)) {
				setPortalTarget(fsElement);
			} else {
				setPortalTarget(document.body);
			}
		};
		updateTarget();
		document.addEventListener('fullscreenchange', updateTarget);
		document.addEventListener('webkitfullscreenchange', updateTarget);
		document.addEventListener('mozfullscreenchange', updateTarget);
		document.addEventListener('MSFullscreenChange', updateTarget);
		return () => {
			document.removeEventListener('fullscreenchange', updateTarget);
			document.removeEventListener('webkitfullscreenchange', updateTarget);
			document.removeEventListener('mozfullscreenchange', updateTarget);
			document.removeEventListener('MSFullscreenChange', updateTarget);
		};
	}, [portalHost]);
	const Icon = getVolumeIcon(volume, isMuted);
	const displayVolume = isMuted ? 0 : volume;
	const isOpen = isHovered || isDragging;
	const updatePopoutPosition = useCallback(() => {
		const button = buttonRef.current;
		if (!button) return;
		const rect = button.getBoundingClientRect();
		const toLayoutPx = portalTarget === document.body ? appZoomLayoutPx : (value: number) => value;
		setPopoutPosition({
			top: position === 'below' ? toLayoutPx(rect.bottom) : toLayoutPx(rect.top),
			left: toLayoutPx(rect.left),
			width: toLayoutPx(rect.width),
		});
	}, [portalTarget, position]);
	useLayoutEffect(() => {
		if (isOpen) {
			updatePopoutPosition();
		}
	}, [isOpen, updatePopoutPosition]);
	useEffect(() => {
		if (!isOpen) {
			return;
		}
		const handleReposition = () => {
			updatePopoutPosition();
		};
		window.addEventListener('resize', handleReposition);
		window.addEventListener('scroll', handleReposition, true);
		return () => {
			window.removeEventListener('resize', handleReposition);
			window.removeEventListener('scroll', handleReposition, true);
		};
	}, [isOpen, updatePopoutPosition]);
	const cancelClose = useCallback(() => {
		if (closeTimerRef.current !== null) {
			clearTimeout(closeTimerRef.current);
			closeTimerRef.current = null;
		}
	}, []);
	const scheduleClose = useCallback(() => {
		cancelClose();
		closeTimerRef.current = setTimeout(() => {
			if (!isDragging) {
				setIsHovered(false);
			}
			closeTimerRef.current = null;
		}, 200);
	}, [cancelClose, isDragging]);
	const handleButtonMouseEnter = useCallback(() => {
		cancelClose();
		setIsHovered(true);
	}, [cancelClose]);
	const handleButtonMouseLeave = useCallback(() => {
		if (!isDragging) {
			scheduleClose();
		}
	}, [isDragging, scheduleClose]);
	const handlePopoutMouseEnter = useCallback(() => {
		cancelClose();
	}, [cancelClose]);
	const handlePopoutMouseLeave = useCallback(() => {
		if (!isDragging) {
			scheduleClose();
		}
	}, [isDragging, scheduleClose]);
	const handleSliderValueChange = useCallback(
		(nextValue: number) => {
			onVolumeChange(Math.max(0, Math.min(1, nextValue / 100)));
		},
		[onVolumeChange],
	);
	const handleSliderInteractionChange = useCallback(
		(nextIsDragging: boolean) => {
			if (nextIsDragging) {
				cancelClose();
				setIsHovered(true);
			}
			setIsDragging(nextIsDragging);
		},
		[cancelClose],
	);
	const handleMuteClick = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			onToggleMute();
		},
		[onToggleMute],
	);
	const handleButtonKeyDown = useCallback(
		(e: React.KeyboardEvent) => {
			const step = 0.1;
			let newVolume = volume;
			switch (e.key) {
				case 'ArrowUp':
				case 'ArrowRight':
					e.preventDefault();
					e.stopPropagation();
					newVolume = Math.min(1, volume + step);
					break;
				case 'ArrowDown':
				case 'ArrowLeft':
					e.preventDefault();
					e.stopPropagation();
					newVolume = Math.max(0, volume - step);
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
	useEffect(() => {
		return () => {
			cancelClose();
		};
	}, [cancelClose]);
	const muteLabel = isMuted ? i18n._(UNMUTE_DESCRIPTOR) : i18n._(MUTE_DESCRIPTOR);
	const sliderValue = displayVolume * 100;
	const volumePercent = Math.round(sliderValue);
	const popoutElement =
		isOpen &&
		popoutPosition &&
		portalTarget &&
		createPortal(
			<div
				ref={popoutRef}
				role="group"
				className={styles.popout}
				style={{
					position: 'fixed',
					top: position === 'below' ? popoutPosition.top + POPOUT_GAP : popoutPosition.top - POPOUT_GAP,
					left: popoutPosition.left,
					width: popoutPosition.width,
					...(position === 'above' ? {transform: 'translateY(-100%)'} : {}),
					zIndex: 'var(--z-index-popout, 3000)',
				}}
				onMouseEnter={handlePopoutMouseEnter}
				onMouseLeave={handlePopoutMouseLeave}
				data-flx="voice.media-player.media-vertical-volume-control.popout"
			>
				<div className={styles.sliderHost} data-flx="voice.media-player.media-vertical-volume-control.slider-host">
					<Slider
						defaultValue={sliderValue}
						factoryDefaultValue={100}
						minValue={0}
						maxValue={100}
						step={1}
						value={sliderValue}
						orientation="vertical"
						ariaLabel={i18n._(VOLUME_DESCRIPTOR)}
						ariaValueText={`${volumePercent}%`}
						onValueChange={handleSliderValueChange}
						onPointerInteractionChange={handleSliderInteractionChange}
						stopEventPropagation
						className={styles.volumeSlider}
						data-flx="voice.media-player.media-vertical-volume-control.volume-slider"
					/>
				</div>
			</div>,
			portalTarget,
		);
	return (
		<div
			className={clsx(styles.container, className)}
			role="group"
			aria-label={i18n._(VOLUME_CONTROL_DESCRIPTOR)}
			data-flx="voice.media-player.media-vertical-volume-control.container"
		>
			{popoutElement}
			<FocusRing offset={-2} data-flx="voice.media-player.media-vertical-volume-control.focus-ring">
				<button
					ref={buttonRef}
					type="button"
					onClick={handleMuteClick}
					onKeyDown={handleButtonKeyDown}
					onMouseEnter={handleButtonMouseEnter}
					onMouseLeave={handleButtonMouseLeave}
					className={styles.muteButton}
					aria-label={muteLabel}
					data-flx="voice.media-player.media-vertical-volume-control.mute-button.mute-click"
				>
					<Icon
						size={remFromPx(iconSize)}
						weight="fill"
						data-flx="voice.media-player.media-vertical-volume-control.icon"
					/>
				</button>
			</FocusRing>
		</div>
	);
}
