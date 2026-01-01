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

import {useCallback, useEffect} from 'react';
import {SEEK_STEP, VOLUME_STEP} from '../utils/mediaConstants';

export interface UseMediaKeyboardOptions {
	containerRef: React.RefObject<HTMLElement | null>;
	enabled?: boolean;
	onTogglePlay?: () => void;
	onSeekBackward?: (amount: number) => void;
	onSeekForward?: (amount: number) => void;
	onVolumeUp?: (step: number) => void;
	onVolumeDown?: (step: number) => void;
	onToggleMute?: () => void;
	onToggleFullscreen?: () => void;
	onSeekPercentage?: (percentage: number) => void;
	seekAmount?: number;
	volumeStep?: number;
}

export interface UseMediaKeyboardReturn {
	handleKeyDown: (event: React.KeyboardEvent) => void;
}

function keyMatches(key: string, keys: ReadonlyArray<string>): boolean {
	return keys.includes(key);
}

const PLAY_PAUSE_KEYS = ['Space', 'k', 'K'] as const;
const SEEK_BACKWARD_KEYS = ['ArrowLeft', 'j', 'J'] as const;
const SEEK_FORWARD_KEYS = ['ArrowRight', 'l', 'L'] as const;
const VOLUME_UP_KEYS = ['ArrowUp'] as const;
const VOLUME_DOWN_KEYS = ['ArrowDown'] as const;
const MUTE_KEYS = ['m', 'M'] as const;
const FULLSCREEN_KEYS = ['f', 'F'] as const;
const SEEK_PERCENTAGE_KEYS = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] as const;

export function useMediaKeyboard(options: UseMediaKeyboardOptions): UseMediaKeyboardReturn {
	const {
		containerRef,
		enabled = true,
		onTogglePlay,
		onSeekBackward,
		onSeekForward,
		onVolumeUp,
		onVolumeDown,
		onToggleMute,
		onToggleFullscreen,
		onSeekPercentage,
		seekAmount = SEEK_STEP,
		volumeStep = VOLUME_STEP,
	} = options;

	const handleKeyDown = useCallback(
		(event: React.KeyboardEvent | KeyboardEvent) => {
			if (!enabled) return;

			const target = event.target as HTMLElement;
			if (target.tagName === 'INPUT' || target.tagName === 'TEXTAREA' || target.isContentEditable) {
				return;
			}

			const {key} = event;

			if (keyMatches(key, PLAY_PAUSE_KEYS)) {
				event.preventDefault();
				onTogglePlay?.();
			} else if (keyMatches(key, SEEK_BACKWARD_KEYS)) {
				event.preventDefault();
				onSeekBackward?.(seekAmount);
			} else if (keyMatches(key, SEEK_FORWARD_KEYS)) {
				event.preventDefault();
				onSeekForward?.(seekAmount);
			} else if (keyMatches(key, VOLUME_UP_KEYS)) {
				event.preventDefault();
				onVolumeUp?.(volumeStep);
			} else if (keyMatches(key, VOLUME_DOWN_KEYS)) {
				event.preventDefault();
				onVolumeDown?.(volumeStep);
			} else if (keyMatches(key, MUTE_KEYS)) {
				event.preventDefault();
				onToggleMute?.();
			} else if (keyMatches(key, FULLSCREEN_KEYS)) {
				event.preventDefault();
				onToggleFullscreen?.();
			} else if (keyMatches(key, SEEK_PERCENTAGE_KEYS)) {
				event.preventDefault();
				const percentage = parseInt(key, 10) * 10;
				onSeekPercentage?.(percentage);
			}
		},
		[
			enabled,
			onTogglePlay,
			onSeekBackward,
			onSeekForward,
			onVolumeUp,
			onVolumeDown,
			onToggleMute,
			onToggleFullscreen,
			onSeekPercentage,
			seekAmount,
			volumeStep,
		],
	);

	useEffect(() => {
		const container = containerRef.current;
		if (!container || !enabled) return;

		const handleContainerKeyDown = (event: KeyboardEvent) => {
			if (container.contains(document.activeElement)) {
				handleKeyDown(event);
			}
		};

		container.addEventListener('keydown', handleContainerKeyDown);

		return () => {
			container.removeEventListener('keydown', handleContainerKeyDown);
		};
	}, [containerRef, enabled, handleKeyDown]);

	return {
		handleKeyDown: handleKeyDown as (event: React.KeyboardEvent) => void,
	};
}
