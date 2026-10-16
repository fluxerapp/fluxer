// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createMediaControlsVisibilitySnapshot,
	getMediaControlsVisibilityValue,
	type MediaControlsVisibilityEvent,
	type MediaControlsVisibilitySignals,
	selectMediaControlsVisible,
	transitionMediaControlsVisibilitySnapshot,
} from '@app/features/voice/components/media_player/MediaControlsVisibilityStateMachine';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

export const CONTROLS_AUTOHIDE_DELAY_MS = 3000;
export const CONTROLS_AUTOHIDE_FULLSCREEN_DELAY_MS = 1000;

interface UseControlsVisibilityOptions {
	autohideDelay?: number;
	disabled?: boolean;
	isPlaying?: boolean;
	isInteracting?: boolean;
}

export interface UseControlsVisibilityReturn {
	controlsVisible: boolean;
	showControls: () => void;
	hideControls: () => void;
	containerProps: {
		onMouseMove: () => void;
		onMouseEnter: () => void;
		onMouseLeave: () => void;
		onTouchStart: () => void;
	};
}

export function useControlsVisibility(options: UseControlsVisibilityOptions = {}): UseControlsVisibilityReturn {
	const {
		autohideDelay = CONTROLS_AUTOHIDE_DELAY_MS,
		disabled = false,
		isPlaying = false,
		isInteracting = false,
	} = options;
	const signals = useMemo<MediaControlsVisibilitySignals>(
		() => ({disabled, isPlaying, isInteracting}),
		[disabled, isPlaying, isInteracting],
	);
	const [snapshot, setSnapshot] = useState(createMediaControlsVisibilitySnapshot);
	const send = useCallback((event: MediaControlsVisibilityEvent) => {
		setSnapshot((currentSnapshot) => transitionMediaControlsVisibilitySnapshot(currentSnapshot, event));
	}, []);
	const hideTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);
	const clearHideTimer = useCallback(() => {
		if (hideTimerRef.current === null) return;
		clearTimeout(hideTimerRef.current);
		hideTimerRef.current = null;
	}, []);
	const armHideTimer = useCallback(() => {
		clearHideTimer();
		if (disabled || !isPlaying || isInteracting) return;
		hideTimerRef.current = setTimeout(() => {
			hideTimerRef.current = null;
			send({type: 'controls.hide'});
		}, autohideDelay);
	}, [autohideDelay, clearHideTimer, disabled, isInteracting, isPlaying, send]);
	useEffect(() => {
		if (disabled || !isPlaying || isInteracting) {
			clearHideTimer();
			send({type: 'controls.show'});
			return clearHideTimer;
		}
		armHideTimer();
		return clearHideTimer;
	}, [armHideTimer, clearHideTimer, disabled, isInteracting, isPlaying, send]);
	const showControls = useCallback(() => {
		send({type: 'controls.show'});
		armHideTimer();
	}, [armHideTimer, send]);
	const hideControls = useCallback(() => {
		clearHideTimer();
		send({type: 'controls.hide'});
	}, [clearHideTimer, send]);
	const visibilityValue = getMediaControlsVisibilityValue(snapshot);
	const handleMouseMove = useCallback(() => {
		if (visibilityValue === 'hidden') {
			send({type: 'controls.mouseMove'});
		}
		armHideTimer();
	}, [armHideTimer, send, visibilityValue]);
	const handleMouseEnter = useCallback(() => {
		send({type: 'controls.mouseEnter'});
		armHideTimer();
	}, [armHideTimer, send]);
	const handleMouseLeave = useCallback(() => {
		clearHideTimer();
		send({type: 'controls.mouseLeave', signals});
	}, [clearHideTimer, send, signals]);
	const handleTouchStart = useCallback(() => {
		send({type: 'controls.touchStart', signals});
		armHideTimer();
	}, [armHideTimer, send, signals]);
	const controlsVisible = selectMediaControlsVisible(snapshot, signals);
	return {
		controlsVisible,
		showControls,
		hideControls,
		containerProps: {
			onMouseMove: handleMouseMove,
			onMouseEnter: handleMouseEnter,
			onMouseLeave: handleMouseLeave,
			onTouchStart: handleTouchStart,
		},
	};
}
