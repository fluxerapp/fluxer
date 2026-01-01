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

import {useCallback, useRef, useState} from 'react';

export interface UseControlsVisibilityOptions {
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
	const {disabled = false, isPlaying = false, isInteracting = false} = options;

	const [controlsVisible, setControlsVisible] = useState(true);
	const isHoveredRef = useRef(false);

	const shouldShowControls = disabled || !isPlaying || isHoveredRef.current || isInteracting;

	const showControls = useCallback(() => {
		setControlsVisible(true);
	}, []);

	const hideControls = useCallback(() => {
		setControlsVisible(false);
	}, []);

	const handleMouseMove = useCallback(() => {
		if (!controlsVisible) {
			setControlsVisible(true);
		}
	}, [controlsVisible]);

	const handleMouseEnter = useCallback(() => {
		isHoveredRef.current = true;
		setControlsVisible(true);
	}, []);

	const handleMouseLeave = useCallback(() => {
		isHoveredRef.current = false;
		if (isPlaying && !isInteracting) {
			setControlsVisible(false);
		}
	}, [isPlaying, isInteracting]);

	const handleTouchStart = useCallback(() => {
		if (isPlaying && !isInteracting) {
			setControlsVisible((prev) => !prev);
		} else {
			setControlsVisible(true);
		}
	}, [isPlaying, isInteracting]);

	const finalVisible = shouldShowControls || controlsVisible;

	return {
		controlsVisible: finalVisible,
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
