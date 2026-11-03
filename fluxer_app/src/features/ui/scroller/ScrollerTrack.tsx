// SPDX-License-Identifier: AGPL-3.0-or-later

import {isTouchDevice} from '@app/features/app/hooks/usePressable';
import styles from '@app/features/ui/scroller/ScrollerTrack.module.css';
import {clsx} from 'clsx';
import type {PointerEvent as ReactPointerEvent, WheelEvent as ReactWheelEvent, RefCallback} from 'react';

type ScrollAxis = 'vertical' | 'horizontal';

interface ScrollerTrackProps {
	orientation: ScrollAxis;
	scrollbar: 'thin' | 'regular';
	hasTrack: boolean;
	isVisible: boolean;
	isDragging: boolean;
	trackRef: RefCallback<HTMLDivElement>;
	thumbRef: RefCallback<HTMLDivElement>;
	onTrackPointerDown: (event: ReactPointerEvent<HTMLDivElement>) => void;
	onThumbPointerDown: (event: ReactPointerEvent<HTMLDivElement>) => void;
	onWheel?: (event: ReactWheelEvent<HTMLDivElement>) => void;
}

export function ScrollerTrack({
	orientation,
	scrollbar,
	hasTrack,
	isVisible,
	isDragging,
	trackRef,
	thumbRef,
	onTrackPointerDown,
	onThumbPointerDown,
	onWheel,
}: ScrollerTrackProps) {
	if (!hasTrack) {
		return null;
	}
	const trackClassName = clsx(styles.track, {
		[styles.vertical]: orientation === 'vertical',
		[styles.horizontal]: orientation === 'horizontal',
		[styles.regular]: scrollbar === 'regular',
		[styles.visible]: isVisible,
	});
	const thumbClassName = clsx(styles.thumb, {[styles.active]: isDragging});
	const trackPointerEvents = !isTouchDevice && isVisible ? 'auto' : 'none';
	return (
		<div
			ref={trackRef}
			className={trackClassName}
			onPointerDown={onTrackPointerDown}
			onWheel={onWheel}
			style={{pointerEvents: trackPointerEvents}}
			role="presentation"
			data-sheet-drag-disabled="true"
			data-flx="ui.scroller.scroller-track.presentation.track-pointer-down"
		>
			<div
				ref={thumbRef}
				className={thumbClassName}
				onPointerDown={onThumbPointerDown}
				data-scroller-thumb="true"
				data-sheet-drag-disabled="true"
				role="presentation"
				data-flx="ui.scroller.scroller-track.presentation.thumb-pointer-down"
			/>
		</div>
	);
}
