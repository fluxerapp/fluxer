// SPDX-License-Identifier: AGPL-3.0-or-later

import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import FocusRingContext from '@app/features/ui/focus_ring/FocusRingContext';
import styles from '@app/features/voice/components/VoiceCallView.module.css';
import VoiceCallLayout from '@app/features/voice/state/VoiceCallLayout';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback, useContext, useLayoutEffect, useRef, useState} from 'react';

const RESIZE_MEMBERS_ROW_DESCRIPTOR = msg({
	message: 'Resize members row',
	comment:
		'Accessible label for the draggable divider between a focused stream and the row of call members below it. Dragging it changes how tall the stream is.',
});
const MEMBERS_ROW_HEIGHT_VAR = '--voice-focus-members-row-user-height';
const MEMBERS_ROW_MIN_HEIGHT = 64;
const MEMBERS_ROW_MAX_FRACTION = 0.4;
const KEYBOARD_STEP = 16;
const KEYBOARD_STEP_LARGE = 64;

interface DragState {
	pointerId: number;
	startY: number;
	startHeight: number;
	latest: number;
}

interface FocusMembersRowDividerProps {
	rowRef: React.RefObject<HTMLElement | null>;
}

function getLayout(dividerRef: React.RefObject<HTMLElement | null>): HTMLElement | null {
	return dividerRef.current?.parentElement ?? null;
}

function getMaxHeight(layout: HTMLElement | null): number {
	if (!layout) return MEMBERS_ROW_MIN_HEIGHT;
	return Math.max(MEMBERS_ROW_MIN_HEIGHT, Math.floor(layout.clientHeight * MEMBERS_ROW_MAX_FRACTION));
}

function clampHeight(height: number, layout: HTMLElement | null): number {
	return Math.max(MEMBERS_ROW_MIN_HEIGHT, Math.min(Math.round(height), getMaxHeight(layout)));
}

export function FocusMembersRowDivider({rowRef}: FocusMembersRowDividerProps) {
	const {i18n} = useLingui();
	const dividerRef = useRef<HTMLDivElement | null>(null);
	const dragRef = useRef<DragState | null>(null);
	const ringContext = useContext(FocusRingContext);
	const [isDragging, setIsDragging] = useState(false);
	const [ariaValue, setAriaValue] = useState({now: MEMBERS_ROW_MIN_HEIGHT, max: MEMBERS_ROW_MIN_HEIGHT});
	const measureRowHeight = useCallback(() => Math.round(rowRef.current?.getBoundingClientRect().height ?? 0), [rowRef]);
	const syncAriaValue = useCallback(() => {
		setAriaValue({now: measureRowHeight(), max: getMaxHeight(getLayout(dividerRef))});
	}, [measureRowHeight]);
	useLayoutEffect(() => {
		syncAriaValue();
	}, [syncAriaValue]);
	const commitHeight = useCallback(
		(height: number | null) => {
			VoiceCallLayout.setFocusMembersRowHeight(height);
			requestAnimationFrame(() => {
				syncAriaValue();
				ringContext.invalidate();
			});
		},
		[ringContext, syncAriaValue],
	);
	const handlePointerDown = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			if (!event.isPrimary || event.button !== 0) return;
			event.preventDefault();
			event.stopPropagation();
			const startHeight = measureRowHeight();
			dragRef.current = {pointerId: event.pointerId, startY: event.clientY, startHeight, latest: startHeight};
			event.currentTarget.setPointerCapture(event.pointerId);
			setIsDragging(true);
		},
		[measureRowHeight],
	);
	const handlePointerMove = useCallback((event: React.PointerEvent<HTMLDivElement>) => {
		const state = dragRef.current;
		if (!state || state.pointerId !== event.pointerId) return;
		event.preventDefault();
		const layout = getLayout(dividerRef);
		state.latest = clampHeight(state.startHeight + state.startY - event.clientY, layout);
		layout?.style.setProperty(MEMBERS_ROW_HEIGHT_VAR, `${state.latest}px`);
	}, []);
	const handlePointerEnd = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			const state = dragRef.current;
			if (!state || state.pointerId !== event.pointerId) return;
			dragRef.current = null;
			setIsDragging(false);
			if (event.currentTarget.hasPointerCapture(event.pointerId)) {
				event.currentTarget.releasePointerCapture(event.pointerId);
			}
			if (state.latest !== state.startHeight) {
				commitHeight(state.latest);
			} else if (VoiceCallLayout.focusMembersRowHeight == null) {
				getLayout(dividerRef)?.style.removeProperty(MEMBERS_ROW_HEIGHT_VAR);
			}
		},
		[commitHeight],
	);
	const handleKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLDivElement>) => {
			const layout = getLayout(dividerRef);
			const step = event.shiftKey ? KEYBOARD_STEP_LARGE : KEYBOARD_STEP;
			let next: number | null;
			switch (event.key) {
				case 'ArrowUp':
					next = clampHeight(measureRowHeight() + step, layout);
					break;
				case 'ArrowDown':
					next = clampHeight(measureRowHeight() - step, layout);
					break;
				case 'Home':
					next = MEMBERS_ROW_MIN_HEIGHT;
					break;
				case 'End':
					next = getMaxHeight(layout);
					break;
				case 'Enter':
					next = null;
					break;
				default:
					return;
			}
			event.preventDefault();
			event.stopPropagation();
			commitHeight(next);
		},
		[commitHeight, measureRowHeight],
	);
	const handleDoubleClick = useCallback(
		(event: React.MouseEvent<HTMLDivElement>) => {
			event.preventDefault();
			event.stopPropagation();
			commitHeight(null);
		},
		[commitHeight],
	);
	return (
		<FocusRing offset={-2} data-flx="voice.focus-members-row-divider.focus-ring">
			<div
				ref={dividerRef}
				role="separator"
				tabIndex={0}
				aria-orientation="horizontal"
				aria-label={i18n._(RESIZE_MEMBERS_ROW_DESCRIPTOR)}
				aria-valuemin={MEMBERS_ROW_MIN_HEIGHT}
				aria-valuemax={ariaValue.max}
				aria-valuenow={ariaValue.now}
				className={styles.focusMembersRowDivider}
				onPointerDown={handlePointerDown}
				onPointerMove={handlePointerMove}
				onPointerUp={handlePointerEnd}
				onPointerCancel={handlePointerEnd}
				onLostPointerCapture={handlePointerEnd}
				onKeyDown={handleKeyDown}
				onDoubleClick={handleDoubleClick}
				data-dragging={isDragging}
				data-flx="voice.focus-members-row-divider"
			>
				<span
					className={styles.focusMembersRowDividerLine}
					aria-hidden
					data-flx="voice.focus-members-row-divider.line"
				/>
			</div>
		</FocusRing>
	);
}
