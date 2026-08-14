// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/SidebarResizeHandle.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import SidebarWidth, {
	SIDEBAR_WIDTH_DEFAULT,
	SIDEBAR_WIDTH_MAX,
	SIDEBAR_WIDTH_MIN,
} from '@app/features/ui/state/SidebarWidth';
import {getAppZoomFactor} from '@app/features/ui/utils/AppZoomUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

const RESIZE_SIDEBAR_HANDLE_DESCRIPTOR = msg({
	message: 'Resize sidebar',
	comment: 'Accessible label for the draggable handle that resizes the channel or direct message sidebar.',
});
const RESIZING_CLASS = 'sidebar-resizing';
const KEYBOARD_STEP = 12;
const KEYBOARD_STEP_LARGE = 48;

interface DragState {
	pointerId: number;
	startX: number;
	startWidth: number;
	latest: number;
	zoom: number;
	handle: HTMLElement;
	cleanup: () => void;
}

function resolveZoomFactor(ownerDocument: Document): number {
	const zoomFactor = getAppZoomFactor(ownerDocument);
	if (!Number.isFinite(zoomFactor) || zoomFactor <= 0) return 1;
	return zoomFactor;
}

function clampWidth(value: number): number {
	return Math.max(SIDEBAR_WIDTH_MIN, Math.min(Math.round(value), SIDEBAR_WIDTH_MAX));
}

function resolveSidebarWidth(): number {
	const width = SidebarWidth.width;
	if (width == null) return SIDEBAR_WIDTH_DEFAULT;
	return width;
}

function measureSidebarWidth(handle: HTMLElement): number {
	const frame = handle.parentElement;
	if (frame == null) return resolveSidebarWidth();
	const handleRect = handle.getBoundingClientRect();
	const frameRect = frame.getBoundingClientRect();
	const measured = handleRect.left + handleRect.width / 2 - frameRect.left;
	if (!Number.isFinite(measured) || measured <= 0) return resolveSidebarWidth();
	return clampWidth(measured / resolveZoomFactor(handle.ownerDocument));
}

function resolveCurrentSidebarWidth(handle: HTMLElement): number {
	const storedWidth = SidebarWidth.width;
	if (storedWidth != null) return storedWidth;
	return measureSidebarWidth(handle);
}

function shouldResetSidebarWidth(key: string): boolean {
	return key === 'Enter' || key === 'Backspace' || key === 'Delete';
}

export const SidebarResizeHandle = observer(() => {
	const {i18n} = useLingui();
	const dragRef = useRef<DragState | null>(null);
	const handleRef = useRef<HTMLDivElement | null>(null);
	const configuredWidth = resolveSidebarWidth();
	const [ariaValueNow, setAriaValueNow] = useState(configuredWidth);

	const finalizeDrag = useCallback((persistWidth: boolean) => {
		const state = dragRef.current;
		if (state == null) return;
		dragRef.current = null;
		state.cleanup();
		state.handle.ownerDocument.documentElement.classList.remove(RESIZING_CLASS);
		if (state.handle.hasPointerCapture(state.pointerId)) {
			state.handle.releasePointerCapture(state.pointerId);
		}
		if (persistWidth) SidebarWidth.setWidth(state.latest, true);
	}, []);

	useEffect(() => {
		return () => finalizeDrag(false);
	}, [finalizeDrag]);

	useLayoutEffect(() => {
		const handle = handleRef.current;
		if (handle == null || dragRef.current != null) return;
		const measuredWidth = measureSidebarWidth(handle);
		setAriaValueNow((current) => (current === measuredWidth ? current : measuredWidth));
	}, [configuredWidth]);

	const onPointerDown = useCallback(
		(event: React.PointerEvent<HTMLElement>) => {
			if (!event.isPrimary || event.button !== 0) return;
			if (dragRef.current != null) {
				event.preventDefault();
				event.stopPropagation();
				return;
			}
			const handle = event.currentTarget;
			const ownerDocument = handle.ownerDocument;
			const ownerWindow = ownerDocument.defaultView;
			if (ownerWindow == null) return;
			event.preventDefault();
			event.stopPropagation();
			const startWidth = measureSidebarWidth(handle);
			const state: DragState = {
				pointerId: event.pointerId,
				startX: event.clientX,
				startWidth,
				latest: startWidth,
				zoom: resolveZoomFactor(ownerDocument),
				handle,
				cleanup: () => {},
			};
			const onDocumentPointerMove = (nativeEvent: PointerEvent) => {
				if (dragRef.current !== state || nativeEvent.pointerId !== state.pointerId) return;
				nativeEvent.preventDefault();
				const next = clampWidth(state.startWidth + (nativeEvent.clientX - state.startX) / state.zoom);
				state.latest = next;
				SidebarWidth.setWidth(next, false);
				setAriaValueNow(next);
			};
			const onDocumentPointerEnd = (nativeEvent: PointerEvent) => {
				if (dragRef.current !== state || nativeEvent.pointerId !== state.pointerId) return;
				nativeEvent.preventDefault();
				finalizeDrag(true);
			};
			const onWindowBlur = () => {
				if (dragRef.current === state) finalizeDrag(true);
			};
			state.cleanup = () => {
				ownerDocument.removeEventListener('pointermove', onDocumentPointerMove, true);
				ownerDocument.removeEventListener('pointerup', onDocumentPointerEnd, true);
				ownerDocument.removeEventListener('pointercancel', onDocumentPointerEnd, true);
				ownerWindow.removeEventListener('blur', onWindowBlur);
			};
			dragRef.current = state;
			ownerDocument.addEventListener('pointermove', onDocumentPointerMove, {capture: true, passive: false});
			ownerDocument.addEventListener('pointerup', onDocumentPointerEnd, {capture: true, passive: false});
			ownerDocument.addEventListener('pointercancel', onDocumentPointerEnd, {capture: true, passive: false});
			ownerWindow.addEventListener('blur', onWindowBlur);
			ownerDocument.documentElement.classList.add(RESIZING_CLASS);
			try {
				handle.setPointerCapture(event.pointerId);
			} catch {
				finalizeDrag(false);
				return;
			}
			setAriaValueNow(startWidth);
		},
		[finalizeDrag],
	);

	const onLostPointerCapture = useCallback(
		(event: React.PointerEvent<HTMLElement>) => {
			const state = dragRef.current;
			if (state != null && state.pointerId === event.pointerId) finalizeDrag(true);
		},
		[finalizeDrag],
	);

	const onPointerMove = useCallback((event: React.PointerEvent<HTMLElement>) => {
		const state = dragRef.current;
		if (state == null || state.pointerId !== event.pointerId) return;
		event.preventDefault();
		event.stopPropagation();
	}, []);

	const onPointerCancel = useCallback(
		(event: React.PointerEvent<HTMLElement>) => {
			const state = dragRef.current;
			if (state == null || state.pointerId !== event.pointerId) return;
			event.preventDefault();
			event.stopPropagation();
			finalizeDrag(true);
		},
		[finalizeDrag],
	);

	const onKeyDown = useCallback((event: React.KeyboardEvent<HTMLElement>) => {
		if (shouldResetSidebarWidth(event.key)) {
			event.preventDefault();
			event.stopPropagation();
			SidebarWidth.reset();
			setAriaValueNow(SIDEBAR_WIDTH_DEFAULT);
			return;
		}
		const handle = event.currentTarget;
		const step = event.shiftKey ? KEYBOARD_STEP_LARGE : KEYBOARD_STEP;
		const base = resolveCurrentSidebarWidth(handle);
		let next: number;
		switch (event.key) {
			case 'ArrowLeft':
				next = base - step;
				break;
			case 'ArrowRight':
				next = base + step;
				break;
			case 'Home':
				next = SIDEBAR_WIDTH_MIN;
				break;
			case 'End':
				next = SIDEBAR_WIDTH_MAX;
				break;
			default:
				return;
		}
		event.preventDefault();
		event.stopPropagation();
		const committedWidth = SidebarWidth.setWidth(next, true);
		setAriaValueNow(committedWidth);
	}, []);

	const onDoubleClick = useCallback((event: React.MouseEvent<HTMLElement>) => {
		event.preventDefault();
		event.stopPropagation();
		SidebarWidth.reset();
		setAriaValueNow(SIDEBAR_WIDTH_DEFAULT);
	}, []);

	return (
		<FocusRing offset={-2}>
			<div
				ref={handleRef}
				role="separator"
				tabIndex={0}
				aria-orientation="vertical"
				aria-label={i18n._(RESIZE_SIDEBAR_HANDLE_DESCRIPTOR)}
				aria-valuemin={SIDEBAR_WIDTH_MIN}
				aria-valuemax={SIDEBAR_WIDTH_MAX}
				aria-valuenow={ariaValueNow}
				aria-keyshortcuts="ArrowLeft ArrowRight Home End Enter Backspace Delete"
				className={styles.handle}
				onPointerDown={onPointerDown}
				onPointerMove={onPointerMove}
				onPointerCancel={onPointerCancel}
				onLostPointerCapture={onLostPointerCapture}
				onDoubleClick={onDoubleClick}
				onKeyDown={onKeyDown}
				data-flx="app.sidebar-resize-handle"
			>
				<span className={styles.indicator} aria-hidden />
			</div>
		</FocusRing>
	);
});
