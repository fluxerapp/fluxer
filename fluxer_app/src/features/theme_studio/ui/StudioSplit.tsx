// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import styles from '@app/features/theme_studio/ui/StudioSplit.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type React from 'react';
import type {ReactNode} from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const RESIZE_PANELS_DESCRIPTOR = msg({
	message: 'Resize panels',
	comment: 'Accessible label for the draggable divider between two Theme Studio panes.',
});

interface StudioSplitProps {
	orientation?: 'horizontal' | 'vertical';
	initialSize?: number;
	minSize?: number;
	maxSize?: number;
	first: ReactNode;
	second: ReactNode;
	storageKey?: string;
	className?: string;
}

const readStoredSize = (key: string | undefined): number | null => {
	if (!key) return null;
	if (typeof window === 'undefined') return null;
	try {
		const raw = AppStorage.getItem(key);
		if (raw === null) return null;
		const parsed = Number(raw);
		return Number.isFinite(parsed) ? parsed : null;
	} catch {
		return null;
	}
};
const writeStoredSize = (key: string | undefined, value: number) => {
	if (!key) return;
	if (typeof window === 'undefined') return;
	try {
		AppStorage.setItem(key, String(Math.round(value)));
	} catch {}
};
export const StudioSplit: React.FC<StudioSplitProps> = ({
	orientation = 'horizontal',
	initialSize = 320,
	minSize = 200,
	maxSize = 720,
	first,
	second,
	storageKey,
	className,
}) => {
	const {i18n} = useLingui();
	const containerRef = useRef<HTMLDivElement | null>(null);
	const [size, setSize] = useState<number>(() => readStoredSize(storageKey) ?? initialSize);
	const [dragging, setDragging] = useState(false);
	const clampSize = useCallback((next: number) => Math.max(minSize, Math.min(maxSize, next)), [minSize, maxSize]);
	useEffect(() => {
		writeStoredSize(storageKey, size);
	}, [storageKey, size]);
	const onPointerDown = useCallback((event: React.PointerEvent) => {
		if (event.pointerType === 'mouse' && event.button !== 0) return;
		event.preventDefault();
		event.currentTarget.setPointerCapture?.(event.pointerId);
		setDragging(true);
	}, []);
	const onPointerMove = useCallback(
		(event: React.PointerEvent) => {
			if (!dragging) return;
			const rect = containerRef.current?.getBoundingClientRect();
			if (!rect) return;
			const next = orientation === 'horizontal' ? event.clientX - rect.left : event.clientY - rect.top;
			setSize(clampSize(next));
		},
		[dragging, orientation, clampSize],
	);
	const onPointerUp = useCallback((event: React.PointerEvent) => {
		event.currentTarget.releasePointerCapture?.(event.pointerId);
		setDragging(false);
	}, []);
	const onKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLDivElement>) => {
			const step = event.shiftKey ? 40 : 10;
			let nextSize: number | null = null;
			switch (event.key) {
				case 'ArrowLeft':
				case 'ArrowUp':
					nextSize = size - step;
					break;
				case 'ArrowRight':
				case 'ArrowDown':
					nextSize = size + step;
					break;
				case 'Home':
					nextSize = minSize;
					break;
				case 'End':
					nextSize = maxSize;
					break;
				default:
					return;
			}
			event.preventDefault();
			setSize(clampSize(nextSize));
		},
		[size, minSize, maxSize, clampSize],
	);
	const firstStyle = orientation === 'horizontal' ? {width: size, flexShrink: 0} : {height: size, flexShrink: 0};
	return (
		<div
			ref={containerRef}
			className={clsx(styles.root, styles[orientation], className)}
			onPointerMove={onPointerMove}
			data-flx="theme-studio.ui.studio-split.root"
		>
			<div className={styles.pane} style={firstStyle} data-flx="theme-studio.ui.studio-split.pane">
				{first}
			</div>
			<FocusRing offset={-2} data-flx="theme-studio.ui.studio-split.focus-ring">
				<div
					role="separator"
					tabIndex={0}
					aria-orientation={orientation === 'horizontal' ? 'vertical' : 'horizontal'}
					aria-label={i18n._(RESIZE_PANELS_DESCRIPTOR)}
					aria-valuemin={minSize}
					aria-valuemax={maxSize}
					aria-valuenow={Math.round(size)}
					className={clsx(
						styles.handle,
						orientation === 'horizontal' ? styles.handleHorizontal : styles.handleVertical,
						dragging && styles.handleActive,
					)}
					onPointerDown={onPointerDown}
					onPointerUp={onPointerUp}
					onKeyDown={onKeyDown}
					data-flx="theme-studio.ui.studio-split.handle.key-down"
				/>
			</FocusRing>
			<div className={clsx(styles.pane, styles.paneFlex)} data-flx="theme-studio.ui.studio-split.pane--2">
				{second}
			</div>
		</div>
	);
};
