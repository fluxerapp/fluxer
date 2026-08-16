// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	flushSkeletonLayoutMemoryWrite,
	SKELETON_UNMEASURED_WIDTH_PX,
	setSkeletonLayoutCaptureEnabled,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {getRemScaleForDocument} from '@app/features/theme/layout/RemFromPx';
import {useEffect, useRef} from 'react';

export function useSkeletonLayoutMemoryCapture(enabled: boolean): void {
	useEffect(() => {
		setSkeletonLayoutCaptureEnabled(enabled);
		if (!enabled) {
			return;
		}
		const flushOnLifecycleBoundary = (): void => {
			flushSkeletonLayoutMemoryWrite();
		};
		const handleVisibilityChange = (): void => {
			if (document.visibilityState === 'hidden') {
				flushOnLifecycleBoundary();
			}
		};
		window.addEventListener('pagehide', flushOnLifecycleBoundary);
		window.addEventListener('beforeunload', flushOnLifecycleBoundary);
		document.addEventListener('visibilitychange', handleVisibilityChange);
		return () => {
			window.removeEventListener('pagehide', flushOnLifecycleBoundary);
			window.removeEventListener('beforeunload', flushOnLifecycleBoundary);
			document.removeEventListener('visibilitychange', handleVisibilityChange);
			flushOnLifecycleBoundary();
			setSkeletonLayoutCaptureEnabled(false);
		};
	}, [enabled]);
}

export function measureSkeletonWidthPx(element: Element | null): number {
	if (element == null) {
		return SKELETON_UNMEASURED_WIDTH_PX;
	}
	const width = element.getBoundingClientRect().width;
	if (!Number.isFinite(width) || width <= 0) {
		return SKELETON_UNMEASURED_WIDTH_PX;
	}
	return Math.round(width / getRemScaleForDocument(element.ownerDocument));
}

export function measureSkeletonTextWidthPx(element: Element | null): number {
	const ownerDocument = element?.ownerDocument;
	if (element == null || ownerDocument == null) {
		return SKELETON_UNMEASURED_WIDTH_PX;
	}
	const range = ownerDocument.createRange();
	range.selectNodeContents(element);
	const textWidth = range.getBoundingClientRect().width;
	const boxWidth = element.getBoundingClientRect().width;
	if (!Number.isFinite(textWidth) || textWidth <= 0 || !Number.isFinite(boxWidth) || boxWidth <= 0) {
		return SKELETON_UNMEASURED_WIDTH_PX;
	}
	return Math.round(Math.min(textWidth, boxWidth) / getRemScaleForDocument(ownerDocument));
}

export function measureSkeletonHeightPx(element: Element | null): number {
	if (element == null) {
		return 0;
	}
	const height = element.getBoundingClientRect().height;
	if (!Number.isFinite(height) || height <= 0) {
		return 0;
	}
	return Math.round(height / getRemScaleForDocument(element.ownerDocument));
}

export function useSkeletonLayoutReport(report: () => void, changeKey: string): void {
	const reportRef = useRef(report);
	reportRef.current = report;
	useEffect(() => {
		reportRef.current();
	}, [changeKey]);
}
