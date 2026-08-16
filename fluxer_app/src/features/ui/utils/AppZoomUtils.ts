// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearRemScaleCache, getRemScaleForDocument} from '@app/features/theme/layout/RemFromPx';

export interface AppZoomPoint {
	x: number;
	y: number;
}

export interface AppZoomSize {
	width: number;
	height: number;
}

export interface AppZoomElectronApi {
	setZoomFactor: (factor: number) => void;
}

function getDefaultDocument(): Document | null {
	if (typeof document === 'undefined') return null;
	return document;
}

export function clearAppZoomCache(): void {
	clearRemScaleCache();
}

export function getAppRemScale(ownerDocument: Document | null = getDefaultDocument()): number {
	return getRemScaleForDocument(ownerDocument);
}

export function applyAppZoomToDocument(zoomPercent: number, electronApi?: AppZoomElectronApi | null): void {
	if (typeof document === 'undefined') {
		clearAppZoomCache();
		return;
	}
	const root = document.documentElement;
	const normalizedZoomPercent = Number.isFinite(zoomPercent)
		? Math.max(50, Math.min(200, Math.round(zoomPercent)))
		: 100;
	root.style.removeProperty('zoom');
	root.style.removeProperty('--app-zoom-factor');
	root.style.removeProperty('font-size');
	if (electronApi) {
		root.style.setProperty('--custom-zoom', String(normalizedZoomPercent));
		electronApi.setZoomFactor(1);
	} else {
		root.style.removeProperty('--custom-zoom');
	}
	clearAppZoomCache();
}

export function appZoomLayoutPx(value: number): number {
	if (!Number.isFinite(value)) return 0;
	return value;
}

export function appZoomClientPoint(clientX: number, clientY: number): AppZoomPoint {
	return {
		x: appZoomLayoutPx(clientX),
		y: appZoomLayoutPx(clientY),
	};
}

export function getAppZoomViewportSize(ownerDocument: Document | null = getDefaultDocument()): AppZoomSize {
	if (ownerDocument == null) return {width: 0, height: 0};
	const ownerWindow = ownerDocument.defaultView;
	if (ownerWindow == null) return {width: 0, height: 0};
	const documentElement = ownerDocument.documentElement;
	let width = ownerWindow.innerWidth;
	let height = ownerWindow.innerHeight;
	if (!width) width = documentElement.clientWidth;
	if (!height) height = documentElement.clientHeight;
	return {
		width,
		height,
	};
}

export function appZoomCssPx(value: number): string {
	return `${appZoomLayoutPx(value)}px`;
}
