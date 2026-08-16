// SPDX-License-Identifier: AGPL-3.0-or-later

import {getRemScaleForDocument} from '@app/features/theme/layout/RemFromPx';

const cachedRemScales = new WeakMap<Document, number>();
const clearRemScaleCacheRafIds = new WeakMap<Document, number>();

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

function scheduleRemScaleCacheClear(ownerDocument: Document): void {
	const ownerWindow = ownerDocument.defaultView;
	if (ownerWindow == null || typeof ownerWindow.requestAnimationFrame !== 'function') {
		cachedRemScales.delete(ownerDocument);
		return;
	}
	if (clearRemScaleCacheRafIds.has(ownerDocument)) return;
	const rafId = ownerWindow.requestAnimationFrame(() => {
		clearRemScaleCacheRafIds.delete(ownerDocument);
		cachedRemScales.delete(ownerDocument);
	});
	clearRemScaleCacheRafIds.set(ownerDocument, rafId);
}

export function clearAppZoomCache(ownerDocument: Document | null = getDefaultDocument()): void {
	if (ownerDocument == null) return;
	cachedRemScales.delete(ownerDocument);
	const ownerWindow = ownerDocument.defaultView;
	if (ownerWindow == null) return;
	const remScaleRafId = clearRemScaleCacheRafIds.get(ownerDocument);
	if (remScaleRafId == null) return;
	if (typeof ownerWindow.cancelAnimationFrame === 'function') {
		ownerWindow.cancelAnimationFrame(remScaleRafId);
	}
	clearRemScaleCacheRafIds.delete(ownerDocument);
}

export function getAppRemScale(ownerDocument: Document | null = getDefaultDocument()): number {
	if (ownerDocument == null) return 1;
	const cachedRemScale = cachedRemScales.get(ownerDocument);
	if (cachedRemScale != null) return cachedRemScale;
	const remScale = getRemScaleForDocument(ownerDocument);
	cachedRemScales.set(ownerDocument, remScale);
	scheduleRemScaleCacheClear(ownerDocument);
	return remScale;
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
