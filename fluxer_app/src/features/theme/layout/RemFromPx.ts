// SPDX-License-Identifier: AGPL-3.0-or-later

export const REM_BASE_PX = 16;

class NonFinitePixelValueError extends Error {
	public constructor(value: number) {
		super(`Pixel value must be finite: ${value}`);
		this.name = 'NonFinitePixelValueError';
	}
}

export function remFromPx(px: number): `${number}rem` {
	if (!Number.isFinite(px)) {
		throw new NonFinitePixelValueError(px);
	}
	const rounded = Math.round((px / REM_BASE_PX) * 1e5) / 1e5;
	return `${rounded}rem`;
}

interface CachedRemScale {
	scale: number;
	generation: number;
}

const cachedRemScales = new WeakMap<Document, CachedRemScale>();
const remScaleDisposers = new WeakMap<Document, () => void>();
let remScaleGeneration = 0;

function readRemScaleFromDocument(ownerDocument: Document): number {
	const ownerWindow = ownerDocument.defaultView;
	if (ownerWindow == null) {
		return 1;
	}
	const rootFontSize = Number.parseFloat(ownerWindow.getComputedStyle(ownerDocument.documentElement).fontSize);
	if (!Number.isFinite(rootFontSize) || rootFontSize <= 0) {
		return 1;
	}
	return rootFontSize / REM_BASE_PX;
}

export function getRemScaleForDocument(ownerDocument: Document | null): number {
	if (ownerDocument == null) {
		return 1;
	}
	const cached = cachedRemScales.get(ownerDocument);
	if (cached != null && cached.generation === remScaleGeneration) {
		return cached.scale;
	}
	const scale = readRemScaleFromDocument(ownerDocument);
	cachedRemScales.set(ownerDocument, {scale, generation: remScaleGeneration});
	return scale;
}

export function clearRemScaleCache(ownerDocument?: Document | null): void {
	if (ownerDocument == null) {
		remScaleGeneration += 1;
		return;
	}
	cachedRemScales.delete(ownerDocument);
}

export function startRemScaleTracking(ownerDocument: Document): () => void {
	remScaleDisposers.get(ownerDocument)?.();
	const ownerWindow = ownerDocument.defaultView;
	if (ownerWindow == null) {
		return () => {};
	}
	return trackRemScaleForWindow(ownerDocument, ownerWindow);
}

function trackRemScaleForWindow(ownerDocument: Document, ownerWindow: Window): () => void {
	let resolutionQuery: MediaQueryList | null = null;
	let trackedDevicePixelRatio = ownerWindow.devicePixelRatio;
	let trackedInnerWidth = ownerWindow.innerWidth;

	function invalidate(): void {
		cachedRemScales.delete(ownerDocument);
	}

	function stopWatchingResolution(): void {
		resolutionQuery?.removeEventListener('change', invalidateForRootFontSizeInputs);
		resolutionQuery = null;
	}

	function watchResolution(): void {
		if (typeof ownerWindow.matchMedia !== 'function') {
			return;
		}
		const nextQuery = ownerWindow.matchMedia(`(resolution: ${ownerWindow.devicePixelRatio}dppx)`);
		if (!nextQuery.matches) {
			stopWatchingResolution();
			return;
		}
		stopWatchingResolution();
		resolutionQuery = nextQuery;
		resolutionQuery.addEventListener('change', invalidateForRootFontSizeInputs);
	}

	function invalidateForRootFontSizeInputs(): void {
		trackedDevicePixelRatio = ownerWindow.devicePixelRatio;
		trackedInnerWidth = ownerWindow.innerWidth;
		invalidate();
		watchResolution();
	}

	function handleResize(): void {
		if (ownerWindow.devicePixelRatio === trackedDevicePixelRatio && ownerWindow.innerWidth === trackedInnerWidth) {
			return;
		}
		invalidateForRootFontSizeInputs();
	}

	ownerWindow.addEventListener('resize', handleResize);
	watchResolution();
	invalidate();

	const dispose = (): void => {
		if (remScaleDisposers.get(ownerDocument) !== dispose) {
			return;
		}
		ownerWindow.removeEventListener('resize', handleResize);
		stopWatchingResolution();
		remScaleDisposers.delete(ownerDocument);
		cachedRemScales.delete(ownerDocument);
	};
	remScaleDisposers.set(ownerDocument, dispose);
	return dispose;
}
