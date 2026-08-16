// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearRemScaleCache,
	getRemScaleForDocument,
	REM_BASE_PX,
	remFromPx,
	startRemScaleTracking,
} from '@app/features/theme/layout/RemFromPx';
import {describe, expect, it} from 'vitest';

interface StubDocumentHarness {
	ownerDocument: Document;
	setRootFontSize: (fontSize: string) => void;
	getComputedStyleCallCount: () => number;
	animationFrameRequestCount: () => number;
	resizeListenerCount: () => number;
	resolutionListenerCount: () => number;
	watchedResolutionQueries: () => Array<string>;
	dispatchResize: () => void;
	setDevicePixelRatio: (devicePixelRatio: number) => void;
	setInnerWidth: (innerWidth: number) => void;
	setResolutionQueryMatches: (matches: boolean) => void;
	dispatchResolutionChange: () => void;
}

function createStubDocumentHarness(initialRootFontSize: string): StubDocumentHarness {
	let rootFontSize = initialRootFontSize;
	let getComputedStyleCallCount = 0;
	let animationFrameRequestCount = 0;
	let resolutionQueryMatches = true;
	const resizeListeners = new Set<() => void>();
	const resolutionQueries = new Map<string, Set<() => void>>();
	const documentElement = {} as HTMLElement;
	const ownerWindow = {
		devicePixelRatio: 1,
		innerWidth: 1280,
		getComputedStyle: () => {
			getComputedStyleCallCount += 1;
			return {fontSize: rootFontSize} as CSSStyleDeclaration;
		},
		requestAnimationFrame: () => {
			animationFrameRequestCount += 1;
			return 0;
		},
		cancelAnimationFrame: () => {},
		addEventListener: (type: string, listener: () => void) => {
			if (type !== 'resize') return;
			resizeListeners.add(listener);
		},
		removeEventListener: (type: string, listener: () => void) => {
			if (type !== 'resize') return;
			resizeListeners.delete(listener);
		},
		matchMedia: (media: string) => {
			const listeners = resolutionQueries.get(media) ?? new Set<() => void>();
			resolutionQueries.set(media, listeners);
			return {
				media,
				matches: resolutionQueryMatches,
				addEventListener: (_type: string, listener: () => void) => listeners.add(listener),
				removeEventListener: (_type: string, listener: () => void) => listeners.delete(listener),
			} as unknown as MediaQueryList;
		},
	};
	const ownerDocument = {documentElement, defaultView: ownerWindow} as unknown as Document;
	return {
		ownerDocument,
		setRootFontSize: (fontSize) => {
			rootFontSize = fontSize;
		},
		getComputedStyleCallCount: () => getComputedStyleCallCount,
		animationFrameRequestCount: () => animationFrameRequestCount,
		resizeListenerCount: () => resizeListeners.size,
		resolutionListenerCount: () =>
			[...resolutionQueries.values()].reduce((total, listeners) => total + listeners.size, 0),
		watchedResolutionQueries: () =>
			[...resolutionQueries.entries()].filter(([, listeners]) => listeners.size > 0).map(([media]) => media),
		dispatchResize: () => {
			for (const listener of [...resizeListeners]) listener();
		},
		setDevicePixelRatio: (devicePixelRatio) => {
			ownerWindow.devicePixelRatio = devicePixelRatio;
		},
		setInnerWidth: (innerWidth) => {
			ownerWindow.innerWidth = innerWidth;
		},
		setResolutionQueryMatches: (matches) => {
			resolutionQueryMatches = matches;
		},
		dispatchResolutionChange: () => {
			for (const listeners of [...resolutionQueries.values()]) {
				for (const listener of [...listeners]) listener();
			}
		},
	};
}

describe('remFromPx', () => {
	it('uses a 16px base', () => {
		expect(REM_BASE_PX).toBe(16);
	});

	it('converts whole pixel sizes to rem', () => {
		expect(remFromPx(16)).toBe('1rem');
		expect(remFromPx(40)).toBe('2.5rem');
		expect(remFromPx(80)).toBe('5rem');
		expect(remFromPx(0)).toBe('0rem');
	});

	it('converts fractional pixel sizes without trailing-zero noise', () => {
		expect(remFromPx(10)).toBe('0.625rem');
		expect(remFromPx(3.2)).toBe('0.2rem');
		expect(remFromPx(4.8)).toBe('0.3rem');
		expect(remFromPx(2.5)).toBe('0.15625rem');
	});

	it('preserves the sign of negative offsets', () => {
		expect(remFromPx(-64.8)).toBe('-4.05rem');
	});

	it('always produces a value parseable as a rem length', () => {
		for (const px of [12, 14, 18, 24, 36, 55, 105, 140, 120]) {
			expect(remFromPx(px)).toMatch(/^-?\d+(?:\.\d+)?rem$/);
		}
	});
});

describe('getRemScaleForDocument', () => {
	it('reports the root font size relative to the rem base', () => {
		const defaultRoot = createStubDocumentHarness('16px');
		const scaledRoot = createStubDocumentHarness('20px');
		const zoomedRoot = createStubDocumentHarness('24px');
		expect(getRemScaleForDocument(defaultRoot.ownerDocument)).toBe(1);
		expect(getRemScaleForDocument(scaledRoot.ownerDocument)).toBe(1.25);
		expect(getRemScaleForDocument(zoomedRoot.ownerDocument)).toBe(1.5);
	});

	it('reads the computed root font size once for repeated calls', () => {
		const harness = createStubDocumentHarness('16px');
		for (let call = 0; call < 200; call += 1) {
			expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1);
		}
		expect(harness.getComputedStyleCallCount()).toBe(1);
	});

	it('never schedules per-frame invalidation work', () => {
		const harness = createStubDocumentHarness('16px');
		getRemScaleForDocument(harness.ownerDocument);
		getRemScaleForDocument(harness.ownerDocument);
		expect(harness.animationFrameRequestCount()).toBe(0);
	});

	it('re-reads after a resize that changes the device pixel ratio', () => {
		const harness = createStubDocumentHarness('16px');
		startRemScaleTracking(harness.ownerDocument);
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1);
		harness.setRootFontSize('20px');
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1);
		harness.setDevicePixelRatio(1.25);
		harness.dispatchResize();
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1.25);
		expect(harness.watchedResolutionQueries()).toEqual(['(resolution: 1.25dppx)']);
	});

	it('re-reads after a resize that changes the viewport width', () => {
		const harness = createStubDocumentHarness('16px');
		startRemScaleTracking(harness.ownerDocument);
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1);
		harness.setRootFontSize('20px');
		harness.setInnerWidth(720);
		harness.dispatchResize();
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1.25);
	});

	it('ignores resizes that change neither the device pixel ratio nor the viewport width', () => {
		const harness = createStubDocumentHarness('16px');
		startRemScaleTracking(harness.ownerDocument);
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1);
		harness.setRootFontSize('20px');
		for (let frame = 0; frame < 60; frame += 1) {
			harness.dispatchResize();
			expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1);
		}
		expect(harness.getComputedStyleCallCount()).toBe(1);
	});

	it('re-reads after a browser zoom changes the device pixel ratio', () => {
		const harness = createStubDocumentHarness('16px');
		startRemScaleTracking(harness.ownerDocument);
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1);
		expect(harness.watchedResolutionQueries()).toEqual(['(resolution: 1dppx)']);
		harness.setRootFontSize('24px');
		harness.setDevicePixelRatio(1.5);
		harness.dispatchResolutionChange();
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1.5);
		expect(harness.watchedResolutionQueries()).toEqual(['(resolution: 1.5dppx)']);
		expect(harness.resolutionListenerCount()).toBe(1);
	});

	it('re-binds the resolution query from a resize when the query never matched', () => {
		const harness = createStubDocumentHarness('16px');
		harness.setResolutionQueryMatches(false);
		startRemScaleTracking(harness.ownerDocument);
		expect(harness.resolutionListenerCount()).toBe(0);
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1);
		harness.setRootFontSize('20px');
		harness.dispatchResize();
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1);
		harness.setDevicePixelRatio(1.5);
		harness.setResolutionQueryMatches(true);
		harness.dispatchResize();
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1.25);
		expect(harness.watchedResolutionQueries()).toEqual(['(resolution: 1.5dppx)']);
		expect(harness.resolutionListenerCount()).toBe(1);
	});

	it('re-reads after an explicit cache clear', () => {
		const harness = createStubDocumentHarness('16px');
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1);
		harness.setRootFontSize('12px');
		clearRemScaleCache(harness.ownerDocument);
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(0.75);
		expect(harness.getComputedStyleCallCount()).toBe(2);
	});

	it('releases every listener when tracking is disposed', () => {
		const harness = createStubDocumentHarness('16px');
		const stopTracking = startRemScaleTracking(harness.ownerDocument);
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1);
		expect(harness.resizeListenerCount()).toBe(1);
		expect(harness.resolutionListenerCount()).toBe(1);
		stopTracking();
		expect(harness.resizeListenerCount()).toBe(0);
		expect(harness.resolutionListenerCount()).toBe(0);
		harness.setRootFontSize('20px');
		expect(getRemScaleForDocument(harness.ownerDocument)).toBe(1.25);
		expect(harness.resizeListenerCount()).toBe(0);
		expect(harness.resolutionListenerCount()).toBe(0);
	});

	it('never registers listeners as a side effect of reading', () => {
		const harness = createStubDocumentHarness('16px');
		for (let call = 0; call < 5; call += 1) {
			getRemScaleForDocument(harness.ownerDocument);
		}
		expect(harness.resizeListenerCount()).toBe(0);
		expect(harness.resolutionListenerCount()).toBe(0);
	});

	it('invalidates every document when cleared without a target', () => {
		const main = createStubDocumentHarness('16px');
		const popout = createStubDocumentHarness('16px');
		expect(getRemScaleForDocument(main.ownerDocument)).toBe(1);
		expect(getRemScaleForDocument(popout.ownerDocument)).toBe(1);
		main.setRootFontSize('20px');
		popout.setRootFontSize('20px');
		clearRemScaleCache();
		expect(getRemScaleForDocument(main.ownerDocument)).toBe(1.25);
		expect(getRemScaleForDocument(popout.ownerDocument)).toBe(1.25);
	});

	it('falls back to the rem base when the document has no view or no usable root font size', () => {
		const detachedDocument = {documentElement: {} as HTMLElement, defaultView: null} as unknown as Document;
		const brokenRoot = createStubDocumentHarness('not-a-length');
		expect(getRemScaleForDocument(null)).toBe(1);
		expect(getRemScaleForDocument(detachedDocument)).toBe(1);
		expect(getRemScaleForDocument(brokenRoot.ownerDocument)).toBe(1);
	});
});
