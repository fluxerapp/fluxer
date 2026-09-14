// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import FocusRingContext, {type FocusRingContextManager} from '@app/features/ui/focus_ring/FocusRingContext';
import FocusRingManager from '@app/features/ui/focus_ring/FocusRingManager';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {act, useContext, useRef} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const RING_SELECTOR = '[data-flx="ui.focus-ring.focus-ring-scope.ring.focus-ring"]';

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

type Slot = 'primary' | 'secondary';

let container: HTMLDivElement;
let root: Root;
let unmounted = false;
const ringContexts: Partial<Record<Slot, FocusRingContextManager>> = {};
const targets: Partial<Record<Slot, HTMLDivElement>> = {};

function Capture({slot = 'primary'}: {slot?: Slot}) {
	ringContexts[slot] = useContext(FocusRingContext);
	return (
		<div
			ref={(element) => {
				if (element != null) targets[slot] = element;
			}}
			data-flx="ui.focus-ring.focus-ring-scope-test.capture.div"
		/>
	);
}

function Harness({revision = 0}: {revision?: number}) {
	const containerRef = useRef<HTMLDivElement>(null);
	return (
		<div ref={containerRef} data-revision={revision} data-flx="ui.focus-ring.focus-ring-scope-test.harness.div">
			<FocusRingScope
				containerRef={containerRef}
				data-flx="ui.focus-ring.focus-ring-scope-test.harness.focus-ring-scope"
			>
				<Capture data-flx="ui.focus-ring.focus-ring-scope-test.harness.capture" />
			</FocusRingScope>
		</div>
	);
}

function ScopePair() {
	const primaryRef = useRef<HTMLDivElement>(null);
	const secondaryRef = useRef<HTMLDivElement>(null);
	return (
		<>
			<div ref={primaryRef} data-flx="ui.focus-ring.focus-ring-scope-test.scope-pair.div">
				<FocusRingScope
					containerRef={primaryRef}
					data-flx="ui.focus-ring.focus-ring-scope-test.scope-pair.focus-ring-scope"
				>
					<Capture slot="primary" data-flx="ui.focus-ring.focus-ring-scope-test.scope-pair.capture" />
				</FocusRingScope>
			</div>
			<div ref={secondaryRef} data-flx="ui.focus-ring.focus-ring-scope-test.scope-pair.div--2">
				<FocusRingScope
					containerRef={secondaryRef}
					data-flx="ui.focus-ring.focus-ring-scope-test.scope-pair.focus-ring-scope--2"
				>
					<Capture slot="secondary" data-flx="ui.focus-ring.focus-ring-scope-test.scope-pair.capture--2" />
				</FocusRingScope>
			</div>
		</>
	);
}

function rings(): Array<Element> {
	return Array.from(container.querySelectorAll(RING_SELECTOR));
}

function ring(): Element | null {
	return container.querySelector(RING_SELECTOR);
}

function requireRing(): HTMLElement {
	const element = ring();
	if (element == null) throw new Error('The focus ring is not painted');
	return element as HTMLElement;
}

function requireRingContext(slot: Slot = 'primary'): FocusRingContextManager {
	const value = ringContexts[slot];
	if (value == null) throw new Error(`The focus ring context for ${slot} never rendered`);
	return value;
}

function requireTarget(slot: Slot = 'primary'): HTMLDivElement {
	const value = targets[slot];
	if (value == null) throw new Error(`The focus ring target for ${slot} never rendered`);
	return value;
}

function requireScopeContainer(): HTMLElement {
	const element = container.firstElementChild;
	if (element == null) throw new Error('The scope container never rendered');
	return element as HTMLElement;
}

function domRect(top: number, left: number, width: number, height: number): DOMRect {
	return {
		top,
		left,
		width,
		height,
		right: left + width,
		bottom: top + height,
		x: left,
		y: top,
		toJSON: () => ({}),
	} as DOMRect;
}

function stubBoundingRect(element: Element, read: () => DOMRect) {
	Object.defineProperty(element, 'getBoundingClientRect', {value: read, configurable: true});
}

function unmountRoot() {
	if (unmounted) return;
	unmounted = true;
	act(() => {
		root.unmount();
	});
}

beforeEach(() => {
	delete ringContexts.primary;
	delete ringContexts.secondary;
	delete targets.primary;
	delete targets.secondary;
	unmounted = false;
	container = document.createElement('div');
	document.body.append(container);
	root = createRoot(container);
});

afterEach(() => {
	unmountRoot();
	container.remove();
	FocusRingManager.setRingsEnabled(true);
});

describe('FocusRingScope', () => {
	test('paints the ring once rings are enabled, without a second focus event', () => {
		FocusRingManager.setRingsEnabled(false);
		act(() => {
			root.render(<Harness data-flx="ui.focus-ring.focus-ring-scope-test.harness" />);
		});
		act(() => {
			requireRingContext().showForElement(requireTarget());
		});
		expect(ring()).toBeNull();
		act(() => {
			FocusRingManager.setRingsEnabled(true);
		});
		expect(ring()).not.toBeNull();
	});

	test('restores the ring for a target that never blurred while rings were disabled', () => {
		FocusRingManager.setRingsEnabled(true);
		act(() => {
			root.render(<Harness data-flx="ui.focus-ring.focus-ring-scope-test.harness--2" />);
		});
		act(() => {
			requireRingContext().showForElement(requireTarget());
		});
		expect(ring()).not.toBeNull();
		act(() => {
			FocusRingManager.setRingsEnabled(false);
		});
		expect(ring()).toBeNull();
		act(() => {
			FocusRingManager.setRingsEnabled(true);
		});
		expect(ring()).not.toBeNull();
	});

	test('keeps the ring hidden when nothing is showing', () => {
		FocusRingManager.setRingsEnabled(false);
		act(() => {
			root.render(<Harness data-flx="ui.focus-ring.focus-ring-scope-test.harness--3" />);
		});
		act(() => {
			FocusRingManager.setRingsEnabled(true);
		});
		expect(ring()).toBeNull();
	});

	test('paints the ring on show and removes it on hide', () => {
		act(() => {
			root.render(<Harness data-flx="ui.focus-ring.focus-ring-scope-test.harness--4" />);
		});
		expect(ring()).toBeNull();
		act(() => {
			requireRingContext().showForElement(requireTarget());
		});
		expect(ring()).not.toBeNull();
		act(() => {
			requireRingContext().hide();
		});
		expect(ring()).toBeNull();
	});

	test('repositions the ring when a parent render moves a target that did not resize', () => {
		act(() => {
			root.render(<Harness data-flx="ui.focus-ring.focus-ring-scope-test.harness--5" />);
		});
		stubBoundingRect(requireScopeContainer(), () => domRect(0, 0, 800, 600));
		let targetTop = 200;
		stubBoundingRect(requireTarget(), () => domRect(targetTop, 0, 300, 40));
		act(() => {
			requireRingContext().showForElement(requireTarget());
		});
		expect(requireRing().style.top).toBe('200px');
		targetTop = 320;
		act(() => {
			root.render(<Harness revision={1} data-flx="ui.focus-ring.focus-ring-scope-test.harness--6" />);
		});
		expect(requireRing().style.top).toBe('320px');
	});

	test('repositions the ring when the target itself resizes, and stops observing on unmount', () => {
		const realResizeObserver = globalThis.ResizeObserver;
		const realRequestAnimationFrame = globalThis.requestAnimationFrame;
		const realCancelAnimationFrame = globalThis.cancelAnimationFrame;
		let notifyResize: (() => void) | null = null;
		let disconnectCount = 0;
		class StubResizeObserver {
			constructor(callback: () => void) {
				notifyResize = callback;
			}
			observe() {}
			unobserve() {}
			disconnect() {
				disconnectCount += 1;
			}
		}
		globalThis.ResizeObserver = StubResizeObserver as unknown as typeof ResizeObserver;
		globalThis.requestAnimationFrame = ((callback: FrameRequestCallback) => {
			callback(0);
			return 1;
		}) as typeof requestAnimationFrame;
		globalThis.cancelAnimationFrame = (() => undefined) as typeof cancelAnimationFrame;
		try {
			act(() => {
				root.render(<Harness data-flx="ui.focus-ring.focus-ring-scope-test.harness--7" />);
			});
			stubBoundingRect(requireScopeContainer(), () => domRect(0, 0, 800, 600));
			let targetHeight = 40;
			stubBoundingRect(requireTarget(), () => domRect(100, 0, 300, targetHeight));
			act(() => {
				requireRingContext().showForElement(requireTarget());
			});
			expect(requireRing().style.height).toBe('40px');
			targetHeight = 90;
			act(() => {
				notifyResize?.();
			});
			expect(requireRing().style.height).toBe('90px');
			unmountRoot();
			expect(disconnectCount).toBe(1);
		} finally {
			globalThis.ResizeObserver = realResizeObserver;
			globalThis.requestAnimationFrame = realRequestAnimationFrame;
			globalThis.cancelAnimationFrame = realCancelAnimationFrame;
		}
	});

	test('drops the ring from the previous scope when another scope takes focus', () => {
		act(() => {
			root.render(<ScopePair data-flx="ui.focus-ring.focus-ring-scope-test.scope-pair" />);
		});
		act(() => {
			requireRingContext('primary').showForElement(requireTarget('primary'));
		});
		expect(rings()).toHaveLength(1);
		act(() => {
			requireRingContext('secondary').showForElement(requireTarget('secondary'));
		});
		expect(rings()).toHaveLength(1);
		expect(requireRingContext('primary').visible).toBe(false);
		expect(requireRingContext('secondary').visible).toBe(true);
	});
});
