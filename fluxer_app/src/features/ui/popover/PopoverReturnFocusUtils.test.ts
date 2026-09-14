// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {resolve} from 'node:path';
import {observePopoutKeyboardDismiss, resolvePopoutReturnFocus} from '@app/features/ui/popover/PopoverReturnFocusUtils';
import {FloatingFocusManager, useFloating} from '@floating-ui/react';
import {act, createElement, useLayoutEffect} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const RETURN_FOCUS_REF = {current: null};
const KEYBOARD_DISMISS_REF = {current: null};

describe('resolvePopoutReturnFocus', () => {
	it('returns focus to the target when the open-time policy allows it', () => {
		expect(
			resolvePopoutReturnFocus({
				restoreFocusPolicy: true,
				isKeyboardModeEnabled: false,
				keyboardDismissRef: KEYBOARD_DISMISS_REF,
				isTargetInDOM: true,
			}),
		).toBe(true);
	});

	it('returns focus to the target when the viewer is navigating by keyboard', () => {
		expect(
			resolvePopoutReturnFocus({
				restoreFocusPolicy: false,
				isKeyboardModeEnabled: true,
				keyboardDismissRef: KEYBOARD_DISMISS_REF,
				isTargetInDOM: true,
			}),
		).toBe(true);
	});

	it('defers a pointer-driven popout to the late-bound keyboard-dismiss target', () => {
		expect(
			resolvePopoutReturnFocus({
				restoreFocusPolicy: false,
				isKeyboardModeEnabled: false,
				keyboardDismissRef: KEYBOARD_DISMISS_REF,
				isTargetInDOM: true,
			}),
		).toBe(KEYBOARD_DISMISS_REF);
	});

	it('prefers an explicit return-focus target over the popout target', () => {
		expect(
			resolvePopoutReturnFocus({
				restoreFocusPolicy: false,
				isKeyboardModeEnabled: true,
				returnFocusRef: RETURN_FOCUS_REF,
				keyboardDismissRef: KEYBOARD_DISMISS_REF,
				isTargetInDOM: true,
			}),
		).toBe(RETURN_FOCUS_REF);
	});

	it('defers an explicit return-focus target to the keyboard-dismiss target when no return is wanted', () => {
		expect(
			resolvePopoutReturnFocus({
				restoreFocusPolicy: false,
				isKeyboardModeEnabled: false,
				returnFocusRef: RETURN_FOCUS_REF,
				keyboardDismissRef: KEYBOARD_DISMISS_REF,
				isTargetInDOM: true,
			}),
		).toBe(KEYBOARD_DISMISS_REF);
	});

	it('does not return focus to a target that left the document', () => {
		expect(
			resolvePopoutReturnFocus({
				restoreFocusPolicy: true,
				isKeyboardModeEnabled: true,
				keyboardDismissRef: KEYBOARD_DISMISS_REF,
				isTargetInDOM: false,
			}),
		).toBe(false);
	});
});

describe('observePopoutKeyboardDismiss', () => {
	let trigger: HTMLElement;
	let keyboardDismissRef: {current: HTMLElement | null};
	let dispose: (() => void) | null = null;
	let layerManagerStub: ((event: KeyboardEvent) => void) | null = null;

	const pressKey = (key: string): void => {
		document.body.dispatchEvent(new KeyboardEvent('keydown', {key, bubbles: true, cancelable: true}));
	};

	beforeEach(() => {
		trigger = document.createElement('button');
		document.body.append(trigger);
		keyboardDismissRef = {current: null};
	});

	afterEach(() => {
		dispose?.();
		dispose = null;
		if (layerManagerStub) {
			document.removeEventListener('keydown', layerManagerStub, {capture: true});
			layerManagerStub = null;
		}
		document.body.replaceChildren();
	});

	it('records the focus target when Escape is pressed', () => {
		dispose = observePopoutKeyboardDismiss({
			ownerWindow: window,
			keyboardDismissRef,
			resolveFocusTarget: () => trigger,
		});

		pressKey('Escape');

		expect(keyboardDismissRef.current).toBe(trigger);
	});

	it('records the focus target even though the layer manager stops immediate propagation first', () => {
		let didCloseTopLayer = false;
		layerManagerStub = (event: KeyboardEvent) => {
			if (event.key !== 'Escape') {
				return;
			}
			didCloseTopLayer = true;
			event.preventDefault();
			event.stopImmediatePropagation();
		};
		document.addEventListener('keydown', layerManagerStub, {capture: true});
		dispose = observePopoutKeyboardDismiss({
			ownerWindow: window,
			keyboardDismissRef,
			resolveFocusTarget: () => trigger,
		});

		pressKey('Escape');

		expect(didCloseTopLayer).toBe(true);
		expect(keyboardDismissRef.current).toBe(trigger);
	});

	it('ignores keys other than Escape', () => {
		dispose = observePopoutKeyboardDismiss({
			ownerWindow: window,
			keyboardDismissRef,
			resolveFocusTarget: () => trigger,
		});

		pressKey('Enter');
		pressKey('Tab');

		expect(keyboardDismissRef.current).toBeNull();
	});

	it('records nothing when the popout is not the layer Escape closes', () => {
		dispose = observePopoutKeyboardDismiss({
			ownerWindow: window,
			keyboardDismissRef,
			resolveFocusTarget: () => null,
		});

		pressKey('Escape');

		expect(keyboardDismissRef.current).toBeNull();
	});

	it('stops recording once disposed', () => {
		const stop = observePopoutKeyboardDismiss({
			ownerWindow: window,
			keyboardDismissRef,
			resolveFocusTarget: () => trigger,
		});
		stop();

		pressKey('Escape');

		expect(keyboardDismissRef.current).toBeNull();
	});
});

describe('FloatingFocusManager return-focus ref contract', () => {
	let host: HTMLDivElement;
	let trigger: HTMLButtonElement;
	let root: Root;

	function ReturnFocusHarness({
		reference,
		returnFocus,
	}: {
		reference: HTMLElement;
		returnFocus: {current: HTMLElement | null};
	}) {
		const {refs, context} = useFloating({open: true});
		useLayoutEffect(() => {
			refs.setReference(reference);
		}, [refs, reference]);
		return createElement(FloatingFocusManager, {
			context,
			returnFocus,
			initialFocus: refs.floating,
			children: createElement('div', {ref: refs.setFloating, tabIndex: -1, 'data-testid': 'floating'}),
		});
	}

	async function settle(): Promise<void> {
		for (let attempt = 0; attempt < 10; attempt += 1) {
			await act(async () => {
				await Promise.resolve();
			});
		}
	}

	beforeEach(() => {
		host = document.createElement('div');
		trigger = document.createElement('button');
		document.body.append(host, trigger);
		root = createRoot(host);
	});

	afterEach(() => {
		document.body.replaceChildren();
	});

	it('focuses a return target assigned after the popout opened', async () => {
		const returnFocus: {current: HTMLElement | null} = {current: null};
		await act(async () => {
			root.render(createElement(ReturnFocusHarness, {reference: trigger, returnFocus}));
		});
		await settle();
		host.querySelector<HTMLElement>('[data-testid="floating"]')?.focus();

		returnFocus.current = trigger;
		await act(async () => {
			root.unmount();
		});
		await settle();

		expect(document.activeElement).toBe(trigger);
	});

	it('leaves focus alone when no return target was assigned', async () => {
		const returnFocus: {current: HTMLElement | null} = {current: null};
		await act(async () => {
			root.render(createElement(ReturnFocusHarness, {reference: trigger, returnFocus}));
		});
		await settle();
		host.querySelector<HTMLElement>('[data-testid="floating"]')?.focus();

		await act(async () => {
			root.unmount();
		});
		await settle();

		expect(document.activeElement).not.toBe(trigger);
	});
});

describe('PopoverPopouts keyboard-dismiss wiring', () => {
	const source = readFileSync(resolve(process.cwd(), 'src/features/ui/popover/PopoverPopouts.tsx'), 'utf8');

	it('shares one ref between the Escape watcher and the return-focus resolver', () => {
		expect(source.split('keyboardDismissRef: keyboardDismissReturnRef').length - 1).toBe(2);
		expect(source).toContain('returnFocus={resolvePopoutReturnFocus({');
		expect(source).toContain('resolveFocusTarget: resolveKeyboardDismissFocusTarget');
	});

	it('only records Escape for the layer the layer manager would close', () => {
		expect(source).toContain("LayerManager.isTopLayer('popout', popoutKey)");
	});
});
