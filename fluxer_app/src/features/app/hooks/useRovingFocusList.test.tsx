// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {useRovingFocusList} from '@app/features/app/hooks/useRovingFocusList';
import {act} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

function RovingFocusProbe() {
	const listRef = useRovingFocusList<HTMLDivElement>({
		autoFocusFirst: true,
		focusableSelector: '[data-roving-focus="true"]',
		manageTabIndex: true,
	});
	return (
		<div
			ref={listRef}
			role="menu"
			aria-orientation="vertical"
			data-flx="app.use-roving-focus-list-test.roving-focus-probe.menu"
		>
			<button
				type="button"
				role="menuitem"
				data-roving-focus="true"
				data-flx="app.use-roving-focus-list-test.roving-focus-probe.menuitem.button"
			>
				First
			</button>
			<button
				type="button"
				role="menuitem"
				data-roving-focus="true"
				data-flx="app.use-roving-focus-list-test.roving-focus-probe.menuitem.button--2"
			>
				Second
			</button>
			<div
				role="menuitemcheckbox"
				aria-checked="false"
				tabIndex={0}
				data-roving-focus="true"
				data-flx="app.use-roving-focus-list-test.roving-focus-probe.menuitemcheckbox"
			>
				Third
			</div>
		</div>
	);
}

function ManualEntryRovingFocusProbe() {
	const listRef = useRovingFocusList<HTMLDivElement>({
		focusableSelector: '[data-roving-focus="true"]',
		manageTabIndex: true,
	});
	return (
		<div
			ref={listRef}
			role="menu"
			aria-orientation="vertical"
			tabIndex={-1}
			data-autofocus
			data-flx="app.use-roving-focus-list-test.manual-entry-roving-focus-probe.menu"
		>
			<button
				type="button"
				role="menuitem"
				data-roving-focus="true"
				data-flx="app.use-roving-focus-list-test.manual-entry-roving-focus-probe.menuitem.button"
			>
				First
			</button>
			<button
				type="button"
				role="menuitem"
				data-roving-focus="true"
				data-flx="app.use-roving-focus-list-test.manual-entry-roving-focus-probe.menuitem.button--2"
			>
				Second
			</button>
		</div>
	);
}

const press = (target: Element, key: string): KeyboardEvent => {
	const event = new KeyboardEvent('keydown', {key, bubbles: true, cancelable: true});
	act(() => {
		target.dispatchEvent(event);
	});
	return event;
};

const getItems = (container: HTMLElement): Array<HTMLElement> =>
	Array.from(container.querySelectorAll<HTMLElement>('[data-roving-focus="true"]'));

describe('useRovingFocusList', () => {
	let root: Root | null = null;
	let container: HTMLDivElement | null = null;

	beforeEach(() => {
		container = document.createElement('div');
		document.body.append(container);
		root = createRoot(container);
	});

	afterEach(() => {
		act(() => {
			root?.unmount();
		});
		root = null;
		container?.remove();
		container = null;
	});

	it('moves focus with vertical arrow keys while keeping one tabbable item', () => {
		act(() => {
			root?.render(<RovingFocusProbe data-flx="app.use-roving-focus-list-test.roving-focus-probe" />);
		});
		const items = getItems(container!);

		expect(document.activeElement).toBe(items[0]);
		expect(items.map((item) => item.tabIndex)).toEqual([0, -1, -1]);

		const down = press(items[0], 'ArrowDown');
		expect(down.defaultPrevented).toBe(true);
		expect(document.activeElement).toBe(items[1]);
		expect(items.map((item) => item.tabIndex)).toEqual([-1, 0, -1]);

		press(items[1], 'ArrowDown');
		expect(document.activeElement).toBe(items[2]);
		expect(items.map((item) => item.tabIndex)).toEqual([-1, -1, 0]);

		press(items[2], 'ArrowUp');
		expect(document.activeElement).toBe(items[1]);
		expect(items.map((item) => item.tabIndex)).toEqual([-1, 0, -1]);
	});

	it('supports Home and End in a managed roving list', () => {
		act(() => {
			root?.render(<RovingFocusProbe data-flx="app.use-roving-focus-list-test.roving-focus-probe--2" />);
		});
		const items = getItems(container!);

		press(items[0], 'End');
		expect(document.activeElement).toBe(items[2]);
		expect(items.map((item) => item.tabIndex)).toEqual([-1, -1, 0]);

		press(items[2], 'Home');
		expect(document.activeElement).toBe(items[0]);
		expect(items.map((item) => item.tabIndex)).toEqual([0, -1, -1]);
	});

	it('does not rewrite the roving-managed marker attribute on every navigation', () => {
		act(() => {
			root?.render(<RovingFocusProbe data-flx="app.use-roving-focus-list-test.roving-focus-probe--3" />);
		});
		const items = getItems(container!);
		expect(items.every((item) => item.hasAttribute('data-roving-focus-managed'))).toBe(true);

		// Observe only the marker attribute, after it has already been applied once.
		const observer = new MutationObserver(() => {});
		observer.observe(container!, {
			subtree: true,
			attributes: true,
			attributeFilter: ['data-roving-focus-managed'],
		});

		press(items[0], 'ArrowDown');
		press(items[1], 'ArrowDown');
		press(items[2], 'ArrowUp');
		press(items[1], 'ArrowUp');
		press(items[0], 'End');
		press(items[2], 'Home');

		const records = observer.takeRecords();
		observer.disconnect();

		// The marker is write-once and only ever read via hasAttribute, so navigating
		// must not touch it again. Rewriting it invalidates style for every list item
		// on every keystroke, which is what drove idle style-recalc churn.
		expect(records.length).toBe(0);
		expect(items.every((item) => item.hasAttribute('data-roving-focus-managed'))).toBe(true);
	});

	it('can start on the container with no focused item before ArrowDown enters the list', () => {
		act(() => {
			root?.render(
				<ManualEntryRovingFocusProbe data-flx="app.use-roving-focus-list-test.manual-entry-roving-focus-probe" />,
			);
		});
		const menu = container?.querySelector<HTMLElement>('[role="menu"]');
		const items = getItems(container!);
		expect(menu).toBeInstanceOf(HTMLElement);
		expect(document.activeElement).not.toBe(items[0]);

		act(() => {
			menu?.focus();
		});
		expect(document.activeElement).toBe(menu);
		expect(items.map((item) => item.tabIndex)).toEqual([0, -1]);

		const down = press(menu!, 'ArrowDown');
		expect(down.defaultPrevented).toBe(true);
		expect(document.activeElement).toBe(items[0]);
		expect(items.map((item) => item.tabIndex)).toEqual([0, -1]);
	});
});
