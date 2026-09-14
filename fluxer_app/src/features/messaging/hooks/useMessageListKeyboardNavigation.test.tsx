// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {CHANNEL_MESSAGE_ID_PREFIX, findMessageElement} from '@app/features/messaging/utils/MessageNodeSelectors';
import {act, createElement, type RefObject} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const keyboardModeMock = {keyboardModeEnabled: true};
const messageFocusMock = {focusedMessageId: null as string | null};
const rolloutMock = {enabled: true};

vi.mock('@app/features/ui/state/KeyboardMode', () => ({default: keyboardModeMock}));
vi.mock('@app/features/messaging/state/MessageFocus', () => ({default: messageFocusMock}));
vi.mock('@app/features/messaging/state/MessageKeyboardFocusRollout', () => ({default: rolloutMock}));

const {useMessageListKeyboardNavigation} = await import(
	'@app/features/messaging/hooks/useMessageListKeyboardNavigation'
);

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

const CHANNEL_ID = '900000000000000001';

interface RowSpec {
	messageId: string;
	idPrefix: string;
}

let viewport: HTMLElement;
let host: HTMLDivElement;
let root: Root;

function mountRows(specs: ReadonlyArray<RowSpec>): void {
	for (const spec of specs) {
		const row = document.createElement('div');
		row.id = `${spec.idPrefix}-${CHANNEL_ID}-${spec.messageId}`;
		row.dataset.messageId = spec.messageId;
		row.dataset.channelId = CHANNEL_ID;
		row.tabIndex = -1;
		viewport.append(row);
	}
}

function focusedRowId(): string | null {
	const active = document.activeElement;
	return active instanceof HTMLElement ? (active.dataset.messageId ?? null) : null;
}

function render(onFocusMessage?: (messageId: string) => void): void {
	const containerRef: RefObject<HTMLElement | null> = {current: viewport};
	function Harness(): null {
		useMessageListKeyboardNavigation({containerRef, channelId: CHANNEL_ID, onFocusMessage, allowWhenInactive: true});
		return null;
	}
	act(() => {
		root.render(createElement(Harness));
	});
}

function pressArrow(key: 'ArrowUp' | 'ArrowDown'): void {
	act(() => {
		window.dispatchEvent(new KeyboardEvent('keydown', {key, bubbles: true, cancelable: true}));
	});
}

beforeEach(() => {
	keyboardModeMock.keyboardModeEnabled = true;
	messageFocusMock.focusedMessageId = null;
	rolloutMock.enabled = true;
	viewport = document.createElement('div');
	document.body.append(viewport);
	viewport.addEventListener('focusin', (event) => {
		const target = event.target;
		if (target instanceof HTMLElement && target.dataset.messageId) {
			messageFocusMock.focusedMessageId = target.dataset.messageId;
		}
	});
	host = document.createElement('div');
	document.body.append(host);
	root = createRoot(host);
});

afterEach(() => {
	act(() => {
		root.unmount();
	});
	document.body.replaceChildren();
	vi.restoreAllMocks();
});

describe('useMessageListKeyboardNavigation', () => {
	it('walks from the newest message up through a revealed blocked group', () => {
		mountRows([
			{messageId: '1', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
			{messageId: '2', idPrefix: 'blocked-messages'},
			{messageId: '3', idPrefix: 'blocked-messages'},
			{messageId: '4', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
		]);
		render((messageId) => {
			findMessageElement(document, viewport, CHANNEL_ID, messageId)?.focus({preventScroll: true});
		});

		pressArrow('ArrowUp');
		expect(focusedRowId()).toBe('4');
		pressArrow('ArrowUp');
		expect(focusedRowId()).toBe('3');
		pressArrow('ArrowUp');
		expect(focusedRowId()).toBe('2');
		pressArrow('ArrowUp');
		expect(focusedRowId()).toBe('1');
	});

	it('walks back down out of a revealed blocked group', () => {
		mountRows([
			{messageId: '1', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
			{messageId: '2', idPrefix: 'blocked-messages'},
			{messageId: '3', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
		]);
		render((messageId) => {
			findMessageElement(document, viewport, CHANNEL_ID, messageId)?.focus({preventScroll: true});
		});

		pressArrow('ArrowDown');
		expect(focusedRowId()).toBe('1');
		pressArrow('ArrowDown');
		expect(focusedRowId()).toBe('2');
		pressArrow('ArrowDown');
		expect(focusedRowId()).toBe('3');
	});

	it('keeps navigating when the focus delegate cannot resolve the target element', () => {
		mountRows([
			{messageId: '1', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
			{messageId: '2', idPrefix: 'blocked-messages'},
			{messageId: '3', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
		]);
		const onFocusMessage = vi.fn<(messageId: string) => void>();
		render(onFocusMessage);

		pressArrow('ArrowUp');
		expect(onFocusMessage).toHaveBeenLastCalledWith('3');
		expect(focusedRowId()).toBe('3');
		pressArrow('ArrowUp');
		expect(onFocusMessage).toHaveBeenLastCalledWith('2');
		expect(focusedRowId()).toBe('2');
	});

	it('leaves scrolling to the delegate when the delegate did move focus', () => {
		mountRows([{messageId: '1', idPrefix: CHANNEL_MESSAGE_ID_PREFIX}]);
		const scrollIntoView = vi.fn();
		vi.spyOn(HTMLElement.prototype, 'scrollIntoView').mockImplementation(scrollIntoView);
		render((messageId) => {
			findMessageElement(document, viewport, CHANNEL_ID, messageId)?.focus({preventScroll: true});
		});

		pressArrow('ArrowUp');
		expect(focusedRowId()).toBe('1');
		expect(scrollIntoView).not.toHaveBeenCalled();
	});

	it('scrolls the target into view itself when the delegate did nothing', () => {
		mountRows([{messageId: '1', idPrefix: CHANNEL_MESSAGE_ID_PREFIX}]);
		const scrollIntoView = vi.fn();
		vi.spyOn(HTMLElement.prototype, 'scrollIntoView').mockImplementation(scrollIntoView);
		render(vi.fn());

		pressArrow('ArrowUp');
		expect(scrollIntoView).toHaveBeenCalledTimes(1);
	});

	it('keeps navigating while a checkbox inside the list holds focus', () => {
		mountRows([
			{messageId: '1', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
			{messageId: '2', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
		]);
		const checkbox = document.createElement('input');
		checkbox.type = 'checkbox';
		viewport.append(checkbox);
		render((messageId) => {
			findMessageElement(document, viewport, CHANNEL_ID, messageId)?.focus({preventScroll: true});
		});
		checkbox.focus();

		pressArrow('ArrowUp');
		expect(focusedRowId()).toBe('2');
	});

	it('stops navigating while a text input inside the list holds focus', () => {
		mountRows([
			{messageId: '1', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
			{messageId: '2', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
		]);
		const textInput = document.createElement('input');
		textInput.type = 'text';
		viewport.append(textInput);
		render((messageId) => {
			findMessageElement(document, viewport, CHANNEL_ID, messageId)?.focus({preventScroll: true});
		});
		textInput.focus();

		pressArrow('ArrowUp');
		expect(focusedRowId()).toBeNull();
	});

	it('skips messages that a collapsed group has removed from the DOM', () => {
		mountRows([
			{messageId: '1', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
			{messageId: '4', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
		]);
		render((messageId) => {
			findMessageElement(document, viewport, CHANNEL_ID, messageId)?.focus({preventScroll: true});
		});

		pressArrow('ArrowUp');
		expect(focusedRowId()).toBe('4');
		pressArrow('ArrowUp');
		expect(focusedRowId()).toBe('1');
	});
});

describe('useMessageListKeyboardNavigation control arm', () => {
	beforeEach(() => {
		rolloutMock.enabled = false;
	});

	it('stops at a row the focus delegate cannot resolve', () => {
		mountRows([
			{messageId: '1', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
			{messageId: '2', idPrefix: 'blocked-messages'},
		]);
		const onFocusMessage = vi.fn<(messageId: string) => void>();
		render(onFocusMessage);

		pressArrow('ArrowUp');
		expect(onFocusMessage).toHaveBeenLastCalledWith('2');
		expect(focusedRowId()).toBeNull();
	});

	it('never scrolls the target itself when a delegate is supplied', () => {
		mountRows([{messageId: '1', idPrefix: CHANNEL_MESSAGE_ID_PREFIX}]);
		const scrollIntoView = vi.fn();
		vi.spyOn(HTMLElement.prototype, 'scrollIntoView').mockImplementation(scrollIntoView);
		render(vi.fn());

		pressArrow('ArrowUp');
		expect(scrollIntoView).not.toHaveBeenCalled();
	});

	it('treats a focused checkbox as editable and stops navigating', () => {
		mountRows([
			{messageId: '1', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
			{messageId: '2', idPrefix: CHANNEL_MESSAGE_ID_PREFIX},
		]);
		const checkbox = document.createElement('input');
		checkbox.type = 'checkbox';
		viewport.append(checkbox);
		render((messageId) => {
			findMessageElement(document, viewport, CHANNEL_ID, messageId)?.focus({preventScroll: true});
		});
		checkbox.focus();

		pressArrow('ArrowUp');
		expect(focusedRowId()).toBeNull();
	});
});
