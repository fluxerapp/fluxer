// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CHANNEL_MESSAGE_ID_PREFIX,
	findMessageElement,
	getMessageSelector,
} from '@app/features/messaging/utils/MessageNodeSelectors';
import {afterEach, describe, expect, it} from 'vitest';

const CHANNEL_ID = '900000000000000001';

function messageRow(messageId: string, idPrefix: string): HTMLElement {
	const row = document.createElement('div');
	row.id = `${idPrefix}-${CHANNEL_ID}-${messageId}`;
	row.dataset.messageId = messageId;
	row.dataset.channelId = CHANNEL_ID;
	return row;
}

function viewport(): HTMLElement {
	const element = document.createElement('div');
	document.body.append(element);
	return element;
}

describe('MessageNodeSelectors', () => {
	afterEach(() => {
		document.body.replaceChildren();
	});

	it('resolves a normal stream row through the id fast path', () => {
		const scroller = viewport();
		const row = messageRow('1', CHANNEL_MESSAGE_ID_PREFIX);
		scroller.append(row);

		expect(findMessageElement(document, scroller, CHANNEL_ID, '1')).toBe(row);
	});

	it('resolves a revealed blocked group row that carries a different id prefix', () => {
		const scroller = viewport();
		scroller.append(messageRow('1', CHANNEL_MESSAGE_ID_PREFIX));
		const blockedRow = messageRow('2', 'blocked-messages');
		scroller.append(blockedRow);

		expect(document.getElementById(`${CHANNEL_MESSAGE_ID_PREFIX}-${CHANNEL_ID}-2`)).toBeNull();
		expect(findMessageElement(document, scroller, CHANNEL_ID, '2')).toBe(blockedRow);
	});

	it('resolves a revealed spammer group row', () => {
		const scroller = viewport();
		const spammerRow = messageRow('3', 'spammer-messages');
		scroller.append(spammerRow);

		expect(findMessageElement(document, scroller, CHANNEL_ID, '3')).toBe(spammerRow);
	});

	it('falls back to the viewport-scoped lookup and ignores rows outside it', () => {
		const scroller = viewport();
		const searchPanel = viewport();
		searchPanel.append(messageRow('4', 'blocked-messages'));

		expect(document.getElementById(`${CHANNEL_MESSAGE_ID_PREFIX}-${CHANNEL_ID}-4`)).toBeNull();
		expect(findMessageElement(document, scroller, CHANNEL_ID, '4')).toBeNull();
	});

	it('resolves a row through the document-wide id lookup even when it sits outside the viewport', () => {
		const scroller = viewport();
		const popout = viewport();
		const inPopout = messageRow('8', CHANNEL_MESSAGE_ID_PREFIX);
		popout.append(inPopout);

		expect(findMessageElement(document, scroller, CHANNEL_ID, '8')).toBe(inPopout);
	});

	it('prefers the row inside the viewport when neither row carries the canonical id', () => {
		const scroller = viewport();
		const inStream = messageRow('5', 'blocked-messages');
		scroller.append(inStream);
		const searchPanel = viewport();
		const inSearch = messageRow('5', 'search-messages');
		searchPanel.append(inSearch);

		expect(document.getElementById(`${CHANNEL_MESSAGE_ID_PREFIX}-${CHANNEL_ID}-5`)).toBeNull();
		expect(findMessageElement(document, scroller, CHANNEL_ID, '5')).toBe(inStream);
	});

	it('does not resolve a row belonging to another channel', () => {
		const scroller = viewport();
		const foreign = document.createElement('div');
		foreign.dataset.messageId = '6';
		foreign.dataset.channelId = '900000000000000002';
		scroller.append(foreign);

		expect(findMessageElement(document, scroller, CHANNEL_ID, '6')).toBeNull();
	});

	it('requires both data attributes so message group row wrappers are not candidates', () => {
		const scroller = viewport();
		const groupWrapper = document.createElement('div');
		groupWrapper.dataset.messageId = '7';
		const row = messageRow('7', CHANNEL_MESSAGE_ID_PREFIX);
		groupWrapper.append(row);
		scroller.append(groupWrapper);

		const matches = scroller.querySelectorAll<HTMLElement>(getMessageSelector(CHANNEL_ID));
		expect(Array.from(matches)).toEqual([row]);
	});

	it('leaves snowflake ids untouched instead of emitting identifier escapes', () => {
		expect(getMessageSelector(CHANNEL_ID, '900000000000000009')).toBe(
			'[data-channel-id="900000000000000001"][data-message-id="900000000000000009"]',
		);
	});

	it('escapes quotes and backslashes so an id can never break out of the attribute selector', () => {
		expect(getMessageSelector(undefined, 'a"]b')).toBe('[data-channel-id][data-message-id="a\\"]b"]');
		expect(getMessageSelector(undefined, 'a\\b')).toBe('[data-channel-id][data-message-id="a\\\\b"]');
	});
});
