// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildMessageSelectionCopyTextForRange} from '@app/features/messaging/utils/MessageSelectionCopyUtils';
import {afterEach, describe, expect, it} from 'vitest';

describe('Message selection copy utils', () => {
	afterEach(() => {
		document.body.replaceChildren();
	});
	it('uses markdown table copy text when a rendered table is selected cell-by-cell', () => {
		const tableCopyText = ['| Name | Status |', '| --- | --- |', '| Alpha | Ready |'].join('\n');
		document.body.innerHTML = `
			<div data-message-selection-root="true">
				<div data-message-id="message-1" data-is-group-start="true">
					<div data-message-copy-block="true" data-message-copy-table="true"></div>
				</div>
			</div>
		`;
		const copyBlock = document.querySelector<HTMLElement>('[data-message-copy-block="true"]');
		if (!copyBlock) {
			throw new Error('Missing copy block fixture');
		}
		copyBlock.dataset.messageCopyText = tableCopyText;
		copyBlock.innerHTML = `
			<table>
				<thead>
					<tr><th>Name</th><th>Status</th></tr>
				</thead>
				<tbody>
					<tr><td>Alpha</td><td>Ready</td></tr>
				</tbody>
			</table>
		`;
		const root = document.querySelector<HTMLElement>('[data-message-selection-root="true"]');
		const firstCell = copyBlock.querySelector<HTMLElement>('th');
		const lastCell = Array.from(copyBlock.querySelectorAll<HTMLElement>('td')).at(-1);
		if (!root || !firstCell?.firstChild || !lastCell?.firstChild) {
			throw new Error('Missing table selection fixture');
		}
		const range = document.createRange();
		range.setStart(firstCell.firstChild, 0);
		range.setEnd(lastCell.firstChild, 'Ready'.length);
		expect(buildMessageSelectionCopyTextForRange({rootElement: root, selectionRange: range})).toBe(tableCopyText);
	});
	it('does not duplicate a block message header when a bot badge is selected with the body', () => {
		const messageContent = 'App Canary Deployed\n\nVersion: 2026.519.3\nImage: 2026.519.3';
		document.body.innerHTML = [
			'<div data-message-selection-root="true">',
			'<div data-message-id="message-1" data-is-group-start="true">',
			'<h3>',
			'<span><span data-user-id="user-1">Canary Releases</span><span>Bot</span></span>',
			'<span><time><span aria-hidden="true"> — </span>Today at 4:10</time></span>',
			'</h3>',
			'<div data-message-copy-block="true"><h2><span>App Canary Deployed</span></h2><span>Version: </span>',
			'<code>2026.519.3</code><span>\nImage: </span><code>2026.519.3</code></div>',
			'</div>',
			'</div>',
		].join('');
		const copyBlock = document.querySelector<HTMLElement>('[data-message-copy-block="true"]');
		if (!copyBlock) {
			throw new Error('Missing message copy block fixture');
		}
		copyBlock.dataset.messageCopyText = messageContent;
		const root = document.querySelector<HTMLElement>('[data-message-selection-root="true"]');
		const username = document.querySelector<HTMLElement>('[data-user-id="user-1"]')?.firstChild;
		const imageVersion = Array.from(document.querySelectorAll<HTMLElement>('code')).at(-1)?.firstChild;
		if (!root || !username || !imageVersion) {
			throw new Error('Missing block message selection fixture');
		}
		const range = document.createRange();
		range.setStart(username, 0);
		range.setEnd(imageVersion, '2026.519.3'.length);
		expect(buildMessageSelectionCopyTextForRange({rootElement: root, selectionRange: range})).toBe(
			['Canary Releases — Today at 4:10', messageContent].join('\n'),
		);
	});
	it('does not duplicate a compact message prefix when a bot badge is selected with the body', () => {
		document.body.innerHTML = [
			'<div data-message-selection-root="true">',
			'<div data-message-id="message-1" data-is-group-start="true">',
			'<span><time><span>[</span>4:10<span>]</span></time></span>',
			'<span data-compact-message-prefix="true"><span> </span><span>Bot</span>',
			'<span data-user-id="user-1">Canary Releases</span><span>: </span></span>',
			'<span>Hello from canary.</span>',
			'</div>',
			'</div>',
		].join('');
		const root = document.querySelector<HTMLElement>('[data-message-selection-root="true"]');
		const timestamp = document.querySelector<HTMLElement>('time')?.firstChild;
		const body = document.querySelector<HTMLElement>('[data-compact-message-prefix="true"] + span')?.firstChild;
		if (!root || !timestamp || !body) {
			throw new Error('Missing compact message selection fixture');
		}
		const range = document.createRange();
		range.setStart(timestamp, 0);
		range.setEnd(body, 'Hello from canary.'.length);
		expect(buildMessageSelectionCopyTextForRange({rootElement: root, selectionRange: range})).toBe(
			'[4:10] Canary Releases: Hello from canary.',
		);
	});
});
