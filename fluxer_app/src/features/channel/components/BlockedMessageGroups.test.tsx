// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {act, createElement, type ReactNode} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const rolloutMock = {enabled: true};
const ChannelStreamType = {
	MESSAGE: 'MESSAGE',
	MESSAGE_GROUP_BLOCKED: 'MESSAGE_GROUP_BLOCKED',
	MESSAGE_GROUP_IGNORED: 'MESSAGE_GROUP_IGNORED',
	MESSAGE_GROUP_SPAMMER: 'MESSAGE_GROUP_SPAMMER',
	DIVIDER: 'DIVIDER',
} as const;

vi.mock('@app/features/messaging/utils/MessageGroupingUtils', () => ({ChannelStreamType}));

vi.mock('@app/features/channel/state/BlockedMessageGroupsRollout', () => ({default: rolloutMock}));
vi.mock('@lingui/core/macro', () => ({msg: (value: unknown) => value}));
vi.mock('@lingui/react/macro', () => ({useLingui: () => ({i18n: {_: () => 'blocked messages'}})}));
vi.mock('@app/features/channel/components/ChannelDivider', () => ({
	Divider: ({children}: {children?: ReactNode}) => createElement('div', {'data-divider': true}, children),
}));
vi.mock('@app/features/channel/components/MessageGroup', () => ({
	MessageGroup: () => createElement('div', {'data-message-group': true}),
}));

const {BlockedMessageGroups} = await import('@app/features/channel/components/BlockedMessageGroups');

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

const CHANNEL = {id: 'channel-1', guild_id: null} as never;
const SPACER_SELECTOR = '[data-flx="channel.blocked-message-groups.group-spacer"]';

let container: HTMLDivElement;
let root: Root;
let consoleError: ReturnType<typeof vi.spyOn>;

function message(id: string): Record<string, unknown> {
	return {id, author: {id: 'author-1'}, blocked: true};
}

beforeEach(() => {
	rolloutMock.enabled = true;
	container = document.createElement('div');
	document.body.append(container);
	root = createRoot(container);
	consoleError = vi.spyOn(console, 'error').mockImplementation(() => undefined);
});

afterEach(() => {
	act(() => root.unmount());
	container.remove();
	consoleError.mockRestore();
});

function renderRevealedGroup(messageGroups: Array<unknown>): void {
	act(() => {
		root.render(
			createElement(BlockedMessageGroups, {
				channel: CHANNEL,
				messageGroups: messageGroups as never,
				onReveal: () => undefined,
				revealed: true,
				compact: false,
				messageGroupSpacing: 8,
				variant: 'blocked',
			}),
		);
	});
}

function duplicateKeyWarnings(): Array<unknown> {
	return consoleError.mock.calls.filter((call: Array<unknown>) => String(call[0]).includes('same key'));
}

const DIVIDER_INSIDE_GROUP = [
	{type: ChannelStreamType.MESSAGE, content: message('100'), contentKey: '100', groupId: 'g1'},
	{type: ChannelStreamType.DIVIDER, content: '', unreadId: '200', contentKey: 'divider-200'},
	{type: ChannelStreamType.MESSAGE, content: message('200'), contentKey: '200', groupId: 'g2'},
];

describe('BlockedMessageGroups experiment arm', () => {
	it('keys an unread divider apart from the message group below it', () => {
		renderRevealedGroup(DIVIDER_INSIDE_GROUP);

		expect(duplicateKeyWarnings()).toEqual([]);
		expect(container.querySelectorAll('[data-message-group]')).toHaveLength(2);
	});

	it('skips a divider that leads the revealed group', () => {
		renderRevealedGroup([
			{type: ChannelStreamType.DIVIDER, content: '', unreadId: '100', contentKey: 'divider-100'},
			{type: ChannelStreamType.MESSAGE, content: message('100'), contentKey: '100', groupId: 'g1'},
		]);

		expect(duplicateKeyWarnings()).toEqual([]);
		expect(container.querySelectorAll('[data-message-group]')).toHaveLength(1);
		expect(container.querySelectorAll('[data-divider]')).toHaveLength(0);
	});

	it('spaces consecutive groups apart inside the revealed block', () => {
		renderRevealedGroup([
			{type: ChannelStreamType.MESSAGE, content: message('100'), contentKey: '100', groupId: 'g1'},
			{type: ChannelStreamType.MESSAGE, content: message('200'), contentKey: '200', groupId: 'g2'},
			{type: ChannelStreamType.MESSAGE, content: message('300'), contentKey: '300', groupId: 'g3'},
		]);

		expect(container.querySelectorAll('[data-message-group]')).toHaveLength(3);
		expect(container.querySelectorAll(SPACER_SELECTOR)).toHaveLength(2);
	});
});

describe('BlockedMessageGroups control arm', () => {
	beforeEach(() => {
		rolloutMock.enabled = false;
	});

	it('renders no group spacers', () => {
		renderRevealedGroup([
			{type: ChannelStreamType.MESSAGE, content: message('100'), contentKey: '100', groupId: 'g1'},
			{type: ChannelStreamType.MESSAGE, content: message('200'), contentKey: '200', groupId: 'g2'},
		]);

		expect(container.querySelectorAll('[data-message-group]')).toHaveLength(2);
		expect(container.querySelectorAll(SPACER_SELECTOR)).toHaveLength(0);
	});
});
