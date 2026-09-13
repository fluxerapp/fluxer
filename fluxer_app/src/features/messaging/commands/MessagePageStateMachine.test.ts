// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createMessagePageStateSnapshot,
	type MessagePageStateInput,
	resolveMessagePageState,
	selectMessagePageState,
	transitionMessagePageStateSnapshot,
} from '@app/features/messaging/commands/MessagePageStateMachine';
import {describe, expect, it} from 'vitest';

function input(overrides: Partial<MessagePageStateInput> = {}): MessagePageStateInput {
	return {
		before: null,
		after: null,
		limit: 50,
		messageCount: 0,
		aroundMessageId: null,
		aroundTargetIndex: -1,
		newestFetchedMessageId: null,
		knownLatestMessageId: null,
		...overrides,
	};
}

describe('messagePageStateMachine', () => {
	it('opens older pagination for full replacement pages', () => {
		expect(resolveMessagePageState(input({messageCount: 50}))).toMatchObject({
			isBefore: false,
			isAfter: false,
			hasMoreBefore: true,
			hasMoreAfter: false,
			shouldWarnMissingAroundTarget: false,
			aroundDebug: null,
		});
	});

	it('routes directional pages from before and after cursors', () => {
		expect(resolveMessagePageState(input({before: 'm-1', messageCount: 50}))).toMatchObject({
			isBefore: true,
			isAfter: false,
			hasMoreBefore: true,
			hasMoreAfter: false,
		});
		expect(resolveMessagePageState(input({after: 'm-1', messageCount: 50}))).toMatchObject({
			isBefore: false,
			isAfter: true,
			hasMoreBefore: false,
			hasMoreAfter: true,
		});
	});

	it('closes pagination for partial non-around pages', () => {
		expect(resolveMessagePageState(input({before: 'm-1', messageCount: 10}))).toMatchObject({
			hasMoreBefore: false,
			hasMoreAfter: false,
		});
		expect(resolveMessagePageState(input({after: 'm-1', messageCount: 10}))).toMatchObject({
			hasMoreBefore: false,
			hasMoreAfter: false,
		});
	});

	it('keeps around pagination open when the target is missing', () => {
		expect(
			resolveMessagePageState(
				input({
					aroundMessageId: 'target',
					aroundTargetIndex: -1,
					messageCount: 25,
				}),
			),
		).toMatchObject({
			hasMoreBefore: true,
			hasMoreAfter: true,
			shouldWarnMissingAroundTarget: true,
			aroundDebug: null,
		});
	});

	it('uses around-window pagination when the target is present', () => {
		expect(
			resolveMessagePageState(
				input({
					aroundMessageId: 'target',
					aroundTargetIndex: 25,
					messageCount: 50,
					newestFetchedMessageId: 'newest',
					knownLatestMessageId: 'latest',
				}),
			),
		).toMatchObject({
			hasMoreBefore: true,
			hasMoreAfter: true,
			shouldWarnMissingAroundTarget: false,
			aroundDebug: {
				messagesNewerThanTarget: 25,
				messagesOlderThanTarget: 24,
				expectedNewer: 25,
				expectedOlder: 24,
				pageFilled: true,
			},
		});
	});

	it('closes newer around pagination when the response reaches the known latest message', () => {
		expect(
			resolveMessagePageState(
				input({
					aroundMessageId: 'target',
					aroundTargetIndex: 0,
					messageCount: 25,
					newestFetchedMessageId: 'latest',
					knownLatestMessageId: 'latest',
				}),
			),
		).toMatchObject({
			hasMoreBefore: true,
			hasMoreAfter: false,
		});
	});

	it('re-routes when page inputs change', () => {
		const aroundSnapshot = createMessagePageStateSnapshot(
			input({
				aroundMessageId: 'target',
				aroundTargetIndex: -1,
			}),
		);
		expect(selectMessagePageState(aroundSnapshot)).toMatchObject({shouldWarnMissingAroundTarget: true});

		const standardSnapshot = transitionMessagePageStateSnapshot(aroundSnapshot, {
			type: 'messagePageState.updated',
			input: input({after: 'm-1', messageCount: 50}),
		});

		expect(selectMessagePageState(standardSnapshot)).toMatchObject({
			isAfter: true,
			hasMoreAfter: true,
			shouldWarnMissingAroundTarget: false,
		});
	});
});
