// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelIncomingMessageInput,
	createChannelIncomingMessageSnapshot,
	resolveChannelIncomingMessageDecision,
	selectChannelIncomingMessageDecision,
	transitionChannelIncomingMessageSnapshot,
} from '@app/features/messaging/state/ChannelIncomingMessageStateMachine';
import {describe, expect, it} from 'vitest';

function input(overrides: Partial<ChannelIncomingMessageInput> = {}): ChannelIncomingMessageInput {
	return {
		hasNonceMatch: false,
		isUploadPlaceholder: false,
		hasMoreAfter: false,
		afterBufferAtBoundary: false,
		...overrides,
	};
}

describe('channelIncomingMessageMachine', () => {
	it('completes upload placeholders before any other placement rule', () => {
		expect(
			resolveChannelIncomingMessageDecision(
				input({
					hasNonceMatch: true,
					isUploadPlaceholder: true,
					hasMoreAfter: true,
					afterBufferAtBoundary: true,
				}),
			),
		).toEqual({type: 'completeUploadPlaceholder'});
	});

	it('replaces matching nonce messages before checking the visible window', () => {
		expect(
			resolveChannelIncomingMessageDecision(
				input({
					hasNonceMatch: true,
					isUploadPlaceholder: false,
					hasMoreAfter: true,
				}),
			),
		).toEqual({type: 'replaceNonceMessage'});
	});

	it('ignores incoming messages past the visible window and reports boundary clearing intent', () => {
		expect(
			resolveChannelIncomingMessageDecision(
				input({
					hasMoreAfter: true,
					afterBufferAtBoundary: true,
				}),
			),
		).toEqual({
			type: 'ignorePastVisibleWindow',
			shouldClearAfterBoundary: true,
		});
		expect(resolveChannelIncomingMessageDecision(input({hasMoreAfter: true}))).toEqual({
			type: 'ignorePastVisibleWindow',
			shouldClearAfterBoundary: false,
		});
	});

	it('appends messages when no cache-placement rule intercepts them', () => {
		expect(resolveChannelIncomingMessageDecision(input())).toEqual({type: 'appendIncoming'});
	});

	it('re-routes when incoming message inputs change', () => {
		const staleWindowSnapshot = createChannelIncomingMessageSnapshot(input({hasMoreAfter: true}));
		expect(selectChannelIncomingMessageDecision(staleWindowSnapshot)).toEqual({
			type: 'ignorePastVisibleWindow',
			shouldClearAfterBoundary: false,
		});

		const appendSnapshot = transitionChannelIncomingMessageSnapshot(staleWindowSnapshot, {
			type: 'channelIncomingMessage.updated',
			input: input(),
		});

		expect(selectChannelIncomingMessageDecision(appendSnapshot)).toEqual({type: 'appendIncoming'});
	});
});
