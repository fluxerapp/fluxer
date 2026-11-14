// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ChannelIncomingMessageInput {
	hasNonceMatch: boolean;
	isUploadPlaceholder: boolean;
	hasMoreAfter: boolean;
	afterBufferAtBoundary: boolean;
}

export type ChannelIncomingMessageDecision =
	| {
			type: 'completeUploadPlaceholder';
	  }
	| {
			type: 'replaceNonceMessage';
	  }
	| {
			type: 'ignorePastVisibleWindow';
			shouldClearAfterBoundary: boolean;
	  }
	| {
			type: 'appendIncoming';
	  };
type ChannelIncomingMessageStateValue =
	| 'completeUploadPlaceholder'
	| 'replaceNonceMessage'
	| 'ignorePastVisibleWindow'
	| 'appendIncoming';

function resolveChannelIncomingMessageState(input: ChannelIncomingMessageInput): ChannelIncomingMessageStateValue {
	if (input.hasNonceMatch && input.isUploadPlaceholder) return 'completeUploadPlaceholder';
	if (input.hasNonceMatch) return 'replaceNonceMessage';
	if (input.hasMoreAfter) return 'ignorePastVisibleWindow';
	return 'appendIncoming';
}

function buildChannelIncomingMessageDecision(
	state: ChannelIncomingMessageStateValue,
	input: ChannelIncomingMessageInput,
): ChannelIncomingMessageDecision {
	switch (state) {
		case 'completeUploadPlaceholder':
			return {type: 'completeUploadPlaceholder'};
		case 'replaceNonceMessage':
			return {type: 'replaceNonceMessage'};
		case 'ignorePastVisibleWindow':
			return {
				type: 'ignorePastVisibleWindow',
				shouldClearAfterBoundary: input.afterBufferAtBoundary,
			};
		case 'appendIncoming':
			return {type: 'appendIncoming'};
	}
}

export function resolveChannelIncomingMessageDecision(
	input: ChannelIncomingMessageInput,
): ChannelIncomingMessageDecision {
	return buildChannelIncomingMessageDecision(resolveChannelIncomingMessageState(input), input);
}
