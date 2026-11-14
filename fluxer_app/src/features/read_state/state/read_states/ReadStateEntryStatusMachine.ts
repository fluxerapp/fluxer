// SPDX-License-Identifier: AGPL-3.0-or-later

import {compareMessageIds} from '@app/features/read_state/state/read_states/shared';

export interface ReadStateEntryStatusInput {
	supportsUnreadTracking: boolean;
	hasBlockedDirectMessageRecipient: boolean;
	lastMessageId: string | null;
	ackMessageId: string | null;
	ackTimestamp: number;
	lastMessageTimestamp: number;
	mentionCount: number;
}

type ReadStateEntryStatusValue = 'untracked' | 'blocked' | 'read' | 'unread';

export interface ReadStateEntryStatusModel {
	state: ReadStateEntryStatusValue;
	canBeUnread: boolean;
	supportsMentions: boolean;
	hasUnread: boolean;
	hasMentions: boolean;
	isUnreadOrMentioned: boolean;
}

function isUnread(context: ReadStateEntryStatusInput): boolean {
	if (context.lastMessageId == null) return false;
	if (context.ackMessageId != null) {
		return compareMessageIds(context.ackMessageId, context.lastMessageId) < 0;
	}
	return context.ackTimestamp < context.lastMessageTimestamp;
}

function getStatusValueFromInput(input: ReadStateEntryStatusInput): ReadStateEntryStatusValue {
	if (!input.supportsUnreadTracking) return 'untracked';
	if (input.hasBlockedDirectMessageRecipient) return 'blocked';
	if (isUnread(input)) return 'unread';
	return 'read';
}

function buildStatusModel(
	state: ReadStateEntryStatusValue,
	input: ReadStateEntryStatusInput,
): ReadStateEntryStatusModel {
	const hasMentions = input.mentionCount > 0;
	const canBeUnread = state !== 'untracked';
	const supportsMentions = hasMentions && state !== 'untracked' && state !== 'blocked';
	const hasUnread = state === 'unread';
	return {
		state,
		canBeUnread,
		supportsMentions,
		hasUnread,
		hasMentions,
		isUnreadOrMentioned: hasUnread || supportsMentions,
	};
}

export function resolveReadStateEntryStatus(input: ReadStateEntryStatusInput): ReadStateEntryStatusModel {
	return buildStatusModel(getStatusValueFromInput(input), input);
}
