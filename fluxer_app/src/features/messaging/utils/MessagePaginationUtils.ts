// SPDX-License-Identifier: AGPL-3.0-or-later

import {compare as compareSnowflakes} from '@fluxer/snowflake/src/SnowflakeUtils';

export interface AroundWindowCounts {
	newer: number;
	older: number;
}

export interface AroundPaginationStateInput {
	limit: number;
	messageCount: number;
	targetIndex: number;
	newestFetchedMessageId: string | null;
	knownLatestMessageId: string | null;
}

export interface AroundPaginationState {
	expectedNewer: number;
	expectedOlder: number;
	messagesNewer: number;
	messagesOlder: number;
	hasMoreBefore: boolean;
	hasMoreAfter: boolean;
	isAtKnownLatest: boolean;
}

export function getAroundWindowCounts(limit: number): AroundWindowCounts {
	const newer = Math.floor(Math.max(0, limit) / 2);
	return {
		newer,
		older: Math.max(0, limit - 1 - newer),
	};
}

export function calculateAroundPaginationState(input: AroundPaginationStateInput): AroundPaginationState {
	const {newer: expectedNewer, older: expectedOlder} = getAroundWindowCounts(input.limit);
	const messagesNewer = Math.max(0, input.targetIndex);
	const messagesOlder = Math.max(0, input.messageCount - input.targetIndex - 1);
	const isAtKnownLatest =
		input.newestFetchedMessageId != null &&
		input.knownLatestMessageId != null &&
		input.newestFetchedMessageId === input.knownLatestMessageId;
	return {
		expectedNewer,
		expectedOlder,
		messagesNewer,
		messagesOlder,
		hasMoreBefore: expectedOlder > 0 && messagesOlder >= expectedOlder,
		hasMoreAfter: expectedNewer > 0 && messagesNewer >= expectedNewer && !isAtKnownLatest,
		isAtKnownLatest,
	};
}

export function mergeAscendingById<T extends {id: string}>(existing: Array<T>, incoming: Array<T>): Array<T> {
	if (incoming.length === 0) return existing;
	const tail = existing[existing.length - 1];
	if (tail == null || compareSnowflakes(incoming[0].id, tail.id) > 0) {
		return existing.concat(incoming);
	}
	const merged: Array<T> = [];
	let left = 0;
	let right = 0;
	while (left < existing.length && right < incoming.length) {
		if (compareSnowflakes(incoming[right].id, existing[left].id) < 0) {
			merged.push(incoming[right]);
			right += 1;
		} else {
			merged.push(existing[left]);
			left += 1;
		}
	}
	while (left < existing.length) {
		merged.push(existing[left]);
		left += 1;
	}
	while (right < incoming.length) {
		merged.push(incoming[right]);
		right += 1;
	}
	return merged;
}
