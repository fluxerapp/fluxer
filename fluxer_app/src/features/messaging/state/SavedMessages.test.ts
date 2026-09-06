// SPDX-License-Identifier: AGPL-3.0-or-later

import {SavedMessageEntry} from '@app/features/messaging/models/SavedMessageEntry';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import {SAVED_MESSAGES_PAGE_SIZE} from '@fluxer/constants/src/LimitConstants';
import {beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/messaging/models/MessagingMessage', () => ({
	Message: class {
		readonly id: string;
		constructor(data: {id: string}) {
			this.id = data.id;
		}
	},
}));

function entryPage(startId: number, count: number): Array<SavedMessageEntry> {
	return Array.from({length: count}, (_unused, index) =>
		SavedMessageEntry.fromResponse({
			id: String(startId - index),
			channel_id: '10',
			message_id: String(startId - index),
			status: 'missing_permissions',
			message: null,
		}),
	);
}

describe('SavedMessages pagination', () => {
	beforeEach(() => {
		SavedMessages.handleGatewayReady();
	});

	it('reports more pages while a full page comes back and tracks the oldest entry', () => {
		const requestId = SavedMessages.handleFetchPending();
		SavedMessages.fetchSuccess(requestId, entryPage(1000, SAVED_MESSAGES_PAGE_SIZE), false);
		expect(SavedMessages.getHasMore()).toBe(true);
		expect(SavedMessages.getIsLoadingMore()).toBe(false);
		expect(SavedMessages.getCursor()).toBe(String(1000 - (SAVED_MESSAGES_PAGE_SIZE - 1)));
	});

	it('appends the next page instead of replacing the loaded one', () => {
		const first = SavedMessages.handleFetchPending();
		SavedMessages.fetchSuccess(first, entryPage(1000, SAVED_MESSAGES_PAGE_SIZE), false);
		const second = SavedMessages.handleFetchPending();
		SavedMessages.fetchSuccess(second, entryPage(900, 10), true);
		expect(SavedMessages.getMissingEntries()).toHaveLength(SAVED_MESSAGES_PAGE_SIZE + 10);
		expect(SavedMessages.getHasMore()).toBe(false);
		expect(SavedMessages.getCursor()).toBe('891');
	});

	it('ignores a page that a newer request has superseded', () => {
		const stale = SavedMessages.handleFetchPending();
		SavedMessages.handleFetchPending();
		SavedMessages.fetchSuccess(stale, entryPage(1000, 5), false);
		expect(SavedMessages.fetched).toBe(false);
		expect(SavedMessages.getMissingEntries()).toHaveLength(0);
	});

	it('keeps the loaded pages when loading more fails', () => {
		const first = SavedMessages.handleFetchPending();
		SavedMessages.fetchSuccess(first, entryPage(1000, SAVED_MESSAGES_PAGE_SIZE), false);
		const second = SavedMessages.handleFetchPending();
		SavedMessages.fetchError(second, true);
		expect(SavedMessages.getMissingEntries()).toHaveLength(SAVED_MESSAGES_PAGE_SIZE);
		expect(SavedMessages.getIsLoadingMore()).toBe(false);
		expect(SavedMessages.fetched).toBe(true);
	});

	it('clears everything when the first page fails', () => {
		const first = SavedMessages.handleFetchPending();
		SavedMessages.fetchSuccess(first, entryPage(1000, 5), false);
		const retry = SavedMessages.handleFetchPending();
		SavedMessages.fetchError(retry, false);
		expect(SavedMessages.getMissingEntries()).toHaveLength(0);
		expect(SavedMessages.fetched).toBe(false);
		expect(SavedMessages.getCursor()).toBeNull();
		expect(SavedMessages.getHasMore()).toBe(true);
	});
});
