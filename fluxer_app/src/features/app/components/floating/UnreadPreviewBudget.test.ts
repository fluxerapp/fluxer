// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BULK_PREVIEW_CHANNEL_BATCH_SIZE,
	UNREAD_PREVIEW_MESSAGE_LIMIT,
} from '@app/features/app/components/floating/UnreadPreviewBudget';
import {BulkMessageFetchRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import {describe, expect, it} from 'vitest';

const buildBatch = (limit: number) => ({
	requests: Array.from({length: BULK_PREVIEW_CHANNEL_BATCH_SIZE}, (_, index) => ({
		channel_id: String(1000000000000000000n + BigInt(index)),
		limit,
	})),
});

describe('unread preview fetch budget', () => {
	it('keeps the anchored window inside the bulk fetch schema', () => {
		expect(() => BulkMessageFetchRequest.parse(buildBatch(UNREAD_PREVIEW_MESSAGE_LIMIT * 2))).not.toThrow();
	});

	it('keeps the unanchored window inside the bulk fetch schema', () => {
		expect(() => BulkMessageFetchRequest.parse(buildBatch(UNREAD_PREVIEW_MESSAGE_LIMIT))).not.toThrow();
	});

	it('rejects a batch one channel wider than the client sends', () => {
		const oversized = buildBatch(UNREAD_PREVIEW_MESSAGE_LIMIT * 2);
		oversized.requests.push({channel_id: '1000000000000000099', limit: UNREAD_PREVIEW_MESSAGE_LIMIT * 2});
		expect(() => BulkMessageFetchRequest.parse(oversized)).toThrow();
	});
});
