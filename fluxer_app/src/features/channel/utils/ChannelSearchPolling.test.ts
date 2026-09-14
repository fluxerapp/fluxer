// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CHANNEL_SEARCH_INDEXING_INITIAL_POLL_INTERVAL_MS,
	CHANNEL_SEARCH_INDEXING_MAX_POLL_INTERVAL_MS,
	getChannelSearchIndexingPollInterval,
} from '@app/features/channel/utils/ChannelSearchPolling';
import {describe, expect, it} from 'vitest';

describe('getChannelSearchIndexingPollInterval', () => {
	it('starts with a short retry interval for newly indexing channels', () => {
		expect(getChannelSearchIndexingPollInterval(0)).toBe(CHANNEL_SEARCH_INDEXING_INITIAL_POLL_INTERVAL_MS);
	});
	it('backs off without exceeding the maximum interval', () => {
		expect(getChannelSearchIndexingPollInterval(1)).toBeGreaterThan(CHANNEL_SEARCH_INDEXING_INITIAL_POLL_INTERVAL_MS);
		expect(getChannelSearchIndexingPollInterval(100)).toBe(CHANNEL_SEARCH_INDEXING_MAX_POLL_INTERVAL_MS);
	});
	it('treats negative poll counts as the first poll', () => {
		expect(getChannelSearchIndexingPollInterval(-1)).toBe(CHANNEL_SEARCH_INDEXING_INITIAL_POLL_INTERVAL_MS);
	});
});
