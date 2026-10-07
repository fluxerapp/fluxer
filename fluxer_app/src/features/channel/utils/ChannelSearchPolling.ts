// SPDX-License-Identifier: AGPL-3.0-or-later

const CHANNEL_SEARCH_INDEXING_INITIAL_POLL_INTERVAL_MS = 1500;
const CHANNEL_SEARCH_INDEXING_MAX_POLL_INTERVAL_MS = 5000;
const CHANNEL_SEARCH_INDEXING_POLL_BACKOFF_MULTIPLIER = 1.25;

export function getChannelSearchIndexingPollInterval(pollCount: number): number {
	const normalizedPollCount = Math.max(0, pollCount);
	return Math.min(
		CHANNEL_SEARCH_INDEXING_INITIAL_POLL_INTERVAL_MS *
			CHANNEL_SEARCH_INDEXING_POLL_BACKOFF_MULTIPLIER ** normalizedPollCount,
		CHANNEL_SEARCH_INDEXING_MAX_POLL_INTERVAL_MS,
	);
}
