// SPDX-License-Identifier: AGPL-3.0-or-later

export const CROSSPOST_CHANNEL_RATE_LIMIT = {maxAttempts: 10, windowMs: 3_600_000} as const;
export const PUBLISHED_MESSAGE_EDIT_RATE_LIMIT = {maxAttempts: 3, windowMs: 3_600_000} as const;

export const CROSSPOST_FANOUT_CHUNK_SIZE = 25;
export const CROSSPOST_FANOUT_CONCURRENCY = 5;
export const CROSSPOST_FANOUT_PAGE_SIZE = 500;
export const CROSSPOST_SYNC_COALESCE_MS = 10_000;
export const CROSSPOST_PENDING_RECLAIM_AFTER_MS = 60_000;
export const CROSSPOST_CHUNK_RETRY_DELAYS_MS: ReadonlyArray<number> = [
	10_000, 30_000, 60_000, 120_000, 300_000, 600_000, 1_200_000, 2_400_000,
];

export const CHANNEL_FOLLOWER_STATS_CACHE_SECONDS = 60;
export const FOLLOWER_WEBHOOK_NAME_MAX_LENGTH = 80;
export const CROSSPOST_SOURCE_DELETED_CONTENT = '[Original message deleted]';
