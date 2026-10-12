// SPDX-License-Identifier: AGPL-3.0-or-later

import {CHANNEL_REINDEX_AFTER_TIMESTAMP} from '@fluxer/constants/src/ChannelConstants';

export function channelNeedsReindexing(
	indexedAt: Date | null | undefined,
	cutoffSeconds = CHANNEL_REINDEX_AFTER_TIMESTAMP,
): boolean {
	if (!indexedAt) {
		return true;
	}
	const indexedAtSeconds = Math.floor(indexedAt.getTime() / 1000);
	return indexedAtSeconds < cutoffSeconds;
}
