// SPDX-License-Identifier: AGPL-3.0-or-later

export const DEFAULT_SYNCED_FIELD_MAX_ENCODED_BYTES = 65_536;

export const FAVORITE_GIF_MAX_ENCODED_BYTES = 384 * 1024;

export type OversizePushDecision = 'within-budget' | 'push-shrinks' | 'drop';

export function decideOversizePush(args: {
	encodedBytes: number;
	maxEncodedBytes: number;
	lastPreparedBytes: number | null;
}): OversizePushDecision {
	if (args.encodedBytes <= args.maxEncodedBytes) return 'within-budget';
	if (args.lastPreparedBytes != null && args.encodedBytes < args.lastPreparedBytes) return 'push-shrinks';
	return 'drop';
}
