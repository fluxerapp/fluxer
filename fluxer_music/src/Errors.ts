// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MusicErrorCode} from '@fluxer/music_protocol/src/Types';

const DEFAULT_MESSAGES: Record<MusicErrorCode, string> = {
	NOT_FOUND: "I couldn't find that.",
	QUEUE_FULL: 'The queue is full.',
	TOO_LONG: 'That track is too long.',
	LIVE_NOT_SUPPORTED: "Live streams aren't supported.",
	NO_RESULTS: 'No results found.',
	SOURCE_FAILED: "I couldn't load that track.",
	BAD_REQUEST: "I didn't understand that.",
};

export class MusicError extends Error {
	constructor(
		readonly code: MusicErrorCode,
		message?: string,
	) {
		super(message ?? DEFAULT_MESSAGES[code]);
		this.name = 'MusicError';
	}
}
