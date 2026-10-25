// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MusicErrorCode} from '@fluxer/music_protocol/src/Types';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export function musicErrorText(code: MusicErrorCode | 'TIMEOUT'): MessageDescriptor {
	switch (code) {
		case 'NOT_FOUND':
			return msg`That song isn't in the queue anymore.`;
		case 'QUEUE_FULL':
			return msg`The queue is full (500 songs).`;
		case 'TOO_LONG':
			return msg`That's longer than 3 hours.`;
		case 'LIVE_NOT_SUPPORTED':
			return msg`Live streams can't be played.`;
		case 'NO_RESULTS':
			return msg`No results.`;
		case 'SOURCE_FAILED':
			return msg`Couldn't load that. Try again in a moment.`;
		case 'BAD_REQUEST':
			return msg`That didn't work.`;
		case 'TIMEOUT':
			return msg`The music bot didn't answer.`;
	}
}
