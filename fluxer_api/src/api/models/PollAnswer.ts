// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessagePollAnswer} from '../database/types/PollTypes';
import {PollMedia} from './PollMedia';

export class PollAnswer {
	readonly answer_id: number | null;
	readonly poll_media: PollMedia | null;

	constructor(answer: MessagePollAnswer) {
		this.answer_id = answer.answer_id ?? null;
		this.poll_media = answer.poll_media ? new PollMedia(answer.poll_media) : null;
	}

	toMessagePollAnswer(): MessagePollAnswer {
		return {
			answer_id: this.answer_id,
			poll_media: this.poll_media?.toMessagePollMedia() ?? null,
		};
	}
}
