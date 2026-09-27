// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessagePollAnswerCount, MessagePollAnswerCountDb} from '../database/types/PollTypes';

export class PollAnswerCount {
	readonly id: number | null;
	readonly count: number | null;
	readonly me_voted: boolean | null;

	constructor(answer_count: MessagePollAnswerCount | MessagePollAnswerCountDb) {
		this.id = answer_count.id ?? null;
		this.count = answer_count.count ?? null;
		this.me_voted = ('me_voted' in answer_count && answer_count.me_voted) ?? null;
	}

	toMessagePollAnswerCount(): MessagePollAnswerCount {
		return {
			id: this.id,
			count: this.count,
			me_voted: this.me_voted,
		};
	}
}
