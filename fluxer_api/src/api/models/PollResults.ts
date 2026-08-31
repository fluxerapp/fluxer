// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessagePollResults, MessagePollResultsDb} from '../database/types/PollTypes';
import {PollAnswerCount} from './PollAnswerCount';

export class PollResults {
	readonly answer_counts: Array<PollAnswerCount>;
	readonly is_finalized: boolean | null;

	constructor(results: MessagePollResults | MessagePollResultsDb) {
		this.answer_counts = (results.answer_counts ?? []).map((answer_count) => new PollAnswerCount(answer_count));
		this.is_finalized = results.is_finalized ?? null;
	}

	toMessagePollResults(): MessagePollResults {
		return {
			answer_counts:
				this.answer_counts.length > 0
					? this.answer_counts.map((answer_count) => answer_count.toMessagePollAnswerCount())
					: null,
			is_finalized: this.is_finalized,
		};
	}
}
