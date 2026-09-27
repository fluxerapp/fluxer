// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessagePoll, MessagePollDb} from '../database/types/PollTypes';
import {PollAnswer} from './PollAnswer';
import {PollMedia} from './PollMedia';
import {PollResults} from './PollResults';

export class Poll {
	readonly question: PollMedia | null;
	readonly answers: Array<PollAnswer>;
	readonly expiry: string | null;
	readonly anonymous_voting: boolean | null;
	readonly allow_multiselect: boolean | null;
	readonly layout_type: number | null;
	readonly results: PollResults | null;

	constructor(poll: MessagePollDb) {
		this.question = poll.question ? new PollMedia(poll.question) : null;
		this.answers = (poll.answers ?? []).map((answer) => new PollAnswer(answer));
		this.expiry = poll.expiry ?? null;
		this.anonymous_voting = poll.anonymous_voting ?? null;
		this.allow_multiselect = poll.allow_multiselect ?? null;
		this.layout_type = poll.layout_type ?? null;
		this.results = poll.results ? new PollResults(poll.results) : null;
	}

	toMessagePoll(): MessagePoll {
		return {
			question: this.question?.toMessagePollMedia() ?? null,
			answers: this.answers.length > 0 ? this.answers.map((answer) => answer.toMessagePollAnswer()) : null,
			expiry: this.expiry ?? null,
			anonymous_voting: this.anonymous_voting,
			allow_multiselect: this.allow_multiselect,
			layout_type: this.layout_type,
			results: this.results?.toMessagePollResults() ?? null,
		};
	}
}
