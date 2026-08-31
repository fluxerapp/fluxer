// SPDX-License-Identifier: AGPL-3.0-or-later

import {bench, describe} from 'vitest';
import {
	createPollVoteMachineSnapshot,
	emptyMap,
	mapToAnswerCounts,
	transitionPollVoteSnapshot,
} from './PollVoteStateMachine';

const ANSWERS: Array<number> = Array.from({length: 32}, (_value, index) => (index % 4));

describe('PollVoteStateMachine benchmarks', () => {
	bench('apply 1k pollVote add/remove transitions for visible messages', () => {
		let snapshot = createPollVoteMachineSnapshot(emptyMap(), 'me');
		for (let index = 0; index < 1_000; index += 1) {
			const answerId = ANSWERS[index % ANSWERS.length];
			const userId = `user-${index % 250}`;
			snapshot = transitionPollVoteSnapshot(snapshot, {
				type: 'pollVote.add',
				answerId,
				userId,
				isCurrentUser: userId === 'me',
			});
			if (index % 4 === 0) {
				snapshot = transitionPollVoteSnapshot(snapshot, {
					type: 'pollVote.remove',
					answerId,
					userId,
					isCurrentUser: false,
				});
			}
		}
		mapToAnswerCounts(snapshot.context.map);
	});

	bench('hydrate 500-message pollVote payload shape', () => {
		let snapshot = createPollVoteMachineSnapshot(emptyMap(), 'me');
		for (let index = 0; index < 500; index += 1) {
			snapshot = transitionPollVoteSnapshot(snapshot, {
				type: 'pollVote.hydrate',
				currentUserId: 'me',
				votes: ANSWERS.slice(0, 8).map((answerId, pollVoteIndex) => ({
					answerId,
					count: 1 + ((index + pollVoteIndex) % 50),
					me: pollVoteIndex === index % 8 ? true : undefined,
				})),
			});
		}
		mapToAnswerCounts(snapshot.context.map);
	});
});
