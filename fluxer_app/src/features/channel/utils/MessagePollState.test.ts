// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PollResponse} from '@fluxer/schema/src/domains/message/PollSchemas';
import {describe, expect, it} from 'vitest';
import {
	getPollOptionPercentage,
	getPollOptionRank,
	getPollOptionResultValue,
	getPollTotalResultValue,
	getPollTotalVotes,
	getSelectedPollOptionIds,
	isPollClosed,
	mergePollViewerSelection,
	togglePollOptionSelection,
} from './MessagePollState';

const makePoll = (overrides: Partial<PollResponse> = {}): PollResponse => ({
	id: '100',
	title: 'Lunch?',
	expires_at: '2026-07-05T12:00:00.000Z',
	closed: false,
	anonymous: false,
	allow_ranked_choice: false,
	allow_custom_answers: false,
	options: [
		{id: '101', text: 'Pizza', attachment_id: null, vote_count: 2, me: true, voter_ids: ['1']},
		{id: '102', text: 'Sushi', attachment_id: null, vote_count: 1, voter_ids: ['2']},
	],
	...overrides,
});

describe('MessagePollState', () => {
	it('treats explicit and expired polls as closed', () => {
		expect(isPollClosed(makePoll({closed: true}), Date.parse('2026-07-05T10:00:00.000Z'))).toBe(true);
		expect(isPollClosed(makePoll(), Date.parse('2026-07-05T12:00:00.000Z'))).toBe(true);
		expect(isPollClosed(makePoll(), Date.parse('2026-07-05T11:59:59.000Z'))).toBe(false);
	});

	it('calculates totals and whole-number percentages', () => {
		const poll = makePoll();
		const totalVotes = getPollTotalVotes(poll);
		expect(totalVotes).toBe(3);
		expect(getPollOptionPercentage(poll.options[0].vote_count, totalVotes)).toBe(67);
		expect(getPollOptionPercentage(poll.options[1].vote_count, totalVotes)).toBe(33);
		expect(getPollOptionPercentage(1, 0)).toBe(0);
	});

	it('uses ranked scores as the result value for ranked-choice polls', () => {
		const poll = makePoll({
			allow_ranked_choice: true,
			options: [
				{id: '101', text: 'Pizza', attachment_id: null, vote_count: 1, ranked_score: 5, rank_counts: [1, 1]},
				{id: '102', text: 'Sushi', attachment_id: null, vote_count: 1, ranked_score: 3, rank_counts: [1, 0]},
			],
		});
		expect(getPollOptionResultValue(poll.options[0], true)).toBe(5);
		expect(getPollTotalResultValue(poll)).toBe(8);
		expect(
			getPollOptionPercentage(getPollOptionResultValue(poll.options[0], true), getPollTotalResultValue(poll)),
		).toBe(63);
	});

	it('reads selected options and toggles ranked-choice order', () => {
		const poll = makePoll();
		expect(getSelectedPollOptionIds(poll)).toEqual(['101']);
		expect(togglePollOptionSelection(['101'], '102', false)).toEqual(['102']);
		expect(togglePollOptionSelection(['101'], '102', true)).toEqual(['101', '102']);
		expect(togglePollOptionSelection(['101', '102'], '101', true)).toEqual(['102']);
		expect(getPollOptionRank(['101', '102'], '102')).toBe(2);
		expect(getPollOptionRank(['101', '102'], '103')).toBeNull();
	});

	it('can preserve viewer poll selections across neutral updates', () => {
		const current = makePoll();
		const incoming = makePoll({
			options: [
				{id: '101', text: 'Pizza', attachment_id: null, vote_count: 3, voter_ids: ['1', '3']},
				{id: '102', text: 'Sushi', attachment_id: null, vote_count: 1, voter_ids: ['2']},
			],
		});

		expect(mergePollViewerSelection(current, incoming, true)?.options[0]).toMatchObject({
			id: '101',
			me: true,
			vote_count: 3,
		});
		expect(mergePollViewerSelection(current, incoming, false)?.options[0].me).toBeUndefined();
	});
});
