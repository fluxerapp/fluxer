// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PollOptionResponse, PollResponse} from '@fluxer/schema/src/domains/message/PollSchemas';

export function isPollClosed(poll: PollResponse, nowMs = Date.now()): boolean {
	if (poll.closed) {
		return true;
	}
	const expiresAtMs = Date.parse(poll.expires_at);
	return Number.isFinite(expiresAtMs) && expiresAtMs <= nowMs;
}

export function getPollTotalVotes(poll: Pick<PollResponse, 'options'>): number {
	return poll.options.reduce((total, option) => total + Math.max(0, option.vote_count), 0);
}

export function getPollOptionResultValue(
	option: Pick<PollOptionResponse, 'ranked_score' | 'vote_count'>,
	allowRankedChoice: boolean,
): number {
	return Math.max(0, allowRankedChoice ? (option.ranked_score ?? option.vote_count) : option.vote_count);
}

export function getPollTotalResultValue(poll: Pick<PollResponse, 'allow_ranked_choice' | 'options'>): number {
	return poll.options.reduce((total, option) => total + getPollOptionResultValue(option, poll.allow_ranked_choice), 0);
}

export function getPollOptionPercentage(voteCount: number, totalVotes: number): number {
	if (totalVotes <= 0) {
		return 0;
	}
	return Math.round((Math.max(0, voteCount) / totalVotes) * 100);
}

export function getSelectedPollOptionIds(poll: Pick<PollResponse, 'options'>): Array<string> {
	return poll.options.filter((option) => option.me).map((option) => option.id);
}

export function togglePollOptionSelection(
	selectedOptionIds: ReadonlyArray<string>,
	optionId: string,
	allowRankedChoice: boolean,
): Array<string> {
	if (!allowRankedChoice) {
		return [optionId];
	}
	if (selectedOptionIds.includes(optionId)) {
		return selectedOptionIds.filter((selectedOptionId) => selectedOptionId !== optionId);
	}
	return [...selectedOptionIds, optionId];
}

export function getPollOptionRank(selectedOptionIds: ReadonlyArray<string>, optionId: string): number | null {
	const index = selectedOptionIds.indexOf(optionId);
	return index === -1 ? null : index + 1;
}

export function mergePollViewerSelection(
	currentPoll: PollResponse | null,
	incomingPoll: PollResponse | null,
	preserveViewerSelection: boolean,
): PollResponse | null {
	if (!preserveViewerSelection || !currentPoll || !incomingPoll || currentPoll.id !== incomingPoll.id) {
		return incomingPoll;
	}
	const selectedOptionIds = new Set(getSelectedPollOptionIds(currentPoll));
	if (selectedOptionIds.size === 0) {
		return incomingPoll;
	}
	return {
		...incomingPoll,
		options: incomingPoll.options.map((option) => {
			if (option.me !== undefined || !selectedOptionIds.has(option.id)) {
				return option;
			}
			return {...option, me: true};
		}),
	};
}
