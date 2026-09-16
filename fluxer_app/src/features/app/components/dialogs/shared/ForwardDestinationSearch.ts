// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ForwardResultType} from '@app/features/app/components/dialogs/shared/ForwardDestinationQuery';
import {
	buildChannelSearchTerms,
	consumeBestSearchTerm,
	createSearchTerm,
	escapeSearchPattern,
	fuzzySearch,
	scoreSearchTerm,
	stripCombiningMarks,
	toConfusableSkeleton,
} from '@app/features/search/utils/SearchTextMatching';

const SCORE_SCALE = 1000;
const FRIEND_BOOST = 0.2;
const OPEN_DM_BOOST = 0.1;
const GROUP_DM_MEMBER_SCORE_CAP = 5;
const CHANNEL_CONTEXT_WEIGHT = 0.5;
const CHANNEL_CONTEXT_SCORE_CAP = 6;
const VOICE_IN_TEXT_SEARCH_PENALTY = 1;
const VOICE_IN_TEXT_SEARCH_FLOOR = 0.5;
const CHANNEL_FRECENCY_BONUS = 3;

type ForwardChannelKind = 'text' | 'voice';

export interface ForwardUserCandidate {
	readonly friendNickname: string | null;
	readonly globalName: string | null;
	readonly id: string;
	readonly nicknames: ReadonlyArray<string>;
	readonly username: string;
}

export interface ForwardGroupDMCandidate {
	readonly id: string;
	readonly memberFields: ReadonlyArray<string>;
	readonly name: string;
}

export interface ForwardChannelCandidate {
	readonly canAccess: boolean;
	readonly guildName: string | null;
	readonly hasFrecency: boolean;
	readonly id: string;
	readonly kind: ForwardChannelKind;
	readonly name: string;
	readonly parentName: string | null;
}

export interface ForwardFrequentItem {
	readonly id: string;
	readonly kind: 'dm' | 'group_dm' | 'text' | 'voice' | 'other';
	readonly recipientId?: string | null;
	readonly score: number;
}

export interface ForwardSearchBoosters {
	readonly groupDMs: ReadonlyMap<string, number>;
	readonly textChannels: ReadonlyMap<string, number>;
	readonly users: ReadonlyMap<string, number>;
	readonly voiceChannels: ReadonlyMap<string, number>;
}

export interface ForwardSearchResult {
	readonly comparator: string;
	readonly id: string;
	readonly score: number;
	readonly type: ForwardResultType;
}

interface BuildForwardSearchBoostersRequest {
	readonly dmUserIds: Iterable<string>;
	readonly frequent: ReadonlyArray<ForwardFrequentItem>;
	readonly friendIds: Iterable<string>;
}

interface SearchForwardDestinationsRequest {
	readonly blacklist: ReadonlySet<string>;
	readonly boosters: ForwardSearchBoosters;
	readonly channels: ReadonlyArray<ForwardChannelCandidate>;
	readonly confusables: ReadonlyMap<string, string>;
	readonly groupDMs: ReadonlyArray<ForwardGroupDMCandidate>;
	readonly limit: number;
	readonly query: string;
	readonly resultTypes: ReadonlyArray<ForwardResultType>;
	readonly users: ReadonlyArray<ForwardUserCandidate>;
}

export function buildForwardSearchBoosters({
	dmUserIds,
	frequent,
	friendIds,
}: BuildForwardSearchBoostersRequest): ForwardSearchBoosters {
	const maxScore = frequent.reduce((max, item) => Math.max(max, item.score), 0);
	const users = new Map<string, number>();
	const groupDMs = new Map<string, number>();
	const textChannels = new Map<string, number>();
	const voiceChannels = new Map<string, number>();
	for (const item of frequent) {
		const boost = maxScore > 0 ? 1 + item.score / maxScore : 1;
		switch (item.kind) {
			case 'dm':
				if (item.recipientId != null) users.set(item.recipientId, boost);
				break;
			case 'group_dm':
				groupDMs.set(item.id, boost);
				break;
			case 'text':
				textChannels.set(item.id, boost);
				break;
			case 'voice':
				voiceChannels.set(item.id, boost);
				break;
			case 'other':
				break;
		}
	}
	for (const friendId of friendIds) users.set(friendId, (users.get(friendId) ?? 1) + FRIEND_BOOST);
	for (const userId of dmUserIds) users.set(userId, (users.get(userId) ?? 1) + OPEN_DM_BOOST);
	return Object.freeze({groupDMs, textChannels, users, voiceChannels});
}

export function searchForwardDestinations(
	request: SearchForwardDestinationsRequest,
): ReadonlyArray<ForwardSearchResult> {
	const {boosters, channels, confusables, limit, query, resultTypes} = request;
	if (query.trim() === '') return [];
	const results = [
		...(resultTypes.includes('user')
			? searchUsers(query, request.users, boosters.users, request.blacklist, confusables, limit)
			: []),
		...(resultTypes.includes('group_dm')
			? searchGroupDMs(query, request.groupDMs, boosters.groupDMs, confusables, limit)
			: []),
		...(resultTypes.includes('text_channel')
			? searchChannels(query, channels, 'text', boosters.textChannels, limit)
			: []),
		...(resultTypes.includes('voice_channel')
			? searchChannels(query, channels, 'voice', boosters.voiceChannels, limit)
			: []),
	];
	const seenKeys = new Set<string>();
	const merged = results.filter((result) => {
		const key = `${result.type}-${result.id}`;
		if (seenKeys.has(key)) return false;
		seenKeys.add(key);
		return true;
	});
	return merged.sort(compareSearchResults);
}

function compareSearchResults(left: ForwardSearchResult, right: ForwardSearchResult): number {
	if (left.score === right.score && left.type === 'user') {
		const leftName = left.comparator.toLocaleLowerCase();
		const rightName = right.comparator.toLocaleLowerCase();
		if (leftName < rightName) return -1;
		if (leftName > rightName) return 1;
	}
	return right.score - left.score;
}

function sortAndLimit(results: Array<ForwardSearchResult>, limit: number): Array<ForwardSearchResult> {
	return results.sort(compareSearchResults).slice(0, limit);
}

function searchUsers(
	query: string,
	users: ReadonlyArray<ForwardUserCandidate>,
	boosters: ReadonlyMap<string, number>,
	blacklist: ReadonlySet<string>,
	confusables: ReadonlyMap<string, string>,
	limit: number,
): Array<ForwardSearchResult> {
	const escapedQuery = escapeSearchPattern(query);
	const prefixQuery = new RegExp(`^${escapedQuery}`, 'i');
	const containQuery = new RegExp(escapedQuery, 'i');
	const queryLower = query.toLocaleLowerCase();
	const querySkeleton = toConfusableSkeleton(queryLower, confusables);
	const scoreField = (field: string): number => {
		if (prefixQuery.test(field)) return 10;
		if (containQuery.test(field)) return 5;
		const stripped = stripCombiningMarks(field.toLocaleLowerCase());
		if (fuzzySearch(queryLower, stripped)) return 1;
		if (fuzzySearch(querySkeleton, toConfusableSkeleton(stripped, confusables))) return 1;
		return 0;
	};
	const matches: Array<ForwardSearchResult> = [];
	for (const user of users) {
		if (blacklist.has(user.id)) continue;
		const booster = boosters.get(user.id) ?? 1;
		if (user.id === query) {
			matches.push({comparator: user.id, id: user.id, score: 10 * booster, type: 'user'});
			continue;
		}
		let best: ForwardSearchResult | null = null;
		for (const field of [user.username, user.friendNickname, user.globalName, ...user.nicknames]) {
			if (field == null) continue;
			const score = scoreField(field) * booster;
			if (score === 0 || (best != null && best.score >= score)) continue;
			best = {comparator: field, id: user.id, score, type: 'user'};
		}
		if (best != null) matches.push(best);
	}
	return sortAndLimit(matches, limit).map((match) => Object.freeze({...match, score: SCORE_SCALE * match.score}));
}

function searchGroupDMs(
	query: string,
	groupDMs: ReadonlyArray<ForwardGroupDMCandidate>,
	boosters: ReadonlyMap<string, number>,
	confusables: ReadonlyMap<string, string>,
	limit: number,
): Array<ForwardSearchResult> {
	const normalize = (text: string): string =>
		stripCombiningMarks(toConfusableSkeleton(text.toLocaleLowerCase(), confusables));
	const term = createSearchTerm(normalize(query));
	const results: Array<ForwardSearchResult> = [];
	for (const groupDM of groupDMs) {
		let score = scoreSearchTerm(normalize(groupDM.name), term);
		for (const field of groupDM.memberFields) {
			score = Math.max(score, Math.min(GROUP_DM_MEMBER_SCORE_CAP, scoreSearchTerm(normalize(field), term)));
		}
		if (score === 0) continue;
		results.push(
			Object.freeze({
				comparator: groupDM.name,
				id: groupDM.id,
				score: SCORE_SCALE * score * (boosters.get(groupDM.id) ?? 1),
				type: 'group_dm',
			}),
		);
	}
	return sortAndLimit(results, limit);
}

function searchChannels(
	query: string,
	channels: ReadonlyArray<ForwardChannelCandidate>,
	searchKind: ForwardChannelKind,
	boosters: ReadonlyMap<string, number>,
	limit: number,
): Array<ForwardSearchResult> {
	const terms = buildChannelSearchTerms(query);
	const results: Array<ForwardSearchResult> = [];
	for (const channel of channels) {
		if (searchKind === 'voice' && channel.kind !== 'voice') continue;
		if (!channel.canAccess) continue;
		const remainingTerms = [...terms];
		let score = consumeBestSearchTerm(channel.name.toLocaleLowerCase(), remainingTerms, true);
		if (score === 0) continue;
		if (remainingTerms.length > 0) {
			for (const context of [channel.guildName, channel.parentName]) {
				if (context == null || context === '') continue;
				score += CHANNEL_CONTEXT_WEIGHT * consumeBestSearchTerm(context.toLocaleLowerCase(), remainingTerms, false);
			}
			score = Math.min(CHANNEL_CONTEXT_SCORE_CAP, score);
		}
		if (remainingTerms.length > 1) continue;
		if (remainingTerms.length === 1 && !remainingTerms[0].isFullMatch) continue;
		if (searchKind === 'text' && channel.kind === 'voice') {
			score = Math.max(score - VOICE_IN_TEXT_SEARCH_PENALTY, VOICE_IN_TEXT_SEARCH_FLOOR);
		}
		score = Math.min(score + (channel.hasFrecency ? CHANNEL_FRECENCY_BONUS : 0), score >= 7 ? 10 : 7);
		results.push(
			Object.freeze({
				comparator: channel.name,
				id: channel.id,
				score: SCORE_SCALE * score * (boosters.get(channel.id) ?? 1),
				type: channel.kind === 'voice' ? 'voice_channel' : 'text_channel',
			}),
		);
	}
	return sortAndLimit(results, limit);
}
