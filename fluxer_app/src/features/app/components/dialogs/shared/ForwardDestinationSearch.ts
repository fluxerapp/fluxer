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
const SNOWFLAKE_SCORE = 10;

export type ForwardChannelKind = 'text' | 'voice';

export interface ForwardUserCandidate {
	readonly friendAlias: string | null;
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

export interface ForwardGuildCandidate {
	readonly id: string;
	readonly name: string;
}

export interface ForwardFrequentItem {
	readonly id: string;
	readonly kind: 'dm' | 'group_dm' | 'guild' | 'text' | 'voice' | 'other';
	readonly recipientId?: string | null;
	readonly score: number;
}

export interface ForwardSearchWeights {
	readonly groupDMs: ReadonlyMap<string, number>;
	readonly guilds: ReadonlyMap<string, number>;
	readonly textChannel: ReadonlyMap<string, number>;
	readonly users: ReadonlyMap<string, number>;
	readonly voiceChannel: ReadonlyMap<string, number>;
}

export interface ForwardSearchResult {
	readonly matchedText: string;
	readonly id: string;
	readonly score: number;
	readonly type: ForwardResultType;
}

interface BuildForwardSearchWeightsRequest {
	readonly dmUserIds: Iterable<string>;
	readonly frequent: ReadonlyArray<ForwardFrequentItem>;
	readonly friendIds: Iterable<string>;
}

interface SearchForwardDestinationsRequest {
	readonly excludedIds: ReadonlySet<string>;
	readonly weights: ForwardSearchWeights;
	readonly channels: ReadonlyArray<ForwardChannelCandidate>;
	readonly confusables: ReadonlyMap<string, string>;
	readonly groupDMs: ReadonlyArray<ForwardGroupDMCandidate>;
	readonly limit: number;
	readonly query: string;
	readonly kinds: ReadonlyArray<ForwardResultType>;
	readonly users: ReadonlyArray<ForwardUserCandidate>;
}

export function buildForwardSearchWeights({
	dmUserIds,
	frequent,
	friendIds,
}: BuildForwardSearchWeightsRequest): ForwardSearchWeights {
	const maxScore = frequent.reduce((max, item) => Math.max(max, item.score), 0);
	const users = new Map<string, number>();
	const groupDMs = new Map<string, number>();
	const guilds = new Map<string, number>();
	const textChannel = new Map<string, number>();
	const voiceChannel = new Map<string, number>();
	for (const item of frequent) {
		const weight = maxScore > 0 ? 1 + item.score / maxScore : 1;
		switch (item.kind) {
			case 'dm':
				if (item.recipientId != null) users.set(item.recipientId, weight);
				break;
			case 'group_dm':
				groupDMs.set(item.id, weight);
				break;
			case 'guild':
				guilds.set(item.id, weight);
				break;
			case 'text':
				textChannel.set(item.id, weight);
				break;
			case 'voice':
				voiceChannel.set(item.id, weight);
				break;
			case 'other':
				break;
		}
	}
	for (const friendId of friendIds) users.set(friendId, (users.get(friendId) ?? 1) + FRIEND_BOOST);
	for (const userId of dmUserIds) users.set(userId, (users.get(userId) ?? 1) + OPEN_DM_BOOST);
	return Object.freeze({groupDMs, guilds, textChannel, users, voiceChannel});
}

export function searchForwardDestinations(
	request: SearchForwardDestinationsRequest,
): ReadonlyArray<ForwardSearchResult> {
	const {weights, channels, confusables, limit, query, kinds} = request;
	if (query.trim() === '') return [];
	const results = [
		...(kinds.includes('user')
			? searchUsers(query, request.users, weights.users, request.excludedIds, confusables, limit)
			: []),
		...(kinds.includes('group_dm')
			? searchGroupDMs(query, request.groupDMs, weights.groupDMs, confusables, limit)
			: []),
		...(kinds.includes('text_channel') ? searchChannels(query, channels, 'text', weights.textChannel, limit) : []),
		...(kinds.includes('voice_channel') ? searchChannels(query, channels, 'voice', weights.voiceChannel, limit) : []),
	];
	const seenKeys = new Set<string>();
	const merged = results.filter((result) => {
		const key = `${result.type}|${result.id}`;
		if (seenKeys.has(key)) return false;
		seenKeys.add(key);
		return true;
	});
	return merged.sort(compareSearchResults);
}

export function compareSearchResults(left: ForwardSearchResult, right: ForwardSearchResult): number {
	if (left.score === right.score && left.type === 'user') {
		const leftName = left.matchedText.toLocaleLowerCase();
		const rightName = right.matchedText.toLocaleLowerCase();
		if (leftName < rightName) return -1;
		if (leftName > rightName) return 1;
	}
	return right.score - left.score;
}

function sortAndLimit(results: Array<ForwardSearchResult>, limit: number): Array<ForwardSearchResult> {
	return results.sort(compareSearchResults).slice(0, limit);
}

export function searchUsers(
	query: string,
	users: ReadonlyArray<ForwardUserCandidate>,
	weights: ReadonlyMap<string, number>,
	excludedIds: ReadonlySet<string>,
	confusables: ReadonlyMap<string, string>,
	limit: number,
): Array<ForwardSearchResult> {
	const escapedQuery = escapeSearchPattern(query);
	const prefixQuery = new RegExp(`^${escapedQuery}`, 'i');
	const substringPattern = new RegExp(escapedQuery, 'i');
	const loweredText = query.toLocaleLowerCase();
	const querySkeleton = toConfusableSkeleton(loweredText, confusables);
	const scoreField = (field: string): number => {
		if (prefixQuery.test(field)) return 10;
		if (substringPattern.test(field)) return 5;
		const stripped = stripCombiningMarks(field.toLocaleLowerCase());
		if (fuzzySearch(loweredText, stripped)) return 1;
		if (fuzzySearch(querySkeleton, toConfusableSkeleton(stripped, confusables))) return 1;
		return 0;
	};
	const matches: Array<ForwardSearchResult> = [];
	for (const user of users) {
		if (excludedIds.has(user.id)) continue;
		const booster = weights.get(user.id) ?? 1;
		if (user.id === query) {
			matches.push({matchedText: user.id, id: user.id, score: 10 * booster, type: 'user'});
			continue;
		}
		let best: ForwardSearchResult | null = null;
		for (const field of [user.username, user.friendAlias, user.globalName, ...user.nicknames]) {
			if (field == null) continue;
			const score = scoreField(field) * booster;
			if (score === 0 || (best != null && best.score >= score)) continue;
			best = {matchedText: field, id: user.id, score, type: 'user'};
		}
		if (best != null) matches.push(best);
	}
	return sortAndLimit(matches, limit).map((match) => Object.freeze({...match, score: SCORE_SCALE * match.score}));
}

export function searchGroupDMs(
	query: string,
	groupDMs: ReadonlyArray<ForwardGroupDMCandidate>,
	weights: ReadonlyMap<string, number>,
	confusables: ReadonlyMap<string, string>,
	limit: number,
): Array<ForwardSearchResult> {
	const foldText = (text: string): string =>
		stripCombiningMarks(toConfusableSkeleton(text.toLocaleLowerCase(), confusables));
	const term = createSearchTerm(foldText(query));
	const results: Array<ForwardSearchResult> = [];
	for (const groupDM of groupDMs) {
		let score = scoreSearchTerm(foldText(groupDM.name), term);
		for (const field of groupDM.memberFields) {
			score = Math.max(score, Math.min(GROUP_DM_MEMBER_SCORE_CAP, scoreSearchTerm(foldText(field), term)));
		}
		if (score === 0) continue;
		results.push(
			Object.freeze({
				matchedText: groupDM.name,
				id: groupDM.id,
				score: SCORE_SCALE * score * (weights.get(groupDM.id) ?? 1),
				type: 'group_dm',
			}),
		);
	}
	return sortAndLimit(results, limit);
}

export function searchChannels(
	query: string,
	channels: ReadonlyArray<ForwardChannelCandidate>,
	searchKind: ForwardChannelKind,
	weights: ReadonlyMap<string, number>,
	limit: number,
	matchExactIds = false,
): Array<ForwardSearchResult> {
	const terms = buildChannelSearchTerms(query);
	const results: Array<ForwardSearchResult> = [];
	for (const channel of channels) {
		if (searchKind === 'voice' && channel.kind !== 'voice') continue;
		if (!channel.canAccess) continue;
		const remainingTerms = [...terms];
		const isSnowflakeMatch = matchExactIds && channel.id === query;
		let score = isSnowflakeMatch
			? SNOWFLAKE_SCORE
			: consumeBestSearchTerm(channel.name.toLocaleLowerCase(), remainingTerms, true);
		if (score === 0) continue;
		if (!isSnowflakeMatch && remainingTerms.length > 0) {
			for (const context of [channel.guildName, channel.parentName]) {
				if (context == null || context === '') continue;
				score += CHANNEL_CONTEXT_WEIGHT * consumeBestSearchTerm(context.toLocaleLowerCase(), remainingTerms, false);
			}
			score = Math.min(CHANNEL_CONTEXT_SCORE_CAP, score);
		}
		if (!isSnowflakeMatch && remainingTerms.length > 1) continue;
		if (!isSnowflakeMatch && remainingTerms.length === 1 && !remainingTerms[0].spansWholeQuery) continue;
		if (searchKind === 'text' && channel.kind === 'voice') {
			score = Math.max(score - VOICE_IN_TEXT_SEARCH_PENALTY, VOICE_IN_TEXT_SEARCH_FLOOR);
		}
		score = Math.min(score + (channel.hasFrecency ? CHANNEL_FRECENCY_BONUS : 0), score >= 7 ? 10 : 7);
		results.push(
			Object.freeze({
				matchedText: channel.name,
				id: channel.id,
				score: SCORE_SCALE * score * (weights.get(channel.id) ?? 1),
				type: channel.kind === 'voice' ? 'voice_channel' : 'text_channel',
			}),
		);
	}
	return sortAndLimit(results, limit);
}

export interface GuildSearchResult {
	readonly matchedText: string;
	readonly id: string;
	readonly score: number;
}

export function searchGuilds(
	query: string,
	guilds: ReadonlyArray<ForwardGuildCandidate>,
	weights: ReadonlyMap<string, number>,
	excludedIds: ReadonlySet<string>,
	limit: number,
	matchExactIds = false,
): Array<GuildSearchResult> {
	const term = createSearchTerm(query.toLocaleLowerCase());
	const results: Array<GuildSearchResult> = [];
	for (const guild of guilds) {
		if (excludedIds.has(guild.id)) continue;
		const score =
			matchExactIds && guild.id === query ? SNOWFLAKE_SCORE : scoreSearchTerm(guild.name.toLocaleLowerCase(), term);
		if (score === 0) continue;
		results.push(
			Object.freeze({
				matchedText: guild.name,
				id: guild.id,
				score: SCORE_SCALE * score * (weights.get(guild.id) ?? 1),
			}),
		);
	}
	return results.sort((left, right) => right.score - left.score).slice(0, limit);
}
