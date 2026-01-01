/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

interface TransformedUser {
	id: string;
	username: string;
	isBot?: boolean;
	isFriend?: boolean;
	guildIds?: Array<string>;
	_delete?: boolean;
	_removeGuild?: string;
	[key: string]: string | boolean | undefined | Array<string>;
}

interface SearchFilters {
	friends?: boolean;
	guild?: string;
}

interface SearchQuery {
	query: string;
	filters?: SearchFilters;
	blacklist?: Array<string>;
	whitelist?: Array<string>;
	boosters?: Record<string, number>;
	limit: number;
}

interface SearchResult {
	id: string;
	username: string;
	comparator: string;
	score: number;
	isBot?: boolean;
}

export enum MessageTypes {
	UPDATE_USERS = 'UPDATE_USERS',
	USER_RESULTS = 'USER_RESULTS',
	QUERY_SET = 'QUERY_SET',
	QUERY_CLEAR = 'QUERY_CLEAR',
}

interface WorkerMessage<T = unknown> {
	uuid?: string;
	type: MessageTypes;
	payload?: T;
}

interface UpdateUsersPayload {
	users: Array<TransformedUser>;
}

const userIndex: Map<string, TransformedUser> = new Map();
const activeQueries: Map<string, SearchQuery> = new Map();
const pendingSearches: Set<string> = new Set();

const SCORE_EXACT_PREFIX = 10;
const SCORE_CONTAINS = 5;
const SCORE_FUZZY = 1;

const FRIEND_KEY = 'isFriend';
const BOT_KEY = 'isBot';
const USERNAME_KEY = 'username';
const IGNORED_KEYS = new Set([BOT_KEY, FRIEND_KEY, USERNAME_KEY, 'guildIds']);

function escapeRegex(text: string): string {
	return text.replace(/[-[\]/{}()*+?.\\^$|]/g, '\\$&');
}

function fuzzyMatch(needle: string, haystack: string): boolean {
	const needleLength = needle.length;
	const haystackLength = haystack.length;

	if (needleLength > haystackLength) return false;
	if (needleLength === haystackLength) return needle === haystack;

	let needleIndex = 0;
	for (let haystackIndex = 0; haystackIndex < haystackLength; haystackIndex++) {
		if (needle.charCodeAt(needleIndex) === haystack.charCodeAt(haystackIndex)) {
			needleIndex++;
			if (needleIndex === needleLength) return true;
		}
	}
	return false;
}

function sortByMatchScore(a: SearchResult, b: SearchResult): number {
	if (a.score === b.score) {
		const aComp = a.comparator.toLowerCase();
		const bComp = b.comparator.toLowerCase();
		if (aComp < bComp) return -1;
		if (aComp > bComp) return 1;
		return 0;
	}
	return b.score - a.score;
}

function shouldIncludeUser(
	userId: string,
	user: TransformedUser,
	filters?: SearchFilters,
	blacklist?: Array<string>,
	whitelist?: Array<string>,
): boolean {
	if (blacklist?.includes(userId)) return false;
	if (whitelist?.includes(userId)) return true;

	if (filters?.friends === true && user.isFriend !== true) {
		return false;
	}

	if (filters?.guild) {
		return user[filters.guild] === true;
	}

	return true;
}

function calculateScore(baseScore: number, booster?: number): number {
	return baseScore * (booster ?? 1);
}

function postSearchResults(uuid: string, results: Array<SearchResult>): void {
	const payload = results.map((r) => ({
		id: r.id,
		username: r.username,
		isBot: r.isBot,
	}));

	const message: WorkerMessage<typeof payload> = {
		uuid,
		type: MessageTypes.USER_RESULTS,
		payload,
	};

	postMessage(message);
}

function executeSearch(uuid: string, searchQuery: SearchQuery): void {
	const {query, limit, filters, blacklist, whitelist, boosters} = searchQuery;
	const results: Array<SearchResult> = [];

	if (query === '') {
		postSearchResults(uuid, results);
		return;
	}

	const exactPrefixRegex = new RegExp(`^${escapeRegex(query)}`, 'i');
	const containsRegex = new RegExp(escapeRegex(query), 'i');
	const queryLower = query.toLowerCase();

	userIndex.forEach((user, userId) => {
		if (!shouldIncludeUser(userId, user, filters, blacklist, whitelist)) {
			return;
		}

		const username = user.username;
		let bestMatch: SearchResult | null = null;

		for (const key of Object.keys(user)) {
			const value = user[key];
			if (key === BOT_KEY || key === FRIEND_KEY || !value || typeof value !== 'string') {
				continue;
			}

			let matchResult: SearchResult | null = null;

			if (exactPrefixRegex.test(value)) {
				matchResult = {
					id: userId,
					username,
					comparator: value,
					score: calculateScore(SCORE_EXACT_PREFIX, boosters?.[userId]),
					isBot: user.isBot,
				};
			} else if (containsRegex.test(value)) {
				matchResult = {
					id: userId,
					username,
					comparator: value,
					score: calculateScore(SCORE_CONTAINS, boosters?.[userId]),
					isBot: user.isBot,
				};
			} else if (fuzzyMatch(queryLower, value.toLowerCase())) {
				matchResult = {
					id: userId,
					username,
					comparator: value,
					score: calculateScore(SCORE_FUZZY, boosters?.[userId]),
					isBot: user.isBot,
				};
			}

			if (matchResult && (!bestMatch || bestMatch.score < matchResult.score)) {
				bestMatch = matchResult;
			}
		}

		if (bestMatch) {
			results.push(bestMatch);
		}
	});

	results.sort(sortByMatchScore);

	if (results.length > limit) {
		results.length = limit;
	}

	postSearchResults(uuid, results);
}

function updateUsers(users: Array<TransformedUser>): void {
	let shouldTriggerSearch = false;
	const updatedGuilds = new Set<string>();

	for (const update of users) {
		const userId = update.id;

		if (update._delete === true) {
			userIndex.delete(userId);
			shouldTriggerSearch = true;
			continue;
		}

		const existingUser: TransformedUser = userIndex.get(userId) ?? {id: userId, username: ''};
		const mergedUser: TransformedUser = {...existingUser, ...update};

		const guildIdsSet = new Set<string>(existingUser.guildIds ?? []);
		if (update.guildIds) {
			for (const guildId of update.guildIds) {
				guildIdsSet.add(guildId);
			}
		}

		if (update._removeGuild) {
			const guildKey = update._removeGuild;
			if (guildKey in mergedUser) {
				delete mergedUser[guildKey];
			}
			delete mergedUser._removeGuild;
			guildIdsSet.delete(guildKey);
			updatedGuilds.add(guildKey);
		}

		delete mergedUser._delete;

		if (guildIdsSet.size > 0) {
			mergedUser.guildIds = Array.from(guildIdsSet);
		} else {
			delete mergedUser.guildIds;
		}

		const wasFriend = Boolean(existingUser.isFriend);
		const isFriendNow = Boolean(mergedUser.isFriend);

		userIndex.set(userId, mergedUser);

		if (activeQueries.size > 0) {
			if (isFriendNow || wasFriend !== isFriendNow) {
				shouldTriggerSearch = true;
			}

			for (const key of Object.keys(mergedUser)) {
				if (IGNORED_KEYS.has(key)) {
					continue;
				}
				updatedGuilds.add(key);
			}
		}
	}

	if (!shouldTriggerSearch && updatedGuilds.size === 0) {
		return;
	}

	for (const [uuid, query] of activeQueries.entries()) {
		const {filters} = query;

		const interestedInFriends = !filters || filters.friends === true;
		const interestedInGuild = !filters?.guild || updatedGuilds.has(filters.guild);

		if ((shouldTriggerSearch && interestedInFriends) || interestedInGuild) {
			pendingSearches.add(uuid);
		}
	}

	if (pendingSearches.size > 0) {
		debouncedExecuteSearches();
	}
}

function setQuery(uuid: string, query: SearchQuery): void {
	activeQueries.set(uuid, query);
	executeSearch(uuid, query);
}

function clearQuery(uuid: string): void {
	activeQueries.delete(uuid);
	pendingSearches.delete(uuid);
}

let debounceTimeout: ReturnType<typeof setTimeout> | null = null;

function debouncedExecuteSearches(): void {
	if (debounceTimeout) {
		clearTimeout(debounceTimeout);
	}

	debounceTimeout = setTimeout(() => {
		for (const uuid of pendingSearches) {
			const query = activeQueries.get(uuid);
			if (query) {
				executeSearch(uuid, query);
			}
		}
		pendingSearches.clear();
		debounceTimeout = null;
	}, 100);
}

addEventListener('message', (event: MessageEvent<WorkerMessage>) => {
	const data = event.data;
	if (!data) {
		throw new Error('Invalid data');
	}

	const {uuid, type, payload} = data;

	switch (type) {
		case MessageTypes.UPDATE_USERS: {
			const p = payload as UpdateUsersPayload | undefined;
			if (p?.users) {
				updateUsers(p.users);
			}
			break;
		}
		case MessageTypes.QUERY_SET: {
			if (!uuid) return;
			setQuery(uuid, payload as SearchQuery);
			break;
		}
		case MessageTypes.QUERY_CLEAR: {
			if (!uuid) return;
			clearQuery(uuid);
			break;
		}
	}
});
