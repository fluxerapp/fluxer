// SPDX-License-Identifier: AGPL-3.0-or-later

export enum MessageTypes {
	UPDATE_USERS = 'UPDATE_USERS',
	USER_RESULTS = 'USER_RESULTS',
	QUERY_SET = 'QUERY_SET',
	QUERY_CLEAR = 'QUERY_CLEAR',
}

interface TransformedUser {
	id: string;
	username: string;
	globalName?: string | null;
	guildNicknames?: Record<string, string | null>;
	isBot?: boolean;
	isFriend?: boolean;
	guildIds?: Array<string>;
	_delete?: boolean;
	_removeGuild?: string;
	[key: string]: string | boolean | null | undefined | Array<string> | Record<string, string | null>;
}

interface SearchResult {
	id: string;
	username: string;
	comparator: string;
	score: number;
	isBot?: boolean;
}

interface SearchFilters {
	friends?: boolean;
	guild?: string;
}

interface SearchQuery {
	query: string;
	limit: number;
	filters?: SearchFilters;
	blacklist?: Array<string>;
	whitelist?: Array<string>;
	boosters?: Record<string, number>;
	generation?: number;
}

interface WorkerMessage<T = unknown> {
	uuid?: string;
	type: MessageTypes;
	payload?: T;
	generation?: number;
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
const MAX_SEARCH_RESULTS = 100;
const FRIEND_KEY = 'isFriend';
const BOT_KEY = 'isBot';
const USERNAME_KEY = 'username';
const IGNORED_KEYS = new Set([BOT_KEY, FRIEND_KEY, USERNAME_KEY, 'guildIds', 'guildNicknames']);

function getSearchValues(user: TransformedUser, filters?: SearchFilters): Array<string> {
	const values: Array<string> = [];
	if (user.username.length > 0) {
		values.push(user.username);
	}
	for (const key of Object.keys(user)) {
		if (IGNORED_KEYS.has(key) || key === '_delete' || key === '_removeGuild') {
			continue;
		}
		const value = user[key];
		if (typeof value === 'string' && value.length > 0) {
			values.push(value);
		}
	}
	const guildNicknames = user.guildNicknames;
	if (guildNicknames == null) {
		return values;
	}
	const guildId = filters == null ? null : filters.guild;
	if (guildId != null && guildId.length > 0) {
		const nickname = guildNicknames[guildId];
		if (typeof nickname === 'string' && nickname.length > 0) {
			values.push(nickname);
		}
		return values;
	}
	for (const nickname of Object.values(guildNicknames)) {
		if (typeof nickname === 'string' && nickname.length > 0) {
			values.push(nickname);
		}
	}
	return values;
}

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

function normalizeSearchLimit(limit: number): number {
	if (!Number.isFinite(limit)) {
		return 0;
	}
	const normalizedLimit = Math.floor(limit);
	if (normalizedLimit <= 0) {
		return 0;
	}
	return Math.min(normalizedLimit, MAX_SEARCH_RESULTS);
}

function insertSearchResult(results: Array<SearchResult>, candidate: SearchResult, limit: number): void {
	if (results.length === 0) {
		results.push(candidate);
		return;
	}
	let insertIndex = results.length;
	for (let index = 0; index < results.length; index += 1) {
		if (sortByMatchScore(candidate, results[index]) < 0) {
			insertIndex = index;
			break;
		}
	}
	if (insertIndex === results.length && results.length >= limit) {
		return;
	}
	results.splice(insertIndex, 0, candidate);
	if (results.length > limit) {
		results.pop();
	}
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

function postSearchResults(uuid: string, results: Array<SearchResult>, generation: number): void {
	const payload = results.map((r) => ({
		id: r.id,
		username: r.username,
		isBot: r.isBot,
	}));
	const message: WorkerMessage<typeof payload> = {
		uuid,
		type: MessageTypes.USER_RESULTS,
		payload,
		generation,
	};
	postMessage(message);
}

function getEmptyQueryComparator(user: TransformedUser, filters?: SearchFilters): string {
	const guildNicknames = user.guildNicknames;
	const guildId = filters == null ? null : filters.guild;
	if (guildNicknames != null && guildId != null && guildId.length > 0) {
		const nickname = guildNicknames[guildId];
		if (typeof nickname === 'string' && nickname.length > 0) {
			return nickname;
		}
	}
	if (typeof user.globalName === 'string' && user.globalName.length > 0) {
		return user.globalName;
	}
	return user.username;
}

function executeSearch(uuid: string, searchQuery: SearchQuery): void {
	const {query, limit, filters, blacklist, whitelist, boosters, generation = 0} = searchQuery;
	const normalizedLimit = normalizeSearchLimit(limit);
	const results: Array<SearchResult> = [];
	if (normalizedLimit === 0) {
		postSearchResults(uuid, results, generation);
		return;
	}
	if (query === '') {
		userIndex.forEach((user, userId) => {
			if (!shouldIncludeUser(userId, user, filters, blacklist, whitelist)) {
				return;
			}
			insertSearchResult(
				results,
				{
					id: userId,
					username: user.username,
					comparator: getEmptyQueryComparator(user, filters),
					score: 0,
					isBot: user.isBot,
				},
				normalizedLimit,
			);
		});
		postSearchResults(uuid, results, generation);
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
		for (const value of getSearchValues(user, filters)) {
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
			insertSearchResult(results, bestMatch, normalizedLimit);
		}
	});
	postSearchResults(uuid, results, generation);
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
		const existingUser = userIndex.get(userId);
		if (update._removeGuild && existingUser == null) {
			continue;
		}
		const baseUser: TransformedUser = existingUser == null ? {id: userId, username: ''} : existingUser;
		const mergedUser: TransformedUser = {...baseUser, ...update};
		if (update._removeGuild && baseUser.username.length > 0) {
			mergedUser.username = baseUser.username;
		}
		const existingGuildNicknames = baseUser.guildNicknames;
		const updateGuildNicknames = update.guildNicknames;
		if (existingGuildNicknames != null || updateGuildNicknames != null) {
			mergedUser.guildNicknames = {
				...(existingGuildNicknames == null ? {} : existingGuildNicknames),
				...(updateGuildNicknames == null ? {} : updateGuildNicknames),
			};
		}
		const guildIdsSet = new Set<string>(baseUser.guildIds == null ? [] : baseUser.guildIds);
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
			if (mergedUser.guildNicknames != null) {
				delete mergedUser.guildNicknames[guildKey];
				if (Object.keys(mergedUser.guildNicknames).length === 0) {
					delete mergedUser.guildNicknames;
				}
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
		const wasFriend = Boolean(baseUser.isFriend);
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

let debounceTimeout: NodeJS.Timeout | null = null;

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
