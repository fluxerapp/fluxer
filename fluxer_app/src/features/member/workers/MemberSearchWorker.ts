// SPDX-License-Identifier: AGPL-3.0-or-later

export enum MessageTypes {
	INGEST_DIRECTORY = 'INGEST_DIRECTORY',
	MATCHES_READY = 'MATCHES_READY',
	SEARCH_BEGIN = 'SEARCH_BEGIN',
	SEARCH_CANCEL = 'SEARCH_CANCEL',
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
	rankLabel: string;
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

interface SearchForm {
	value: string;
	lower: string;
	stripped: string;
}

interface IndexedUser {
	user: TransformedUser;
	named: Array<SearchForm>;
	perGuild: Record<string, SearchForm> | null;
}

const userIndex: Map<string, IndexedUser> = new Map();
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
const COMBINING_MARKS = /[\u0300-\u036f]/g;

function stripCombiningMarks(text: string): string {
	const decomposed = text.normalize('NFD');
	const withoutMarks = decomposed.replace(COMBINING_MARKS, '');
	if (withoutMarks.length === decomposed.length) {
		return text;
	}
	return withoutMarks.normalize('NFC');
}

function toSearchForm(value: string): SearchForm {
	const lower = value.toLowerCase();
	return {value, lower, stripped: stripCombiningMarks(lower)};
}

function indexUser(user: TransformedUser): IndexedUser {
	const named: Array<SearchForm> = [];
	if (user.username.length > 0) {
		named.push(toSearchForm(user.username));
	}
	const globalName = user.globalName;
	if (typeof globalName === 'string' && globalName.length > 0) {
		named.push(toSearchForm(globalName));
	}
	const guildNicknames = user.guildNicknames;
	if (guildNicknames == null) {
		return {user, named, perGuild: null};
	}
	let perGuild: Record<string, SearchForm> | null = null;
	for (const [guildId, nickname] of Object.entries(guildNicknames)) {
		if (typeof nickname !== 'string' || nickname.length === 0) {
			continue;
		}
		if (perGuild == null) {
			perGuild = {};
		}
		perGuild[guildId] = toSearchForm(nickname);
	}
	return {user, named, perGuild};
}

function getSearchForms(entry: IndexedUser, filters?: SearchFilters): Array<SearchForm> {
	const {named, perGuild} = entry;
	if (perGuild == null) {
		return named;
	}
	const guildId = filters == null ? null : filters.guild;
	if (guildId != null && guildId.length > 0) {
		const nickname = perGuild[guildId];
		return nickname == null ? named : [...named, nickname];
	}
	return [...named, ...Object.values(perGuild)];
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
		const aLabel = a.rankLabel.toLowerCase();
		const bLabel = b.rankLabel.toLowerCase();
		if (aLabel < bLabel) return -1;
		if (aLabel > bLabel) return 1;
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

function insertSearchResult(shortlist: Array<SearchResult>, candidate: SearchResult, limit: number): void {
	if (shortlist.length === 0) {
		shortlist.push(candidate);
		return;
	}
	let insertIndex = shortlist.length;
	for (let index = 0; index < shortlist.length; index += 1) {
		if (sortByMatchScore(candidate, shortlist[index]) < 0) {
			insertIndex = index;
			break;
		}
	}
	if (insertIndex === shortlist.length && shortlist.length >= limit) {
		return;
	}
	shortlist.splice(insertIndex, 0, candidate);
	if (shortlist.length > limit) {
		shortlist.pop();
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

function calculateScore(tierScore: number, booster?: number): number {
	return tierScore * (booster ?? 1);
}

function postSearchResults(uuid: string, shortlist: Array<SearchResult>, generation: number): void {
	const payload = shortlist.map((hit) => ({
		id: hit.id,
		username: hit.username,
		isBot: hit.isBot,
	}));
	const message: WorkerMessage<typeof payload> = {
		uuid,
		type: MessageTypes.MATCHES_READY,
		payload,
		generation,
	};
	postMessage(message);
}

function getFallbackLabel(user: TransformedUser, filters?: SearchFilters): string {
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

function scoreSearchForm(form: SearchForm, loweredQuery: string, strippedQuery: string): number {
	const needsStrippedPass = form.stripped !== form.lower || strippedQuery !== loweredQuery;
	if (form.lower.startsWith(loweredQuery)) {
		return SCORE_EXACT_PREFIX;
	}
	if (needsStrippedPass && form.stripped.startsWith(strippedQuery)) {
		return SCORE_EXACT_PREFIX;
	}
	if (form.lower.includes(loweredQuery)) {
		return SCORE_CONTAINS;
	}
	if (needsStrippedPass && form.stripped.includes(strippedQuery)) {
		return SCORE_CONTAINS;
	}
	if (fuzzyMatch(loweredQuery, form.lower)) {
		return SCORE_FUZZY;
	}
	if (needsStrippedPass && fuzzyMatch(strippedQuery, form.stripped)) {
		return SCORE_FUZZY;
	}
	return 0;
}

function executeSearch(uuid: string, searchQuery: SearchQuery): void {
	const {query, limit, filters, blacklist, whitelist, boosters, generation = 0} = searchQuery;
	const normalizedLimit = normalizeSearchLimit(limit);
	const hits: Array<SearchResult> = [];
	if (normalizedLimit === 0) {
		postSearchResults(uuid, hits, generation);
		return;
	}
	if (query === '') {
		userIndex.forEach((entry, userId) => {
			const user = entry.user;
			if (!shouldIncludeUser(userId, user, filters, blacklist, whitelist)) {
				return;
			}
			insertSearchResult(
				hits,
				{
					id: userId,
					username: user.username,
					rankLabel: getFallbackLabel(user, filters),
					score: 0,
					isBot: user.isBot,
				},
				normalizedLimit,
			);
		});
		postSearchResults(uuid, hits, generation);
		return;
	}
	const loweredQuery = query.toLowerCase();
	const strippedQuery = stripCombiningMarks(loweredQuery);
	userIndex.forEach((entry, userId) => {
		const user = entry.user;
		if (!shouldIncludeUser(userId, user, filters, blacklist, whitelist)) {
			return;
		}
		const booster = boosters == null ? undefined : boosters[userId];
		if (query === userId) {
			insertSearchResult(
				hits,
				{
					id: userId,
					username: user.username,
					rankLabel: getFallbackLabel(user, filters),
					score: calculateScore(SCORE_EXACT_PREFIX, booster),
					isBot: user.isBot,
				},
				normalizedLimit,
			);
			return;
		}
		let bestScore = 0;
		let bestForm: SearchForm | null = null;
		for (const form of getSearchForms(entry, filters)) {
			const score = scoreSearchForm(form, loweredQuery, strippedQuery);
			if (score > bestScore) {
				bestScore = score;
				bestForm = form;
			}
		}
		if (bestForm == null) {
			return;
		}
		insertSearchResult(
			hits,
			{
				id: userId,
				username: user.username,
				rankLabel: bestForm.value,
				score: calculateScore(bestScore, booster),
				isBot: user.isBot,
			},
			normalizedLimit,
		);
	});
	postSearchResults(uuid, hits, generation);
}

function applyUserUpdates(users: Array<TransformedUser>): void {
	let shouldTriggerSearch = false;
	const updatedGuilds = new Set<string>();
	for (const update of users) {
		const userId = update.id;
		if (update._delete === true) {
			userIndex.delete(userId);
			shouldTriggerSearch = true;
			continue;
		}
		const existingEntry = userIndex.get(userId);
		if (update._removeGuild && existingEntry == null) {
			continue;
		}
		const baseUser: TransformedUser = existingEntry == null ? {id: userId, username: ''} : existingEntry.user;
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
		userIndex.set(userId, indexUser(mergedUser));
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

function registerQuery(uuid: string, query: SearchQuery): void {
	activeQueries.set(uuid, query);
	executeSearch(uuid, query);
}

function unregisterQuery(uuid: string): void {
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
		case MessageTypes.INGEST_DIRECTORY: {
			const p = payload as UpdateUsersPayload | undefined;
			if (p?.users) {
				applyUserUpdates(p.users);
			}
			break;
		}
		case MessageTypes.SEARCH_BEGIN: {
			if (!uuid) return;
			registerQuery(uuid, payload as SearchQuery);
			break;
		}
		case MessageTypes.SEARCH_CANCEL: {
			if (!uuid) return;
			unregisterQuery(uuid);
			break;
		}
	}
});
