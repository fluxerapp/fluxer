// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import type {MessageSearchParams} from '@app/features/search/utils/SearchUtils';
import Users from '@app/features/user/state/Users';
import {MS_PER_DAY, MS_PER_HOUR, MS_PER_MINUTE, MS_PER_SECOND} from '@fluxer/date_utils/src/DateConstants';
import {fromTimestamp, isProbablyAValidSnowflake} from '@fluxer/snowflake/src/SnowflakeUtils';
import {DateTime} from 'luxon';

export interface ParserContext {
	usersByTag?: Record<string, string>;
	channelsByName?: Record<string, string>;
}

export interface SearchHints {
	usersByTag?: Record<string, string>;
	channelsByName?: Record<string, string>;
}

export interface ParsedToken {
	key: string;
	value: string;
	start: number;
	end: number;
	raw: string;
	quoted: boolean;
	exclude: boolean;
}

export const SearchChipRole = Object.freeze({
	KEY: 'key',
	VALUE: 'value',
	UNAPPLIED_VALUE: 'unapplied-value',
} as const);

export type SearchChipRole = (typeof SearchChipRole)[keyof typeof SearchChipRole];

export interface SearchChip {
	role: SearchChipRole;
	key: string;
	exclude: boolean;
	start: number;
	end: number;
	mutable: boolean;
}

const QUOTE_CHARS = new Set(['"', '\u201c', '\u201d', '\u201f', '\u2033', '\u00ab', '\u00bb']);
const isQuoteChar = (ch: string | undefined): boolean => ch !== undefined && QUOTE_CHARS.has(ch);
const KNOWN_KEYS = new Set([
	'from',
	'-from',
	'mentions',
	'-mentions',
	'in',
	'-in',
	'before',
	'during',
	'on',
	'after',
	'has',
	'-has',
	'pinned',
	'author-type',
	'sort',
	'order',
	'mature',
	'embed-type',
	'-embed-type',
	'embed-provider',
	'-embed-provider',
	'link',
	'-link',
	'filename',
	'-filename',
	'ext',
	'-ext',
	'last',
	'beforeid',
	'afterid',
	'any',
	'scope',
]);
const USER_TAG_RE = /^([A-Za-z0-9_]+)#(\d{4})$/;
const normalizeSpaces = (s: string) => s.replace(/\s+/g, ' ').trim();

type HasFilter = NonNullable<MessageSearchParams['has']>[number];

const HAS_FILTERS: ReadonlySet<HasFilter> = new Set([
	'image',
	'sound',
	'video',
	'file',
	'sticker',
	'embed',
	'link',
	'poll',
	'snapshot',
]);
const HAS_FILTER_ALIASES: Record<string, HasFilter> = {
	forward: 'snapshot',
};

function normalizeHasValue(value: string): HasFilter | null {
	const lowered = value.toLowerCase();
	if (Object.hasOwn(HAS_FILTER_ALIASES, lowered)) {
		return HAS_FILTER_ALIASES[lowered]!;
	}
	return HAS_FILTERS.has(lowered as HasFilter) ? (lowered as HasFilter) : null;
}

type AuthorTypeFilter = NonNullable<MessageSearchParams['authorType']>[number];

const AUTHOR_FILTERS: ReadonlySet<AuthorTypeFilter> = new Set(['user', 'bot', 'webhook']);
const isAuthorFilter = (value: string): value is AuthorTypeFilter => AUTHOR_FILTERS.has(value as AuthorTypeFilter);

type EmbedTypeFilter = NonNullable<MessageSearchParams['embedType']>[number];

const EMBED_TYPE_FILTERS: ReadonlySet<EmbedTypeFilter> = new Set(['image', 'video', 'sound', 'article']);
const isEmbedTypeFilter = (value: string): value is EmbedTypeFilter => EMBED_TYPE_FILTERS.has(value as EmbedTypeFilter);

type SortField = NonNullable<MessageSearchParams['sortBy']>;

const SORT_FIELDS: ReadonlySet<SortField> = new Set(['timestamp', 'relevance']);
const isSortField = (value: string): value is SortField => SORT_FIELDS.has(value as SortField);

type SortDirection = NonNullable<MessageSearchParams['sortOrder']>;

const SORT_ORDERS: ReadonlySet<SortDirection> = new Set(['asc', 'desc']);
const isSortDirection = (value: string): value is SortDirection => SORT_ORDERS.has(value as SortDirection);

type SearchScope = NonNullable<MessageSearchParams['scope']>;

const SEARCH_SCOPES: ReadonlySet<SearchScope> = new Set([
	'current',
	'open_dms',
	'all_dms',
	'all_guilds',
	'all',
	'open_dms_and_all_guilds',
]);
const isSearchScope = (value: string): value is SearchScope => SEARCH_SCOPES.has(value as SearchScope);

interface FilterKeyMatch {
	key: string;
	exclude: boolean;
	valueStart: number;
}

function matchFilterKeyAt(query: string, index: number): FilterKeyMatch | null {
	const n = query.length;
	const exclude = query[index] === '-';
	const keyStart = exclude ? index + 1 : index;
	let cursor = keyStart;
	while (cursor < n && query[cursor] !== ':' && query[cursor] !== ' ') cursor++;
	if (cursor >= n || query[cursor] !== ':' || cursor === keyStart) return null;
	const key = query.slice(keyStart, cursor).toLowerCase();
	if (!KNOWN_KEYS.has(key)) return null;
	return {key, exclude, valueStart: cursor + 1};
}

interface QuerySpan {
	start: number;
	end: number;
}

export function tokenize(query: string): {
	tokens: Array<ParsedToken>;
	content: string;
	exactPhrases: Array<string>;
	chips: Array<SearchChip>;
} {
	const tokens: Array<ParsedToken> = [];
	const chips: Array<SearchChip> = [];
	const consumed: Array<QuerySpan> = [];
	const n = query['length'];
	let i = 0;
	while (i < n) {
		if (query[i] === ' ') {
			i++;
			continue;
		}
		const match = matchFilterKeyAt(query, i);
		if (match === null) {
			i++;
			continue;
		}
		const {key, exclude: isExclude, valueStart} = match;
		let j = valueStart;
		while (j < n && query[j] === ' ') j++;
		const absorbedSpaces = j > valueStart;
		if (j >= n || (absorbedSpaces && matchFilterKeyAt(query, j) !== null)) {
			chips.push({role: SearchChipRole.KEY, key, exclude: isExclude, start: i, end: valueStart, mutable: false});
			consumed.push({start: i, end: valueStart});
			i = valueStart;
			continue;
		}
		let end = j;
		let inQuotes = false;
		let escaped = false;
		while (end < n) {
			const ch = query[end];
			if (escaped) {
				escaped = false;
				end++;
				continue;
			}
			if (ch === '\\') {
				escaped = true;
				end++;
				continue;
			}
			if (isQuoteChar(ch)) {
				inQuotes = !inQuotes;
				end++;
				continue;
			}
			if (!inQuotes && ch === ' ') break;
			end++;
		}
		const value = query['slice'](j, end);
		const raw = query['slice'](i, end);
		const quoted = isQuoteChar(value[0]);
		tokens.push({key, value, start: i, end, raw, quoted, exclude: isExclude});
		chips.push({role: SearchChipRole.KEY, key, exclude: isExclude, start: i, end: valueStart, mutable: false});
		if (isSearchFilterValueChippable(key, value)) {
			chips.push({
				role: SearchChipRole.VALUE,
				key,
				exclude: isExclude,
				start: valueStart,
				end,
				mutable: isSearchFilterValueMutable(key),
			});
		} else if (value.trim().length > 0) {
			chips.push({
				role: SearchChipRole.UNAPPLIED_VALUE,
				key,
				exclude: isExclude,
				start: valueStart,
				end,
				mutable: true,
			});
		}
		consumed.push({start: i, end});
		i = end;
	}
	let remaining = '';
	let pos = 0;
	for (const span of consumed) {
		if (span.start > pos) remaining += query['slice'](pos, span.start);
		pos = span.end;
	}
	if (pos < n) remaining += query['slice'](pos);
	const exactPhrases: Array<string> = [];
	let content = '';
	const remainingTrimmed = remaining;
	let ri = 0;
	const rn = remainingTrimmed.length;
	while (ri < rn) {
		if (isQuoteChar(remainingTrimmed[ri])) {
			ri++;
			let phrase = '';
			let escaped = false;
			while (ri < rn) {
				const ch = remainingTrimmed[ri];
				if (escaped) {
					phrase += ch;
					escaped = false;
					ri++;
					continue;
				}
				if (ch === '\\') {
					escaped = true;
					ri++;
					continue;
				}
				if (isQuoteChar(ch)) {
					ri++;
					break;
				}
				phrase += ch;
				ri++;
			}
			const trimmedPhrase = phrase.trim();
			if (trimmedPhrase) {
				exactPhrases.push(trimmedPhrase);
			}
		} else {
			content += remainingTrimmed[ri];
			ri++;
		}
	}
	return {tokens, content: normalizeSpaces(content), exactPhrases, chips};
}

const splitCSV = (input: string): Array<string> => {
	const items: Array<string> = [];
	let i = 0;
	const n = input['length'];
	while (i < n) {
		while (i < n && /[\s,]/.test(input[i])) i++;
		if (i >= n) break;
		let quoted = false;
		let buf = '';
		if (isQuoteChar(input[i])) {
			quoted = true;
			i++;
		}
		let escaped = false;
		for (; i < n; i++) {
			const ch = input[i];
			if (escaped) {
				buf += ch;
				escaped = false;
				continue;
			}
			if (ch === '\\') {
				escaped = true;
				continue;
			}
			if (quoted) {
				if (isQuoteChar(ch)) {
					i++;
					break;
				}
				buf += ch;
			} else {
				if (ch === ',') break;
				buf += ch;
			}
		}
		items.push(buf.trim());
		while (i < n && input[i] !== ',') i++;
		if (i < n && input[i] === ',') i++;
	}
	return items.filter(Boolean);
};
const normalizeToken = (value: string): string => value.trim().toLowerCase();
const CURRENT_USER_TOKENS = new Set(['@me']);
const isCurrentUserToken = (value: string): boolean => CURRENT_USER_TOKENS.has(normalizeToken(value));
const getCurrentUserId = (): string | null => Users.getCurrentUser()?.id ?? null;
const tryResolveUser = (tag: string, hints?: SearchHints): string | null => {
	const trimmedTag = tag.trim();
	if (!trimmedTag) {
		return null;
	}
	if (isProbablyAValidSnowflake(trimmedTag)) {
		return trimmedTag;
	}
	if (isCurrentUserToken(trimmedTag)) {
		return getCurrentUserId();
	}
	if (hints?.usersByTag?.[trimmedTag]) return hints.usersByTag[trimmedTag];
	if (!USER_TAG_RE.test(trimmedTag)) return null;
	const user = Users.getUserByTag(trimmedTag);
	return user?.id ?? null;
};
export function resolveSearchChannelDisplayName(channel: {
	name?: string | null;
	isDM: () => boolean;
	getRecipientId: () => string | undefined;
}): string {
	if (channel.isDM()) {
		const recipientId = channel.getRecipientId();
		if (recipientId == null) return '';
		return Users.getUser(recipientId)?.tag ?? '';
	}
	return channel.name?.trim() ?? '';
}

const tryResolveChannel = (name: string, guildId?: string | null, hints?: SearchHints): string | null => {
	if (hints?.channelsByName?.[name]) return hints.channelsByName[name];
	const target = name.toLowerCase();
	if (!guildId) {
		const privateChannels = Channels.getPrivateChannels();
		const exact = privateChannels.find((c) => resolveSearchChannelDisplayName(c).toLowerCase() === target);
		if (exact) return exact.id;
		const partial = privateChannels.find((c) => resolveSearchChannelDisplayName(c).toLowerCase().includes(target));
		return partial?.id ?? null;
	}
	const channels = Channels.getGuildChannels(guildId);
	const matches = channels.filter((c) => (c.name || '').toLowerCase() === target);
	if (matches.length > 0) return matches[0].id;
	const partial = channels.find((c) => (c.name || '').toLowerCase().includes(target));
	return partial?.id ?? null;
};

export interface ParseContext {
	guildId?: string | null;
}

const toSnowflakeAt = (dt: DateTime) => fromTimestamp(dt.toMillis()).toString();

export function parseCompactDateTime(input: string, now: DateTime = DateTime.local()): DateTime | null {
	const trimmed = input['trim']();
	if (trimmed.length === 0) return null;
	if (trimmed.toLowerCase() === 'now') return now;
	if (trimmed.toLowerCase() === 'today') return now.startOf('day');
	if (trimmed.toLowerCase() === 'yesterday') return now.minus({days: 1}).startOf('day');
	const maybeIso = trimmed.replace('_', 'T').replace(' ', 'T');
	let dt = DateTime.fromISO(maybeIso, {setZone: true});
	if (dt.isValid) return dt;
	const m = trimmed.match(/^(\d{4})(\d{2})(\d{2})(?:T(\d{2})(\d{2})(\d{2})?)?Z?$/);
	if (m) {
		const [, y, mo, d, hh, mm, ss] = m;
		dt = DateTime.fromObject(
			{
				year: Number(y),
				month: Number(mo),
				day: Number(d),
				hour: hh ? Number(hh) : 0,
				minute: mm ? Number(mm) : 0,
				second: ss ? Number(ss) : 0,
			},
			{zone: now.zone},
		);
		return dt.isValid ? dt : null;
	}
	const js = new Date(trimmed);
	if (!Number.isNaN(js.getTime())) return DateTime.fromJSDate(js);
	return null;
}

export interface SearchDatePeriod {
	readonly start: DateTime;
	readonly end: DateTime;
}

const SEARCH_DATE_YEAR_FLOOR = 2015;
const EXPLICIT_CLOCK_TIME_RE = /[T_ ]\d{1,2}:\d{2}|[T_ ]\d{1,2}$|[T_ ]\d{4,6}$|\d{8}T\d{4}/;
const RELATIVE_PERIOD_UNITS: Record<string, 'week' | 'month' | 'year'> = {
	week: 'week',
	month: 'month',
	year: 'year',
};
const WEEKDAY_NAMES: ReadonlyArray<string> = [
	'monday',
	'tuesday',
	'wednesday',
	'thursday',
	'friday',
	'saturday',
	'sunday',
];
const MONTH_NAMES: ReadonlyArray<string> = [
	'january',
	'february',
	'march',
	'april',
	'may',
	'june',
	'july',
	'august',
	'september',
	'october',
	'november',
	'december',
];

const RELATIVE_DATE_WORDS: ReadonlyArray<string> = ['today', 'yesterday', ...Object.keys(RELATIVE_PERIOD_UNITS)];

export function getSearchDateWords(now: DateTime = DateTime.local()): Array<string> {
	const years: Array<string> = [];
	for (let year = now.year; year >= SEARCH_DATE_YEAR_FLOOR; year -= 1) {
		years.push(String(year));
	}
	return [...RELATIVE_DATE_WORDS, ...years, ...MONTH_NAMES, ...WEEKDAY_NAMES];
}

function isSubsequence(needle: string, haystack: string): boolean {
	const haystackLength = haystack.length;
	const needleLength = needle.length;
	if (needleLength > haystackLength) return false;
	if (needleLength === haystackLength) return needle === haystack;
	let haystackIndex = 0;
	outer: for (let needleIndex = 0; needleIndex < needleLength; needleIndex += 1) {
		const code = needle.charCodeAt(needleIndex);
		while (haystackIndex < haystackLength) {
			if (haystack.charCodeAt(haystackIndex++) === code) continue outer;
		}
		return false;
	}
	return true;
}

export function matchSearchDateWords(term: string, limit: number, now: DateTime = DateTime.local()): Array<string> {
	const needle = term.toLocaleLowerCase();
	const exact: Array<string> = [];
	const prefixed: Array<string> = [];
	const scattered: Array<string> = [];
	for (const word of getSearchDateWords(now)) {
		const candidate = word.toLocaleLowerCase();
		if (candidate === needle) {
			exact.push(word);
		} else if (candidate.startsWith(needle)) {
			prefixed.push(word);
		} else if (isSubsequence(needle, candidate)) {
			scattered.push(word);
		}
	}
	return [...exact, ...prefixed, ...scattered].slice(0, limit);
}

function localeWeekStart(now: DateTime): DateTime {
	return now.startOf('week', {useLocaleWeeks: true});
}

function dayPeriod(start: DateTime): SearchDatePeriod {
	const floor = start.startOf('day');
	return {start: floor, end: floor.plus({days: 1})};
}

function resolveSearchDateWord(word: string, now: DateTime): SearchDatePeriod | null {
	if (word === 'today') return dayPeriod(now);
	if (word === 'yesterday') return dayPeriod(now.minus({days: 1}));
	const relativeUnit = RELATIVE_PERIOD_UNITS[word];
	if (relativeUnit != null) {
		const floor = relativeUnit === 'week' ? localeWeekStart(now) : now.startOf(relativeUnit);
		return {start: floor, end: floor.plus({[relativeUnit]: 1})};
	}
	const monthIndex = MONTH_NAMES.indexOf(word);
	if (monthIndex >= 0) {
		const floor = DateTime.fromObject({year: now.year, month: monthIndex + 1, day: 1}, {zone: now.zone});
		return {start: floor, end: floor.plus({months: 1})};
	}
	const weekdayIndex = WEEKDAY_NAMES.indexOf(word);
	if (weekdayIndex >= 0) {
		const weekStart = localeWeekStart(now);
		for (let offset = 0; offset < 7; offset += 1) {
			const candidate = weekStart.plus({days: offset});
			if (candidate.weekday === weekdayIndex + 1) return dayPeriod(candidate);
		}
		return null;
	}
	if (!/^\d{4}$/.test(word)) return null;
	const year = Number(word);
	if (year < SEARCH_DATE_YEAR_FLOOR || year > now.year) return null;
	const floor = DateTime.fromObject({year, month: 1, day: 1}, {zone: now.zone});
	return {start: floor, end: floor.plus({years: 1})};
}

export function resolveSearchDatePeriod(input: string, now: DateTime = DateTime.local()): SearchDatePeriod | null {
	const word = input.trim().toLowerCase();
	if (word.length === 0) return null;
	const wordPeriod = resolveSearchDateWord(word, now);
	if (wordPeriod != null) return wordPeriod;
	const instant = parseCompactDateTime(input, now);
	if (instant == null) return null;
	if (word === 'now' || EXPLICIT_CLOCK_TIME_RE.test(input.trim())) {
		return {start: instant, end: instant};
	}
	return dayPeriod(instant);
}

const parseDuration = (
	input: string,
): {
	millis: number;
} | null => {
	const m = input['trim']().match(/^(\d+)(ms|s|m|h|d|w)$/i);
	if (!m) return null;
	const n = Number(m[1]);
	const unit = m[2].toLowerCase();
	const map: Record<string, number> = {
		ms: 1,
		s: MS_PER_SECOND,
		m: MS_PER_MINUTE,
		h: MS_PER_HOUR,
		d: MS_PER_DAY,
		w: 7 * MS_PER_DAY,
	};
	return {millis: n * map[unit]};
};

const normalizeSearchFilterKey = (key: string): string => {
	const stripped = key.startsWith('-') ? key.slice(1) : key;
	if (stripped === 'on') return 'during';
	return stripped;
};

const NON_MUTABLE_VALUE_KEYS: ReadonlySet<string> = new Set(['has', 'pinned', 'author-type']);

export function isSearchFilterValueMutable(key: string): boolean {
	return !NON_MUTABLE_VALUE_KEYS.has(normalizeSearchFilterKey(key));
}

const BOOLEAN_VALUES: ReadonlySet<string> = new Set(['true', 'false', 'yes', 'no', '1', '0']);

const isEveryCSVItem = (value: string, predicate: (item: string) => boolean): boolean => {
	const items = splitCSV(value);
	return items.length > 0 && items.every((item) => predicate(item.toLowerCase()));
};

function isSearchDateValue(value: string): boolean {
	if (!value.includes('..')) return resolveSearchDatePeriod(value) !== null;
	const [lower, upper] = value.split('..');
	return resolveSearchDatePeriod(lower) !== null && resolveSearchDatePeriod(upper) !== null;
}

const SEARCH_FILTER_VALUE_VALIDATORS: Record<string, (value: string) => boolean> = {
	has: (value) => isEveryCSVItem(value, (item) => normalizeHasValue(item) !== null),
	'author-type': (value) => isEveryCSVItem(value, isAuthorFilter),
	'embed-type': (value) => isEveryCSVItem(value, isEmbedTypeFilter),
	pinned: (value) => BOOLEAN_VALUES.has(normalizeToken(value)),
	mature: (value) => normalizeToken(value) === 'true' || normalizeToken(value) === 'false',
	sort: (value) => isSortField(normalizeToken(value)),
	order: (value) => isSortDirection(normalizeToken(value)),
	scope: (value) => splitCSV(value).some((item) => isSearchScope(normalizeToken(item))),
	last: (value) => parseDuration(value) !== null,
	before: isSearchDateValue,
	after: isSearchDateValue,
	during: isSearchDateValue,
};

export function isSearchFilterValueChippable(key: string, value: string): boolean {
	const trimmed = value.trim();
	if (trimmed.length === 0) return false;
	const validator = SEARCH_FILTER_VALUE_VALIDATORS[normalizeSearchFilterKey(key)];
	if (validator == null) return true;
	return validator(trimmed);
}

export function resolveSearchChipDeletion(query: string, caret: number, isForward: boolean): SearchChip | null {
	for (const chip of tokenize(query).chips) {
		if (chip.mutable) continue;
		let isWithin: boolean;
		if (isForward) {
			isWithin = caret >= chip.start && caret < chip.end;
		} else {
			isWithin = caret > chip.start && caret <= chip.end;
		}
		if (isWithin) return chip;
	}
	return null;
}

function resolveDuringUpperBound(period: SearchDatePeriod): DateTime {
	if (period.end > period.start) return period.end;
	return period.start.startOf('day').plus({days: 1});
}

export function addUniqueSearchParam<T>(values: Array<T> | undefined, value: T): Array<T> {
	if (values == null) return [value];
	if (values.includes(value)) return values;
	return [...values, value];
}

export function parseQuery(query: string, hints?: SearchHints, ctx?: ParseContext): MessageSearchParams {
	const {tokens, content, exactPhrases} = tokenize(query);
	const params: MessageSearchParams = {};
	if (content) params.content = content;
	if (exactPhrases.length > 0) params.exactPhrases = exactPhrases;
	const add = addUniqueSearchParam;
	for (const tok of tokens) {
		const key = tok.key === 'on' ? 'during' : tok.key;
		const normalizedKey = key.startsWith('-') ? key.slice(1) : key;
		const isExcludeKey = key.startsWith('-') || tok.exclude;
		switch (normalizedKey) {
			case 'from': {
				const values = splitCSV(tok.value);
				for (const tag of values) {
					const id = tryResolveUser(tag, hints);
					if (id) {
						if (isExcludeKey) {
							params.excludeAuthorId = add(params.excludeAuthorId, id);
						} else {
							params.authorId = add(params.authorId, id);
						}
					}
				}
				break;
			}
			case 'mentions': {
				const values = splitCSV(tok.value);
				for (const v of values) {
					const lower = v.toLowerCase();
					if (lower === 'everyone' || lower === 'here') {
						params.mentionEveryone = true;
					} else {
						const id = tryResolveUser(v, hints);
						if (id) {
							if (isExcludeKey) {
								params.excludeMentions = add(params.excludeMentions, id);
							} else {
								params.mentions = add(params.mentions, id);
							}
						}
					}
				}
				break;
			}
			case 'in': {
				const values = splitCSV(tok.value);
				for (const v of values) {
					const id = tryResolveChannel(v, ctx?.guildId ?? null, hints);
					if (id) {
						if (isExcludeKey) {
							params.excludeChannelId = add(params.excludeChannelId, id);
						} else {
							params.channelId = add(params.channelId, id);
						}
					}
				}
				break;
			}
			case 'has': {
				const raw = tok.value.trim();
				if (!raw) break;
				const values = raw
					.split(',')
					.map((v) => v.trim())
					.filter(Boolean);
				for (const value of values) {
					const normalized = normalizeHasValue(value);
					if (!normalized) continue;
					if (isExcludeKey) {
						params.excludeHas = add(params.excludeHas, normalized);
					} else {
						params.has = add(params.has, normalized);
					}
				}
				break;
			}
			case 'pinned': {
				const v = tok.value.trim().toLowerCase();
				if (['true', 'false', 'yes', 'no', '1', '0'].includes(v)) {
					params.pinned = v === 'true' || v === 'yes' || v === '1';
				}
				break;
			}
			case 'author-type': {
				const raw = tok.value.trim();
				if (!raw) break;
				const values = raw
					.split(',')
					.map((v) => v.trim())
					.filter(Boolean);
				for (const value of values) {
					const normalized = value.toLowerCase();
					if (isAuthorFilter(normalized)) {
						params.authorType = add(params.authorType, normalized);
					}
				}
				break;
			}
			case 'sort': {
				const v = tok.value.trim().toLowerCase();
				if (isSortField(v)) params.sortBy = v;
				break;
			}
			case 'order': {
				const v = tok.value.trim().toLowerCase();
				if (isSortDirection(v)) params.sortOrder = v;
				break;
			}
			case 'mature': {
				const v = tok.value.trim().toLowerCase();
				if (v === 'true' || v === 'false') params.includeNsfw = v === 'true';
				break;
			}
			case 'embed-type': {
				const values = splitCSV(tok.value).map((v) => v.toLowerCase());
				for (const v of values) {
					if (isExcludeKey) {
						if (isEmbedTypeFilter(v)) {
							params.excludeEmbedType = add(params.excludeEmbedType, v);
						}
					} else if (isEmbedTypeFilter(v)) {
						params.embedType = add(params.embedType, v);
					}
				}
				break;
			}
			case 'embed-provider': {
				const values = splitCSV(tok.value);
				for (const v of values) {
					if (isExcludeKey) {
						params.excludeEmbedProvider = add(params.excludeEmbedProvider, v);
					} else {
						params.embedProvider = add(params.embedProvider, v);
					}
				}
				break;
			}
			case 'link': {
				const values = splitCSV(tok.value);
				for (const v of values) {
					if (isExcludeKey) {
						params.excludeLinkHostname = add(params.excludeLinkHostname, v);
					} else {
						params.linkHostname = add(params.linkHostname, v);
					}
				}
				break;
			}
			case 'filename': {
				const values = splitCSV(tok.value);
				for (const v of values) {
					if (isExcludeKey) {
						params.excludeAttachmentFilename = add(params.excludeAttachmentFilename, v);
					} else {
						params.attachmentFilename = add(params.attachmentFilename, v);
					}
				}
				break;
			}
			case 'ext': {
				const values = splitCSV(tok.value).map((v) => v.replace(/^\./, ''));
				for (const v of values) {
					if (isExcludeKey) {
						params.excludeAttachmentExtension = add(params.excludeAttachmentExtension, v);
					} else {
						params.attachmentExtension = add(params.attachmentExtension, v);
					}
				}
				break;
			}
			case 'scope': {
				for (const item of splitCSV(tok.value)) {
					const normalized = normalizeToken(item);
					if (isSearchScope(normalized)) {
						params.scope = normalized;
						break;
					}
				}
				break;
			}
			case 'last': {
				const dur = parseDuration(tok.value);
				if (dur) {
					const dt = DateTime.local().minus({milliseconds: dur.millis});
					params.minId = toSnowflakeAt(dt);
				}
				break;
			}
			case 'beforeid': {
				const id = tok.value.trim();
				if (id) params.maxId = id;
				break;
			}
			case 'afterid': {
				const id = tok.value.trim();
				if (id) params.minId = id;
				break;
			}
			case 'any': {
				const values = splitCSV(tok.value);
				for (const v of values) params.contents = add(params.contents, v);
				break;
			}
			case 'before':
			case 'after':
			case 'during': {
				if (tok.value.includes('..')) {
					const [a, b] = tok.value.split('..');
					const lower = resolveSearchDatePeriod(a);
					const upper = resolveSearchDatePeriod(b);
					if (!lower || !upper) break;
					params.minId = toSnowflakeAt(lower.start);
					params.maxId = toSnowflakeAt(upper.end);
					break;
				}
				const period = resolveSearchDatePeriod(tok.value);
				if (!period) break;
				if (key === 'before') {
					params.maxId = toSnowflakeAt(period.start);
					break;
				}
				if (key === 'after') {
					params.minId = toSnowflakeAt(period.end);
					break;
				}
				params.minId = toSnowflakeAt(period.start);
				params.maxId = toSnowflakeAt(resolveDuringUpperBound(period));
				break;
			}
			default:
				break;
		}
	}
	return params;
}
