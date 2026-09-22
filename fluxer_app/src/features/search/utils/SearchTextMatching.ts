// SPDX-License-Identifier: AGPL-3.0-or-later

const SEARCH_PATTERN_SPECIAL_CHARACTERS = /[-[\]/{}()*+?.\\^$|]/g;
const COMBINING_MARKS = /[\u{300}-\u{36f}]/gu;
const QUERY_WORD_SEPARATORS = /[, ]+/;

interface SearchTerm {
	readonly containQuery: RegExp;
	readonly exactQuery: RegExp;
	readonly isFullMatch: boolean;
	readonly queryLower: string;
}

let loadedUnicodeConfusables: ReadonlyMap<string, string> | null = null;
let pendingUnicodeConfusables: Promise<ReadonlyMap<string, string>> | null = null;

export function escapeSearchPattern(text: string): string {
	return text.replace(SEARCH_PATTERN_SPECIAL_CHARACTERS, '\\$&');
}

export function stripCombiningMarks(text: string): string {
	return text.normalize('NFD').replace(COMBINING_MARKS, '').normalize('NFC');
}

export function toConfusableSkeleton(text: string, confusables: ReadonlyMap<string, string>): string {
	let skeleton = '';
	for (const character of text.normalize('NFD')) {
		skeleton += confusables.get(character) ?? character;
	}
	return skeleton.normalize('NFD').toLocaleLowerCase();
}

export function fuzzySearch(needle: string, haystack: string): boolean {
	if (needle.length > haystack.length) return false;
	if (needle.length === haystack.length) return needle === haystack;
	let searchFrom = 0;
	for (let index = 0; index < needle.length; index++) {
		searchFrom = haystack.indexOf(needle[index], searchFrom) + 1;
		if (searchFrom === 0) return false;
	}
	return true;
}

export function scoreSearchTerm(value: string, term: SearchTerm, fuzzy = true): number {
	if (term.exactQuery.test(value)) return value.toLocaleLowerCase() === term.queryLower ? 10 : 7;
	if (term.containQuery.test(value)) return 5;
	const words = term.queryLower.split(QUERY_WORD_SEPARATORS);
	if (words.every((word) => new RegExp(escapeSearchPattern(word), 'i').test(value))) return 3;
	if (fuzzy && fuzzySearch(term.queryLower, value)) return 1;
	return 0;
}

export function consumeBestSearchTerm(value: string, terms: Array<SearchTerm>, fuzzy: boolean): number {
	let bestScore = 0;
	let bestIndex = -1;
	for (let index = 0; index < terms.length; index++) {
		const score = scoreSearchTerm(value, terms[index], fuzzy);
		if (score <= bestScore) continue;
		bestScore = score;
		bestIndex = index;
	}
	if (bestIndex === -1) return 0;
	if (terms[bestIndex].isFullMatch) {
		terms.length = 0;
	} else {
		terms.splice(bestIndex, 1);
	}
	return bestScore;
}

function buildSearchTerm(queryLower: string, pattern: string, isFullMatch: boolean): SearchTerm {
	return Object.freeze({
		containQuery: new RegExp(pattern, 'i'),
		exactQuery: new RegExp(`^${pattern}`, 'i'),
		isFullMatch,
		queryLower,
	});
}

export function createSearchTerm(queryLower: string): SearchTerm {
	return buildSearchTerm(queryLower, escapeSearchPattern(queryLower), false);
}

export function buildChannelSearchTerms(query: string): ReadonlyArray<SearchTerm> {
	const terms = query
		.split(' ')
		.filter((token) => token !== '')
		.map((token) => createSearchTerm(token.toLocaleLowerCase()));
	if (query.includes(' ')) {
		const queryLower = query.toLocaleLowerCase();
		terms.unshift(buildSearchTerm(queryLower, escapeSearchPattern(queryLower).replace(' ', '( |-)'), true));
	}
	return terms;
}

function decodeCodePoints(encoded: string): string {
	return String.fromCodePoint(...encoded.split(' ').map((codePoint) => Number.parseInt(codePoint, 16)));
}

function decodeUnicodeConfusables(encoded: string): ReadonlyMap<string, string> {
	const confusables = new Map<string, string>();
	for (const entry of encoded.split(',')) {
		const separator = entry.indexOf(':');
		confusables.set(decodeCodePoints(entry.slice(0, separator)), decodeCodePoints(entry.slice(separator + 1)));
	}
	return confusables;
}

export function getLoadedUnicodeConfusables(): ReadonlyMap<string, string> | null {
	return loadedUnicodeConfusables;
}

export function loadUnicodeConfusables(): Promise<ReadonlyMap<string, string>> {
	if (pendingUnicodeConfusables == null) {
		pendingUnicodeConfusables = import('@app/features/search/utils/UnicodeConfusables')
			.then(({UNICODE_CONFUSABLES}) => {
				const confusables = decodeUnicodeConfusables(UNICODE_CONFUSABLES);
				loadedUnicodeConfusables = confusables;
				return confusables;
			})
			.catch((error: unknown) => {
				pendingUnicodeConfusables = null;
				throw error;
			});
	}
	return pendingUnicodeConfusables;
}
