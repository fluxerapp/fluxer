// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';

type Brand<T, TBrand extends string> = T & {readonly __brand: TBrand};

type EmojiOrdinal = Brand<number, 'EmojiOrdinal'>;
type NormalizedEmojiQuery = Brand<string, 'NormalizedEmojiQuery'>;
type NGram = Brand<string, 'NGram'>;

const MATCH_TIER = {
	exactName: 0,
	nameStartsWith: 1,
	nameBoundary: 2,
	nameContains: 3,
	keywordStartsWith: 4,
	keywordContains: 5,
} as const satisfies Record<string, number>;

const EMPTY_POSTINGS = new Uint32Array(0);

type MatchTier = (typeof MATCH_TIER)[keyof typeof MATCH_TIER];

type EmojiSearchMeta<TEmoji extends FlatEmoji> = Readonly<{
	emoji: TEmoji;
	names: ReadonlyArray<string>;
	lowerNames: ReadonlyArray<string>;
	compactNames: ReadonlyArray<string>;
	lowerKeywords: ReadonlyArray<string>;
	primaryName: string;
	order: number;
}>;

type SearchResult<TEmoji extends FlatEmoji> = Readonly<{
	emoji: TEmoji;
	tier: MatchTier;
	frecency: number;
	primaryName: string;
	order: number;
}>;

export type EmojiSearchOptions<TEmoji extends FlatEmoji> = Readonly<{
	count?: number;
	canUse?: (emoji: TEmoji) => boolean;
	getFrecencyScore?: (emoji: TEmoji) => number;
}>;

export type EmojiSearchIndexStats = Readonly<{
	emojiCount: number;
	gramCount: number;
	postingCount: number;
}>;

function asOrdinal(index: number): EmojiOrdinal {
	return index as EmojiOrdinal;
}

function asNGram(value: string): NGram {
	return value as NGram;
}

function normalizeQuery(query: string): NormalizedEmojiQuery {
	return query.toLowerCase() as NormalizedEmojiQuery;
}

function compactName(value: string): string {
	return value.replace(/[_-]/g, '');
}

function getEmojiNames(emoji: FlatEmoji): ReadonlyArray<string> {
	if (emoji.names && emoji.names.length > 0) {
		return emoji.names;
	}
	if (emoji.uniqueName && emoji.uniqueName !== emoji.name) {
		return [emoji.uniqueName, emoji.name];
	}
	return emoji.uniqueName ? [emoji.uniqueName] : [emoji.name];
}

function isUpperAsciiLetter(char: string | undefined): boolean {
	return !!char && char >= 'A' && char <= 'Z';
}

function isNameBoundary(char: string | undefined): boolean {
	return char === undefined || char === '_' || isUpperAsciiLetter(char);
}

function hasBoundaryMatch(name: string, lowerName: string, lowerCasedQuery: string): boolean {
	let index = lowerName.indexOf(lowerCasedQuery);
	while (index !== -1) {
		if (isNameBoundary(name[index - 1])) {
			const endIndex = index + lowerCasedQuery.length;
			if (isNameBoundary(name[endIndex])) return true;
			if (lowerName[endIndex] === 's' && isNameBoundary(name[endIndex + 1])) return true;
		}
		index = lowerName.indexOf(lowerCasedQuery, index + 1);
	}
	return false;
}

function getEmojiMatchTier<TEmoji extends FlatEmoji>(
	meta: EmojiSearchMeta<TEmoji>,
	lowerCasedQuery: NormalizedEmojiQuery,
	compactQuery: NormalizedEmojiQuery,
): MatchTier | null {
	for (const lowerName of meta.lowerNames) {
		if (lowerName === lowerCasedQuery) return MATCH_TIER.exactName;
	}
	for (let i = 0; i < meta.names.length; i++) {
		if (meta.lowerNames[i].startsWith(lowerCasedQuery)) return MATCH_TIER.nameStartsWith;
		if (compactQuery && meta.compactNames[i].startsWith(compactQuery)) return MATCH_TIER.nameStartsWith;
	}
	for (let i = 0; i < meta.names.length; i++) {
		if (hasBoundaryMatch(meta.names[i], meta.lowerNames[i], lowerCasedQuery)) return MATCH_TIER.nameBoundary;
	}
	for (let i = 0; i < meta.names.length; i++) {
		if (meta.lowerNames[i].includes(lowerCasedQuery)) return MATCH_TIER.nameContains;
		if (compactQuery && meta.compactNames[i].includes(compactQuery)) return MATCH_TIER.nameContains;
	}
	let hasKeywordContains = false;
	for (const lowerKeyword of meta.lowerKeywords) {
		if (lowerKeyword === lowerCasedQuery || lowerKeyword.startsWith(lowerCasedQuery)) {
			return MATCH_TIER.keywordStartsWith;
		}
		if (lowerKeyword.includes(lowerCasedQuery)) hasKeywordContains = true;
	}
	return hasKeywordContains ? MATCH_TIER.keywordContains : null;
}

function compareSearchResults<TEmoji extends FlatEmoji>(a: SearchResult<TEmoji>, b: SearchResult<TEmoji>): number {
	if (a.tier !== b.tier) return a.tier - b.tier;
	if (a.frecency !== b.frecency) return b.frecency - a.frecency;
	const nameOrder = a.primaryName.localeCompare(b.primaryName);
	return nameOrder !== 0 ? nameOrder : a.order - b.order;
}

function uniqueGramsForToken(token: string): ReadonlyArray<NGram> {
	if (token.length === 0) return [];
	const grams = new Set<NGram>();
	const maxGramLength = Math.min(3, token.length);
	for (let gramLength = 1; gramLength <= maxGramLength; gramLength++) {
		for (let index = 0; index <= token.length - gramLength; index++) {
			grams.add(asNGram(token.slice(index, index + gramLength)));
		}
	}
	return Array.from(grams);
}

function queryGrams(query: NormalizedEmojiQuery): ReadonlyArray<NGram> {
	if (query.length === 0) return [];
	const gramLength = Math.min(3, query.length);
	const grams = new Set<NGram>();
	for (let index = 0; index <= query.length - gramLength; index++) {
		grams.add(asNGram(query.slice(index, index + gramLength)));
	}
	return Array.from(grams);
}

function includesOrdinal(postings: Uint32Array, ordinal: number): boolean {
	let low = 0;
	let high = postings.length - 1;
	while (low <= high) {
		const mid = (low + high) >>> 1;
		const value = postings[mid];
		if (value === ordinal) return true;
		if (value < ordinal) {
			low = mid + 1;
		} else {
			high = mid - 1;
		}
	}
	return false;
}

function insertBoundedResult<TEmoji extends FlatEmoji>(
	results: Array<SearchResult<TEmoji>>,
	result: SearchResult<TEmoji>,
	limit: number,
): void {
	if (limit <= 0) {
		results.push(result);
		return;
	}
	if (results.length < limit) {
		results.push(result);
		results.sort(compareSearchResults);
		return;
	}
	const lastIndex = results.length - 1;
	if (compareSearchResults(result, results[lastIndex]) >= 0) {
		return;
	}
	results[lastIndex] = result;
	results.sort(compareSearchResults);
}

export class EmojiSearchIndex<TEmoji extends FlatEmoji = FlatEmoji> {
	private readonly metas: ReadonlyArray<EmojiSearchMeta<TEmoji>>;
	private readonly gramsToOrdinals: ReadonlyMap<NGram, Uint32Array>;
	private readonly stats: EmojiSearchIndexStats;

	constructor(emojis: ReadonlyArray<TEmoji>) {
		this.metas = Object.freeze(
			emojis.map((emoji, order) => {
				const names = getEmojiNames(emoji);
				const lowerNames = names.map((name) => name.toLowerCase());
				return {
					emoji,
					names,
					lowerNames,
					compactNames: lowerNames.map(compactName),
					lowerKeywords: emoji.keywords?.map((keyword) => keyword.toLowerCase()) ?? [],
					primaryName: emoji.uniqueName || names[0] || emoji.name,
					order,
				} satisfies EmojiSearchMeta<TEmoji>;
			}),
		);
		const postings = new Map<NGram, Set<EmojiOrdinal>>();
		for (const meta of this.metas) {
			const ordinal = asOrdinal(meta.order);
			const indexedGrams = new Set<NGram>();
			for (const token of [...meta.lowerNames, ...meta.lowerKeywords]) {
				for (const gram of uniqueGramsForToken(token)) {
					indexedGrams.add(gram);
				}
				for (const gram of uniqueGramsForToken(compactName(token))) {
					indexedGrams.add(gram);
				}
			}
			for (const gram of indexedGrams) {
				let gramPostings = postings.get(gram);
				if (!gramPostings) {
					gramPostings = new Set<EmojiOrdinal>();
					postings.set(gram, gramPostings);
				}
				gramPostings.add(ordinal);
			}
		}
		let postingCount = 0;
		const frozenPostings = new Map<NGram, Uint32Array>();
		for (const [gram, ordinals] of postings) {
			const packedOrdinals = Uint32Array.from(ordinals);
			postingCount += packedOrdinals.length;
			frozenPostings.set(gram, packedOrdinals);
		}
		this.gramsToOrdinals = frozenPostings;
		this.stats = {
			emojiCount: this.metas.length,
			gramCount: this.gramsToOrdinals.size,
			postingCount,
		};
	}

	getStats(): EmojiSearchIndexStats {
		return this.stats;
	}

	search(query: string, options: EmojiSearchOptions<TEmoji> = {}): ReadonlyArray<TEmoji> {
		const normalizedQuery = normalizeQuery(query);
		if (!normalizedQuery) return this.metas.map((meta) => meta.emoji);
		const compactQuery = compactName(normalizedQuery) as NormalizedEmojiQuery;
		const candidateOrdinals = this.getCandidateOrdinals(compactQuery || normalizedQuery);
		if (candidateOrdinals.length === 0) return [];
		const results: Array<SearchResult<TEmoji>> = [];
		const limit = options.count ?? 0;
		for (const ordinal of candidateOrdinals) {
			const meta = this.metas[ordinal];
			if (!meta) continue;
			const tier = getEmojiMatchTier(meta, normalizedQuery, compactQuery);
			if (tier === null) continue;
			if (options.canUse && !options.canUse(meta.emoji)) continue;
			insertBoundedResult(
				results,
				{
					emoji: meta.emoji,
					tier,
					frecency: options.getFrecencyScore?.(meta.emoji) ?? 0,
					primaryName: meta.primaryName,
					order: meta.order,
				},
				limit,
			);
		}
		if (limit <= 0) {
			results.sort(compareSearchResults);
		}
		return results.map((result) => result.emoji);
	}

	private getCandidateOrdinals(query: NormalizedEmojiQuery): Uint32Array {
		const grams = queryGrams(query);
		if (grams.length === 0) return EMPTY_POSTINGS;
		let rarestPostings: Uint32Array | null = null;
		const remainingPostings: Array<Uint32Array> = [];
		for (const gram of grams) {
			const postings = this.gramsToOrdinals.get(gram);
			if (!postings) return EMPTY_POSTINGS;
			if (!rarestPostings || postings.length < rarestPostings.length) {
				if (rarestPostings) remainingPostings.push(rarestPostings);
				rarestPostings = postings;
			} else {
				remainingPostings.push(postings);
			}
		}
		if (!rarestPostings) return EMPTY_POSTINGS;
		if (remainingPostings.length === 0) return rarestPostings;
		const intersection: Array<number> = [];
		for (const ordinal of rarestPostings) {
			let appearsInAllPostings = true;
			for (const postings of remainingPostings) {
				if (!includesOrdinal(postings, ordinal)) {
					appearsInAllPostings = false;
					break;
				}
			}
			if (appearsInAllPostings) intersection.push(ordinal);
		}
		return Uint32Array.from(intersection);
	}
}

export function createEmojiSearchIndex<const TEmoji extends FlatEmoji>(
	emojis: ReadonlyArray<TEmoji>,
): EmojiSearchIndex<TEmoji> {
	return new EmojiSearchIndex(emojis);
}
