// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CONFUSABLE_PROTOTYPES,
	CONFUSABLE_SOURCES,
	RESTRICTED_IDENTIFIER_RANGES,
} from '@app/api/operator_pass/OperatorPassNameUnicodeData';
import {OPERATOR_PASS_CERTIFICATE_NAME_MAX_LENGTH} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {OPERATOR_PASS_CERTIFICATE_NAME_SCRIPTS} from '@pkgs/email/src/operator_pass_certificate/OperatorPassCertificateNameText';

const MAX_CODE_POINTS_PER_CHARACTER = 10;
const LETTER = /^[\p{L}\p{M}\p{N}]$/u;
const SYMBOL = /^[\p{P}\p{Sc}\p{Sm}\p{Sk}]$/u;
const JOINER = /^[\u200C\u200D]$/u;
const MONGOLIAN_VARIATION = /^[\u180B-\u180F]$/u;
const MONGOLIAN_LETTER = /^\p{Script=Mongolian}$/u;
const GRAPHEMES = new Intl.Segmenter('en', {granularity: 'grapheme'});

const CONFUSABLES: ReadonlyMap<number, string> = new Map(
	CONFUSABLE_SOURCES.map((source, index) => [source, CONFUSABLE_PROTOTYPES[index] ?? '']),
);

const SCRIPT_EXTENSIONS = OPERATOR_PASS_CERTIFICATE_NAME_SCRIPTS.map(
	(script) => [script, new RegExp(`^\\p{Script_Extensions=${script}}$`, 'u')] as const,
);

const SCRIPT_GROUPS: Readonly<Record<string, ReadonlyArray<string>>> = {
	Han: ['Han', 'Japanese', 'Korean', 'Bopomofo'],
	Hiragana: ['Japanese'],
	Katakana: ['Japanese'],
	Hangul: ['Korean'],
	Bopomofo: ['Bopomofo'],
};

const LATIN_PARTNERS_EXCLUDED: ReadonlySet<string> = new Set(['Greek', 'Cyrillic']);

const HOMOGLYPHS: Readonly<Record<string, string>> = {
	в: 'b',
	к: 'k',
	м: 'm',
	н: 'h',
	т: 't',
	ȷ: 'j',
	ɛ: 'e',
	ə: 'e',
	ɔ: 'o',
	β: 'b',
	ε: 'e',
	κ: 'k',
	τ: 't',
	χ: 'x',
	ᴀ: 'a',
	ʙ: 'b',
	ᴅ: 'd',
	ᴇ: 'e',
	ꜰ: 'f',
	ɢ: 'g',
	ʜ: 'h',
	ᴊ: 'j',
	ᴋ: 'k',
	ʟ: 'l',
	ᴍ: 'm',
	ɴ: 'n',
	ᴘ: 'p',
	ʀ: 'r',
	ᴛ: 't',
	'3': 'e',
	'4': 'a',
	'5': 's',
	'7': 't',
	'@': 'a',
	$: 's',
};

const RESERVED_SUBSTRINGS: ReadonlyArray<string> = ['fluxer', 'plutonium', 'hampuskraft'];

const ROLE_WORDS: ReadonlySet<string> = new Set([
	'staff',
	'official',
	'admin',
	'administrator',
	'moderator',
	'support',
	'team',
]);

const BRAND_WORDS: ReadonlySet<string> = new Set([
	'anthropic',
	'apple',
	'amazon',
	'discord',
	'facebook',
	'github',
	'google',
	'guilded',
	'instagram',
	'mastercard',
	'meta',
	'microsoft',
	'netflix',
	'nintendo',
	'openai',
	'paypal',
	'playstation',
	'reddit',
	'revolt',
	'samsung',
	'signal',
	'slack',
	'sony',
	'spotify',
	'steam',
	'stripe',
	'telegram',
	'tesla',
	'tiktok',
	'twitch',
	'twitter',
	'valve',
	'visa',
	'whatsapp',
	'xbox',
	'youtube',
]);

const COMPANY_WORDS: ReadonlySet<string> = new Set([
	'ab',
	'co',
	'company',
	'corp',
	'corporation',
	'gmbh',
	'inc',
	'llc',
	'ltd',
	'plc',
]);

function isRestrictedIdentifierCharacter(codePoint: number): boolean {
	let low = 0;
	let high = RESTRICTED_IDENTIFIER_RANGES.length / 2 - 1;
	while (low <= high) {
		const middle = (low + high) >> 1;
		const start = RESTRICTED_IDENTIFIER_RANGES[middle * 2] ?? 0;
		const end = RESTRICTED_IDENTIFIER_RANGES[middle * 2 + 1] ?? 0;
		if (codePoint < start) {
			high = middle - 1;
		} else if (codePoint > end) {
			low = middle + 1;
		} else {
			return true;
		}
	}
	return false;
}

function skeleton(text: string): string {
	return Array.from(
		text.normalize('NFD'),
		(character) => CONFUSABLES.get(character.codePointAt(0) ?? 0) ?? character,
	).join('');
}

function withoutMarks(text: string): string {
	return text
		.normalize('NFKD')
		.replace(/\p{M}+/gu, '')
		.toLowerCase();
}

function foldWord(text: string): string {
	const lookalikes = Array.from(withoutMarks(text), (character) => HOMOGLYPHS[character] ?? character).join('');
	return withoutMarks(skeleton(lookalikes));
}

function foldForComparison(name: string): ReadonlyArray<string> {
	return [foldWord(name), foldWord(skeleton(name))];
}

const FOLDED_RESERVED_SUBSTRINGS = RESERVED_SUBSTRINGS.map(foldWord);
function foldWords(words: ReadonlySet<string>): ReadonlySet<string> {
	return new Set([...words].flatMap((word) => [foldWord(word), foldWord(skeleton(word.toUpperCase()))]));
}

const FOLDED_ROLE_WORDS = foldWords(ROLE_WORDS);
const FOLDED_BRAND_WORDS = foldWords(BRAND_WORDS);
const FOLDED_COMPANY_WORDS = foldWords(COMPANY_WORDS);

function scriptsOf(character: string): ReadonlySet<string> | null {
	const scripts = new Set<string>();
	for (const [script, pattern] of SCRIPT_EXTENSIONS) {
		if (pattern.test(character)) {
			for (const group of SCRIPT_GROUPS[script] ?? [script]) {
				scripts.add(group);
			}
		}
	}
	return scripts.size > 0 ? scripts : null;
}

function commonScripts(characters: ReadonlyArray<string>): ReadonlySet<string> | null {
	let shared: ReadonlySet<string> | null = null;
	for (const character of characters) {
		const scripts = scriptsOf(character);
		if (scripts) {
			const previous: ReadonlySet<string> = shared ?? scripts;
			shared = new Set([...previous].filter((script) => scripts.has(script)));
		}
	}
	return shared;
}

function mixesScripts(word: string): boolean {
	const characters = Array.from(word);
	const shared = commonScripts(characters);
	if (shared === null || shared.size > 0) {
		return false;
	}
	const others = commonScripts(characters.filter((character) => !scriptsOf(character)?.has('Latin')));
	return others === null || others.size === 0 || [...others].every((script) => LATIN_PARTNERS_EXCLUDED.has(script));
}

function matchesReservedName(folded: string): boolean {
	const compact = folded.replace(/[^\p{L}\p{N}]+/gu, '');
	if (FOLDED_RESERVED_SUBSTRINGS.some((reserved) => compact.includes(reserved))) {
		return true;
	}
	const words = folded.split(/[^\p{L}\p{N}]+/u).filter((word) => word.length > 0);
	if (
		words.some((word) => FOLDED_ROLE_WORDS.has(word)) ||
		FOLDED_ROLE_WORDS.has(compact) ||
		FOLDED_BRAND_WORDS.has(compact)
	) {
		return true;
	}
	return (
		words.some((word) => FOLDED_BRAND_WORDS.has(word)) &&
		words.every((word) => FOLDED_BRAND_WORDS.has(word) || FOLDED_ROLE_WORDS.has(word) || FOLDED_COMPANY_WORDS.has(word))
	);
}

export function normalizeOperatorPassCertificateName(name: string): string {
	return name.normalize('NFC').replace(/\s+/gu, ' ').trim();
}

export function isOperatorPassReservedName(name: string): boolean {
	const words = name.split(/[^\p{L}\p{M}\p{N}]+/u);
	return words.some(mixesScripts) || foldForComparison(name).some(matchesReservedName);
}

function hasOnlyPrintableCharacters(name: string): boolean {
	const characters = Array.from(name);
	return characters.every((character, index) => {
		const previous = characters[index - 1] ?? '';
		if (character === ' ' || SYMBOL.test(character)) {
			return true;
		}
		if (JOINER.test(character)) {
			return LETTER.test(previous) && LETTER.test(characters[index + 1] ?? '');
		}
		if (MONGOLIAN_VARIATION.test(character)) {
			return MONGOLIAN_LETTER.test(previous);
		}
		return LETTER.test(character) && !isRestrictedIdentifierCharacter(character.codePointAt(0) ?? 0);
	});
}

function hasReasonableCharacters(name: string): boolean {
	const characters = Array.from(GRAPHEMES.segment(name), ({segment}) => segment);
	return (
		characters.length <= OPERATOR_PASS_CERTIFICATE_NAME_MAX_LENGTH &&
		characters.every((character) => Array.from(character).length <= MAX_CODE_POINTS_PER_CHARACTER)
	);
}

export function isAcceptableOperatorPassCertificateName(name: string): boolean {
	const normalized = normalizeOperatorPassCertificateName(name);
	return (
		normalized.length > 0 &&
		/\p{L}/u.test(normalized) &&
		hasReasonableCharacters(normalized) &&
		hasOnlyPrintableCharacters(normalized) &&
		!isOperatorPassReservedName(normalized)
	);
}
