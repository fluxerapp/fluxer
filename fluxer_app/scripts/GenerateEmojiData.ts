// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {readFileSync, writeFileSync} from 'node:fs';
import {join} from 'node:path';
import {parseArgs} from 'node:util';

const SOURCES = {
	emojiTest: {
		url: 'https://www.unicode.org/Public/17.0.0/emoji/emoji-test.txt',
		sha256: '1d8a944f88d7952f7ef7c5167fef3c67995bcae24543949710231b03a201acda',
	},
	annotations: {
		url: 'https://raw.githubusercontent.com/unicode-org/cldr/release-48/common/annotations/en.xml',
		sha256: '8511aadd046fdba2f0ffe590266ced8bbf48175ad139b2675d85d7141057b235',
	},
	annotationsDerived: {
		url: 'https://raw.githubusercontent.com/unicode-org/cldr/release-48/common/annotationsDerived/en.xml',
		sha256: 'd76bd041c8c9e7b00b716aff8b7d9dbf509877010e191d5efd068be2553e066e',
	},
} as const;

type SourceKey = keyof typeof SOURCES;

const CATEGORY_BY_GROUP: Record<string, string | null> = {
	'Smileys & Emotion': 'people',
	'People & Body': 'people',
	Component: null,
	'Animals & Nature': 'nature',
	'Food & Drink': 'food',
	Activities: 'activity',
	'Travel & Places': 'travel',
	Objects: 'objects',
	Symbols: 'symbols',
	Flags: 'flags',
};

const CATEGORY_ORDER = ['people', 'nature', 'food', 'activity', 'travel', 'objects', 'symbols', 'flags'];

const SKIN_TONE_RE = /[\u{1F3FB}-\u{1F3FF}]/gu;
const HAS_SKIN_TONE_RE = /[\u{1F3FB}-\u{1F3FF}]/u;
const FE0F_RE = /️/g;

interface SkinOutput {
	names: Array<string>;
	surrogates: string;
}

interface EmojiOutput {
	names: Array<string>;
	surrogates: string;
	keywords?: Array<string>;
	skins?: Array<SkinOutput>;
}

interface TestEntry {
	surrogates: string;
	name: string;
	category: string;
	skins: Array<{surrogates: string; name: string}>;
}

interface EmojiNamesFile {
	names: Record<string, Array<string>>;
	shortcuts: Record<string, string>;
}

async function loadSource(key: SourceKey): Promise<string> {
	const {url, sha256} = SOURCES[key];
	const response = await fetch(url);
	if (!response.ok) {
		throw new Error(`Download failed for ${url}: ${response.status}`);
	}
	const bytes = Buffer.from(await response.arrayBuffer());
	const actual = createHash('sha256').update(bytes).digest('hex');
	if (actual !== sha256) {
		throw new Error(`sha256 mismatch for ${url}: expected ${sha256}, got ${actual}`);
	}
	return bytes.toString('utf-8');
}

function canonical(surrogates: string): string {
	return surrogates.replace(FE0F_RE, '');
}

function decodeXml(text: string): string {
	return text
		.replace(/&quot;/g, '"')
		.replace(/&apos;/g, "'")
		.replace(/&lt;/g, '<')
		.replace(/&gt;/g, '>')
		.replace(/&amp;/g, '&');
}

function parseKeywords(...xmls: Array<string>): Map<string, Array<string>> {
	const keywords = new Map<string, Array<string>>();
	for (const xml of xmls) {
		for (const match of xml.matchAll(/<annotation cp="([^"]+)">([^<]*)<\/annotation>/g)) {
			const key = canonical(decodeXml(match[1]));
			if (keywords.has(key)) continue;
			keywords.set(
				key,
				decodeXml(match[2])
					.trim()
					.split('|')
					.map((keyword) => keyword.trim()),
			);
		}
	}
	return keywords;
}

function baseNameOf(name: string): string {
	const [head, rest = ''] = name.split(': ');
	const parts = rest.split(', ').filter((part) => part && !part.endsWith('skin tone'));
	return parts.length > 0 ? `${head}: ${parts.join(', ')}` : head;
}

function parseEmojiTest(text: string): Array<TestEntry> {
	const entries: Array<TestEntry> = [];
	const entriesByName = new Map<string, TestEntry>();
	let category: string | null = null;
	for (const line of text.split('\n')) {
		const group = line.match(/^# group: (.+)$/);
		if (group) {
			if (!(group[1] in CATEGORY_BY_GROUP)) {
				throw new Error(`Unknown emoji group: ${group[1]}`);
			}
			category = CATEGORY_BY_GROUP[group[1]];
			continue;
		}
		const data = line.match(/^([0-9A-F ]+?)\s*;\s*fully-qualified\s*#\s*\S+\s+E\d+\.\d+\s+(.+)$/);
		if (!data || category == null) continue;
		const surrogates = String.fromCodePoint(...data[1].split(' ').map((hex) => Number.parseInt(hex, 16)));
		const name = data[2];
		if (!HAS_SKIN_TONE_RE.test(surrogates)) {
			const entry = {surrogates, name, category, skins: []};
			entries.push(entry);
			entriesByName.set(name, entry);
			continue;
		}
		const base = entriesByName.get(baseNameOf(name)) ?? entriesByName.get(name.split(': ')[0]);
		if (!base) throw new Error(`Skin tone variant without base: ${name}`);
		base.skins.push({surrogates, name});
	}
	return entries;
}

function sortSkins(base: string, skins: Array<{surrogates: string; name: string}>) {
	const baseKey = canonical(base);
	const single = skins.filter((skin) => new Set(skin.surrogates.match(SKIN_TONE_RE)).size === 1);
	const mixed = skins.filter((skin) => new Set(skin.surrogates.match(SKIN_TONE_RE)).size > 1);
	for (const skin of single) {
		if (canonical(skin.surrogates.replace(SKIN_TONE_RE, '')) !== baseKey) {
			throw new Error(`Single-tone variant ${skin.name} does not match its base`);
		}
	}
	if (single.length !== 5 || (mixed.length !== 0 && mixed.length !== 20)) {
		throw new Error(`Unexpected skin tone layout for ${base}: ${single.length} single, ${mixed.length} mixed`);
	}
	return [...single, ...mixed];
}

function keywordsFor(
	surrogates: string,
	names: Array<string>,
	keywords: Map<string, Array<string>>,
): Array<string> | undefined {
	const phrases = keywords.get(canonical(surrogates));
	if (!phrases) return undefined;
	const nameSet = new Set(names);
	const words = [...new Set(phrases.flatMap((phrase) => phrase.split(/\s+/)))].filter(
		(word) => word && !nameSet.has(word),
	);
	words.sort((a, b) => {
		const byLower = a.toLowerCase().localeCompare(b.toLowerCase(), 'en');
		return byLower !== 0 ? byLower : a.localeCompare(b, 'en');
	});
	return words.length > 0 ? words : undefined;
}

function regionalIndicators(): Array<TestEntry> {
	const entries: Array<TestEntry> = [];
	for (let offset = 25; offset >= 0; offset--) {
		const letter = String.fromCharCode(0x61 + offset);
		entries.push({
			surrogates: String.fromCodePoint(0x1f1e6 + offset),
			name: `regional indicator symbol letter ${letter}`,
			category: 'symbols',
			skins: [],
		});
	}
	return entries;
}

function loadNames(): EmojiNamesFile {
	return JSON.parse(readFileSync(join(import.meta.dirname, 'EmojiNames.json'), 'utf-8')) as EmojiNamesFile;
}

async function main(): Promise<void> {
	const {values} = parseArgs({
		options: {
			out: {type: 'string', default: join(import.meta.dirname, '..', 'src', 'media', 'data', 'emojis.json')},
		},
	});
	const [emojiTest, annotationsXml, derivedXml] = await Promise.all([
		loadSource('emojiTest'),
		loadSource('annotations'),
		loadSource('annotationsDerived'),
	]);
	const keywords = parseKeywords(annotationsXml, derivedXml);
	const entries = [...parseEmojiTest(emojiTest), ...regionalIndicators()];
	const nameFile = loadNames();

	const owner = new Map<string, string>();
	const missing: Array<string> = [];
	const namesFor = (surrogates: string, unicodeName: string): Array<string> => {
		const names = nameFile.names[canonical(surrogates)];
		if (!names || names.length === 0) {
			missing.push(`${surrogates} (${unicodeName})`);
			return [];
		}
		for (const name of names) {
			const current = owner.get(name);
			if (current !== undefined && current !== surrogates) {
				throw new Error(`Duplicate name ${name} for ${surrogates} and ${current}`);
			}
			owner.set(name, surrogates);
		}
		return names;
	};

	const categories: Record<string, Array<EmojiOutput>> = Object.fromEntries(
		CATEGORY_ORDER.map((category) => [category, []]),
	);
	for (const entry of entries) {
		const names = namesFor(entry.surrogates, entry.name);
		const output: EmojiOutput = {names, surrogates: entry.surrogates};
		const entryKeywords = keywordsFor(entry.surrogates, names, keywords);
		if (entryKeywords) output.keywords = entryKeywords;
		if (entry.skins.length > 0) {
			output.skins = sortSkins(entry.surrogates, entry.skins).map((skin) => ({
				names: namesFor(skin.surrogates, skin.name),
				surrogates: skin.surrogates,
			}));
		}
		categories[entry.category].push(output);
	}
	if (missing.length > 0) {
		throw new Error(`Add names to EmojiNames.json for: ${missing.join(', ')}`);
	}

	for (const [shortcut, name] of Object.entries(nameFile.shortcuts)) {
		if (!owner.has(name)) throw new Error(`Shortcut ${shortcut} points at unknown name ${name}`);
	}

	writeFileSync(values.out, JSON.stringify({shortcuts: nameFile.shortcuts, categories}));
	const total = Object.values(categories).reduce((sum, list) => sum + list.length, 0);
	console.log(`Wrote ${total} emoji to ${values.out}`);
}

await main();
