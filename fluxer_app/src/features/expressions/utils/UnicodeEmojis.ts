// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UnicodeEmoji} from '@app/features/emoji/types/EmojiTypes';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import * as RegexUtils from '@app/features/messaging/utils/RegexUtils';
import emojiData from '@app/media/data/emojis.json';
import {SKIN_TONE_SURROGATES} from '@fluxer/constants/src/EmojiConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {
	BicycleIcon,
	BowlFoodIcon,
	FlagIcon,
	GameControllerIcon,
	HeartIcon,
	LeafIcon,
	MagnetIcon,
	SmileyIcon,
} from '@phosphor-icons/react';

const PEOPLE_DESCRIPTOR = msg({
	message: 'People',
	comment: 'Unicode emoji category label.',
});
const NATURE_DESCRIPTOR = msg({
	message: 'Nature',
	comment: 'Unicode emoji category label.',
});
const FOOD_DRINK_DESCRIPTOR = msg({
	message: 'Food & drink',
	comment: 'Unicode emoji category label.',
});
const ACTIVITIES_DESCRIPTOR = msg({
	message: 'Activities',
	comment: 'Unicode emoji category label.',
});
const TRAVEL_PLACES_DESCRIPTOR = msg({
	message: 'Travel & places',
	comment: 'Unicode emoji category label.',
});
const OBJECTS_DESCRIPTOR = msg({
	message: 'Objects',
	comment: 'Unicode emoji category label.',
});
const SYMBOLS_DESCRIPTOR = msg({
	message: 'Symbols',
	comment: 'Unicode emoji category label.',
});
const FLAGS_DESCRIPTOR = msg({
	message: 'Flags',
	comment: 'Unicode emoji category label.',
});
export const EMOJI_SPRITES = {
	BasePerRow: 42,
	SkinTonePerRow: 10,
	PickerPerRow: 11,
	PickerCount: 50,
};
const categories = Object.freeze(Object.keys(emojiData.categories));

const toCanonicalSurrogate = (surrogate: string): string => surrogate.replace(/️/g, '');

let defaultSkinTone: string = '';

class UnicodeEmojiClass {
	uniqueName: string;
	names: ReadonlyArray<string>;
	keywords?: ReadonlyArray<string>;
	allNamesString: string;
	defaultUrl?: string;
	surrogates: string;
	hasSkinTones: boolean;
	managed: boolean;
	useSpriteSheet: boolean;
	index?: number;
	skinToneIndex?: number;
	skinTonesByName: Record<
		string,
		{
			url: string;
			name: string;
			surrogatePair: string;
		}
	>;
	urlBySkinToneSurrogate: Record<string, string>;

	constructor(emojiObject: {
		names: Array<string>;
		surrogates: string;
		hasSkinTones?: boolean;
		keywords?: Array<string>;
		skins?: Array<{
			surrogates: string;
		}>;
	}) {
		const {names, surrogates} = emojiObject;
		const name = names[0] || '';
		this.uniqueName = name;
		this.names = names;
		this.keywords = emojiObject.keywords;
		this.allNamesString = names.length > 1 ? `:${names.join(': :')}:` : `:${name}:`;
		this.defaultUrl = EmojiUtils.getEmojiURL(surrogates) ?? undefined;
		this.surrogates = surrogates;
		this.useSpriteSheet = false;
		this.index = undefined;
		this.skinToneIndex = undefined;
		this.urlBySkinToneSurrogate = {};
		this.skinTonesByName = {};
		this.hasSkinTones = emojiObject.hasSkinTones || !!(emojiObject.skins && emojiObject.skins.length > 0);
		this.managed = true;
		if (this.hasSkinTones && emojiObject.skins) {
			SKIN_TONE_SURROGATES.forEach((skinTone, index) => {
				const skinData = emojiObject.skins?.[index];
				if (skinData) {
					const surrogatePair = skinData.surrogates;
					const url = EmojiUtils.getEmojiURL(surrogatePair);
					if (url) {
						this.urlBySkinToneSurrogate[skinTone] = url;
						names.forEach((name) => {
							const skinName = `${name}::skin-tone-${index + 1}`;
							this.skinTonesByName[skinName] = {
								name: skinName,
								surrogatePair,
								url,
							};
						});
					}
				}
			});
		}
	}

	setSpriteSheetIndex(index: number, isSkinTone = false) {
		if (isSkinTone) {
			this.skinToneIndex = index;
		} else {
			this.index = index;
		}
		this.useSpriteSheet = true;
	}

	toJSON(): UnicodeEmoji {
		return {
			uniqueName: this.uniqueName,
			name: this.uniqueName,
			names: this.names,
			keywords: this.keywords,
			allNamesString: this.allNamesString,
			url: this.defaultUrl,
			surrogates: this.surrogates,
			hasSkinTones: this.hasSkinTones,
			managed: this.managed,
			useSpriteSheet: this.useSpriteSheet,
			index: this.index,
			skinToneIndex: this.skinToneIndex,
		};
	}
}

interface EmojiIndex {
	emojisByCategory: Record<string, Array<UnicodeEmoji>>;
	categoryByEmojiName: Record<string, string>;
	nameToEmoji: Record<string, UnicodeEmoji>;
	shortcodeNameToEmoji: Record<string, UnicodeEmoji>;
	nameToSurrogate: Record<string, string>;
	surrogateToName: Record<string, string>;
	canonicalSurrogateToName: Record<string, string>;
	shortcutToName: Record<string, string>;
	emojis: Array<UnicodeEmoji>;
	skinToneSpriteCount: number;
	baseSpriteCount: number;
	emojiSurrogateRegex: RegExp;
	emojiShortcutRegex: RegExp;
}

let cachedEmojiIndex: EmojiIndex | null = null;

function buildEmojiIndex(): EmojiIndex {
	const emojisByCategory: Record<string, Array<UnicodeEmoji>> = {};
	const categoryByEmojiName: Record<string, string> = {};
	const nameToEmoji: Record<string, UnicodeEmoji> = {};
	const shortcodeNameToEmoji: Record<string, UnicodeEmoji> = {};
	const nameToSurrogate: Record<string, string> = {};
	const surrogateToName: Record<string, string> = {};
	const canonicalSurrogateToName: Record<string, string> = {};
	const shortcutToName: Record<string, string> = {};
	const emojis: Array<UnicodeEmoji> = [];
	let skinToneSpriteCount = 0;
	let baseSpriteCount = 0;

	Object.entries(emojiData.categories).forEach(([category, emojiObjects]) => {
		emojisByCategory[category] = emojiObjects.map((emojiObject) => {
			const emoji = new UnicodeEmojiClass(emojiObject);
			if (emoji.hasSkinTones) {
				emoji.setSpriteSheetIndex(skinToneSpriteCount++, true);
			}
			emoji.setSpriteSheetIndex(baseSpriteCount++, false);
			surrogateToName[emoji.surrogates] = emoji.uniqueName;
			canonicalSurrogateToName[toCanonicalSurrogate(emoji.surrogates)] = emoji.uniqueName;
			const emojiJson = emoji.toJSON();
			emoji.names.forEach((name) => {
				nameToEmoji[name] = emojiJson;
				shortcodeNameToEmoji[name] = emojiJson;
				nameToSurrogate[name] = emoji.surrogates;
			});
			Object.values(emoji.skinTonesByName).forEach((skinToneEntry) => {
				const skinTonedEmoji: UnicodeEmoji = {
					...emojiJson,
					name: skinToneEntry.name,
					surrogates: skinToneEntry.surrogatePair,
					url: skinToneEntry.url,
				};
				nameToEmoji[skinToneEntry.name] = skinTonedEmoji;
				shortcodeNameToEmoji[skinToneEntry.name] = skinTonedEmoji;
				nameToSurrogate[skinToneEntry.name] = skinToneEntry.surrogatePair;
				surrogateToName[skinToneEntry.surrogatePair] = skinToneEntry.name;
				canonicalSurrogateToName[toCanonicalSurrogate(skinToneEntry.surrogatePair)] = skinToneEntry.name;
			});
			categoryByEmojiName[emoji.uniqueName] = category;
			emojis.push(emojiJson);
			return emojiJson;
		});
	});

	SKIN_TONE_SURROGATES.forEach((surrogatePair, index) => {
		nameToSurrogate[`skin-tone-${index + 1}`] = surrogatePair;
		surrogateToName[surrogatePair] = `skin-tone-${index + 1}`;
		canonicalSurrogateToName[toCanonicalSurrogate(surrogatePair)] = `skin-tone-${index + 1}`;
	});

	const keywordOwner: Record<string, string> = {};
	const keywordCount: Record<string, number> = {};

	Object.values(emojiData.categories).forEach((entries) => {
		entries.forEach((entry) => {
			const keywords = (entry as {keywords?: ReadonlyArray<string>}).keywords;
			if (!keywords) return;
			const owner = entry.names[0];
			if (!owner) return;
			keywords.forEach((kw) => {
				keywordCount[kw] = (keywordCount[kw] ?? 0) + 1;
				keywordOwner[kw] = owner;
			});
		});
	});

	Object.entries(keywordOwner).forEach(([kw, owner]) => {
		if ((keywordCount[kw] ?? 0) !== 1) return;
		if (nameToEmoji[kw]) return;
		const target = nameToEmoji[owner];
		if (!target) return;
		nameToEmoji[kw] = target;
		nameToSurrogate[kw] = target.surrogates;
	});

	Object.entries(emojiData.shortcuts).forEach(([shortcut, emoji]) => {
		shortcutToName[shortcut] = emoji;
	});

	const surrogateAlternation = Object.keys(surrogateToName)
		.sort((a, b) => b.length - a.length)
		.map(RegexUtils.escapeRegex)
		.join('|');
	const shortcutAlternation = Object.keys(shortcutToName).map(RegexUtils.escapeRegex).join('|');

	return {
		emojisByCategory,
		categoryByEmojiName,
		nameToEmoji,
		shortcodeNameToEmoji,
		nameToSurrogate,
		surrogateToName,
		canonicalSurrogateToName,
		shortcutToName,
		emojis,
		skinToneSpriteCount,
		baseSpriteCount,
		emojiSurrogateRegex: new RegExp(`(${surrogateAlternation})`, 'g'),
		emojiShortcutRegex: new RegExp(`^(${shortcutAlternation})`),
	};
}

function getEmojiIndex(): EmojiIndex {
	cachedEmojiIndex ??= buildEmojiIndex();
	return cachedEmojiIndex;
}

const lookupSurrogateName = (surrogate: string): string | null =>
	getEmojiIndex().canonicalSurrogateToName[toCanonicalSurrogate(surrogate)] ?? null;

const EMOJI_SHORTCODE_RE = /^:([^\s:]+(?:::skin-tone-[0-9])?):/;
const categoryIcons = {
	people: SmileyIcon,
	nature: LeafIcon,
	food: BowlFoodIcon,
	activity: GameControllerIcon,
	travel: BicycleIcon,
	objects: MagnetIcon,
	symbols: HeartIcon,
	flags: FlagIcon,
};
const getCategoryLabel = (category: string, i18n: I18n): string => {
	switch (category) {
		case 'people':
			return i18n._(PEOPLE_DESCRIPTOR);
		case 'nature':
			return i18n._(NATURE_DESCRIPTOR);
		case 'food':
			return i18n._(FOOD_DRINK_DESCRIPTOR);
		case 'activity':
			return i18n._(ACTIVITIES_DESCRIPTOR);
		case 'travel':
			return i18n._(TRAVEL_PLACES_DESCRIPTOR);
		case 'objects':
			return i18n._(OBJECTS_DESCRIPTOR);
		case 'symbols':
			return i18n._(SYMBOLS_DESCRIPTOR);
		case 'flags':
			return i18n._(FLAGS_DESCRIPTOR);
		default:
			return category;
	}
};

export default {
	getDefaultSkinTone: (): string => defaultSkinTone,
	setDefaultSkinTone: (skinTone: string): void => {
		defaultSkinTone = skinTone || '';
	},
	getCategories: (): ReadonlyArray<string> => categories,
	getByName: (emojiName: string): UnicodeEmoji | null => getEmojiIndex().nameToEmoji[emojiName] || null,
	getByCategory: (emojiCategory: string): ReadonlyArray<UnicodeEmoji> | null =>
		getEmojiIndex().emojisByCategory[emojiCategory] || null,
	shortcodeTextToSurrogates: (content: string): string => {
		return content.replace(EMOJI_SHORTCODE_RE, (original, emoji) => getEmojiIndex().nameToSurrogate[emoji] || original);
	},
	surrogateTextToShortcodes: (content: string): string => {
		const index = getEmojiIndex();
		return content.replace(index.emojiSurrogateRegex, (_, surrogate) => {
			const name = index.surrogateToName[surrogate];
			return name ? `:${name}:` : surrogate;
		});
	},
	surrogateForName: (emojiName: string, defaultSurrogate = ''): string => {
		return getEmojiIndex().nameToSurrogate[emojiName] || defaultSurrogate;
	},
	normalizeEmojiNameToSurrogate: (emojiName: string): string => {
		const trimmed = emojiName.trim();
		const name =
			trimmed.startsWith(':') && trimmed.endsWith(':') && trimmed.length > 2 ? trimmed.slice(1, -1) : trimmed;
		return getEmojiIndex().nameToSurrogate[name] || trimmed;
	},
	nameForSurrogate: (surrogate: string, includeColons = true, defaultName = ''): string => {
		const name = lookupSurrogateName(surrogate);
		if (!name) return defaultName;
		return includeColons ? `:${name}:` : name;
	},
	convertShortcutToName: (shortcut: string, includeColons = true, defaultName = ''): string => {
		const name = getEmojiIndex().shortcutToName[shortcut] || defaultName;
		return includeColons && name ? `:${name}:` : name;
	},
	forEachEmoji: (callback: (emoji: UnicodeEmoji) => void): void => {
		getEmojiIndex().emojis.forEach(callback);
	},
	all: (): ReadonlyArray<UnicodeEmoji> => getEmojiIndex().emojis,
	getCategoryForEmoji: (emoji: UnicodeEmoji): string | null => {
		return getEmojiIndex().categoryByEmojiName[emoji.uniqueName] ?? null;
	},
	getCategoryIcon: (
		category: string,
	): React.ComponentType<{
		className?: string;
	}> => {
		return categoryIcons[category as keyof typeof categoryIcons] || SmileyIcon;
	},
	getCategoryLabel,
	getSurrogateName: (surrogate: string): string | null => {
		return lookupSurrogateName(surrogate);
	},
	findEmojiByName: (emojiName: string): UnicodeEmoji | null => {
		return getEmojiIndex().nameToEmoji[emojiName] || null;
	},
	findEmojiByShortcodeName: (emojiName: string): UnicodeEmoji | null => {
		return getEmojiIndex().shortcodeNameToEmoji[emojiName] || null;
	},
	findEmojiWithSkinTone: (baseName: string, skinToneSurrogate: string): UnicodeEmoji | null => {
		const index = getEmojiIndex();
		const skinToneName = index.surrogateToName[skinToneSurrogate];
		if (!skinToneName) return null;
		const skinToneEmojiName = `${baseName}::${skinToneName}`;
		return index.nameToEmoji[skinToneEmojiName] || null;
	},
	get skinToneSpriteCount(): number {
		return getEmojiIndex().skinToneSpriteCount;
	},
	get baseSpriteCount(): number {
		return getEmojiIndex().baseSpriteCount;
	},
	EMOJI_SHORTCODE_RE,
	get EMOTICON_PREFIX_RE(): RegExp {
		return getEmojiIndex().emojiShortcutRegex;
	},
	get EMOJI_SURROGATE_RE(): RegExp {
		return getEmojiIndex().emojiSurrogateRegex;
	},
	EMOJI_SPRITES,
};
