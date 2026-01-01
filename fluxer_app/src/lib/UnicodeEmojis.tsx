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
import {SKIN_TONE_SURROGATES} from '~/Constants';
import emojiShortcuts from '~/data/emoji-shortcuts.json';
import emojiData from '~/data/emojis.json';
import * as EmojiUtils from '~/utils/EmojiUtils';
import * as RegexUtils from '~/utils/RegexUtils';

export const EMOJI_SPRITES = {
	NonDiversityPerRow: 42,
	DiversityPerRow: 10,
	PickerPerRow: 11,
	PickerCount: 50,
};

export interface UnicodeEmoji {
	id?: string;
	uniqueName: string;
	name: string;
	names: ReadonlyArray<string>;
	allNamesString: string;
	url?: string;
	surrogates: string;
	hasDiversity: boolean;
	managed: boolean;
	useSpriteSheet: boolean;
	index?: number;
	diversityIndex?: number;
	guildId?: string;
}

const emojisByCategory: Record<string, Array<UnicodeEmoji>> = {};
const nameToEmoji: Record<string, UnicodeEmoji> = {};
const nameToSurrogate: Record<string, string> = {};
const surrogateToName: Record<string, string> = {};
const shortcutToName: Record<string, string> = {};
const emojis: Array<UnicodeEmoji> = [];

let defaultSkinTone: string = '';
let numDiversitySprites = 0;
let numNonDiversitySprites = 0;

class UnicodeEmojiClass {
	uniqueName: string;
	names: ReadonlyArray<string>;
	allNamesString: string;
	defaultUrl?: string;
	surrogates: string;
	hasDiversity: boolean;
	managed: boolean;
	useSpriteSheet: boolean;
	index?: number;
	diversityIndex?: number;
	diversitiesByName: Record<string, {url: string; name: string; surrogatePair: string}>;
	urlForDiversitySurrogate: Record<string, string>;

	constructor(emojiObject: {names: Array<string>; surrogates: string; hasDiversity?: boolean; skins?: Array<any>}) {
		const {names, surrogates} = emojiObject;
		const name = names[0] || '';

		this.uniqueName = name;
		this.names = names;
		this.allNamesString = names.length > 1 ? `:${names.join(': :')}:` : `:${name}:`;
		this.defaultUrl = EmojiUtils.getEmojiURL(surrogates) ?? undefined;
		this.surrogates = surrogates;
		this.useSpriteSheet = false;
		this.index = undefined;
		this.diversityIndex = undefined;
		this.urlForDiversitySurrogate = {};
		this.diversitiesByName = {};
		this.hasDiversity = emojiObject.hasDiversity || !!(emojiObject.skins && emojiObject.skins.length > 0);
		this.managed = true;

		if (this.hasDiversity && emojiObject.skins) {
			SKIN_TONE_SURROGATES.forEach((skinTone, index) => {
				const skinData = emojiObject.skins?.[index];
				if (skinData) {
					const surrogatePair = skinData.surrogates;
					const url = EmojiUtils.getEmojiURL(surrogatePair);
					if (url) {
						this.urlForDiversitySurrogate[skinTone] = url;

						names.forEach((name) => {
							const skinName = `${name}::skin-tone-${index + 1}`;
							this.diversitiesByName[skinName] = {
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

	get url(): string | undefined {
		if (this.hasDiversity && defaultSkinTone !== '' && this.urlForDiversitySurrogate[defaultSkinTone]) {
			return this.urlForDiversitySurrogate[defaultSkinTone];
		}
		return this.defaultUrl;
	}

	get name(): string {
		if (this.hasDiversity && defaultSkinTone !== '') {
			return `${this.uniqueName}::${surrogateToName[defaultSkinTone]}`;
		}
		return this.uniqueName;
	}

	get surrogatePair(): string {
		const diversity = this.diversitiesByName[this.name];
		return diversity ? diversity.surrogatePair : this.surrogates;
	}

	setSpriteSheetIndex(index: number, isDiversity = false) {
		if (isDiversity) {
			this.diversityIndex = index;
		} else {
			this.index = index;
		}
		this.useSpriteSheet = true;
	}

	toJSON(): UnicodeEmoji {
		return {
			uniqueName: this.uniqueName,
			name: this.name,
			names: this.names,
			allNamesString: this.allNamesString,
			url: this.url,
			surrogates: this.surrogatePair,
			hasDiversity: this.hasDiversity,
			managed: this.managed,
			useSpriteSheet: this.useSpriteSheet,
			index: this.index,
			diversityIndex: this.diversityIndex,
		};
	}
}

Object.entries(emojiData).forEach(([category, emojiObjects]) => {
	emojisByCategory[category] = emojiObjects.map((emojiObject) => {
		const emoji = new UnicodeEmojiClass(emojiObject);

		if (emoji.hasDiversity) {
			emoji.setSpriteSheetIndex(numDiversitySprites++, true);
		}
		emoji.setSpriteSheetIndex(numNonDiversitySprites++, false);

		surrogateToName[emoji.surrogates] = emoji.uniqueName;

		emoji.names.forEach((name) => {
			nameToEmoji[name] = emoji.toJSON();
			nameToSurrogate[name] = emoji.surrogates;
		});

		Object.values(emoji.diversitiesByName).forEach((diversity) => {
			nameToEmoji[diversity.name] = emoji.toJSON();
			nameToSurrogate[diversity.name] = diversity.surrogatePair;
			surrogateToName[diversity.surrogatePair] = diversity.name;
		});

		const emojiJson = emoji.toJSON();
		emojis.push(emojiJson);
		return emojiJson;
	});
});

SKIN_TONE_SURROGATES.forEach((surrogatePair, index) => {
	nameToSurrogate[`skin-tone-${index + 1}`] = surrogatePair;
	surrogateToName[surrogatePair] = `skin-tone-${index + 1}`;
});

emojiShortcuts.forEach(({emoji, shortcuts}) => {
	shortcuts.forEach((shortcut: string) => {
		shortcutToName[shortcut] = emoji;
	});
});

const EMOJI_NAME_RE = /^:([^\s:]+?(?:::skin-tone-\d)?):/;
const EMOJI_NAME_AND_DIVERSITY_RE = /^:([^\s:]+?(?:::skin-tone-\d)?):/;
const EMOJI_SURROGATE_RE = (() => {
	const emojiRegex = Object.keys(surrogateToName)
		.sort((a, b) => b.length - a.length)
		.map(RegexUtils.escapeRegex)
		.join('|');
	return new RegExp(`(${emojiRegex})`, 'g');
})();
const EMOJI_SHORTCUT_RE = (() => {
	const emojiRegex = Object.keys(shortcutToName).map(RegexUtils.escapeRegex).join('|');
	return new RegExp(`^(${emojiRegex})`);
})();

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
			return i18n._(msg`People`);
		case 'nature':
			return i18n._(msg`Nature`);
		case 'food':
			return i18n._(msg`Food & Drink`);
		case 'activity':
			return i18n._(msg`Activities`);
		case 'travel':
			return i18n._(msg`Travel & Places`);
		case 'objects':
			return i18n._(msg`Objects`);
		case 'symbols':
			return i18n._(msg`Symbols`);
		case 'flags':
			return i18n._(msg`Flags`);
		default:
			return category;
	}
};

export default {
	getDefaultSkinTone: (): string => defaultSkinTone,
	setDefaultSkinTone: (skinTone: string): void => {
		defaultSkinTone = skinTone || '';
	},
	getCategories: (): ReadonlyArray<string> => Object.freeze(Object.keys(emojisByCategory)),
	getByName: (emojiName: string): UnicodeEmoji | null => nameToEmoji[emojiName] || null,
	getByCategory: (emojiCategory: string): ReadonlyArray<UnicodeEmoji> | null => emojisByCategory[emojiCategory] || null,
	translateInlineEmojiToSurrogates: (content: string): string => {
		return content.replace(EMOJI_NAME_AND_DIVERSITY_RE, (original, emoji) => nameToSurrogate[emoji] || original);
	},
	translateSurrogatesToInlineEmoji: (content: string): string => {
		return content.replace(EMOJI_SURROGATE_RE, (_, surrogate) => {
			const name = surrogateToName[surrogate];
			return name ? `:${name}:` : surrogate;
		});
	},
	convertNameToSurrogate: (emojiName: string, defaultSurrogate = ''): string => {
		return nameToSurrogate[emojiName] || defaultSurrogate;
	},
	convertSurrogateToName: (surrogate: string, includeColons = true, defaultName = ''): string => {
		const name = surrogateToName[surrogate] || defaultName;
		return includeColons && name ? `:${name}:` : name;
	},
	convertShortcutToName: (shortcut: string, includeColons = true, defaultName = ''): string => {
		const name = shortcutToName[shortcut] || defaultName;
		return includeColons && name ? `:${name}:` : name;
	},
	forEachEmoji: (callback: (emoji: UnicodeEmoji) => void): void => {
		emojis.forEach(callback);
	},
	all: (): ReadonlyArray<UnicodeEmoji> => emojis,
	getCategoryForEmoji: (emoji: UnicodeEmoji): string | null => {
		for (const [category, categoryEmojis] of Object.entries(emojisByCategory)) {
			if (categoryEmojis.some((e) => e.uniqueName === emoji.uniqueName)) {
				return category;
			}
		}
		return null;
	},
	getCategoryIcon: (category: string): React.ComponentType<{className?: string}> => {
		return categoryIcons[category as keyof typeof categoryIcons] || SmileyIcon;
	},
	getCategoryLabel,
	getSurrogateName: (surrogate: string): string | null => {
		return surrogateToName[surrogate] || null;
	},
	findEmojiByName: (emojiName: string): UnicodeEmoji | null => {
		return nameToEmoji[emojiName] || null;
	},
	findEmojiWithSkinTone: (baseName: string, skinToneSurrogate: string): UnicodeEmoji | null => {
		const skinToneName = surrogateToName[skinToneSurrogate];
		if (!skinToneName) return null;

		const skinToneEmojiName = `${baseName}::${skinToneName}`;
		return nameToEmoji[skinToneEmojiName] || null;
	},
	numDiversitySprites,
	numNonDiversitySprites,
	EMOJI_NAME_RE,
	EMOJI_NAME_AND_DIVERSITY_RE,
	EMOJI_SHORTCUT_RE,
	EMOJI_SPRITES,
};
