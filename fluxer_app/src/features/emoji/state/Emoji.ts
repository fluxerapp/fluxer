// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import EmojiPicker, {
	CUSTOM_EMOJI_USAGE_KEY_PREFIX,
	UNICODE_EMOJI_USAGE_KEY_PREFIX,
} from '@app/features/emoji/state/EmojiPicker';
import {createEmojiSearchIndex, type EmojiSearchIndex} from '@app/features/emoji/state/EmojiSearchIndex';
import type {FlatEmoji, UnicodeEmoji} from '@app/features/emoji/types/EmojiTypes';
import {GuildEmoji} from '@app/features/expressions/models/GuildEmoji';
import {patchGuildEmojiCacheFromGateway} from '@app/features/expressions/state/GuildExpressionTabCache';
import {checkEmojiAvailability} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import GuildList from '@app/features/guild/state/GuildList';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import type {GuildEmoji as WireGuildEmoji} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {EmojiStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/pickers_pb';
import {sortBySnowflakeDesc} from '@fluxer/snowflake/src/SnowflakeUtils';
import {i18n} from '@lingui/core';
import {makeAutoObservable} from 'mobx';

type GuildEmojiContext = Readonly<{
	emojis: ReadonlyArray<GuildEmoji>;
	usableEmojis: ReadonlyArray<GuildEmoji>;
}>;

const PUNCTUATION_QUERY_ALIASES: Record<string, string> = {
	'!': 'exclamation',
	'!!': 'bangbang',
	'!?': 'interrobang',
	'?': 'question',
	'+1': 'thumbsup',
	'-1': 'thumbsdown',
};

export function normalizeEmojiSearchQuery(query: string): string {
	const trimmed = query['trim']().replace(/^:+/, '').replace(/:+$/, '').replace(/ /g, '_');
	return PUNCTUATION_QUERY_ALIASES[trimmed] ?? trimmed;
}

const MAX_ALL_EMOJI_CONTEXT_CACHE_SIZE = 4;

function toFlatUnicodeEmoji(unicodeEmoji: UnicodeEmoji): FlatEmoji {
	return {
		...unicodeEmoji,
		name: unicodeEmoji.uniqueName,
		url: unicodeEmoji.url || undefined,
		useSpriteSheet: unicodeEmoji.useSpriteSheet,
		index: unicodeEmoji.index,
		skinToneIndex: unicodeEmoji.skinToneIndex,
		hasSkinTones: unicodeEmoji.hasSkinTones,
	};
}

const EMOJI_CATEGORIES: ReadonlyArray<string> = Object.freeze(['custom', ...UnicodeEmojis.getCategories()]);

type BaseUnicodeEmojiIndex = Readonly<{
	emojis: ReadonlyArray<FlatEmoji>;
	byUsageName: ReadonlyMap<string, FlatEmoji>;
	nameCounts: ReadonlyMap<string, number>;
}>;

let cachedBaseUnicodeEmojiIndex: BaseUnicodeEmojiIndex | null = null;

function getBaseUnicodeEmojiIndex(): BaseUnicodeEmojiIndex {
	if (!cachedBaseUnicodeEmojiIndex) {
		const emojis: ReadonlyArray<FlatEmoji> = Object.freeze(UnicodeEmojis.all().map(toFlatUnicodeEmoji));
		const byUsageName = new Map<string, FlatEmoji>();
		const nameCounts = new Map<string, number>();
		for (const emoji of emojis) {
			byUsageName.set(emoji.uniqueName, emoji);
			nameCounts.set(emoji.name, (nameCounts.get(emoji.name) ?? 0) + 1);
		}
		cachedBaseUnicodeEmojiIndex = {emojis, byUsageName, nameCounts};
	}
	return cachedBaseUnicodeEmojiIndex;
}

function parseCustomEmojiShortcodeName(emojiName: string): {baseName: string; disambiguationIndex: number | null} {
	const match = /^(.*)~([1-9]\d*)$/.exec(emojiName);
	if (!match) {
		return {baseName: emojiName, disambiguationIndex: null};
	}
	return {
		baseName: match[1],
		disambiguationIndex: Number(match[2]),
	};
}

type AllEmojiContextCacheEntry = {
	version: number;
	guilds: ReadonlyArray<{id: string}>;
	emojis: ReadonlyArray<FlatEmoji>;
	searchIndex?: EmojiSearchIndex<FlatEmoji>;
};

class EmojiGuildRegistry {
	private guilds = new Map<string, GuildEmojiContext>();
	private customEmojisById = new Map<string, GuildEmoji>();
	private customEmojisByLowerName = new Map<string, ReadonlyArray<GuildEmoji>>();
	private customEmojisByLowerNameByGuild = new Map<string, Map<string, ReadonlyArray<GuildEmoji>>>();
	private emojiIdsByGuild = new Map<string, Set<string>>();
	private allEmojiContextCache = new Map<string, AllEmojiContextCacheEntry>();
	private version = 0;

	reset(): void {
		this.guilds.clear();
		this.customEmojisById.clear();
		this.customEmojisByLowerName.clear();
		this.customEmojisByLowerNameByGuild.clear();
		this.emojiIdsByGuild.clear();
		this.invalidateCaches();
	}

	deleteGuild(guildId: string): void {
		const didDelete = this.deleteGuildIndexes(guildId);
		if (didDelete) {
			this.invalidateCaches();
		}
	}

	private deleteGuildIndexes(guildId: string): boolean {
		const didDeleteGuild = this.guilds.delete(guildId);
		const previousIds = this.emojiIdsByGuild.get(guildId);
		if (previousIds) {
			for (const emojiId of previousIds) {
				this.customEmojisById.delete(emojiId);
			}
			this.emojiIdsByGuild.delete(guildId);
		}
		const previousNames = this.customEmojisByLowerNameByGuild.get(guildId);
		if (previousNames) {
			for (const [lowerName] of previousNames) {
				const existing = this.customEmojisByLowerName.get(lowerName);
				if (!existing) continue;
				const next = existing.filter((emoji) => emoji.guildId !== guildId);
				if (next.length > 0) {
					this.customEmojisByLowerName.set(lowerName, Object.freeze(next));
				} else {
					this.customEmojisByLowerName.delete(lowerName);
				}
			}
			this.customEmojisByLowerNameByGuild.delete(guildId);
		}
		return didDeleteGuild || previousIds !== undefined || previousNames !== undefined;
	}

	get(guildId: string): GuildEmojiContext | undefined {
		return this.guilds.get(guildId);
	}

	rebuildRegistry(): void {
		this.customEmojisById.clear();
		this.customEmojisByLowerName.clear();
		this.customEmojisByLowerNameByGuild.clear();
		this.emojiIdsByGuild.clear();
		for (const [guildId, guild] of this.guilds.entries()) {
			this.indexGuildEmojis(guildId, guild.usableEmojis);
		}
		this.invalidateCaches();
	}

	updateGuild(guildId: string, guildEmojis?: ReadonlyArray<WireGuildEmoji>): void {
		const didDelete = this.deleteGuildIndexes(guildId);
		if (!guildEmojis) {
			if (didDelete) {
				this.invalidateCaches();
			}
			return;
		}
		const emojiRecords = guildEmojis.map((emoji) => new GuildEmoji(guildId, emoji));
		const sortedEmojis = sortBySnowflakeDesc(emojiRecords);
		const frozenEmojis = Object.freeze(sortedEmojis);
		this.guilds.set(guildId, {
			emojis: frozenEmojis,
			usableEmojis: frozenEmojis,
		});
		this.indexGuildEmojis(guildId, frozenEmojis);
		this.invalidateCaches();
	}

	private indexGuildEmojis(guildId: string, emojis: ReadonlyArray<GuildEmoji>): void {
		const ids = new Set<string>();
		const byLowerName = new Map<string, Array<GuildEmoji>>();
		for (const emoji of emojis) {
			this.customEmojisById.set(emoji.id, emoji);
			ids.add(emoji.id);
			const lowerName = emoji.name.toLowerCase();
			let nameEntries = byLowerName.get(lowerName);
			if (!nameEntries) {
				nameEntries = [];
				byLowerName.set(lowerName, nameEntries);
			}
			nameEntries.push(emoji);
		}
		this.emojiIdsByGuild.set(guildId, ids);
		const frozenByLowerName = new Map<string, ReadonlyArray<GuildEmoji>>();
		for (const [lowerName, guildEmojis] of byLowerName) {
			const frozenGuildEmojis = Object.freeze(guildEmojis);
			frozenByLowerName.set(lowerName, frozenGuildEmojis);
			const existing = this.customEmojisByLowerName.get(lowerName);
			this.customEmojisByLowerName.set(
				lowerName,
				Object.freeze(existing ? [...existing, ...guildEmojis] : [...guildEmojis]),
			);
		}
		this.customEmojisByLowerNameByGuild.set(guildId, frozenByLowerName);
	}

	private invalidateCaches(): void {
		this.version++;
		this.allEmojiContextCache.clear();
	}

	getGuildEmojis(guildId: string): ReadonlyArray<GuildEmoji> {
		return this.guilds.get(guildId)?.usableEmojis ?? [];
	}

	getCustomEmojiById(emojiId: string): GuildEmoji | undefined {
		return this.customEmojisById.get(emojiId);
	}

	getCustomEmojiByShortcodeName(guildId: string | null | undefined, emojiName: string): GuildEmoji | undefined {
		const lowerName = emojiName.toLowerCase();
		const {baseName, disambiguationIndex} = parseCustomEmojiShortcodeName(lowerName);
		const orderedMatches = this.getCustomEmojiNameMatches(guildId, baseName);
		if (orderedMatches.length === 0) return undefined;
		const unicodeNameCount = getBaseUnicodeEmojiIndex().nameCounts.get(baseName) ?? 0;
		const targetIndex = disambiguationIndex === null ? -unicodeNameCount : disambiguationIndex - unicodeNameCount;
		return targetIndex >= 0 ? orderedMatches[targetIndex] : undefined;
	}

	private getCustomEmojiNameMatches(guildId: string | null | undefined, lowerName: string): ReadonlyArray<GuildEmoji> {
		const allMatches = this.customEmojisByLowerName.get(lowerName);
		if (!allMatches || !guildId) {
			return allMatches ?? [];
		}
		const guildMatches = this.customEmojisByLowerNameByGuild.get(guildId)?.get(lowerName);
		if (!guildMatches) return allMatches;
		if (guildMatches.length === allMatches.length) return guildMatches;
		return [...guildMatches, ...allMatches.filter((emoji) => emoji.guildId !== guildId)];
	}

	getAllEmojis(guildId?: string | null): ReadonlyArray<FlatEmoji> {
		return this.getEmojiContext(guildId).emojis;
	}

	search(
		guildId: string | null | undefined,
		query: string,
		count: number,
		canUse: (emoji: FlatEmoji) => boolean,
	): ReadonlyArray<FlatEmoji> {
		const context = this.getEmojiContext(guildId);
		if (!query) {
			return count > 0 ? context.emojis.slice(0, count) : context.emojis;
		}
		context.searchIndex ??= createEmojiSearchIndex(context.emojis);
		return context.searchIndex.search(query, {
			count,
			canUse,
			getFrecencyScore: EmojiPicker.getFrecencyScoreForEmoji,
		});
	}

	private getEmojiContext(guildId?: string | null): AllEmojiContextCacheEntry {
		const cacheKey = guildId ?? '';
		const guilds = GuildList.guilds;
		const cached = this.allEmojiContextCache.get(cacheKey);
		if (cached && cached.version === this.version && cached.guilds === guilds) {
			return cached;
		}
		const emojis = this.buildAllEmojis(guildId ?? null, guilds);
		const context: AllEmojiContextCacheEntry = {
			version: this.version,
			guilds,
			emojis,
		};
		this.allEmojiContextCache.set(cacheKey, context);
		if (this.allEmojiContextCache.size > MAX_ALL_EMOJI_CONTEXT_CACHE_SIZE) {
			const firstKey = this.allEmojiContextCache.keys().next().value;
			if (firstKey !== undefined) this.allEmojiContextCache.delete(firstKey);
		}
		return context;
	}

	private buildAllEmojis(guildId: string | null, guilds: ReadonlyArray<{id: string}>): ReadonlyArray<FlatEmoji> {
		const baseUnicodeEmojiIndex = getBaseUnicodeEmojiIndex();
		if (this.guilds.size === 0) {
			return baseUnicodeEmojiIndex.emojis;
		}
		const emojiCountByName = new Map(baseUnicodeEmojiIndex.nameCounts);
		const result: Array<FlatEmoji> = [...baseUnicodeEmojiIndex.emojis];
		const addCustomEmoji = (emoji: GuildEmoji): void => {
			const uniqueName = emoji.name;
			const existingCount = emojiCountByName.get(uniqueName) ?? 0;
			emojiCountByName.set(uniqueName, existingCount + 1);
			if (existingCount === 0) {
				result.push(emoji);
				return;
			}
			result.push({
				...emoji,
				name: `${uniqueName}~${existingCount}`,
				uniqueName,
				allNamesString: `:${uniqueName}~${existingCount}:`,
				useSpriteSheet: false,
			});
		};
		const addGuildEmojis = (guildIdToAdd: string): void => {
			const guildEmoji = this.guilds.get(guildIdToAdd);
			if (!guildEmoji) return;
			for (const emoji of guildEmoji.usableEmojis) {
				addCustomEmoji(emoji);
			}
		};
		if (guildId) {
			addGuildEmojis(guildId);
		}
		for (const guild of guilds) {
			if (guild.id !== guildId) {
				addGuildEmojis(guild.id);
			}
		}
		return Object.freeze(result);
	}
}

const emojiGuildRegistry = new EmojiGuildRegistry();

class Emoji {
	skinTone = '';

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'emoji',
			schema: EmojiStateSchema,
			persist: ['skinTone'],
			toMessage: (s) => ({skinTone: s.skinTone}),
			applyMessage: (s, m) => {
				s.skinTone = m.skinTone;
			},
		});
		UnicodeEmojis.setDefaultSkinTone(this.skinTone);
	}

	get categories(): ReadonlyArray<string> {
		return EMOJI_CATEGORIES;
	}

	getGuildEmoji(guildId: string): ReadonlyArray<GuildEmoji> {
		return emojiGuildRegistry.getGuildEmojis(guildId);
	}

	getEmojiById(emojiId: string): FlatEmoji | undefined {
		return emojiGuildRegistry.getCustomEmojiById(emojiId);
	}

	getEmojiByUsageKey(emojiKey: string): FlatEmoji | undefined {
		if (emojiKey.startsWith(UNICODE_EMOJI_USAGE_KEY_PREFIX)) {
			const emojiName = emojiKey.slice(UNICODE_EMOJI_USAGE_KEY_PREFIX.length);
			return getBaseUnicodeEmojiIndex().byUsageName.get(emojiName);
		}
		if (emojiKey.startsWith(CUSTOM_EMOJI_USAGE_KEY_PREFIX)) {
			const lastSeparatorIndex = emojiKey.lastIndexOf(':');
			if (lastSeparatorIndex < CUSTOM_EMOJI_USAGE_KEY_PREFIX.length) {
				return undefined;
			}
			const emojiId = emojiKey.slice(lastSeparatorIndex + 1);
			return emojiGuildRegistry.getCustomEmojiById(emojiId);
		}
		return undefined;
	}

	findCustomEmojiForShortcode(
		channel: Channel | null,
		emojiName: string,
		guildIdFallback: string | null = null,
	): FlatEmoji | undefined {
		return emojiGuildRegistry.getCustomEmojiByShortcodeName(channel?.guildId ?? guildIdFallback, emojiName);
	}

	getEmojiMarkdown(emoji: FlatEmoji): string {
		if (emoji.id) {
			return `<${emoji.animated ? 'a' : ''}:${emoji.uniqueName}:${emoji.id}>`;
		}
		if (emoji.hasSkinTones && this.skinTone) {
			const skinToneName = UnicodeEmojis.nameForSurrogate(this.skinTone, false);
			if (skinToneName) {
				return `:${emoji.uniqueName}::${skinToneName}:`;
			}
		}
		return `:${emoji.uniqueName}:`;
	}

	getAllEmojis(channel: Channel | null): ReadonlyArray<FlatEmoji> {
		return emojiGuildRegistry.getAllEmojis(channel?.guildId);
	}

	getQuickReactionEmojis(channel: Channel | null, count: number): Array<FlatEmoji> {
		const result: Array<FlatEmoji> = [];
		const seenKeys = new Set<string>();
		const addEmoji = (emojiKey: string, emoji: FlatEmoji | undefined): void => {
			if (!emoji || seenKeys.has(emojiKey) || result.length >= count) {
				return;
			}
			if (!checkEmojiAvailability(i18n, emoji, channel).canUse) {
				return;
			}
			seenKeys.add(emojiKey);
			result.push(emoji);
		};
		for (const emojiKey of EmojiPicker.getFrecentEmojiKeys(0)) {
			addEmoji(emojiKey, this.getEmojiByUsageKey(emojiKey));
			if (result.length >= count) {
				return result;
			}
		}
		const needed = count - result.length;
		for (const emojiName of EmojiPicker.getDefaultQuickEmojiNames(needed)) {
			const emojiKey = `${UNICODE_EMOJI_USAGE_KEY_PREFIX}${emojiName}`;
			addEmoji(emojiKey, this.getEmojiByUsageKey(emojiKey));
		}
		return result;
	}

	search(channel: Channel | null, query: string, count = 0): ReadonlyArray<FlatEmoji> {
		const normalizedQuery = normalizeEmojiSearchQuery(query);
		return emojiGuildRegistry.search(
			channel?.guildId,
			normalizedQuery,
			count,
			(emoji) => checkEmojiAvailability(i18n, emoji, channel).canUse,
		);
	}

	setSkinTone(skinTone: string): void {
		this.skinTone = skinTone;
		UnicodeEmojis.setDefaultSkinTone(skinTone);
	}

	handleGatewayReady({guilds}: {guilds: ReadonlyArray<GuildReadyData>}): void {
		emojiGuildRegistry.reset();
		for (const guild of guilds) {
			emojiGuildRegistry.updateGuild(guild.id, guild.emojis);
		}
		ComponentBus.dispatch('EMOJI_PICKER_RERENDER');
	}

	handleGuildUpdate({guild}: {guild: GuildReadyData | WireGuild}): void {
		if (!('emojis' in guild)) {
			ComponentBus.dispatch('EMOJI_PICKER_RERENDER');
			return;
		}
		emojiGuildRegistry.updateGuild(guild.id, guild.emojis);
		ComponentBus.dispatch('EMOJI_PICKER_RERENDER');
	}

	handleGuildEmojiUpdated({guildId, emojis}: {guildId: string; emojis: ReadonlyArray<WireGuildEmoji>}): void {
		emojiGuildRegistry.updateGuild(guildId, emojis);
		patchGuildEmojiCacheFromGateway(guildId, emojis);
		ComponentBus.dispatch('EMOJI_PICKER_RERENDER');
	}

	handleGuildDelete({guildId}: {guildId: string}): void {
		emojiGuildRegistry.deleteGuild(guildId);
		ComponentBus.dispatch('EMOJI_PICKER_RERENDER');
	}
}

export default new Emoji();
