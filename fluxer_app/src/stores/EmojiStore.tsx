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

import {i18n} from '@lingui/core';
import {makeAutoObservable} from 'mobx';
import {makePersistent} from '~/lib/MobXPersistence';
import type {UnicodeEmoji} from '~/lib/UnicodeEmojis';
import UnicodeEmojis from '~/lib/UnicodeEmojis';
import type {ChannelRecord} from '~/records/ChannelRecord';
import {type GuildEmoji, GuildEmojiRecord} from '~/records/GuildEmojiRecord';
import type {GuildMember} from '~/records/GuildMemberRecord';
import type {Guild, GuildReadyData} from '~/records/GuildRecord';
import EmojiPickerStore from '~/stores/EmojiPickerStore';
import {patchGuildEmojiCacheFromGateway} from '~/stores/GuildExpressionTabCache';
import GuildListStore from '~/stores/GuildListStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import UserStore from '~/stores/UserStore';
import {filterEmojisForAutocomplete} from '~/utils/ExpressionPermissionUtils';
import * as RegexUtils from '~/utils/RegexUtils';
import {sortBySnowflakeDesc} from '~/utils/SnowflakeUtils';

export type Emoji = Readonly<
	Partial<GuildEmojiRecord> &
		Partial<UnicodeEmoji> & {
			name: string;
			allNamesString: string;
			uniqueName: string;
			useSpriteSheet?: boolean;
			index?: number;
			diversityIndex?: number;
			hasDiversity?: boolean;
		}
>;

type GuildEmojiContext = Readonly<{
	emojis: ReadonlyArray<GuildEmojiRecord>;
	usableEmojis: ReadonlyArray<GuildEmojiRecord>;
}>;

class EmojiDisambiguations {
	private static _lastInstance: EmojiDisambiguations | null = null;
	private readonly guildId: string | null;
	private disambiguatedEmoji: ReadonlyArray<Emoji> | null = null;
	private customEmojis: ReadonlyMap<string, Emoji> | null = null;
	private emojisByName: ReadonlyMap<string, Emoji> | null = null;
	private emojisById: ReadonlyMap<string, Emoji> | null = null;

	private constructor(guildId?: string | null) {
		this.guildId = guildId ?? null;
	}

	static getInstance(guildId?: string | null): EmojiDisambiguations {
		if (!EmojiDisambiguations._lastInstance || EmojiDisambiguations._lastInstance.guildId !== guildId) {
			EmojiDisambiguations._lastInstance = new EmojiDisambiguations(guildId);
		}
		return EmojiDisambiguations._lastInstance;
	}

	static reset(): void {
		EmojiDisambiguations._lastInstance = null;
	}

	static clear(guildId?: string | null): void {
		if (EmojiDisambiguations._lastInstance?.guildId === guildId) {
			EmojiDisambiguations._lastInstance = null;
		}
	}

	getDisambiguatedEmoji(): ReadonlyArray<Emoji> {
		this.ensureDisambiguated();
		return this.disambiguatedEmoji ?? [];
	}

	getCustomEmoji(): ReadonlyMap<string, Emoji> {
		this.ensureDisambiguated();
		return this.customEmojis ?? new Map();
	}

	getByName(disambiguatedEmojiName: string): Emoji | undefined {
		this.ensureDisambiguated();
		return this.emojisByName?.get(disambiguatedEmojiName);
	}

	getById(emojiId: string): Emoji | undefined {
		this.ensureDisambiguated();
		return this.emojisById?.get(emojiId);
	}

	nameMatchesChain(testName: (name: string) => boolean): ReadonlyArray<Emoji> {
		return this.getDisambiguatedEmoji().filter(({names, name}) => (names ? names.some(testName) : testName(name)));
	}

	private ensureDisambiguated(): void {
		if (!this.disambiguatedEmoji) {
			const result = this.buildDisambiguatedCustomEmoji();
			this.disambiguatedEmoji = result.disambiguatedEmoji;
			this.customEmojis = result.customEmojis;
			this.emojisByName = result.emojisByName;
			this.emojisById = result.emojisById;
		}
	}

	private buildDisambiguatedCustomEmoji() {
		const emojiCountByName = new Map<string, number>();
		const disambiguatedEmoji: Array<Emoji> = [];
		const customEmojis = new Map<string, Emoji>();
		const emojisByName = new Map<string, Emoji>();
		const emojisById = new Map<string, Emoji>();

		const disambiguateEmoji = (emoji: Emoji): void => {
			const uniqueName = emoji.name;
			const existingCount = emojiCountByName.get(uniqueName) ?? 0;
			emojiCountByName.set(uniqueName, existingCount + 1);

			const finalEmoji =
				existingCount > 0
					? {
							...emoji,
							name: `${uniqueName}~${existingCount}`,
							uniqueName,
							allNamesString: `:${uniqueName}~${existingCount}:`,
						}
					: emoji;

			emojisByName.set(finalEmoji.name, finalEmoji);
			if (finalEmoji.id) {
				emojisById.set(finalEmoji.id, finalEmoji);
				customEmojis.set(finalEmoji.name, finalEmoji);
			}
			disambiguatedEmoji.push(finalEmoji);
		};

		UnicodeEmojis.forEachEmoji((unicodeEmoji) => {
			const compatibleEmoji: Emoji = {
				...unicodeEmoji,
				name: unicodeEmoji.uniqueName,
				url: unicodeEmoji.url || undefined,
				useSpriteSheet: unicodeEmoji.useSpriteSheet,
				index: unicodeEmoji.index,
				diversityIndex: unicodeEmoji.diversityIndex,
				hasDiversity: unicodeEmoji.hasDiversity,
			};
			disambiguateEmoji(compatibleEmoji);
		});

		const processGuildEmojis = (guildId: string) => {
			const guildEmoji = emojiGuildRegistry.get(guildId);
			if (!guildEmoji) return;
			guildEmoji.usableEmojis.forEach((emoji) => {
				const emojiForDisambiguation: Emoji = {
					...emoji,
					name: emoji.name,
					uniqueName: emoji.name,
					allNamesString: emoji.allNamesString,
					url: emoji.url,
					useSpriteSheet: false,
				};
				disambiguateEmoji(emojiForDisambiguation);
			});
		};

		if (this.guildId) {
			processGuildEmojis(this.guildId);
		}

		for (const guild of GuildListStore.guilds.filter((guild) => guild.id !== this.guildId)) {
			processGuildEmojis(guild.id);
		}

		return {
			disambiguatedEmoji: Object.freeze(disambiguatedEmoji),
			customEmojis: new Map(customEmojis),
			emojisByName: new Map(emojisByName),
			emojisById: new Map(emojisById),
		};
	}
}

class EmojiGuildRegistry {
	private guilds = new Map<string, GuildEmojiContext>();
	private customEmojisById = new Map<string, GuildEmojiRecord>();

	reset(): void {
		this.guilds.clear();
		this.customEmojisById.clear();
		EmojiDisambiguations.reset();
	}

	deleteGuild(guildId: string): void {
		this.guilds.delete(guildId);
	}

	get(guildId: string): GuildEmojiContext | undefined {
		return this.guilds.get(guildId);
	}

	rebuildRegistry(): void {
		this.customEmojisById.clear();
		for (const guild of this.guilds.values()) {
			for (const emoji of guild.usableEmojis) {
				this.customEmojisById.set(emoji.id, emoji);
			}
		}
		EmojiDisambiguations.reset();
	}

	updateGuild(guildId: string, guildEmojis?: ReadonlyArray<GuildEmoji>): void {
		this.deleteGuild(guildId);
		EmojiDisambiguations.clear(guildId);

		if (!guildEmojis) return;

		const currentUser = UserStore.getCurrentUser();
		if (!currentUser) return;

		const localUser = GuildMemberStore.getMember(guildId, currentUser.id);
		if (!localUser) return;

		const emojiRecords = guildEmojis.map((emoji) => new GuildEmojiRecord(guildId, emoji));
		const sortedEmojis = sortBySnowflakeDesc(emojiRecords);
		const frozenEmojis = Object.freeze(sortedEmojis);

		this.guilds.set(guildId, {
			emojis: frozenEmojis,
			usableEmojis: frozenEmojis,
		});
	}

	getGuildEmojis(guildId: string): ReadonlyArray<GuildEmojiRecord> {
		return this.guilds.get(guildId)?.usableEmojis ?? [];
	}
}

const emojiGuildRegistry = new EmojiGuildRegistry();

class EmojiStore {
	skinTone = '';

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'EmojiStore', ['skinTone']);
		UnicodeEmojis.setDefaultSkinTone(this.skinTone);
	}

	get categories(): ReadonlyArray<string> {
		return Object.freeze(['custom', ...UnicodeEmojis.getCategories()]);
	}

	getGuildEmoji(guildId: string): ReadonlyArray<GuildEmojiRecord> {
		return emojiGuildRegistry.getGuildEmojis(guildId);
	}

	getEmojiById(emojiId: string): Emoji | undefined {
		return this.getDisambiguatedEmojiContext(null).getById(emojiId);
	}

	getDisambiguatedEmojiContext(guildId?: string | null): EmojiDisambiguations {
		return EmojiDisambiguations.getInstance(guildId);
	}

	getEmojiMarkdown(emoji: Emoji): string {
		return emoji.id ? `<${emoji.animated ? 'a' : ''}:${emoji.uniqueName}:${emoji.id}>` : `:${emoji.uniqueName}:`;
	}

	filterExternal(
		channel: ChannelRecord | null,
		nameTest: (name: string) => boolean,
		count: number,
	): ReadonlyArray<Emoji> {
		const results = EmojiDisambiguations.getInstance(channel?.guildId).nameMatchesChain(nameTest);

		const filtered = filterEmojisForAutocomplete(i18n, results, channel);

		return count > 0 ? filtered.slice(0, count) : filtered;
	}

	getAllEmojis(channel: ChannelRecord | null): ReadonlyArray<Emoji> {
		return this.getDisambiguatedEmojiContext(channel?.guildId).getDisambiguatedEmoji();
	}

	search(channel: ChannelRecord | null, query: string, count = 0): ReadonlyArray<Emoji> {
		const lowerCasedQuery = query.toLowerCase();
		if (!lowerCasedQuery) {
			const allEmojis = this.getAllEmojis(channel);
			const sorted = [...allEmojis].sort(
				(a, b) => EmojiPickerStore.getFrecencyScoreForEmoji(b) - EmojiPickerStore.getFrecencyScoreForEmoji(a),
			);
			return count > 0 ? sorted.slice(0, count) : sorted;
		}

		const escapedQuery = RegexUtils.escapeRegex(lowerCasedQuery);

		const containsRegex = new RegExp(escapedQuery, 'i');
		const startsWithRegex = new RegExp(`^${escapedQuery}`, 'i');
		const boundaryRegex = new RegExp(`(^|_|[A-Z])${escapedQuery}s?([A-Z]|_|$)`);

		const searchResults = this.filterExternal(channel, containsRegex.test.bind(containsRegex), 0);

		if (searchResults.length === 0) return searchResults;

		const getScore = (name: string): number => {
			const nameLower = name.toLowerCase();
			return (
				1 +
				(nameLower === lowerCasedQuery ? 4 : 0) +
				(boundaryRegex.test(nameLower) || boundaryRegex.test(name) ? 2 : 0) +
				(startsWithRegex.test(name) ? 1 : 0)
			);
		};

		const sortedResults = [...searchResults].sort((a, b) => {
			const frecencyDiff = EmojiPickerStore.getFrecencyScoreForEmoji(b) - EmojiPickerStore.getFrecencyScoreForEmoji(a);

			if (frecencyDiff !== 0) {
				return frecencyDiff;
			}

			const aName = a.names?.[0] ?? a.name;
			const bName = b.names?.[0] ?? b.name;
			const scoreDiff = getScore(bName) - getScore(aName);
			return scoreDiff || aName.localeCompare(bName);
		});

		return count > 0 ? sortedResults.slice(0, count) : sortedResults;
	}

	setSkinTone(skinTone: string): void {
		this.skinTone = skinTone;
		UnicodeEmojis.setDefaultSkinTone(skinTone);
	}

	handleConnectionOpen({guilds}: {guilds: ReadonlyArray<GuildReadyData>}): void {
		emojiGuildRegistry.reset();
		for (const guild of guilds) {
			emojiGuildRegistry.updateGuild(guild.id, guild.emojis);
		}
		emojiGuildRegistry.rebuildRegistry();
	}

	handleGuildUpdate({guild}: {guild: Guild | GuildReadyData}): void {
		emojiGuildRegistry.updateGuild(guild.id, 'emojis' in guild ? guild.emojis : undefined);
		emojiGuildRegistry.rebuildRegistry();
	}

	handleGuildEmojiUpdated({guildId, emojis}: {guildId: string; emojis: ReadonlyArray<GuildEmoji>}): void {
		emojiGuildRegistry.updateGuild(guildId, emojis);
		emojiGuildRegistry.rebuildRegistry();
		patchGuildEmojiCacheFromGateway(guildId, emojis);
	}

	handleGuildDelete({guildId}: {guildId: string}): void {
		emojiGuildRegistry.deleteGuild(guildId);
		emojiGuildRegistry.rebuildRegistry();
	}

	handleGuildMemberUpdate({guildId, member}: {guildId: string; member: GuildMember}): void {
		if (member.user.id !== UserStore.getCurrentUser()?.id) {
			return;
		}

		const currentGuildEmojis = emojiGuildRegistry.getGuildEmojis(guildId).map((emoji) => ({
			...emoji.toJSON(),
			guild_id: guildId,
		}));

		emojiGuildRegistry.updateGuild(guildId, currentGuildEmojis);
		emojiGuildRegistry.rebuildRegistry();
	}
}

export default new EmojiStore();
