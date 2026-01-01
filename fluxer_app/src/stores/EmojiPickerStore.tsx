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

import {makeAutoObservable} from 'mobx';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import {Logger} from '~/lib/Logger';
import {makePersistent} from '~/lib/MobXPersistence';
import type {Emoji} from '~/stores/EmojiStore';

const logger = new Logger('EmojiPickerStore');

type EmojiUsageEntry = Readonly<{
	count: number;
	lastUsed: number;
}>;

const MAX_FRECENT_EMOJIS = 42;
const FRECENCY_TIME_DECAY_HOURS = 24 * 7;

const DEFAULT_QUICK_EMOJIS = [
	{name: 'thumbsup', uniqueName: 'thumbsup'},
	{name: 'ok_hand', uniqueName: 'ok_hand'},
	{name: 'tada', uniqueName: 'tada'},
];

class EmojiPickerStore {
	emojiUsage: Record<string, EmojiUsageEntry> = {};
	favoriteEmojis: Array<string> = [];
	collapsedCategories: Array<string> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'EmojiPickerStore', ['emojiUsage', 'favoriteEmojis', 'collapsedCategories']);
	}

	trackEmojiUsage(emojiKey: string): void {
		const now = Date.now();
		const currentUsage = this.emojiUsage[emojiKey];
		const newCount = (currentUsage?.count ?? 0) + 1;

		this.emojiUsage[emojiKey] = {
			count: newCount,
			lastUsed: now,
		};

		logger.debug(`Tracked emoji usage: ${emojiKey}`);
	}

	toggleFavorite(emojiKey: string): void {
		if (this.favoriteEmojis.includes(emojiKey)) {
			const index = this.favoriteEmojis.indexOf(emojiKey);
			if (index > -1) {
				this.favoriteEmojis.splice(index, 1);
			}
		} else {
			this.favoriteEmojis.push(emojiKey);
		}

		ComponentDispatch.dispatch('EMOJI_PICKER_RERENDER');
		logger.debug(`Toggled favorite emoji: ${emojiKey}`);
	}

	toggleCategory(category: string): void {
		if (this.collapsedCategories.includes(category)) {
			const index = this.collapsedCategories.indexOf(category);
			if (index > -1) {
				this.collapsedCategories.splice(index, 1);
			}
		} else {
			this.collapsedCategories.push(category);
		}

		ComponentDispatch.dispatch('EMOJI_PICKER_RERENDER');
		logger.debug(`Toggled category: ${category}`);
	}

	isFavorite(emoji: Emoji): boolean {
		return this.favoriteEmojis.includes(this.getEmojiKey(emoji));
	}

	isCategoryCollapsed(categoryId: string): boolean {
		return this.collapsedCategories.includes(categoryId);
	}

	private getFrecencyScore(entry: EmojiUsageEntry): number {
		const now = Date.now();
		const hoursSinceLastUse = (now - entry.lastUsed) / (1000 * 60 * 60);
		const timeDecay = Math.max(0, 1 - hoursSinceLastUse / FRECENCY_TIME_DECAY_HOURS);
		return entry.count * (1 + timeDecay);
	}

	getFrecentEmojis(allEmojis: ReadonlyArray<Emoji>, limit: number = MAX_FRECENT_EMOJIS): Array<Emoji> {
		const emojiScores: Array<{emoji: Emoji; score: number}> = [];

		for (const emoji of allEmojis) {
			const emojiKey = this.getEmojiKey(emoji);
			const usage = this.emojiUsage[emojiKey];

			if (usage) {
				const score = this.getFrecencyScore(usage);
				emojiScores.push({emoji, score});
			}
		}

		emojiScores.sort((a, b) => b.score - a.score);
		return emojiScores.slice(0, limit).map((item) => item.emoji);
	}

	getFavoriteEmojis(allEmojis: ReadonlyArray<Emoji>): Array<Emoji> {
		const favorites: Array<Emoji> = [];

		for (const emoji of allEmojis) {
			if (this.isFavorite(emoji)) {
				favorites.push(emoji);
			}
		}

		return favorites;
	}

	getFrecencyScoreForEmoji(emoji: Emoji): number {
		const usage = this.emojiUsage[this.getEmojiKey(emoji)];
		return usage ? this.getFrecencyScore(usage) : 0;
	}

	getQuickReactionEmojis(allEmojis: ReadonlyArray<Emoji>, count: number): Array<Emoji> {
		const frecent = this.getFrecentEmojis(allEmojis, count);

		if (frecent.length >= count) {
			return frecent.slice(0, count);
		}

		const result = [...frecent];
		const needed = count - frecent.length;

		for (let i = 0; i < needed && i < DEFAULT_QUICK_EMOJIS.length; i++) {
			const defaultEmoji = DEFAULT_QUICK_EMOJIS[i];
			const found = allEmojis.find((e) => e.uniqueName === defaultEmoji.uniqueName);
			if (found && !result.some((r) => this.getEmojiKey(r) === this.getEmojiKey(found))) {
				result.push(found);
			}
		}

		return result.slice(0, count);
	}

	private getEmojiKey(emoji: Emoji): string {
		if (emoji.id) {
			return `custom:${emoji.guildId}:${emoji.id}`;
		}
		return `unicode:${emoji.uniqueName}`;
	}

	trackEmoji(emoji: Emoji): void {
		this.trackEmojiUsage(this.getEmojiKey(emoji));
	}
}

export default new EmojiPickerStore();
