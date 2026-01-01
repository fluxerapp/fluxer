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
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';

type StickerUsageEntry = Readonly<{
	count: number;
	lastUsed: number;
}>;

const MAX_FRECENT_STICKERS = 21;
const FRECENCY_TIME_DECAY_HOURS = 24 * 7;

const logger = new Logger('StickerPickerStore');

class StickerPickerStore {
	stickerUsage: Record<string, StickerUsageEntry> = {};
	favoriteStickers: Array<string> = [];
	collapsedCategories: Array<string> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'StickerPickerStore', ['stickerUsage', 'favoriteStickers', 'collapsedCategories']);
	}

	trackStickerUsage(stickerKey: string): void {
		const now = Date.now();
		const currentUsage = this.stickerUsage[stickerKey];
		const newCount = (currentUsage?.count ?? 0) + 1;

		this.stickerUsage[stickerKey] = {
			count: newCount,
			lastUsed: now,
		};
	}

	toggleFavorite(stickerKey: string): void {
		if (this.favoriteStickers.includes(stickerKey)) {
			const index = this.favoriteStickers.indexOf(stickerKey);
			if (index > -1) {
				this.favoriteStickers.splice(index, 1);
			}
		} else {
			this.favoriteStickers.push(stickerKey);
		}

		ComponentDispatch.dispatch('STICKER_PICKER_RERENDER');
		logger.debug(`Toggled favorite sticker: ${stickerKey}`);
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

		ComponentDispatch.dispatch('STICKER_PICKER_RERENDER');
		logger.debug(`Toggled category: ${category}`);
	}

	isFavorite(sticker: GuildStickerRecord): boolean {
		return this.favoriteStickers.includes(this.getStickerKey(sticker));
	}

	isCategoryCollapsed(categoryId: string): boolean {
		return this.collapsedCategories.includes(categoryId);
	}

	private getFrecencyScore(entry: StickerUsageEntry): number {
		const now = Date.now();
		const hoursSinceLastUse = (now - entry.lastUsed) / (1000 * 60 * 60);
		const timeDecay = Math.max(0, 1 - hoursSinceLastUse / FRECENCY_TIME_DECAY_HOURS);
		return entry.count * (1 + timeDecay);
	}

	getFrecentStickers(
		allStickers: ReadonlyArray<GuildStickerRecord>,
		limit: number = MAX_FRECENT_STICKERS,
	): Array<GuildStickerRecord> {
		const stickerScores: Array<{sticker: GuildStickerRecord; score: number}> = [];

		for (const sticker of allStickers) {
			const stickerKey = this.getStickerKey(sticker);
			const usage = this.stickerUsage[stickerKey];

			if (usage) {
				const score = this.getFrecencyScore(usage);
				stickerScores.push({sticker, score});
			}
		}

		stickerScores.sort((a, b) => b.score - a.score);
		const result = stickerScores.slice(0, limit).map((item) => item.sticker);
		return result;
	}

	getFavoriteStickers(allStickers: ReadonlyArray<GuildStickerRecord>): Array<GuildStickerRecord> {
		const favorites: Array<GuildStickerRecord> = [];

		for (const sticker of allStickers) {
			if (this.isFavorite(sticker)) {
				favorites.push(sticker);
			}
		}

		return favorites;
	}

	getFrecencyScoreForSticker(sticker: GuildStickerRecord): number {
		const usage = this.stickerUsage[this.getStickerKey(sticker)];
		return usage ? this.getFrecencyScore(usage) : 0;
	}

	private getStickerKey(sticker: GuildStickerRecord): string {
		return `${sticker.guildId}:${sticker.id}`;
	}
}

export default new StickerPickerStore();
