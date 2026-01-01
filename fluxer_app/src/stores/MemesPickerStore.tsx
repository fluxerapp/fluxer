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
import type {FavoriteMemeRecord} from '~/records/FavoriteMemeRecord';

type MemeUsageEntry = Readonly<{
	count: number;
	lastUsed: number;
}>;

const MAX_FRECENT_MEMES = 21;
const FRECENCY_TIME_DECAY_HOURS = 24 * 7;

const logger = new Logger('MemesPickerStore');

class MemesPickerStore {
	memeUsage: Record<string, MemeUsageEntry> = {};
	favoriteMemes: Array<string> = [];
	collapsedCategories: Array<string> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'MemesPickerStore', ['memeUsage', 'favoriteMemes', 'collapsedCategories']);
	}

	trackMemeUsage(memeKey: string): void {
		const now = Date.now();
		const currentUsage = this.memeUsage[memeKey];
		const newCount = (currentUsage?.count ?? 0) + 1;

		this.memeUsage[memeKey] = {
			count: newCount,
			lastUsed: now,
		};
	}

	toggleFavorite(memeKey: string): void {
		if (this.favoriteMemes.includes(memeKey)) {
			const index = this.favoriteMemes.indexOf(memeKey);
			if (index > -1) {
				this.favoriteMemes.splice(index, 1);
			}
		} else {
			this.favoriteMemes.push(memeKey);
		}

		ComponentDispatch.dispatch('MEMES_PICKER_RERENDER');
		logger.debug(`Toggled favorite meme: ${memeKey}`);
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

		ComponentDispatch.dispatch('MEMES_PICKER_RERENDER');
		logger.debug(`Toggled category: ${category}`);
	}

	isFavorite(meme: FavoriteMemeRecord): boolean {
		return this.favoriteMemes.includes(this.getMemeKey(meme));
	}

	isCategoryCollapsed(categoryId: string): boolean {
		return this.collapsedCategories.includes(categoryId);
	}

	private getFrecencyScore(entry: MemeUsageEntry): number {
		const now = Date.now();
		const hoursSinceLastUse = (now - entry.lastUsed) / (1000 * 60 * 60);
		const timeDecay = Math.max(0, 1 - hoursSinceLastUse / FRECENCY_TIME_DECAY_HOURS);
		return entry.count * (1 + timeDecay);
	}

	getFrecentMemes(
		allMemes: ReadonlyArray<FavoriteMemeRecord>,
		limit: number = MAX_FRECENT_MEMES,
	): Array<FavoriteMemeRecord> {
		const memeScores: Array<{meme: FavoriteMemeRecord; score: number}> = [];

		for (const meme of allMemes) {
			const memeKey = this.getMemeKey(meme);
			const usage = this.memeUsage[memeKey];

			if (usage) {
				const score = this.getFrecencyScore(usage);
				memeScores.push({meme, score});
			}
		}

		memeScores.sort((a, b) => b.score - a.score);
		return memeScores.slice(0, limit).map((item) => item.meme);
	}

	getFavoriteMemes(allMemes: ReadonlyArray<FavoriteMemeRecord>): Array<FavoriteMemeRecord> {
		const favorites: Array<FavoriteMemeRecord> = [];

		for (const meme of allMemes) {
			if (this.isFavorite(meme)) {
				favorites.push(meme);
			}
		}

		return favorites;
	}

	getFrecencyScoreForMeme(meme: FavoriteMemeRecord): number {
		const usage = this.memeUsage[this.getMemeKey(meme)];
		return usage ? this.getFrecencyScore(usage) : 0;
	}

	private getMemeKey(meme: FavoriteMemeRecord): string {
		return meme.id;
	}
}

export default new MemesPickerStore();
