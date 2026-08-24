// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	bumpUsageEntry,
	dedupeBoundedIds,
	MAX_TRACKED_USAGE_KEYS,
	mergeWireUsageMaps,
	sanitizeUsageMap,
	type UsageEntry,
	usageEntryFromWire,
	usageEntryToWire,
	usageFrecencyScore,
} from '@app/features/emoji/state/UsageFrecency';
import type {FavoriteMeme} from '@app/features/expressions/models/FavoriteMeme';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {MemesPickerStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/pickers_pb';
import {makeAutoObservable} from 'mobx';

type MemeUsageEntry = UsageEntry;

const MAX_FRECENT_MEMES = 21;
const MAX_FAVORITE_MEMES = 500;
const MAX_COLLAPSED_MEME_CATEGORIES = 200;
const USAGE_SYNC_DEBOUNCE_MS = 1_500;
const logger = new Logger('MemesPicker');

class MemesPicker {
	memeUsage: Record<string, MemeUsageEntry> = {};
	favoriteMemes: Array<string> = [];
	collapsedCategories: Array<string> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'memesPicker',
			schema: MemesPickerStateSchema,
			persist: ['memeUsage', 'favoriteMemes', 'collapsedCategories'],
			debounceMs: USAGE_SYNC_DEBOUNCE_MS,
			toMessage: (s) => ({
				usage: Object.fromEntries(Object.entries(s.memeUsage).map(([key, entry]) => [key, usageEntryToWire(entry)])),
				favoriteMemeIds: [...s.favoriteMemes],
				collapsedCategoryIds: [...s.collapsedCategories],
			}),
			applyMessage: (s, m) => {
				const usage: Record<string, {count: number; lastUsed: number}> = {};
				for (const [key, stat] of Object.entries(m.usage)) {
					usage[key] = usageEntryFromWire(stat);
				}
				s.memeUsage = sanitizeUsageMap(usage, Date.now());
				s.favoriteMemes = dedupeBoundedIds(m.favoriteMemeIds, MAX_FAVORITE_MEMES);
				s.collapsedCategories = dedupeBoundedIds(m.collapsedCategoryIds, MAX_COLLAPSED_MEME_CATEGORIES);
			},
			mergeRemote: (local, incoming) => ({
				usage: mergeWireUsageMaps(local.usage, incoming.usage, Date.now()),
				favoriteMemeIds: [...incoming.favoriteMemeIds],
				collapsedCategoryIds: [...incoming.collapsedCategoryIds],
			}),
		});
	}

	trackMemeUsage(memeKey: string): void {
		const now = Date.now();
		this.memeUsage[memeKey] = bumpUsageEntry(this.memeUsage[memeKey], now);
		if (Object.keys(this.memeUsage).length > MAX_TRACKED_USAGE_KEYS) {
			this.memeUsage = sanitizeUsageMap(this.memeUsage, now);
		}
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
		ComponentBus.dispatch('MEMES_PICKER_RERENDER');
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
		ComponentBus.dispatch('MEMES_PICKER_RERENDER');
		logger.debug(`Toggled category: ${category}`);
	}

	isFavorite(meme: FavoriteMeme): boolean {
		return this.favoriteMemes.includes(this.getMemeKey(meme));
	}

	isCategoryCollapsed(categoryId: string): boolean {
		return this.collapsedCategories.includes(categoryId);
	}

	private getFrecencyScore(entry: MemeUsageEntry): number {
		return usageFrecencyScore(entry, Date.now());
	}

	getFrecentMemes(allMemes: ReadonlyArray<FavoriteMeme>, limit: number = MAX_FRECENT_MEMES): Array<FavoriteMeme> {
		const memeScores: Array<{
			meme: FavoriteMeme;
			score: number;
		}> = [];
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

	getFavoriteMemes(allMemes: ReadonlyArray<FavoriteMeme>): Array<FavoriteMeme> {
		const favorites: Array<FavoriteMeme> = [];
		for (const meme of allMemes) {
			if (this.isFavorite(meme)) {
				favorites.push(meme);
			}
		}
		return favorites;
	}

	getFrecencyScoreForMeme(meme: FavoriteMeme): number {
		const usage = this.memeUsage[this.getMemeKey(meme)];
		return usage ? this.getFrecencyScore(usage) : 0;
	}

	private getMemeKey(meme: FavoriteMeme): string {
		return meme.id;
	}
}

export default new MemesPicker();
