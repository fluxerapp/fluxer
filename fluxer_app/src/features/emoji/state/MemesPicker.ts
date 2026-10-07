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
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {MemesPickerStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/pickers_pb';
import {makeAutoObservable} from 'mobx';

type MemeUsageEntry = UsageEntry;

const MAX_FRECENT_MEMES = 21;
const MAX_FAVORITE_MEMES = 500;
const MAX_COLLAPSED_MEME_CATEGORIES = 200;
const USAGE_SYNC_DEBOUNCE_MS = 1_500;

class MemesPicker {
	memeUsage: Record<string, MemeUsageEntry> = {};
	favoriteMemes: Array<string> = [];
	collapsedCategories: Array<string> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
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

	getFrecencyScoreForMeme(meme: FavoriteMeme): number {
		const usage = this.memeUsage[this.getMemeKey(meme)];
		return usage ? this.getFrecencyScore(usage) : 0;
	}

	private getMemeKey(meme: FavoriteMeme): string {
		return meme.id;
	}
}

export default new MemesPicker();
