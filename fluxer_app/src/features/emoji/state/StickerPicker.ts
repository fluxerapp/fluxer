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
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {StickerPickerStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/pickers_pb';
import {makeAutoObservable} from 'mobx';

type StickerKeyInput = Readonly<Pick<GuildSticker, 'guildId' | 'id'>>;
type StickerUsageEntry = UsageEntry;

const MAX_FRECENT_STICKERS = 21;
const MAX_FAVORITE_STICKERS = 500;
const MAX_COLLAPSED_STICKER_CATEGORIES = 200;
const USAGE_SYNC_DEBOUNCE_MS = 1_500;
const logger = new Logger('StickerPicker');

class StickerPicker {
	stickerUsage: Record<string, StickerUsageEntry> = {};
	favoriteStickers: Array<string> = [];
	collapsedCategories: Array<string> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'stickerPicker',
			schema: StickerPickerStateSchema,
			persist: ['stickerUsage', 'favoriteStickers', 'collapsedCategories'],
			debounceMs: USAGE_SYNC_DEBOUNCE_MS,
			toMessage: (s) => ({
				usage: Object.fromEntries(Object.entries(s.stickerUsage).map(([key, entry]) => [key, usageEntryToWire(entry)])),
				favoriteStickerIds: [...s.favoriteStickers],
				collapsedCategoryIds: [...s.collapsedCategories],
			}),
			applyMessage: (s, m) => {
				const usage: Record<string, {count: number; lastUsed: number}> = {};
				for (const [key, stat] of Object.entries(m.usage)) {
					usage[key] = usageEntryFromWire(stat);
				}
				s.stickerUsage = sanitizeUsageMap(usage, Date.now());
				s.favoriteStickers = dedupeBoundedIds(m.favoriteStickerIds, MAX_FAVORITE_STICKERS);
				s.collapsedCategories = dedupeBoundedIds(m.collapsedCategoryIds, MAX_COLLAPSED_STICKER_CATEGORIES);
			},
			mergeRemote: (local, incoming) => ({
				usage: mergeWireUsageMaps(local.usage, incoming.usage, Date.now()),
				favoriteStickerIds: [...incoming.favoriteStickerIds],
				collapsedCategoryIds: [...incoming.collapsedCategoryIds],
			}),
		});
	}

	trackStickerUsage(stickerKey: string): void {
		const now = Date.now();
		this.stickerUsage[stickerKey] = bumpUsageEntry(this.stickerUsage[stickerKey], now);
		if (Object.keys(this.stickerUsage).length > MAX_TRACKED_USAGE_KEYS) {
			this.stickerUsage = sanitizeUsageMap(this.stickerUsage, now);
		}
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
		ComponentBus.dispatch('STICKER_PICKER_RERENDER');
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
		ComponentBus.dispatch('STICKER_PICKER_RERENDER');
		logger.debug(`Toggled category: ${category}`);
	}

	isFavorite(sticker: StickerKeyInput): boolean {
		return this.favoriteStickers.includes(this.getStickerKey(sticker));
	}

	isCategoryCollapsed(categoryId: string): boolean {
		return this.collapsedCategories.includes(categoryId);
	}

	private getFrecencyScore(entry: StickerUsageEntry): number {
		return usageFrecencyScore(entry, Date.now());
	}

	getFrecentStickers(
		allStickers: ReadonlyArray<GuildSticker>,
		limit: number = MAX_FRECENT_STICKERS,
	): Array<GuildSticker> {
		const stickerScores: Array<{
			sticker: GuildSticker;
			score: number;
		}> = [];
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

	getFavoriteStickers(allStickers: ReadonlyArray<GuildSticker>): Array<GuildSticker> {
		const favorites: Array<GuildSticker> = [];
		for (const sticker of allStickers) {
			if (this.isFavorite(sticker)) {
				favorites.push(sticker);
			}
		}
		return favorites;
	}

	getFrecencyScoreForSticker(sticker: StickerKeyInput): number {
		const usage = this.stickerUsage[this.getStickerKey(sticker)];
		return usage ? this.getFrecencyScore(usage) : 0;
	}

	private getStickerKey(sticker: StickerKeyInput): string {
		return `${sticker.guildId}:${sticker.id}`;
	}
}

export default new StickerPicker();
