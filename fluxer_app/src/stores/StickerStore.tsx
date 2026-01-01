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
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildReadyData} from '~/records/GuildRecord';
import {type GuildSticker, GuildStickerRecord} from '~/records/GuildStickerRecord';
import {patchGuildStickerCacheFromGateway} from '~/stores/GuildExpressionTabCache';
import StickerPickerStore from '~/stores/StickerPickerStore';
import {sortBySnowflakeDesc} from '~/utils/SnowflakeUtils';

interface GuildStickerContext {
	stickers: Array<GuildStickerRecord>;
}

interface GuildStickersPayload {
	id: string;
	stickers?: ReadonlyArray<GuildSticker> | null;
}

class StickerStore {
	guildStickers: Map<string, GuildStickerContext> = new Map();
	stickerById: Map<string, GuildStickerRecord> = new Map();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getGuildStickers(guildId: string): ReadonlyArray<GuildStickerRecord> {
		return this.guildStickers.get(guildId)?.stickers ?? [];
	}

	getSticker(guildId: string, stickerId: string): GuildStickerRecord | null {
		return this.guildStickers.get(guildId)?.stickers.find((s) => s.id === stickerId) ?? null;
	}

	getStickerById(stickerId: string): GuildStickerRecord | null {
		return this.stickerById.get(stickerId) ?? null;
	}

	getAllStickers(): ReadonlyArray<GuildStickerRecord> {
		const allStickers: Array<GuildStickerRecord> = [];
		for (const context of this.guildStickers.values()) {
			allStickers.push(...context.stickers);
		}
		return allStickers;
	}

	search(guildId: string | null, searchTerm: string): ReadonlyArray<GuildStickerRecord> {
		let stickers: ReadonlyArray<GuildStickerRecord>;

		if (guildId) {
			stickers = this.getGuildStickers(guildId);
		} else {
			stickers = this.getAllStickers();
		}

		if (!searchTerm || searchTerm.trim() === '') {
			return stickers;
		}

		const term = searchTerm.toLowerCase();
		const filtered = stickers.filter((sticker) => {
			const nameMatch = sticker.name.toLowerCase().includes(term);
			const descMatch = sticker.description?.toLowerCase().includes(term);
			const tagMatch = sticker.tags.some((tag) => tag.toLowerCase().includes(term));
			return nameMatch || descMatch || tagMatch;
		});

		return this.sortByFrecency(filtered);
	}

	searchWithChannel(channel: ChannelRecord | null, searchTerm: string): ReadonlyArray<GuildStickerRecord> {
		const stickers = this.getAllStickers();
		const guildId = channel?.guildId;

		if (!searchTerm || searchTerm.trim() === '') {
			return stickers;
		}

		const term = searchTerm.toLowerCase();
		const filtered = stickers.filter((sticker) => {
			const nameMatch = sticker.name.toLowerCase().includes(term);
			const descMatch = sticker.description?.toLowerCase().includes(term);
			const tagMatch = sticker.tags.some((tag) => tag.toLowerCase().includes(term));
			return nameMatch || descMatch || tagMatch;
		});

		if (guildId) {
			filtered.sort((a, b) => {
				const aInGuild = a.guildId === guildId;
				const bInGuild = b.guildId === guildId;
				if (aInGuild === bInGuild) return 0;
				return aInGuild ? -1 : 1;
			});
		}

		return this.sortByFrecency(filtered);
	}

	handleConnectionOpen(guilds: ReadonlyArray<GuildReadyData>): void {
		this.guildStickers.clear();
		this.stickerById.clear();

		for (const guild of guilds) {
			if (guild.stickers && guild.stickers.length > 0) {
				const stickerRecords = guild.stickers.map((sticker) => new GuildStickerRecord(guild.id, sticker));
				const sortedStickers = sortBySnowflakeDesc(stickerRecords);

				this.guildStickers.set(guild.id, {stickers: sortedStickers});

				for (const sticker of sortedStickers) {
					this.stickerById.set(sticker.id, sticker);
				}
			}
		}
	}

	handleGuildUpdate(guild: GuildStickersPayload): void {
		if (!guild.stickers || guild.stickers.length === 0) {
			return;
		}

		this.updateGuildStickers(guild.id, guild.stickers);
	}

	handleGuildStickersUpdate(guildId: string, stickers: ReadonlyArray<GuildSticker>): void {
		this.updateGuildStickers(guildId, stickers);
		patchGuildStickerCacheFromGateway(guildId, stickers);
	}

	handleGuildDelete(guildId: string): void {
		const oldStickers = this.guildStickers.get(guildId)?.stickers ?? [];

		for (const oldSticker of oldStickers) {
			this.stickerById.delete(oldSticker.id);
		}

		this.guildStickers.delete(guildId);
	}

	private updateGuildStickers(guildId: string, guildStickers: ReadonlyArray<GuildSticker>): void {
		const stickerRecords = guildStickers.map((sticker) => new GuildStickerRecord(guildId, sticker));
		const sortedStickers = sortBySnowflakeDesc(stickerRecords);

		const oldStickers = this.guildStickers.get(guildId)?.stickers ?? [];

		for (const oldSticker of oldStickers) {
			this.stickerById.delete(oldSticker.id);
		}

		this.guildStickers.set(guildId, {stickers: sortedStickers});

		for (const sticker of sortedStickers) {
			this.stickerById.set(sticker.id, sticker);
		}
	}

	private sortByFrecency(stickers: ReadonlyArray<GuildStickerRecord>): ReadonlyArray<GuildStickerRecord> {
		return [...stickers].sort((a, b) => {
			const frecencyDiff =
				StickerPickerStore.getFrecencyScoreForSticker(b) - StickerPickerStore.getFrecencyScoreForSticker(a);
			if (frecencyDiff !== 0) {
				return frecencyDiff;
			}
			return a.name.localeCompare(b.name);
		});
	}
}

export default new StickerStore();
