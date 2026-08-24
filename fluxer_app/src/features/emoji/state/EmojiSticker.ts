// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import StickerPicker from '@app/features/emoji/state/StickerPicker';
import {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import {patchGuildStickerCacheFromGateway} from '@app/features/expressions/state/GuildExpressionTabCache';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import type {GuildSticker as WireGuildSticker} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {sortBySnowflakeDesc} from '@fluxer/snowflake/src/SnowflakeUtils';
import {makeAutoObservable} from 'mobx';

interface GuildStickerContext {
	stickers: Array<GuildSticker>;
}

interface GuildStickersPayload {
	id: string;
	stickers?: ReadonlyArray<WireGuildSticker> | null;
}

class Sticker {
	guildStickers: Map<string, GuildStickerContext> = new Map();
	stickerById: Map<string, GuildSticker> = new Map();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getGuildStickers(guildId: string): ReadonlyArray<GuildSticker> {
		return this.guildStickers.get(guildId)?.stickers ?? [];
	}

	getSticker(guildId: string, stickerId: string): GuildSticker | null {
		return this.guildStickers.get(guildId)?.stickers.find((s) => s.id === stickerId) ?? null;
	}

	getStickerById(stickerId: string): GuildSticker | null {
		return this.stickerById.get(stickerId) ?? null;
	}

	getAllStickers(): ReadonlyArray<GuildSticker> {
		const allStickers: Array<GuildSticker> = [];
		for (const context of this.guildStickers.values()) {
			allStickers.push(...context.stickers);
		}
		return allStickers;
	}

	search(guildId: string | null, searchTerm: string): ReadonlyArray<GuildSticker> {
		let stickers: ReadonlyArray<GuildSticker>;
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

	searchWithChannel(channel: Channel | null, searchTerm: string): ReadonlyArray<GuildSticker> {
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

	handleGatewayReady(guilds: ReadonlyArray<GuildReadyData>): void {
		this.guildStickers.clear();
		this.stickerById.clear();
		for (const guild of guilds) {
			if (guild.stickers && guild.stickers.length > 0) {
				const stickerRecords = guild.stickers.map((sticker) => new GuildSticker(guild.id, sticker));
				const sortedStickers = sortBySnowflakeDesc(stickerRecords);
				this.guildStickers.set(guild.id, {stickers: sortedStickers});
				for (const sticker of sortedStickers) {
					this.stickerById.set(sticker.id, sticker);
				}
			}
		}
		ComponentBus.dispatch('STICKER_PICKER_RERENDER');
	}

	handleGuildUpdate(guild: GuildStickersPayload): void {
		if (!guild.stickers || guild.stickers.length === 0) {
			return;
		}
		this.updateGuildStickers(guild.id, guild.stickers);
	}

	handleGuildStickersUpdate(guildId: string, stickers: ReadonlyArray<WireGuildSticker>): void {
		this.updateGuildStickers(guildId, stickers);
		patchGuildStickerCacheFromGateway(guildId, stickers);
	}

	handleGuildDelete(guildId: string): void {
		const oldStickers = this.guildStickers.get(guildId)?.stickers ?? [];
		for (const oldSticker of oldStickers) {
			this.stickerById.delete(oldSticker.id);
		}
		this.guildStickers.delete(guildId);
		ComponentBus.dispatch('STICKER_PICKER_RERENDER');
	}

	private updateGuildStickers(guildId: string, guildStickers: ReadonlyArray<WireGuildSticker>): void {
		const stickerRecords = guildStickers.map((sticker) => new GuildSticker(guildId, sticker));
		const sortedStickers = sortBySnowflakeDesc(stickerRecords);
		const oldStickers = this.guildStickers.get(guildId)?.stickers ?? [];
		for (const oldSticker of oldStickers) {
			this.stickerById.delete(oldSticker.id);
		}
		this.guildStickers.set(guildId, {stickers: sortedStickers});
		for (const sticker of sortedStickers) {
			this.stickerById.set(sticker.id, sticker);
		}
		ComponentBus.dispatch('STICKER_PICKER_RERENDER');
	}

	private sortByFrecency(stickers: ReadonlyArray<GuildSticker>): ReadonlyArray<GuildSticker> {
		const scores = new Map<GuildSticker, number>();
		for (const sticker of stickers) {
			scores.set(sticker, StickerPicker.getFrecencyScoreForSticker(sticker));
		}
		return [...stickers].sort((a, b) => {
			const frecencyDiff = (scores.get(b) ?? 0) - (scores.get(a) ?? 0);
			if (frecencyDiff !== 0) {
				return frecencyDiff;
			}
			return a.name.localeCompare(b.name);
		});
	}
}

export default new Sticker();
