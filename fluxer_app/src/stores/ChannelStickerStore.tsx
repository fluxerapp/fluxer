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

import {makeAutoObservable, observable} from 'mobx';
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';

class ChannelStickerStore {
	pendingStickers: Map<string, GuildStickerRecord> = observable.map();

	constructor() {
		makeAutoObservable(
			this,
			{
				pendingStickers: false,
			},
			{autoBind: true},
		);
	}

	setPendingSticker(channelId: string, sticker: GuildStickerRecord): void {
		this.pendingStickers.set(channelId, sticker);
	}

	removePendingSticker(channelId: string): void {
		this.pendingStickers.delete(channelId);
	}

	clearPendingStickerOnMessageSend(channelId: string): void {
		if (this.pendingStickers.has(channelId)) {
			this.pendingStickers.delete(channelId);
		}
	}

	getPendingSticker(channelId: string): GuildStickerRecord | null {
		return this.pendingStickers.get(channelId) ?? null;
	}
}

export default new ChannelStickerStore();
