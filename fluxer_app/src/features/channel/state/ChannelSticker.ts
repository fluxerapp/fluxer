// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import {makeAutoObservable, observable} from 'mobx';

class ChannelSticker {
	pendingStickers: Map<string, GuildSticker> = observable.map();

	constructor() {
		makeAutoObservable(
			this,
			{
				pendingStickers: false,
			},
			{autoBind: true},
		);
	}

	setPendingSticker(channelId: string, sticker: GuildSticker): void {
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

	getPendingSticker(channelId: string): GuildSticker | null {
		return this.pendingStickers.get(channelId) ?? null;
	}

	reset(): void {
		this.pendingStickers.clear();
	}
}

const channelSticker = new ChannelSticker();

AccountScopedWork.registerCancellation(() => channelSticker.reset());

export default channelSticker;
