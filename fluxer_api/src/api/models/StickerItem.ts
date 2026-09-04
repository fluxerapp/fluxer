// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StickerID} from '../BrandedTypes';
import type {MessageStickerItem} from '../database/types/MessageTypes';

export class StickerItem {
	readonly id: StickerID;
	readonly name: string;
	readonly animated: boolean;

	constructor(sticker: MessageStickerItem) {
		this.id = sticker.sticker_id;
		this.name = sticker.name;
		this.animated = sticker.animated ?? false;
	}

	toMessageStickerItem(): MessageStickerItem {
		return {
			sticker_id: this.id,
			name: this.name,
			animated: this.animated,
		};
	}
}
