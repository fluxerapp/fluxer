// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {
	EmojiPickerLayout,
	EmojiStickerLayoutSettingsSchema,
	StickerPickerViewMode,
} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/pickers_pb';
import {makeAutoObservable} from 'mobx';

export type EmojiLayout = 'list' | 'grid';
export type StickerViewMode = 'cozy' | 'compact';

const EMOJI_FROM_PROTO: Record<EmojiPickerLayout, EmojiLayout | null> = {
	[EmojiPickerLayout.UNSPECIFIED]: null,
	[EmojiPickerLayout.LIST]: 'list',
	[EmojiPickerLayout.GRID]: 'grid',
};
const EMOJI_TO_PROTO: Record<EmojiLayout, EmojiPickerLayout> = {
	list: EmojiPickerLayout.LIST,
	grid: EmojiPickerLayout.GRID,
};
const STICKER_FROM_PROTO: Record<StickerPickerViewMode, StickerViewMode | null> = {
	[StickerPickerViewMode.UNSPECIFIED]: null,
	[StickerPickerViewMode.COZY]: 'cozy',
	[StickerPickerViewMode.COMPACT]: 'compact',
};
const STICKER_TO_PROTO: Record<StickerViewMode, StickerPickerViewMode> = {
	cozy: StickerPickerViewMode.COZY,
	compact: StickerPickerViewMode.COMPACT,
};

class EmojiStickerLayout {
	emojiLayout: EmojiLayout = 'list';
	stickerViewMode: StickerViewMode = 'cozy';

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'emojiStickerLayout',
			schema: EmojiStickerLayoutSettingsSchema,
			persist: ['emojiLayout', 'stickerViewMode'],
			toMessage: (s) => ({
				emojiLayout: EMOJI_TO_PROTO[s.emojiLayout],
				stickerViewMode: STICKER_TO_PROTO[s.stickerViewMode],
			}),
			applyMessage: (s, m) => {
				const emoji = EMOJI_FROM_PROTO[m.emojiLayout];
				if (emoji !== null) s.emojiLayout = emoji;
				const sticker = STICKER_FROM_PROTO[m.stickerViewMode];
				if (sticker !== null) s.stickerViewMode = sticker;
			},
		});
	}

	getEmojiLayout(): EmojiLayout {
		return this.emojiLayout;
	}

	setEmojiLayout(layout: EmojiLayout): void {
		this.emojiLayout = layout;
	}

	getStickerViewMode(): StickerViewMode {
		return this.stickerViewMode;
	}

	setStickerViewMode(mode: StickerViewMode): void {
		this.stickerViewMode = mode;
	}
}

export default new EmojiStickerLayout();
