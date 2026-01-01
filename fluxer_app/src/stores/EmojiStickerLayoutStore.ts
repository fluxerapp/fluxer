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
import {makePersistent} from '~/lib/MobXPersistence';

export type EmojiLayout = 'list' | 'grid';
export type StickerViewMode = 'cozy' | 'compact';

class EmojiStickerLayoutStore {
	emojiLayout: EmojiLayout = 'list';
	stickerViewMode: StickerViewMode = 'cozy';

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'EmojiStickerLayoutStore', ['emojiLayout', 'stickerViewMode']);
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

export default new EmojiStickerLayoutStore();
