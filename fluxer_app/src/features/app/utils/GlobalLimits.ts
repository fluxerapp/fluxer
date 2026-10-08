// SPDX-License-Identifier: AGPL-3.0-or-later

import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';

const FALLBACKS = {
	emoji_max_size: 384 * 1024,
	sticker_max_size: 512 * 1024,
	avatar_max_size: 10 * 1024 * 1024,
} as const;

class GlobalLimitsClass {
	getEmojiMaxSize(): number {
		return LimitResolver.resolve({
			key: 'emoji_max_size',
			fallback: FALLBACKS.emoji_max_size,
		});
	}

	getStickerMaxSize(): number {
		return LimitResolver.resolve({
			key: 'sticker_max_size',
			fallback: FALLBACKS.sticker_max_size,
		});
	}

	get(key: LimitKey, fallback: number): number {
		return LimitResolver.resolve({key, fallback});
	}
}

export const GlobalLimits = new GlobalLimitsClass();
