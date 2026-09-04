// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {createStickerID} from '../../BrandedTypes';
import {StickerItem} from '../StickerItem';

describe('StickerItem', () => {
	it('defaults animated to false when it is omitted', () => {
		const sticker = new StickerItem({
			sticker_id: createStickerID(1n),
			name: 'party-parrot',
		});
		expect(sticker.toMessageStickerItem()).toEqual({
			sticker_id: createStickerID(1n),
			name: 'party-parrot',
			animated: false,
		});
	});
});
