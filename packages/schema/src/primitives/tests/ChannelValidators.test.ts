// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelNameType, GeneralChannelNameType} from '@fluxer/schema/src/primitives/ChannelValidators';
import {describe, expect, it} from 'vitest';

describe('channel names', () => {
	it('keeps joiners inside emoji sequences', () => {
		for (const name of ['🧑‍💻', '🧝‌🏿', 'می‌خواهم']) {
			const general = GeneralChannelNameType.parse(name);
			expect(general).toBe(name);
			expect(ChannelNameType.parse(general)).toBe(name);
		}
	});

	it('drops stray joiners', () => {
		expect(GeneralChannelNameType.parse('‍general‌')).toBe('general');
		expect(GeneralChannelNameType.parse('a ‍ b')).toBe('a b');
	});
});
