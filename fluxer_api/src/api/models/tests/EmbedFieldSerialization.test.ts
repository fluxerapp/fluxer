// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmbedField} from '@app/api/models/EmbedField';
import {describe, expect, it} from 'vitest';

describe('EmbedField serialization', () => {
	it('normalizes null names and values to empty strings for response-safe storage', () => {
		const field = new EmbedField({
			name: null,
			value: null,
			inline: false,
		});

		expect(field.toMessageEmbedField()).toEqual({
			name: '',
			value: '',
			inline: false,
		});
	});

	it('preserves explicitly empty values from custom embeds', () => {
		const field = new EmbedField({
			name: '-----------------------------',
			value: '',
			inline: false,
		});

		expect(field.toMessageEmbedField()).toEqual({
			name: '-----------------------------',
			value: '',
			inline: false,
		});
	});
});
