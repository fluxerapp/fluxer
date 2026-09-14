// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, describe, expect, it, vi} from 'vitest';

const {getMock} = vi.hoisted(() => ({getMock: vi.fn()}));

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));

vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: getMock},
}));

const {fetchEmojiMetadata, fetchStickerMetadata} = await import(
	'@app/features/expressions/commands/ExpressionMetadataCommands'
);

const BASE_BODY = {id: '10', guild_id: '20', name: 'blob', animated: false, allow_cloning: true};

describe('ExpressionMetadataCommands', () => {
	afterEach(() => {
		getMock.mockReset();
	});

	it.each([
		['emoji', fetchEmojiMetadata],
		['sticker', fetchStickerMetadata],
	])('maps a %s response that has no guild to a null guild', async (_kind, fetchMetadata) => {
		getMock.mockResolvedValue({body: BASE_BODY});
		await expect(fetchMetadata('10')).resolves.toEqual({
			id: '10',
			guildId: '20',
			name: 'blob',
			animated: false,
			allowCloning: true,
			guild: null,
		});
	});

	it('maps the source community when the response includes it', async () => {
		getMock.mockResolvedValue({
			body: {...BASE_BODY, guild: {id: '20', name: 'Blob Club', icon: null, features: ['VERIFIED']}},
		});
		const metadata = await fetchEmojiMetadata('10');
		expect(metadata.guild).toEqual({id: '20', name: 'Blob Club', icon: null, features: ['VERIFIED']});
	});
});
