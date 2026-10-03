// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildEventCreateRequest, GuildEventUpdateRequest} from '@fluxer/schema/src/domains/guild/GuildEventSchemas';
import {describe, expect, it} from 'vitest';

const validCreate = {
	name: 'Karaoke night',
	starts_at: '2026-10-04T22:00:00Z',
	ends_at: '2026-10-05T01:00:00Z',
};

describe('GuildEventCreateRequest', () => {
	it('accepts an event whose end is after its start', () => {
		expect(GuildEventCreateRequest.parse(validCreate)).toMatchObject(validCreate);
	});

	it('returns structured end-time validation when the end is not after the start', () => {
		const result = GuildEventCreateRequest.safeParse({
			...validCreate,
			ends_at: '2026-10-04T21:00:00Z',
		});
		expect(result.success).toBe(false);
		if (result.success) return;
		expect(result.error.issues).toEqual([
			{code: 'custom', path: ['ends_at'], message: 'Event end must be after its start'},
		]);
	});
});

describe('GuildEventUpdateRequest', () => {
	it('accepts an end-only patch', () => {
		expect(GuildEventUpdateRequest.parse({ends_at: '2026-10-05T01:00:00Z'}).ends_at).toBe('2026-10-05T01:00:00Z');
	});

	it('returns structured end-time validation when both times are present and inverted', () => {
		const result = GuildEventUpdateRequest.safeParse({
			starts_at: '2026-10-04T22:00:00Z',
			ends_at: '2026-10-04T22:00:00Z',
		});
		expect(result.success).toBe(false);
		if (result.success) return;
		expect(result.error.issues).toEqual([
			{code: 'custom', path: ['ends_at'], message: 'Event end must be after its start'},
		]);
	});
});
