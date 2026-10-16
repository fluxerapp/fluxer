// SPDX-License-Identifier: AGPL-3.0-or-later

import {TemplateChannel} from '@fluxer/schema/src/domains/guild/GuildTemplateSchemas';
import {describe, expect, it} from 'vitest';

describe('TemplateChannel permission overwrites', () => {
	it.each([
		['role', 0],
		['member', 1],
		['0', 0],
		[1, 1],
		[42, 42],
	])('normalizes the imported overwrite type %j', (type, expected) => {
		const channel = TemplateChannel.parse({
			id: '1',
			type: 0,
			position: 0,
			permission_overwrites: [{id: '2', type, allow: '8', deny: '0'}],
		});
		expect(channel.permission_overwrites).toEqual([{id: '2', type: expected, allow: '8', deny: '0'}]);
	});

	it.each(['invalid', 'NaN', 'Infinity', Number.NaN, Number.POSITIVE_INFINITY])(
		'rejects nonfinite overwrite type %j',
		(type) => {
			expect(
				TemplateChannel.safeParse({
					id: '1',
					type: 0,
					position: 0,
					permission_overwrites: [{id: '2', type, allow: '8', deny: '0'}],
				}).success,
			).toBe(false);
		},
	);
});
