// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {describe, expect, it} from 'vitest';
import {getRpcActivitySubtitle, getRpcActivityVerb} from './RpcActivityDisplay';

describe('RpcActivityDisplay', () => {
	it('uses rich presence activity verbs', () => {
		expect(getRpcActivityVerb({name: 'Nebula Drift', type: 0})).toBe('Playing');
		expect(getRpcActivityVerb({name: 'Deep Focus', type: 2})).toBe('Listening to');
		expect(getRpcActivityVerb({name: 'Launch Finals', type: 5})).toBe('Competing in');
	});

	it('falls back for unknown activity types', () => {
		expect(getRpcActivityVerb({name: 'Fluxer Desktop', type: 1})).toBe('Active in');
	});

	it('builds a compact subtitle from unique rich presence lines', () => {
		const activity: UserActivity = {
			name: 'Nebula Drift',
			type: 0,
			details: 'Ranked queue',
			state: 'Ranked queue',
			assets: {large_text: 'Sector 7'},
		};

		expect(getRpcActivitySubtitle(activity)).toBe('Ranked queue · Sector 7');
		expect(getRpcActivitySubtitle({name: 'Nebula Drift', type: 0})).toBeNull();
	});
});
