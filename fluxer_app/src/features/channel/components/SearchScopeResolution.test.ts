// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveSearchScope} from '@app/features/channel/components/SearchScopeResolution';
import type {MessageSearchScope} from '@app/features/search/utils/SearchUtils';
import {describe, expect, it} from 'vitest';

const GUILD_SCOPE_OPTIONS = new Set<MessageSearchScope>([
	'current',
	'all_guilds',
	'all_dms',
	'open_dms',
	'all',
	'open_dms_and_all_guilds',
]);

describe('resolveSearchScope', () => {
	it('uses an explicit control override before captured active scope', () => {
		expect(
			resolveSearchScope({
				activeScope: 'current',
				fallbackScope: 'current',
				scopeOverride: 'all_guilds',
				scopeOptionValues: GUILD_SCOPE_OPTIONS,
			}),
		).toBe('all_guilds');
	});
	it('uses a parsed query scope when there is no control override', () => {
		expect(
			resolveSearchScope({
				activeScope: 'current',
				fallbackScope: 'current',
				parsedScope: 'all',
				scopeOptionValues: GUILD_SCOPE_OPTIONS,
			}),
		).toBe('all');
	});
	it('falls back when the selected scope is not valid for the current channel', () => {
		expect(
			resolveSearchScope({
				activeScope: 'all_guilds',
				fallbackScope: 'current',
				scopeOptionValues: new Set<MessageSearchScope>(['current', 'all_dms', 'open_dms']),
			}),
		).toBe('current');
	});
});
