// SPDX-License-Identifier: AGPL-3.0-or-later

import {rpIdMatchesPage} from '@app/features/auth/passkey_migration/PasskeyMigrationOrigin';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/app/domain_migration/DomainMigrationRollout', () => ({default: {enabled: false}}));

describe('rpIdMatchesPage', () => {
	it('matches the page host and its parent domains', () => {
		expect(rpIdMatchesPage('fluxer.com', 'fluxer.com')).toBe(true);
		expect(rpIdMatchesPage('fluxer.com', 'canary.fluxer.com')).toBe(true);
		expect(rpIdMatchesPage('fluxer.app', 'web.canary.fluxer.app')).toBe(true);
		expect(rpIdMatchesPage('Fluxer.COM', 'fluxer.com')).toBe(true);
	});

	it('rejects other domains', () => {
		expect(rpIdMatchesPage('fluxer.app', 'fluxer.com')).toBe(false);
		expect(rpIdMatchesPage('fluxer.com', 'notfluxer.com')).toBe(false);
		expect(rpIdMatchesPage('canary.fluxer.com', 'fluxer.com')).toBe(false);
	});
});
