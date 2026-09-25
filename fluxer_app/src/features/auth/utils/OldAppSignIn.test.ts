// SPDX-License-Identifier: AGPL-3.0-or-later

import {shouldOfferOldAppSignIn} from '@app/features/auth/utils/OldAppSignIn';
import {describe, expect, it} from 'vitest';

describe('shouldOfferOldAppSignIn', () => {
	it('offers the old app sign-in in an installed WebKit app on an official target origin', () => {
		expect(
			shouldOfferOldAppSignIn({origin: 'https://fluxer.com', installKind: 'webkit', hasStoredAccounts: false}),
		).toBe(true);
		expect(
			shouldOfferOldAppSignIn({origin: 'https://canary.fluxer.com', installKind: 'webkit', hasStoredAccounts: false}),
		).toBe(true);
	});

	it('offers the old app sign-in in an installed Android Chromium app on an official target origin', () => {
		expect(
			shouldOfferOldAppSignIn({
				origin: 'https://fluxer.com',
				installKind: 'chromium-android',
				hasStoredAccounts: false,
			}),
		).toBe(true);
	});

	it('does not offer it once an account is stored', () => {
		expect(
			shouldOfferOldAppSignIn({origin: 'https://fluxer.com', installKind: 'webkit', hasStoredAccounts: true}),
		).toBe(false);
	});

	it('does not offer it outside the installed WebKit and Android Chromium apps', () => {
		for (const installKind of ['none', 'chromium-desktop', 'firefox', 'other'] as const) {
			expect(shouldOfferOldAppSignIn({origin: 'https://fluxer.com', installKind, hasStoredAccounts: false})).toBe(
				false,
			);
		}
	});

	it('does not offer it on source or self-hosted origins', () => {
		for (const origin of ['https://web.fluxer.app', 'https://web.canary.fluxer.app', 'https://chat.example.com']) {
			expect(shouldOfferOldAppSignIn({origin, installKind: 'webkit', hasStoredAccounts: false})).toBe(false);
		}
	});
});
