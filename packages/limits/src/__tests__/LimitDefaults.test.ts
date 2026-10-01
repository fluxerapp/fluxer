// SPDX-License-Identifier: AGPL-3.0-or-later

import {LIMIT_KEYS} from '@fluxer/constants/src/LimitConfigMetadata';
import {describe, expect, it} from 'vitest';

describe('announcement limit defaults', () => {
	it('keeps the webhook caps that follower webhooks count toward', () => {
		expect(LIMIT_KEYS).toContain('max_webhooks_per_channel');
		expect(LIMIT_KEYS).toContain('max_webhooks_per_guild');
	});
});
