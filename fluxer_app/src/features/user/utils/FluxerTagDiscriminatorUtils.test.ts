// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {isVisionaryDiscriminator0000Blocked} from './FluxerTagDiscriminatorUtils';

describe('isVisionaryDiscriminator0000Blocked', () => {
	it('allows #0000 on self-hosted instances', () => {
		expect(
			isVisionaryDiscriminator0000Blocked({
				showPremium: false,
				isVisionary: false,
				discriminator: '0000',
			}),
		).toBe(false);
	});

	it('blocks #0000 for non-visionary users on the main instance', () => {
		expect(
			isVisionaryDiscriminator0000Blocked({
				showPremium: true,
				isVisionary: false,
				discriminator: '0000',
			}),
		).toBe(true);
	});

	it('allows #0000 for visionary users on the main instance', () => {
		expect(
			isVisionaryDiscriminator0000Blocked({
				showPremium: true,
				isVisionary: true,
				discriminator: '0000',
			}),
		).toBe(false);
	});

	it('allows other discriminators on the main instance', () => {
		expect(
			isVisionaryDiscriminator0000Blocked({
				showPremium: true,
				isVisionary: false,
				discriminator: '1337',
			}),
		).toBe(false);
	});
});
