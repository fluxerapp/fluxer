// SPDX-License-Identifier: AGPL-3.0-or-later

import {getConfig} from '@app/api/Config';
import {getDefaultProductName, getInstanceProductName, setCachedProductName} from '@app/api/instance/ProductName';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

describe('getInstanceProductName', () => {
	const originalProductName = getConfig().instance.branding.productName;

	beforeEach(() => {
		setCachedProductName(null);
	});

	afterEach(() => {
		getConfig().instance.branding.productName = originalProductName;
		setCachedProductName(null);
	});

	it('uses the configured name until a stored name is known', () => {
		getConfig().instance.branding.productName = 'Configured Chat';
		expect(getDefaultProductName()).toBe('Configured Chat');
		expect(getInstanceProductName()).toBe('Configured Chat');
	});

	it('falls back to Fluxer when no name is configured', () => {
		getConfig().instance.branding.productName = '';
		expect(getInstanceProductName()).toBe('Fluxer');
	});

	it('prefers the stored name', () => {
		getConfig().instance.branding.productName = 'Configured Chat';
		setCachedProductName('Example Chat');
		expect(getInstanceProductName()).toBe('Example Chat');
	});
});
