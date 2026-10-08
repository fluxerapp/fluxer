// SPDX-License-Identifier: AGPL-3.0-or-later

import {getConfig} from '@app/api/Config';
import {getLegalUrls, resolveLegalUrls, setCachedConfiguredLegalUrls} from '@app/api/instance/LegalUrls';
import {afterEach, describe, expect, it} from 'vitest';

const UNSET = {terms_url: null, guidelines_url: null};
const CONFIGURED = {terms_url: 'https://example.org/tos', guidelines_url: 'https://example.org/rules'};

describe('resolveLegalUrls', () => {
	const originalSelfHosted = getConfig().instance.selfHosted;

	afterEach(() => {
		getConfig().instance.selfHosted = originalSelfHosted;
		setCachedConfiguredLegalUrls(UNSET);
	});

	it('points a hosted instance at the marketing pages by default', () => {
		getConfig().instance.selfHosted = false;
		const marketing = getConfig().endpoints.marketing;
		expect(resolveLegalUrls(UNSET)).toEqual({
			termsUrl: `${marketing}/terms`,
			guidelinesUrl: `${marketing}/guidelines`,
		});
	});

	it('lets a hosted instance override each page', () => {
		getConfig().instance.selfHosted = false;
		expect(resolveLegalUrls(CONFIGURED)).toEqual({
			termsUrl: 'https://example.org/tos',
			guidelinesUrl: 'https://example.org/rules',
		});
		expect(resolveLegalUrls({terms_url: null, guidelines_url: 'https://example.org/rules'}).termsUrl).toBe(
			`${getConfig().endpoints.marketing}/terms`,
		);
	});

	it('gives a self-hosted instance no links until they are configured', () => {
		getConfig().instance.selfHosted = true;
		expect(resolveLegalUrls(UNSET)).toEqual({termsUrl: null, guidelinesUrl: null});
		expect(resolveLegalUrls({terms_url: null, guidelines_url: 'https://example.org/rules'})).toEqual({
			termsUrl: null,
			guidelinesUrl: 'https://example.org/rules',
		});
		expect(resolveLegalUrls(CONFIGURED)).toEqual({
			termsUrl: 'https://example.org/tos',
			guidelinesUrl: 'https://example.org/rules',
		});
	});

	it('resolves the cached instance config', () => {
		getConfig().instance.selfHosted = true;
		expect(getLegalUrls()).toEqual({termsUrl: null, guidelinesUrl: null});
		setCachedConfiguredLegalUrls(CONFIGURED);
		expect(getLegalUrls()).toEqual({termsUrl: 'https://example.org/tos', guidelinesUrl: 'https://example.org/rules'});
		getConfig().instance.selfHosted = false;
		expect(getLegalUrls()).toEqual({termsUrl: 'https://example.org/tos', guidelinesUrl: 'https://example.org/rules'});
	});
});
