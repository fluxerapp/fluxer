// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';

export interface ConfiguredLegalUrls {
	terms_url: string | null;
	guidelines_url: string | null;
}

export interface LegalUrls {
	termsUrl: string | null;
	guidelinesUrl: string | null;
}

let cachedConfiguredLegalUrls: ConfiguredLegalUrls = {terms_url: null, guidelines_url: null};

export function setCachedConfiguredLegalUrls(legal: ConfiguredLegalUrls): void {
	cachedConfiguredLegalUrls = {terms_url: legal.terms_url, guidelines_url: legal.guidelines_url};
}

export function resolveLegalUrls(legal: ConfiguredLegalUrls): LegalUrls {
	const hostedUrl = (path: string): string | null =>
		Config.instance.selfHosted ? null : `${Config.endpoints.marketing}${path}`;
	return {
		termsUrl: legal.terms_url ?? hostedUrl('/terms'),
		guidelinesUrl: legal.guidelines_url ?? hostedUrl('/guidelines'),
	};
}

export function getLegalUrls(): LegalUrls {
	return resolveLegalUrls(cachedConfiguredLegalUrls);
}
