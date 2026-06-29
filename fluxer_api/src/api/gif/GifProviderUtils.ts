// SPDX-License-Identifier: AGPL-3.0-or-later

import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import type {IGifProvider} from './IGifProvider';

export type ResolvedGifProviderSlug = {
	provider: IGifProvider;
	slug: string;
};

export function isOptionalGifProviderError(error: unknown): boolean {
	return error instanceof FeatureTemporarilyDisabledError || error instanceof ServiceUnavailableError;
}

export async function tryExtractGifProviderSlug(provider: IGifProvider, value: string): Promise<string | null> {
	try {
		const slug = provider.extractSlugFromUrl(value);
		const trimmed = slug?.trim() ?? '';
		if (trimmed.length === 0) {
			return null;
		}
		return (await provider.isAvailable()) ? trimmed : null;
	} catch (error) {
		if (isOptionalGifProviderError(error)) {
			return null;
		}
		throw error;
	}
}
