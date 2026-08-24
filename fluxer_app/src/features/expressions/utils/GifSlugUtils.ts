// SPDX-License-Identifier: AGPL-3.0-or-later

import * as KlipyUtils from '@app/features/expressions/utils/KlipyUtils';
import * as TenorUtils from '@app/features/expressions/utils/TenorUtils';

interface GifProviderUrlAdapter {
	extractSlugFromUrl(url: string): string | null;
	buildShareUrl(slug: string): string;
}

const PROVIDER_URL_ADAPTERS: Record<string, GifProviderUrlAdapter> = {
	klipy: {
		extractSlugFromUrl: (url) => KlipyUtils.extractKlipySlug(url),
		buildShareUrl: (slug) => KlipyUtils.buildKlipyShareUrl({slug}),
	},
	tenor: {
		extractSlugFromUrl: (url) => TenorUtils.extractTenorSlugId(url),
		buildShareUrl: (slug) => TenorUtils.buildTenorShareUrl(slug),
	},
};

function getAdapter(provider: string): GifProviderUrlAdapter | null {
	return PROVIDER_URL_ADAPTERS[provider] ?? null;
}

export function extractSlugFromUrl(provider: string, url: string): string | null {
	return getAdapter(provider)?.extractSlugFromUrl(url) ?? null;
}

export function isProviderPageUrl(url: string): boolean {
	if (!url) return false;
	return Object.values(PROVIDER_URL_ADAPTERS).some((adapter) => adapter.extractSlugFromUrl(url) != null);
}

export function isUsableMediaSource(url: string): boolean {
	if (!url) return false;
	if (!/^https?:\/\//i.test(url.trim())) return false;
	return !isProviderPageUrl(url);
}

function looksLikeUrl(value: string): boolean {
	const trimmed = value.trim();
	return /^https?:\/\//i.test(trimmed);
}

function normalizeSlugForProvider(provider: string, raw: string | undefined | null): string | null {
	const trimmed = raw?.trim();
	if (!trimmed) return null;
	if (looksLikeUrl(trimmed)) {
		return extractSlugFromUrl(provider, trimmed);
	}
	return trimmed;
}

export function resolveShareId(
	provider: string,
	gif: {
		id: string;
		slug?: string;
		url?: string;
	},
): string | null {
	const slug = normalizeSlugForProvider(provider, gif.slug);
	if (slug) return slug;
	if (gif.url) {
		const extracted = extractSlugFromUrl(provider, gif.url);
		if (extracted) return extracted;
	}
	return gif.id || null;
}

export function buildShareUrl(provider: string, slug: string): string | null {
	return getAdapter(provider)?.buildShareUrl(slug) ?? null;
}

export function resolveShareUrl(
	provider: string,
	gif: {
		url: string;
		slug?: string;
	},
): string {
	const slug = normalizeSlugForProvider(provider, gif.slug);
	if (slug) {
		const built = buildShareUrl(provider, slug);
		if (built) return built;
	}
	const extracted = extractSlugFromUrl(provider, gif.url);
	if (extracted) {
		const built = buildShareUrl(provider, extracted);
		if (built) return built;
	}
	return gif.url;
}
