// SPDX-License-Identifier: AGPL-3.0-or-later

import {trimTrailingSlash} from '@app/api/utils/UrlUtils';

export const BRANDING_ASSET_FIELDS = [
	'icon_url',
	'symbol_url',
	'logo_url',
	'wordmark_url',
	'favicon_url',
	'favicon_dark_url',
	'sign_in_backdrop_url',
] as const;

export type BrandingAssetField = (typeof BRANDING_ASSET_FIELDS)[number];

const BRANDING_ASSET_REFERENCE = /^branding(?:-backdrops)?\/\d{1,20}\/(?:a_)?[0-9a-f]{8}\.[a-z0-9]{2,5}$/;
const BRANDING_ASSET_PATH = /\/(branding(?:-backdrops)?\/\d{1,20}\/(?:a_)?[0-9a-f]{8}\.[a-z0-9]{2,5})$/;
const CUSTOM_BADGE_ASSET_REFERENCE = /^custom-badges\/\d{1,20}\/(?:a_)?[0-9a-f]{8}\.[a-z0-9]{2,5}$/;

export function isBrandingAssetReference(value: string): boolean {
	return BRANDING_ASSET_REFERENCE.test(value);
}

function brandingAssetReferenceFromOwnUrl(value: string, mediaEndpoint: string): string | null {
	const prefix = `${trimTrailingSlash(mediaEndpoint)}/`;
	if (!value.startsWith(prefix)) return null;
	const remainder = value.slice(prefix.length);
	return isBrandingAssetReference(remainder) ? remainder : null;
}

export function brandingAssetReferenceFromAnyUrl(value: string): string | null {
	let url: URL;
	try {
		url = new URL(value);
	} catch {
		return null;
	}
	if ((url.protocol !== 'https:' && url.protocol !== 'http:') || url.search !== '' || url.hash !== '') return null;
	return BRANDING_ASSET_PATH.exec(url.pathname)?.[1] ?? null;
}

export function assetReferenceHash(reference: string): string {
	const file = reference.slice(reference.lastIndexOf('/') + 1);
	return file.slice(0, file.indexOf('.'));
}

export function assetStorageKey(reference: string): string {
	const hash = assetReferenceHash(reference).replace(/^a_/u, '');
	return `${reference.slice(0, reference.lastIndexOf('/'))}/${hash}`;
}

export function assetReferenceUrl(reference: string, mediaEndpoint: string): string {
	return `${trimTrailingSlash(mediaEndpoint)}/${reference}`;
}

export function isCustomBadgeAssetReference(value: string): boolean {
	return CUSTOM_BADGE_ASSET_REFERENCE.test(value);
}

export function customBadgeAssetReferenceFromOwnUrl(value: string, mediaEndpoint: string): string {
	const prefix = `${trimTrailingSlash(mediaEndpoint)}/`;
	const reference = value.startsWith(prefix) ? value.slice(prefix.length) : value;
	if (!isCustomBadgeAssetReference(reference)) {
		throw new Error('Uploaded custom badge image has an unexpected URL');
	}
	return reference;
}

export function customBadgeImageUrl(reference: string, mediaEndpoint: string): string {
	return `${trimTrailingSlash(mediaEndpoint)}/${reference.slice(0, reference.lastIndexOf('.'))}.webp`;
}

export function toStoredBrandingAsset(
	value: string | null | undefined,
	mediaEndpoint: string,
): string | null | undefined {
	if (value === undefined || value === null) return value;
	return brandingAssetReferenceFromOwnUrl(value, mediaEndpoint) ?? value;
}

export function resolveBrandingAsset(value: string | null, mediaEndpoint: string): string | null {
	if (value === null || !isBrandingAssetReference(value)) return value;
	return assetReferenceUrl(value, mediaEndpoint);
}

export function mapBrandingAssets<T extends Partial<Record<BrandingAssetField, string | null | undefined>>>(
	branding: T,
	map: (value: string | null | undefined) => string | null | undefined,
): T {
	const next = {...branding};
	for (const field of BRANDING_ASSET_FIELDS) {
		if (field in next) {
			next[field] = map(next[field]) as T[BrandingAssetField];
		}
	}
	return next;
}

export function brandingAssetStorageKeys(
	branding: Partial<Record<BrandingAssetField, string | null | undefined>>,
): Set<string> {
	const keys = new Set<string>();
	for (const field of BRANDING_ASSET_FIELDS) {
		const value = branding[field];
		if (typeof value === 'string' && isBrandingAssetReference(value)) keys.add(assetStorageKey(value));
	}
	return keys;
}

export function releasedBrandingAssetReferences(
	before: Partial<Record<BrandingAssetField, string | null | undefined>>,
	after: Partial<Record<BrandingAssetField, string | null | undefined>>,
): Array<string> {
	const kept = brandingAssetStorageKeys(after);
	const released = new Map<string, string>();
	for (const field of BRANDING_ASSET_FIELDS) {
		const value = before[field];
		if (typeof value !== 'string' || !isBrandingAssetReference(value)) continue;
		const key = assetStorageKey(value);
		if (!kept.has(key) && !released.has(key)) released.set(key, value);
	}
	return [...released.values()];
}
