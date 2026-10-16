// SPDX-License-Identifier: AGPL-3.0-or-later

export const BRANDING_ASSET_FIELDS = ['icon_url', 'symbol_url', 'logo_url', 'wordmark_url', 'favicon_url'] as const;

export type BrandingAssetField = (typeof BRANDING_ASSET_FIELDS)[number];

const BRANDING_ASSET_REFERENCE = /^branding\/\d{1,20}\/(?:a_)?[0-9a-f]{8}\.[a-z0-9]{2,5}$/;
const BRANDING_ASSET_PATH = /\/(branding\/\d{1,20}\/(?:a_)?[0-9a-f]{8}\.[a-z0-9]{2,5})$/;

function trimTrailingSlashes(value: string): string {
	return value.replace(/\/+$/u, '');
}

export function isBrandingAssetReference(value: string): boolean {
	return BRANDING_ASSET_REFERENCE.test(value);
}

export function brandingAssetReferenceFromOwnUrl(value: string, mediaEndpoint: string): string | null {
	const prefix = `${trimTrailingSlashes(mediaEndpoint)}/`;
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

export function brandingAssetStorageKey(reference: string): string {
	const slash = reference.lastIndexOf('/');
	const file = reference.slice(slash + 1);
	const hash = file.slice(0, file.indexOf('.')).replace(/^a_/u, '');
	return `${reference.slice(0, slash)}/${hash}`;
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
	return `${trimTrailingSlashes(mediaEndpoint)}/${value}`;
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
