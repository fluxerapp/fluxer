// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfigSnapshot';

type BrandingSnapshot = Pick<
	RuntimeConfigSnapshot,
	'apiEndpoint' | 'apiPublicEndpoint' | 'mediaEndpoint' | 'staticCdnEndpoint' | 'webAppEndpoint' | 'appPublic'
>;

const ASSET_PROTOCOLS: ReadonlySet<string> = new Set(['https:', 'http:']);

function originOf(value: string | null | undefined): string | null {
	if (value == null || value.length === 0) {
		return null;
	}
	try {
		return new URL(value).origin;
	} catch {
		return null;
	}
}

function instanceAssetOrigins(snapshot: BrandingSnapshot): ReadonlySet<string> {
	const origins = new Set<string>();
	for (const endpoint of [
		snapshot.mediaEndpoint,
		snapshot.staticCdnEndpoint,
		snapshot.webAppEndpoint,
		snapshot.apiEndpoint,
		snapshot.apiPublicEndpoint,
	]) {
		const origin = originOf(endpoint);
		if (origin != null && origin !== 'null') {
			origins.add(origin);
		}
	}
	return origins;
}

export function resolveInstanceAssetUrl(
	snapshot: BrandingSnapshot,
	candidate: string | null | undefined,
): string | null {
	const trimmed = candidate?.trim();
	if (trimmed == null || trimmed.length === 0) {
		return null;
	}
	const base = snapshot.mediaEndpoint || snapshot.webAppEndpoint;
	let url: URL;
	try {
		url = new URL(trimmed, base);
	} catch {
		return null;
	}
	if (!ASSET_PROTOCOLS.has(url.protocol) || url.username !== '' || url.password !== '') {
		return null;
	}
	if (!instanceAssetOrigins(snapshot).has(url.origin)) {
		return null;
	}
	return url.href;
}

export function resolveInstanceBrandIconUrl(snapshot: BrandingSnapshot | null | undefined): string | null {
	const branding = snapshot?.appPublic?.branding;
	if (snapshot == null || branding == null) {
		return null;
	}
	for (const candidate of [branding.symbol_url, branding.icon_url, branding.logo_url]) {
		const url = resolveInstanceAssetUrl(snapshot, candidate);
		if (url != null) {
			return url;
		}
	}
	return null;
}

export function resolveInstanceProductName(snapshot: BrandingSnapshot | null | undefined): string | null {
	const name = snapshot?.appPublic?.branding?.product_name?.trim();
	return name == null || name.length === 0 ? null : name;
}
