// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/ProductConstants';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfigSnapshot';
import {isOfficialInstanceHost} from '@fluxer/instance_bootstrap/src/OfficialInstance';

type BrandingSnapshot = Pick<
	RuntimeConfigSnapshot,
	'apiEndpoint' | 'apiPublicEndpoint' | 'mediaEndpoint' | 'staticCdnEndpoint' | 'webAppEndpoint' | 'appPublic'
>;

export interface AuthBrandOptions {
	readonly isDocumentInstance: boolean;
	readonly documentHost: string | null;
}

export interface AuthBrand {
	readonly productName: string;
	readonly usesDefaultName: boolean;
	readonly logoCandidates: ReadonlyArray<string>;
	readonly wordmarkUrl: string | null;
	readonly backdropUrl: string | null;
	readonly host: string | null;
	readonly isOfficial: boolean;
}

const ASSET_PROTOCOLS: ReadonlySet<string> = new Set(['https:', 'http:']);

export function originOf(value: string | null | undefined): string | null {
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

function hostOf(value: string | null | undefined): string | null {
	if (value == null || value.length === 0) {
		return null;
	}
	try {
		const host = new URL(value).host.toLowerCase();
		return host.length === 0 ? null : host;
	} catch {
		return null;
	}
}

export function authBrandHasLogo(brand: AuthBrand): boolean {
	return brand.logoCandidates.length > 0 || brand.usesDefaultName;
}

export function resolveAuthBrand(snapshot: BrandingSnapshot, options: AuthBrandOptions): AuthBrand {
	const branding = snapshot.appPublic.branding;
	const productName = resolveInstanceProductName(snapshot) ?? PRODUCT_NAME;
	const resolveUrl = (candidate: string | null | undefined): string | null => {
		if (!options.isDocumentInstance) {
			return resolveInstanceAssetUrl(snapshot, candidate);
		}
		const trimmed = candidate?.trim();
		return trimmed == null || trimmed.length === 0 ? null : trimmed;
	};
	const logoCandidates: Array<string> = [];
	for (const candidate of [branding.logo_url, branding.icon_url, branding.symbol_url]) {
		const url = resolveUrl(candidate);
		if (url != null && !logoCandidates.includes(url)) {
			logoCandidates.push(url);
		}
	}
	const apiHost = hostOf(snapshot.apiEndpoint);
	const isOfficial = apiHost != null && isOfficialInstanceHost(apiHost);
	const host = isOfficial ? null : ((options.isDocumentInstance ? options.documentHost : null) ?? apiHost);
	return {
		productName,
		usesDefaultName: productName === PRODUCT_NAME,
		logoCandidates,
		wordmarkUrl: resolveUrl(branding.wordmark_url),
		backdropUrl: isOfficial ? null : resolveUrl(branding.sign_in_backdrop_url),
		host,
		isOfficial,
	};
}
