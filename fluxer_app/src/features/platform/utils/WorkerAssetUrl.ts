// SPDX-License-Identifier: AGPL-3.0-or-later

import {OFFICIAL_WEB_APP_HOSTS} from '@fluxer/instance_bootstrap/src/OfficialWebAppHosts';

const ASSETS_PATH_SEGMENT = 'assets';
const OFFICIAL_WORKER_ASSET_HOSTS = new Set<string>(OFFICIAL_WEB_APP_HOSTS);
const WORKER_ASSET_VERSION_QUERY_PARAM = 'v';

function getProxiedAssetPath(pathname: string): string | null {
	const segments = pathname.split('/').filter(Boolean);
	const assetsIndex = segments.lastIndexOf(ASSETS_PATH_SEGMENT);
	if (assetsIndex < 0) {
		return null;
	}
	return `/${segments.slice(assetsIndex).join('/')}`;
}

function normalizeBuildVersion(buildVersion: string | number | null | undefined): string | null {
	if (buildVersion == null) {
		return null;
	}
	const normalized = String(buildVersion).trim();
	if (normalized.length === 0 || normalized === '0' || normalized === 'dev') {
		return null;
	}
	return normalized;
}

function getWorkerAssetBuildVersion(explicitBuildVersion?: string | number | null): string | null {
	if (explicitBuildVersion !== undefined) {
		return normalizeBuildVersion(explicitBuildVersion);
	}
	return normalizeBuildVersion(import.meta.env.PUBLIC_BUILD_VERSION);
}

function withWorkerAssetVersion(assetUrl: URL, buildVersion: string | null): string {
	if (!buildVersion) {
		return assetUrl.toString();
	}
	const versionedUrl = new URL(assetUrl.toString());
	versionedUrl.searchParams.set(WORKER_ASSET_VERSION_QUERY_PARAM, buildVersion);
	return versionedUrl.toString();
}

export function resolveWorkerAssetUrl(
	assetUrl: string | URL,
	workerLocation: Pick<Location, 'href'> = self.location,
	buildVersion?: string | number | null,
): string {
	const resolvedAssetUrl = assetUrl instanceof URL ? assetUrl : new URL(assetUrl, workerLocation.href);
	const workerAssetBuildVersion = getWorkerAssetBuildVersion(buildVersion);
	if (OFFICIAL_WORKER_ASSET_HOSTS.has(resolvedAssetUrl.hostname)) {
		return withWorkerAssetVersion(resolvedAssetUrl, workerAssetBuildVersion);
	}
	const proxiedAssetPath = getProxiedAssetPath(resolvedAssetUrl.pathname);
	if (!proxiedAssetPath) {
		return resolvedAssetUrl.toString();
	}
	return withWorkerAssetVersion(
		new URL(`${proxiedAssetPath}${resolvedAssetUrl.search}${resolvedAssetUrl.hash}`, workerLocation.href),
		workerAssetBuildVersion,
	);
}
