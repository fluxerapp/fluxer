// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '../Config';
import {Logger} from '../Logger';
import {lookupGeoip} from '../utils/IpUtils';
import {parseDesktopArtifactScope} from './DesktopReleaseContract';
import type {DownloadService, GitHubDesktopReleaseResolution} from './DownloadService';

const COUNTRY_DEPENDENT_CACHE_CONTROL = 'private, no-store';

type ArtifactRoute =
	| {kind: 'storage'; cacheControl: string}
	| {kind: 'redirect'; cacheControl: string; location: string};

export async function resolveArtifactRoute(params: {
	request: Request;
	downloadService: DownloadService;
	key: string;
	cacheControl: string;
}): Promise<ArtifactRoute> {
	if (Config.instance.selfHosted) {
		return {kind: 'storage', cacheControl: params.cacheControl};
	}
	if (Config.desktopGitHubRedirectCountries.size === 0) {
		return {kind: 'storage', cacheControl: params.cacheControl};
	}
	if (!parseDesktopArtifactScope(params.key)) {
		return {kind: 'storage', cacheControl: params.cacheControl};
	}
	const geoip = await lookupGeoip(params.request);
	const countryCode = geoip.countryCode?.trim().toUpperCase();
	if (!countryCode || !Config.desktopGitHubRedirectCountries.has(countryCode)) {
		return {kind: 'storage', cacheControl: COUNTRY_DEPENDENT_CACHE_CONTROL};
	}
	let release: GitHubDesktopReleaseResolution;
	try {
		release = await params.downloadService.resolveGitHubDesktopRelease(params.key);
	} catch (error) {
		Logger.error({error, key: params.key}, 'Failed to resolve GitHub desktop download route');
		return {kind: 'storage', cacheControl: COUNTRY_DEPENDENT_CACHE_CONTROL};
	}
	if (release.kind === 'not_current') {
		return {kind: 'storage', cacheControl: COUNTRY_DEPENDENT_CACHE_CONTROL};
	}
	if (release.kind === 'ready') {
		return {
			kind: 'redirect',
			cacheControl: COUNTRY_DEPENDENT_CACHE_CONTROL,
			location: release.location,
		};
	}
	return {kind: 'storage', cacheControl: COUNTRY_DEPENDENT_CACHE_CONTROL};
}
