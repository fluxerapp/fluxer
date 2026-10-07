// SPDX-License-Identifier: AGPL-3.0-or-later

import {normalizeHTTPNetworkOrigin} from '@fluxer/instance_bootstrap/src/NetworkOrigin';

export const OFFICIAL_INSTANCE_NAME = 'Fluxer';

export const OFFICIAL_INSTANCE_DISPLAY_HOST = 'fluxer.app';

export const OFFICIAL_STABLE_MARKETING_ORIGIN = `https://${OFFICIAL_INSTANCE_DISPLAY_HOST}`;

export const OFFICIAL_CLIENT_API_ENDPOINTS = Object.freeze({
	stable: 'https://web.fluxer.app/api',
	canary: 'https://web.canary.fluxer.app/api',
} as const);

export type OfficialReleaseChannel = keyof typeof OFFICIAL_CLIENT_API_ENDPOINTS;

export const OFFICIAL_MARKETING_ORIGINS = Object.freeze({
	stable: OFFICIAL_STABLE_MARKETING_ORIGIN,
	canary: 'https://canary.fluxer.app',
} as const) satisfies Record<OfficialReleaseChannel, string>;

export function officialMarketingOrigin(releaseChannel: string | null | undefined): string {
	return releaseChannel === 'stable' ? OFFICIAL_MARKETING_ORIGINS.stable : OFFICIAL_MARKETING_ORIGINS.canary;
}

const OFFICIAL_MIGRATED_WEB_APP_HOSTS = Object.freeze({
	stable: 'fluxer.com',
	canary: 'canary.fluxer.com',
} as const) satisfies Record<OfficialReleaseChannel, string>;

const OFFICIAL_CLIENT_API_PATH = '/api';

const OFFICIAL_RELEASE_CHANNELS: ReadonlyArray<OfficialReleaseChannel> = Object.freeze(['stable', 'canary']);

const OFFICIAL_CLIENT_API_HOST_CHANNELS: ReadonlyMap<string, OfficialReleaseChannel> = new Map(
	OFFICIAL_RELEASE_CHANNELS.flatMap(
		(channel): Array<[string, OfficialReleaseChannel]> => [
			[new URL(OFFICIAL_CLIENT_API_ENDPOINTS[channel]).host, channel],
			[OFFICIAL_MIGRATED_WEB_APP_HOSTS[channel], channel],
		],
	),
);

export function officialClientApiEndpointForAlias(apiEndpoint: string): string | null {
	let url: URL;
	try {
		url = new URL(apiEndpoint.trim());
	} catch {
		return null;
	}
	if (url.protocol !== 'https:' || url.username || url.password || url.search || url.hash) {
		return null;
	}
	if (url.pathname.replace(/\/+$/u, '') !== OFFICIAL_CLIENT_API_PATH) {
		return null;
	}
	const channel = OFFICIAL_CLIENT_API_HOST_CHANNELS.get(url.host);
	return channel === undefined ? null : OFFICIAL_CLIENT_API_ENDPOINTS[channel];
}

export const OFFICIAL_INSTANCE_HOSTS: ReadonlyArray<string> = Object.freeze([
	OFFICIAL_INSTANCE_DISPLAY_HOST,
	'web.fluxer.app',
	'api.fluxer.app',
	'canary.fluxer.app',
	'web.canary.fluxer.app',
	'api.canary.fluxer.app',
	'fluxer.com',
	'canary.fluxer.com',
]);

export function isOfficialInstanceHost(value: string): boolean {
	const origin = normalizeHTTPNetworkOrigin(value);
	if (origin == null) {
		return false;
	}
	const host = origin.slice(origin.indexOf('//') + 2).toLowerCase();
	return OFFICIAL_INSTANCE_HOSTS.some((officialHost) => officialHost === host);
}
