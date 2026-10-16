// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {buildMediaProxyURL, type MediaProxyOptions} from '@app/features/messaging/utils/MediaProxyUtils';
import {resolveDocumentURLFromRoot} from '@app/features/platform/URLOriginUtils';

type QueryParamPrimitive = string | number | boolean;
type QueryParamValue = QueryParamPrimitive | null | undefined;

const ABSOLUTE_URL_PATTERN = /^[a-zA-Z][a-zA-Z\d+\-.]*:/;

function applyQueryParams(url: URL, query: Record<string, QueryParamValue>) {
	for (const [key, value] of Object.entries(query)) {
		if (value === null || value === undefined) {
			continue;
		}
		url.searchParams.set(key, String(value));
	}
}

export function setUrlQueryParams(urlOrPath: string, query: Record<string, QueryParamValue>): string {
	const isAbsoluteUrl = ABSOLUTE_URL_PATTERN.test(urlOrPath) || urlOrPath.startsWith('//');
	const url = resolveDocumentURLFromRoot(urlOrPath);
	applyQueryParams(url, query);
	if (isAbsoluteUrl) {
		return url.toString();
	}
	return `${url.pathname}${url.search}${url.hash}`;
}

export function setPathQueryParams(path: string, query: Record<string, QueryParamValue>): string {
	const url = resolveDocumentURLFromRoot(path);
	applyQueryParams(url, query);
	const hasLeadingSlash = path.startsWith('/');
	const normalizedPath = hasLeadingSlash ? url.pathname : url.pathname.replace(/^\//, '');
	return `${normalizedPath}${url.search}${url.hash}`;
}

export function mediaUrl(path: string, options?: MediaProxyOptions): string {
	return buildMediaProxyURL(`${RuntimeConfig.mediaEndpoint}/${path}`, options);
}

export function webhookUrl(webhookId: string, token: string): string {
	return `${RuntimeConfig.apiPublicEndpoint}/webhooks/${webhookId}/${token}`;
}

export function marketingUrl(path: string): string {
	return `${RuntimeConfig.marketingEndpoint}/${path}`;
}

export function adminUrl(path: string): string {
	return `${RuntimeConfig.adminEndpoint}/${path}`;
}

export function webAppUrl(path: string): string {
	const normalizedBase = RuntimeConfig.webAppBaseUrl.replace(/\/+$/, '');
	const normalizedPath = path.replace(/^\/+/, '');
	return normalizedPath ? `${normalizedBase}/${normalizedPath}` : normalizedBase;
}
